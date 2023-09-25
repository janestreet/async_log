(* Log messages are stored, starting with V2, as an explicit version followed by the
   message itself.  This makes it easier to move the message format forward while
   still allowing older logs to be read by the new code.

   If you make a new version you must add a version to the Version module below and
   should follow the Make_versioned_serializable pattern.
*)
module Stable = struct
  open Core.Core_stable
  open Import_stable

  module Version = struct
    type t = V2 [@@deriving bin_io, sexp, compare]

    let%expect_test "bin_digest Message.Version.V2" =
      print_endline [%bin_digest: t];
      [%expect {| 6ae8dff060dc8c96585060b4f76d2974 |}]
    ;;

    let ( <> ) t1 t2 = compare t1 t2 <> 0
    let to_string t = Core.Sexp.to_string (sexp_of_t t)
  end

  module type Versioned_serializable = sig
    type t [@@deriving bin_io, sexp]

    val version : Version.t
  end

  module Stable_message_common = struct
    module Serialized = struct
      type 'a t =
        { time : Time.V1.t
        ; level : Level.Stable.V1.t option
        ; message : 'a
        ; tags : (string * string) list
        }
      [@@deriving bin_io]
    end

    module T = struct
      type 'a t =
        { time : Time.V1.t
        ; level : Level.Stable.V1.t option
        ; message : 'a
        ; tags : (string * string) list
        ; is_from_upstream_log : bool
             [@sexp_drop_if Core.Fn.const true] [@sexp.default false]
        }
      [@@deriving
        sexp
        , stable_record
            ~version:[%stable: 'a Serialized.t]
            ~remove:[ is_from_upstream_log ]]

      let to_binable = to_Serialized_t
      let of_binable = of_Serialized_t ~is_from_upstream_log:false
    end

    (* We don't want to change the bin digest with the binable operation above, hence
       the use of [V1] here. *)
    include Binable.Of_binable1.V1 [@alert "-legacy"] (Serialized) (T)
    include T
  end

  module Make_versioned_serializable (T : Versioned_serializable) : sig
    type t [@@deriving bin_io, sexp]
  end
  with type t = T.t = struct
    type t = T.t
    type versioned_serializable = Version.t * T.t [@@deriving bin_io, sexp]

    let t_of_versioned_serializable (version, t) =
      if Version.( <> ) version T.version
      then
        Core.failwithf
          !"version mismatch %{Version} <> to expected version %{Version}"
          version
          T.version
          ()
      else t
    ;;

    let sexp_of_t t = sexp_of_versioned_serializable (T.version, t)

    let t_of_sexp sexp =
      let versioned_t = versioned_serializable_of_sexp sexp in
      t_of_versioned_serializable versioned_t
    ;;

    include
      Binable.Of_binable.V1 [@alert "-legacy"]
        (struct
          type t = versioned_serializable [@@deriving bin_io]
        end)
        (struct
          type t = T.t

          let to_binable t = T.version, t
          let of_binable versioned_t = t_of_versioned_serializable versioned_t
        end)
  end

  module V2 = Make_versioned_serializable (struct
    type t = Sexp_or_string.Stable.V1.t Stable_message_common.t [@@deriving bin_io, sexp]

    let%expect_test "bin_digest Message.V2" =
      print_endline [%bin_digest: t];
      [%expect {| 1dd2225c5392b6ac36b718ee2b1a08db |}]
    ;;

    let version = Version.V2
  end)

  (* this is the serialization scheme in 111.18 and before *)
  module V0 = struct
    type v0_t = string Stable_message_common.t [@@deriving bin_io, sexp]

    let%expect_test "bin_digest Message.V1.v0_t" =
      print_endline [%bin_digest: v0_t];
      [%expect {| d790de8237524f270360ccf1e56f7030 |}]
    ;;

    let v0_to_v2 (v0_t : v0_t) : V2.t =
      { time = v0_t.time
      ; level = v0_t.level
      ; message = `String v0_t.message
      ; tags = v0_t.tags
      ; is_from_upstream_log = v0_t.is_from_upstream_log
      }
    ;;

    let v2_to_v0 (v2_t : V2.t) : v0_t =
      { time = v2_t.time
      ; level = v2_t.level
      ; message = Sexp_or_string.Stable.V1.to_string v2_t.message
      ; tags = v2_t.tags
      ; is_from_upstream_log = v2_t.is_from_upstream_log
      }
    ;;

    include
      Binable.Of_binable.V1 [@alert "-legacy"]
        (struct
          type t = v0_t [@@deriving bin_io]
        end)
        (struct
          let to_binable = v2_to_v0
          let of_binable = v0_to_v2

          type t = Sexp_or_string.Stable.V1.t Stable_message_common.t
        end)

    let sexp_of_t t = sexp_of_v0_t (v2_to_v0 t)
    let t_of_sexp sexp = v0_to_v2 (v0_t_of_sexp sexp)

    type t = V2.t
  end

  module Versioned = struct
    type t = V2.t

    (* this allows for automagical reading of any versioned sexp, so long as we can always
       lift to a Message.t *)
    let t_of_sexp (sexp : Core.Sexp.t) =
      match sexp with
      | List (List (Atom "time" :: _) :: _) -> V0.t_of_sexp sexp
      | List [ (Atom _ as version); _ ] ->
        (match Version.t_of_sexp version with
         | V2 -> V2.t_of_sexp sexp)
      | _ -> Core.failwithf !"Log.Message.t_of_sexp: malformed sexp: %{Core.Sexp}" sexp ()
    ;;
  end
end

open! Core
open! Import
include Stable.V2

let create_raw ?level ~time ?(tags = []) message : t =
  { time; level; message; tags; is_from_upstream_log = false }
;;

let create_from_upstream_log message ~level ~time ~tags : t =
  { time; level = Some level; message; tags; is_from_upstream_log = true }
;;

let is_from_upstream_log (t : t) = t.is_from_upstream_log

let create ?level ?time ?time_source ?tags message =
  let time =
    match time with
    | Some time -> time
    | None -> Time.now ~time_source
  in
  create_raw ?level ~time ?tags message
;;

let time (t : t) = t.time
let level (t : t) = t.level
let set_level (t : t) level = { t with level }
let raw_message (t : t) = t.message
let message (t : t) = Sexp_or_string.Stable.V1.to_string (raw_message t)
let tags (t : t) = t.tags
let add_tags (t : t) tags = { t with tags = List.rev_append tags t.tags }

let to_write_only_text ?(zone = force Time.Zone.local) (t : t) =
  let prefix =
    match t.level with
    | None -> ""
    | Some l -> Level.to_string l ^ " "
  in
  let formatted_tags =
    match t.tags with
    | [] -> []
    | _ :: _ ->
      " --" :: List.concat_map t.tags ~f:(fun (t, v) -> [ " ["; t; ": "; v; "]" ])
  in
  String.concat
    ~sep:""
    (Time.to_string_abs ~zone t.time :: " " :: prefix :: message t :: formatted_tags)
;;
