(** Log messages are stored, starting with V2, as an explicit version followed by the
    message itself. This makes it easier to move the message format forward while still
    allowing older logs to be read by the new code.

    If you make a new version you must add a version to the Version module below and
    should follow the Make_versioned_serializable pattern.
*)
module Stable = struct
  open Core.Core_stable
  open Import_stable

  module T1 = struct
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

    let map_message t ~f = { t with message = f t.message }
  end

  (* this is the serialization scheme in 111.18 (2014) and before *)
  module V0 = struct
    type t = string T1.t [@@deriving bin_shape, sexp]

    let%expect_test "bin_digest Message.V1.t" =
      print_endline [%bin_digest: t];
      [%expect {| d790de8237524f270360ccf1e56f7030 |}]
    ;;
  end

  module V2 = struct
    include Versioned.Stable.Make (struct
      type t = Sexp_or_string.Stable.V1.t T1.t [@@deriving bin_io, sexp]

      let%expect_test "bin_digest Message.V2" =
        print_endline [%bin_digest: t];
        [%expect {| 1dd2225c5392b6ac36b718ee2b1a08db |}]
      ;;

      let version = Versioned.Stable.Version.V2
    end)

    let of_v0 = T1.map_message ~f:(fun m -> `String m)
    let to_v0 = T1.map_message ~f:Sexp_or_string.Stable.V1.to_string

    (* this allows for automagical reading of any versioned sexp, so long as we can always
       lift to a Message.t *)
    let t_of_sexp (sexp : Core.Sexp.t) =
      match sexp with
      | List (List (Atom "time" :: _) :: _) -> V0.t_of_sexp sexp |> of_v0
      | List [ (Atom _ as version); _ ] ->
        (match Versioned.Stable.Version.t_of_sexp version with
         | V2 -> t_of_sexp sexp)
      | _ -> Core.failwithf !"Log.Message.t_of_sexp: malformed sexp: %{Core.Sexp}" sexp ()
    ;;

    module For_testing = struct
      let sexp_of_t_as_v0 t = [%sexp (to_v0 t : V0.t)]
    end
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
