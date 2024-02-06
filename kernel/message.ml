(** Log messages are stored, starting with V2, as an explicit version followed by the
    message itself. This makes it easier to move the message format forward while still
    allowing older logs to be read by the new code.

    If you make a new version you must add a version to the Version module below and
    should follow the Make_versioned_serializable pattern.
*)
module Stable = struct
  open Core.Core_stable

  module T2 = struct
    type ('time, 'message) t =
      { time : 'time
      ; level : Level.Stable.V1.t option
      ; message : 'message
      ; tags : (string * string) list
      }
    [@@deriving bin_io, sexp]

    let map_message t ~f = { t with message = f t.message }
  end

  module T1 = struct
    (* this is the serialization scheme in 111.18 (2014) and before *)
    module V0 = struct
      type 'time t = ('time, string) T2.t [@@deriving bin_shape, sexp]

      let%expect_test "bin_digest Message.V1.t" =
        print_endline [%bin_digest: unit t];
        [%expect {| 33933efbb30917a1607deebcdb187fc6 |}]
      ;;
    end

    module V2 = struct
      include Versioned.Stable.Make (struct
        type 'time t = ('time, Sexp_or_string.Stable.V1.t) T2.t [@@deriving bin_io, sexp]

        let%expect_test "bin_digest Message.V2" =
          print_endline [%bin_digest: unit t];
          [%expect {| 26b02919ac3971aaace97169310e9d15 |}]
        ;;

        let version = Versioned.Stable.Version.V2
      end)

      let of_v0 = T2.map_message ~f:(fun m -> `String m)
      let to_v0 = T2.map_message ~f:Sexp_or_string.Stable.V1.to_string

      (* this allows for automagical reading of any versioned sexp, so long as we can always
         lift to a Message.t *)
      let t_of_sexp time_of_sexp (sexp : Core.Sexp.t) =
        match sexp with
        | List (List (Atom "time" :: _) :: _) -> V0.t_of_sexp time_of_sexp sexp |> of_v0
        | List [ (Atom _ as version); _ ] ->
          (match Versioned.Stable.Version.t_of_sexp version with
           | V2 -> t_of_sexp time_of_sexp sexp)
        | _ ->
          Core.failwithf !"Log.Message.t_of_sexp: malformed sexp: %{Core.Sexp}" sexp ()
      ;;

      module For_testing = struct
        type 'time t_as_v0 = 'time t

        let sexp_of_t_as_v0 sexp_of_time t = [%sexp (to_v0 t : time V0.t)]
      end
    end
  end
end

open! Core
open! Async_kernel
open! Import

module T1 = struct
  type 'time t = 'time Stable.T1.V2.t [@@deriving sexp_of]
end

type t = Time_float.t T1.t

let create_raw ?level ~time ?(tags = []) message : t = { time; level; message; tags }

let create ?level ?time ?tags message =
  let time = Option.value_or_thunk time ~default:Time_float.now in
  create_raw ?level ~time ?tags message
;;

let time (t : t) = t.time
let level (t : t) = t.level
let set_level (t : t) level = { t with level }
let raw_message (t : t) = t.message
let message (t : t) = Sexp_or_string.Stable.V1.to_string (raw_message t)
let tags (t : t) = t.tags
let add_tags (t : t) tags = { t with tags = List.rev_append tags t.tags }

let to_write_only_text (t : t) zone =
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
    (Time_float.to_string_abs ~zone t.time :: " " :: prefix :: message t :: formatted_tags)
;;
