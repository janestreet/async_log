open! Core
open! Import

include
  String_id.Make
    (struct
      let module_name = __MODULE__
    end)
    ()
