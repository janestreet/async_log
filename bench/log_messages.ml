open! Core
open! Async

let log =
  Log.create
    ()
    ~level:`Info
    ~output:[ Log.Output.file `Text ~filename:"/dev/null" ]
    ~on_error:`Raise
;;

let%bench "unused" = [%log.debug log "blah"]
let%bench "used printf" = [%log.format log "blah"]
let%bench "used printf w/subst" = [%log.format log "%s" "blah"]
let%bench "used string" = [%log.string log "blah"]
let%bench "used sexp" = [%log log ("blah" : string)]
let%bench "used message" = [%log log "blah"]
let%bench "flush" = Log.flushed log
