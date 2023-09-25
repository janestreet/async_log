open! Core
open! Async_kernel
open! Import

type t = Output.t

(** [stdout] defaults to [format=`Text] *)
val stdout : ?format:Output_format.t -> unit -> t

(** [stderr] defaults to [format=`Text] *)
val stderr : ?format:Output_format.t -> unit -> t

val writer : Output_format.t -> Writer.t -> t

(** The [perm] argument is passed through to [Writer.open_file], and so has the default
    behavior described there. *)
val file : ?perm:Unix.file_perm -> Output_format.t -> filename:string -> t

val rotating_file
  :  ?perm:Unix.file_perm
  -> ?time_source:Synchronous_time_source.t
  -> ?log_on_rotation:(unit -> Message.t list)
  -> Output_format.t
  -> basename:string
  -> ?suffix:string (** defaults to [".log"] *)
  -> Rotation.t
  -> t

(** Returns a tail of the filenames. When [rotate] is called, the previous filename is
    put on the tail *)
val rotating_file_with_tail
  :  ?perm:Unix.file_perm
  -> ?time_source:Synchronous_time_source.t
  -> ?log_on_rotation:(unit -> Message.t list)
  -> Output_format.t
  -> basename:string
  -> ?suffix:string (** defaults to [".log"] *)
  -> Rotation.t
  -> t * string Tail.t

(** See {!Log_extended} for syslog and colorized console output. *)
