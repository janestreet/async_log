open! Core
open! Async_kernel
open! Import
include module type of Log
module Blocking = Blocking
module Global = Global

module type Global_intf = Global.S

module Level = Level
module Make_global = Global.Make

module Message : sig
  type t = Message.t [@@deriving sexp_of]

  val create
    :  ?level:Level.t
    -> ?time:Time.t
    -> ?time_source:Synchronous_time_source.t
         (** [time_source] is used to provide a default time, if none is specified *)
    -> ?tags:(string * string) list
    -> [ `String of string | `Sexp of Sexp.t ]
    -> t

  val time : t -> Time.t
  val message : t -> string
  val raw_message : t -> [ `String of string | `Sexp of Sexp.t ]
  val level : t -> Level.t option
  val set_level : t -> Level.t option -> t
  val tags : t -> (string * string) list
  val add_tags : t -> (string * string) list -> t

  module Stable : sig
    module V0 : sig
      (** [V0.bin_t] is the [Message.bin_t] in jane-111.18 and before *)
      type nonrec t = t [@@deriving bin_io, sexp]
    end

    module V2 : sig
      type nonrec t = t [@@deriving bin_io, sexp]
    end
  end

  module Private : module type of Message
end

module Output : sig
  type t = Output.t

  (** [create f] returns a [t], given a function that actually performs the final output
      work. It is the responsibility of the write function to contain all state, and to
      clean up after itself when it is garbage collected (which may require a finalizer).
      The function should avoid modifying the contents of the queue; it's reused for each
      [Output.t].

      The "stock" output modules support a sexp and bin_prot output format, and other
      output modules should make efforts to support them as well where it is
      meaningful/appropriate to do so.

      [flush] should return a deferred that is fulfilled only when all previously written
      messages are durable (e.g., on disk, out on the network, etc.).  It is automatically
      called on shutdown by [Log], but isn't automatically called at any other time.  It
      can be called manually by calling [Log.flushed t].

      The [unit Deferred] returned by the function provides an opportunity for pushback if
      that is important.  Only one batch of messages will be "in flight" at any time based
      on this deferred.

      An optional [rotate] function may be given which will be called when [Log.rotate t]
      is called while this output is in effect.  This is useful for programs that want
      very precise control over rotation.

      If [close] is provided it will be called when the log falls out of scope. (Note that
      it is not called directly, even if you close a log which is using this output,
      because outputs are sometimes reused.)  *)
  val create
    :  ?rotate:(unit -> unit Deferred.t)
    -> ?close:(unit -> unit Deferred.t)
    -> flush:(unit -> unit Deferred.t)
    -> (Message.t Queue.t -> unit Deferred.t)
    -> t

  (** [filter_to_level] wraps an output and gives you a new output which only
      logs messages which are as/more verbose than [level].

      This functionality is intended for when you have multiple outputs being displayed in
      different places, and they need to be at different levels.

      If you have one output (or multiple outputs all at the same level), it is better to
      set the [Log.t]'s output directly with [set_level], which is equivalent and more
      efficient. *)
  val filter_to_level : t -> level:Level.t -> t

  include module type of Output_unix with type t := t
  module Format = Output_format
end

module Ppx_log_syntax : sig
  (** [Async_log.Ppx_log_syntax.Ppx_log_syntax] exists so that people can [open
      Async_log.Ppx_log_syntax] to use ppx log, instead of doing a module alias. This is
      consistent with [Monad.Syntax.Let_syntax]. *)
  module Ppx_log_syntax : module type of Ppx_log_syntax
end

module Reader = Message_reader

module Rotation : sig
  module type Id_intf = Rotation_id.S

  (** Description of boundaries for file rotation.

      If all fields are [None] the file will never be rotated.  Any field set to [Some]
      will cause rotation to happen when that boundary is crossed.  Multiple boundaries
      may be set.  Log rotation always causes incrementing rotation conditions (e.g.,
      size) to reset.

      The condition [keep] is special and does not follow the rules above.  When a log is
      rotated, [keep] is examined and logs that do not fall under its instructions are
      deleted.  This deletion takes place on rotation only, and so may not happen.  The
      meaning of keep options are:

      - [`All] -- never delete
      - [`Newer_than span] -- delete files with a timestamp older than [Time.sub (Time.now
        ()) span].  This normally means keeping files that contain at least one message
        logged within [span].  If [span] is short enough this option can delete a
        just-rotated file.
      - [`At_least i] -- keep the [i] most recent files

      Log rotation does not support symlinks, and you're encouraged to avoid them in
      production applications. Issues with symlinks:

      - You can't tail symlinks without being careful (e.g., you must remember to pass
        [-F] to [`tail`]).
      - Symlinks are hard to reason about when the program crashes, especially on
        startup (i.e., is the symlink pointing me at the right log file?).
      - Atomicity is hard.
      - Symlinks encourage tailing, which is a bad way to communicate information.
      - They complicate archiving processes (the symlink must be skipped). *)
  type t = Rotation.t [@@deriving sexp_of]

  val create
    :  ?messages:int
    -> ?size:Byte_units.t
    -> ?time:Time.Ofday.t
    -> ?zone:Time.Zone.t
    -> keep:[ `All | `Newer_than of Time.Span.t | `At_least of int ]
    -> naming_scheme:
         [ `Numbered | `Timestamped | `Dated | `User_defined of (module Id_intf) ]
    -> unit
    -> t

  (** Sane defaults for log rotation.

      Writes dated log files. Files are rotated every time the day changes in the given
      zone (uses the machine's zone by default). If the dated log file already exists,
      it's appended to.

      Logs are never deleted. Best practice is to have an external mechanism archive old
      logs for long-term storage. *)
  val default : ?zone:Time.Zone.t -> unit -> t
end
