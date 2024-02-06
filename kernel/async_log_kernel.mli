open! Core
open! Async_kernel
module Global = Global
module Level = Level
module Log = Log
module Message = Message
module Message_event = Message_event

module Output : sig
  type t = Output.t

  (** [create f] returns a [t], given a function that actually performs the final output
      work. It is the responsibility of the write function to contain all state, and to
      clean up after itself when it is garbage collected (which may require a finalizer).
      The function should avoid modifying the contents of the queue; it's reused for each
      [Output.t].

      [flush] should return a deferred that is fulfilled only when all previously written
      messages are durable (e.g., on disk, out on the network, etc.). It is automatically
      called by [Log] on shutdown and at garbage collection. It can be called manually by
      calling [Log.flushed t].

      The [unit Deferred] returned by the function provides an opportunity for pushback if
      that is important.  Only one batch of messages will be "in flight" at any time based
      on this deferred.

      An optional [rotate] function may be given which will be called when [Log.rotate t]
      is called while this output is in effect.  This is useful for programs that want
      very precise control over rotation.

      [finalize] will be called when the output is finalized (meaning, no more open logs
      use the output, and references to the output are gone). *)
  val create
    :  ?rotate:(unit -> unit Deferred.t)
    -> ?finalize:(unit -> unit Deferred.t)
    -> flush:(unit -> unit Deferred.t)
    -> (Message.t Queue.t -> unit Deferred.t)
    -> t

  val create_expert
    :  ?rotate:(unit -> unit Deferred.t)
    -> ?finalize:(unit -> unit Deferred.t)
    -> flush:(unit -> unit Deferred.t)
    -> (Message_event.t Queue.t -> unit Deferred.t)
    -> t

  (** A version of [create] that writes without buffering internally and processing in a
      background loop as outputs usually do, and has access to richer information in the
      write callback. *)
  val create_unbuffered
    :  ?finalize:(unit -> unit Deferred.t)
    -> flush:(unit -> unit Deferred.t)
    -> (Message_event.t -> unit)
    -> t

  (** [filter_to_level] wraps an output and gives you a new output which only
      logs messages which are as/more verbose than [level].

      This functionality is intended for when you have multiple outputs being displayed in
      different places, and they need to be at different levels.

      If you have one output (or multiple outputs all at the same level), it is better to
      set the [Log.t]'s output directly with [set_level], which is equivalent and more
      efficient. *)
  val filter_to_level : t -> level:Level.t -> t

  val empty : t

  module Format = Output_format
end

module For_testing : sig
  module Mutable_outputs = Mutable_outputs
end
