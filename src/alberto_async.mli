open Async.Std

module Make(A: (module type of Alberto)) : sig
  type t = A.t Deferred.t

  val create: A.t -> t

  val encode: t -> (string option) Deferred.t

  val decode: string Deferred.t -> (t option) Deferred.t

  val encode_exn: t -> string Deferred.t

  val decode_exn: string Deferred.t -> t

  val to_string: t -> string Deferred.t

  val read_term: Reader.t -> t

  val write_term: Writer.t -> t -> unit Deferred.t
end

