open Base

module Position : sig
  type t = Position of (int * int) | NoPosition
  [@@deriving eq, show, ord, sexp]
end

module Span : sig
  type t = Position.t * Position.t [@@deriving eq, show, ord, sexp]

  val subset_of : t -> t -> bool

  val empty : t

  val ( <+> ) : t -> t -> t
end

module Location : sig
  type t = { file_path : string; span : Span.t }
  [@@deriving eq, show, ord, sexp]

  val empty : t

  val ( <+> ) : t -> t -> t
end

type 'a t = Location.t * 'a [@@deriving eq, show, ord, sexp]

val ( <@> ) : 'a t -> 'b t -> Location.t

val location : 'a t -> Location.t

val fold : 'a t list -> Location.t

include Monad.S with type 'a t := 'a t
