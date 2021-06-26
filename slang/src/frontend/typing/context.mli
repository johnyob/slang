open Base
open Syntax.Frontend.Parsed
open Syntax.Frontend.Type

module type S = sig
  type k

  type t

  val empty : t

  val singleton : k -> Scheme.t -> t

  val of_alist : (k * Scheme.t) list -> (t, [ `Duplicate_key of k ]) Result.t

  val extend : t -> k -> Scheme.t -> t

  val extends : t -> t -> t

  val domain : t -> k list

  val range : t -> Scheme.t list

  val remove : t -> k -> t

  val find : t -> k -> Scheme.t option

  val map : t -> f:(Scheme.t -> Scheme.t) -> t

  val fold : t -> init:'a -> f:(key:k -> data:Scheme.t -> 'a -> 'a) -> 'a

  val find_a_dup : t list -> k option
end

module Make (T : Comparator.S) : S with type k = T.t

module Variable : S with type k = Lid.located

module Constructor : S with type k = Uid.located
