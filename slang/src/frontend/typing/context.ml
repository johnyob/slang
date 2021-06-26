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

module Make (Key : Comparator.S) : S with type k = Key.t = struct
  type k = Key.t

  type t = (Key.t, Scheme.t, Key.comparator_witness) Map.t

  let empty = Map.empty (module Key)

  let singleton x = Map.singleton (module Key) x

  let of_alist kvs =
    match Map.of_alist (module Key) kvs with
    | `Ok m -> Ok m
    | `Duplicate_key k -> Error (`Duplicate_key k)

  let extend ctx id t = Map.set ctx ~key:id ~data:t

  let extends ctx1 ctx2 =
    Map.merge_skewed ctx1 ctx2 ~combine:(fun ~key:_ _ v -> v)

  let domain ctx = Map.keys ctx

  let range ctx = Map.data ctx

  let remove ctx id = Map.remove ctx id

  let find ctx tv = Map.find ctx tv

  let map ctx ~f = Map.map ctx ~f

  let fold ctx ~init ~f = Map.fold ctx ~init ~f

  let find_a_dup ctxs =
    ctxs |> List.map ~f:domain |> List.join
    |> List.find_a_dup ~compare:Key.comparator.compare
end

module Variable : S with type k = Lid.located = Make (struct
  module T = struct
    type t = Lid.located [@@deriving ord, sexp]
  end

  include T
  include Comparable.Make (T)
end)

module Constructor : S with type k = Uid.located = Make (struct
  module T = struct
    type t = Uid.located [@@deriving ord, sexp]
  end

  include T
  include Comparable.Make (T)
end)
