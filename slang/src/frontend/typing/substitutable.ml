open Base
module T = Syntax.Frontend.Type
module Ctx = Context

(********************************************************************************)
(* Substitutions                                                                *)
(********************************************************************************)

(* This will be extended to form the Substitution module later, but to avoid nasty recursive 
   modules I've split this module up since some of it's implementation depends on the Monotype
   module *)

module Substitution__ = struct
  module Key = struct
    include T.Var
    include Comparator.Make (T.Var)
  end

  type t = (Key.t, T.Type.t, Key.comparator_witness) Map.t

  let remove subst tv = Map.remove subst tv
  let find subst tv = Map.find subst tv
end

(********************************************************************************)
(* Substitutables                                                               *)
(********************************************************************************)

module type S = sig
  type t

  val apply : Substitution__.t -> t -> t
  val free_vars : t -> T.Var.t list
end

module Type = struct
  include T.Type

  let rec apply subst t =
    match t with
    | TCon c -> TCon c
    | TVar tv ->
      (match Substitution__.find subst tv with
      | Some t' -> t'
      | None -> t)
    | TApp (t1, t2) -> TApp (apply subst t1, apply subst t2)


  let rec free_vars t =
    match t with
    | TCon _ -> []
    | TVar tv -> [ tv ]
    | TApp (t1, t2) -> free_vars t1 @ free_vars t2 |> List.dedup_and_sort ~compare:T.Var.compare
end

module Scheme = struct
  include T.Scheme

  let apply subst (Forall (tvs, t)) =
    let subst' = List.fold_left ~f:Substitution__.remove ~init:subst tvs in
    Forall (tvs, Type.apply subst' t)


  let free_vars (Forall (tvs, t)) =
    let fvs = Type.free_vars t in
    tvs |> List.filter ~f:(fun tv -> not (List.mem ~equal:T.Var.equal fvs tv))
end

module Context = struct
  module Make (C : Ctx.S) : S with type t := C.t = struct
    let apply subst ctx = C.map ctx ~f:(Scheme.apply subst)

    let free_vars ctx =
      ctx
      |> C.fold ~f:(fun ~key:_ ~data:s acc -> Scheme.free_vars s :: acc) ~init:[]
      |> List.join
      |> List.dedup_and_sort ~compare:T.Var.compare
  end

  module Variable : S with type t := Ctx.Variable.t = Make (Ctx.Variable)
  module Constructor : S with type t := Ctx.Constructor.t = Make (Ctx.Constructor)
end

(* Now extend Substitution__ to make Substitution *)

module Substitution = struct
  include Substitution__

  let empty = Map.empty (module Key)
  let singleton tv t = Map.singleton (module Key) tv t

  let compose s1 s2 =
    Map.merge_skewed ~combine:(fun ~key:_ v1 _ -> v1) s1 (Map.map s2 ~f:(Type.apply s1))


  let of_alist kvs =
    match Map.of_alist (module Key) kvs with
    | `Ok m -> Some m
    | `Duplicate_key _ -> None
end
