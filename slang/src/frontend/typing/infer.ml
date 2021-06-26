open Base
open Syntax.Frontend.Type
open Syntax.Frontend.Parsed
module Located = Syntax.Frontend.Located
module Location = Located.Location
module S = Substitutable
module Substitution = S.Substitution

module Unify = struct
  type error = OccursCheck of (Var.t * Type.t) | Unknown of (Type.t * Type.t)
  [@@deriving eq, show]

  let rec unify t1 t2 =
    let open Result in
    let open Type in
    match (t1, t2) with
    | TApp (l1, r1), TApp (l2, r2) ->
        unify l1 l2 >>= fun s1 ->
        unify (S.Type.apply s1 r1) (S.Type.apply s1 r2) >>= fun s2 ->
        return (Substitution.compose s2 s1)
    | TVar tv1, TVar tv2 when Var.equal tv1 tv2 -> return Substitution.empty
    | TCon c1, TCon c2 when Constructor.equal c1 c2 -> return Substitution.empty
    | TVar tv, t | t, TVar tv ->
        if List.mem ~equal:Var.equal (S.Type.free_vars t) tv then
          fail (OccursCheck (tv, t))
        else return (Substitution.singleton tv t)
    | _, _ -> fail (Unknown (t1, t2))
end

module Constraint = struct
  type t = Type.t * Type.t [@@deriving eq, show]

  type error = Unify.error [@@deriving eq, show]

  let apply subst (t1, t2) = (S.Type.apply subst t1, S.Type.apply subst t2)

  let rec solve subst cs =
    let open Result in
    match cs with
    | [] -> Ok subst
    | (t1, t2) :: cs ->
        Unify.unify t1 t2 >>= fun s ->
        solve (Substitution.compose s subst) (List.map ~f:(apply s) cs)
end

module Infer = struct
  type error =
    | UnboundVariable of Lid.located
    | UnboundConstructor of Uid.located
    | DuplicateBinding of Lid.located
    | SolverError of Constraint.error
    | Unknown of Location.t
  [@@deriving eq, show]

  let n = ref (-1)

  let reset () = n := -1

  let fresh_var k =
    n := !n + 1;
    Type.TVar (!n, k)

  let generalize vctx t =
    let open Scheme in
    let vctx_fvs = S.Context.Variable.free_vars vctx in
    let tvs =
      S.Type.free_vars t
      |> List.filter ~f:(fun tv -> not (List.mem ~equal:Var.equal vctx_fvs tv))
    in
    Forall (tvs, t)

  let instantiate Scheme.(Forall (tvs, t)) =
    let s =
      List.fold_right ~init:Substitution.empty
        ~f:(fun tv ->
          let s = Substitution.singleton tv (fresh_var (Var.kind tv)) in
          Substitution.compose s)
        tvs
    in
    S.Type.apply s t

  type context = {
    variable : Context.Variable.t;
    constructor : Context.Constructor.t;
  }
  [@@deriving lens]

  module T = struct
    type 'a t = context -> ('a * Constraint.t list, error) Result.t
  end

  include T

  (* TODO: Refactor to a RWST Monad using an external lib. Unfortunately no library implements these... guess I'll have to make one lol *)
  include Monad.Make (struct
    include T

    let return x _ = Ok (x, [])

    let bind m ~f =
      let open Result in
      fun ctx ->
        m ctx >>= fun (x1, cs1) ->
        f x1 ctx >>= fun (x2, cs2) -> return (x2, cs1 @ cs2)

    let map = `Define_using_bind
  end)

  let run m ctx = m ctx

  let lift m =
    let open Result in
    fun _ -> m >>| fun x -> (x, [])

  let fail err _ = Error err

  let ask ctx = Ok (ctx, [])

  let listen m ctx =
    let open Result in
    run m ctx >>= fun (x, cs) -> return ((x, cs), cs)

  open Lens.Infix

  let context ~lens ctx = Ok (ctx |. lens, [])

  let local ~lens ~f m ctx = m ((lens ^%= f) @@ ctx)

  let ( >| ) m f lens = local m ~f ~lens

  let gaurd b ~error = if b then fail error else return ()

  let unify t1 t2 _ = Ok ((), [ (t1, t2) ])

  let extend (x, s) m =
    (m >| fun ctx -> Context.Variable.extend ctx x s) @@ context_variable

  let extends ctx2 m =
    (m >| fun ctx1 -> Context.Variable.extends ctx1 ctx2) @@ context_variable

  let guard_dups ctxs =
    Context.Variable.find_a_dup ctxs
    |> Option.value_map ~default:(return ()) ~f:(fun k ->
           fail (DuplicateBinding k))

  let gaurd_dup ctx2 =
    context ~lens:context_variable >>= fun ctx1 -> guard_dups [ ctx1; ctx2 ]

  (* TODO: Remove duplication! *)
  let find_variable lid =
    context ~lens:context_variable >>= fun ctx ->
    lift
      ( Context.Variable.find ctx lid
      |> Result.of_option ~error:(UnboundVariable lid) )
    >>| instantiate

  let find_constructor uid =
    context ~lens:context_constructor >>= fun ctx ->
    lift
      ( Context.Constructor.find ctx uid
      |> Result.of_option ~error:(UnboundConstructor uid) )
    >>| instantiate

  let substitution m =
    listen m >>= fun (x, cs) ->
    lift
      ( Constraint.solve Substitution.empty cs
      |> Result.map_error ~f:(fun err -> SolverError err) )
    >>| fun s -> (x, s)

  let context_of_alist alist =
    lift
      ( Context.Variable.of_alist alist
      |> Result.map_error ~f:(fun (`Duplicate_key k) -> DuplicateBinding k) )

  open Type
  open Kind

  let infer_lit (_, lit) =
    let open Literal in
    match lit with
    | LInt _ -> return int
    | LBool _ -> return bool
    | LUnit -> return unit

  let rec infer_pat (_, pat) =
    let open Pattern in
    match pat with
    | PCon (uid, ps) ->
        infer_pats ps >>= fun (ctx, ts) ->
        find_constructor uid >>= fun t ->
        let tv = fresh_var KStar in
        unify t (List.fold_right ts ~init:tv ~f:( @-> )) >>= fun _ ->
        return (ctx, tv)
    | PVar lid ->
        let tv = fresh_var KStar in
        return (Context.Variable.singleton lid (Scheme.from_type tv), tv)
    | PLit lit -> infer_lit lit >>| fun t -> (Context.Variable.empty, t)
    | PWild -> return (Context.Variable.empty, fresh_var KStar)

  and infer_pats ps =
    all (List.map ~f:infer_pat ps) >>= fun ctxs_ts ->
    let ctxs, ts = List.unzip ctxs_ts in
    guard_dups ctxs >>= fun () ->
    let ctx =
      List.fold_right ~init:Context.Variable.empty ~f:Context.Variable.extends
        ctxs
    in
    return (ctx, ts)

  let rec infer_alt (ps, e) =
    infer_pats ps >>= fun (ctx, ts) ->
    gaurd_dup ctx >>= fun () ->
    extends ctx (infer_expr e) >>= fun t ->
    return (List.fold_right ~init:t ~f:( @-> ) ts)

  and infer_alts alts t =
    all (List.map ~f:infer_alt alts) >>= fun ts ->
    ts |> List.map ~f:(unify t) |> all_unit

  and infer_branches bs t =
    infer_alts (List.map bs ~f:(fun (_, (p, e)) -> ([ p ], e))) t

  and infer_binding (_, b) =
    let open Binding in
    match b with
    | BValue (lid, e) -> infer_expr e >>| fun t -> (lid, t)
    | BFunction (lid, ps, e) -> infer_alt (ps, e) >>| fun t -> (lid, t)

  and infer_bindings b_ts =
    all
      (List.map b_ts ~f:(fun (b, t2) ->
           infer_binding b >>= fun (lid, t1) ->
           unify t1 t1 >>= fun () -> return (lid, t2)))

  and infer_binder (_, br) =
    let open Binder in
    match br with
    | NonRec b ->
        substitution (infer_binding b) >>= fun ((lid, t), subst) ->
        context ~lens:context_variable >>= fun ctx ->
        let sc =
          generalize (S.Context.Variable.apply subst ctx) (S.Type.apply subst t)
        in
        return (Context.Variable.singleton lid sc)
    | Rec bs ->
        let b_tvs = List.map ~f:(fun b -> (b, fresh_var KStar)) bs in
        context_of_alist
          ( b_tvs
          |> List.map ~f:(fun ((_, b), tv) ->
                 (Binding.identifier b, Scheme.from_type tv)) )
        >>= fun ctx ->
        substitution (extends ctx (infer_bindings b_tvs))
        >>= fun (lid_ts, subst) ->
        context ~lens:context_variable >>= fun ctx ->
        context_of_alist
          ( lid_ts
          |> List.map ~f:(fun (lid, t) ->
                 ( lid,
                   generalize
                     (S.Context.Variable.apply subst ctx)
                     (S.Type.apply subst t) )) )

  and infer_expr (loc, expr) =
    let open Expr in
    match expr with
    | EVar lid -> find_variable lid
    | ECon uid -> find_constructor uid
    | ELit lit -> infer_lit lit
    | EApp (e1, e2) ->
        infer_expr e1 >>= fun t1 ->
        infer_expr e2 >>= fun t2 ->
        let tv = fresh_var KStar in
        unify t1 (t2 @-> tv) >>= fun () -> return tv
    | ELam (ps, e) -> infer_alt (ps, e)
    | EFix (f, ps, e) ->
        let tv = fresh_var KStar in
        let sc = Scheme.from_type tv in
        extend (f, sc) (infer_alt (ps, e)) >>= fun t ->
        unify tv t >>= fun () -> return tv
    | ELet (b, e) -> infer_binder b >>= fun ctx -> extends ctx (infer_expr e)
    | ECase (e1, bs) ->
        infer_expr e1 >>= fun t ->
        let tv = fresh_var KStar in
        infer_branches bs (t @-> tv) >>= fun _ -> return tv
    | EIf (e1, e2, e3) ->
        infer_expr e1 >>= fun t1 ->
        infer_expr e2 >>= fun t2 ->
        infer_expr e3 >>= fun t3 ->
        unify t1 bool >>= fun () ->
        unify t2 t3 >>= fun () -> return t2
    | EWhile (e1, e2) ->
        infer_expr e1 >>= fun t1 ->
        infer_expr e2 >>= fun t2 ->
        unify t1 bool >>= fun () ->
        unify t2 unit >>= fun () -> return unit
    | ESeq es -> (
        match List.rev es with
        | [] -> fail (Unknown loc)
        | en :: es ->
            List.fold_left es ~init:(infer_expr en) ~f:(fun m e ->
                infer_expr e >>= unify unit >>= fun () -> m) )
end
