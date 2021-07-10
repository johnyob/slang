open Base
open Syntax.Frontend.Type
open Syntax.Frontend.Parsed
module Located = Syntax.Frontend.Located
module Location = Located.Location
module S = Substitutable
module Substitution = S.Substitution

module Unify = struct
  type error =
    | OccursCheck of (Var.t * Type.t)
    | Unknown of (Type.t * Type.t)
  [@@deriving eq, show]

  let rec unify t1 t2 =
    let open Result.Let_syntax in
    let open Type in
    match t1, t2 with
    | TApp (l1, r1), TApp (l2, r2) ->
      let%bind s1 = unify l1 l2 in
      let%bind s2 = unify (S.Type.apply s1 r1) (S.Type.apply s1 r2) in
      return (Substitution.compose s2 s1)
    | TVar tv1, TVar tv2 when Var.equal tv1 tv2 -> return Substitution.empty
    | TCon c1, TCon c2 when Constructor.equal c1 c2 -> return Substitution.empty
    | TVar tv, t | t, TVar tv ->
      if List.mem ~equal:Var.equal (S.Type.free_vars t) tv
      then Result.fail (OccursCheck (tv, t))
      else return (Substitution.singleton tv t)
    | _, _ -> Result.fail (Unknown (t1, t2))
end

module Constraint = struct
  type t = Type.t * Type.t [@@deriving eq, show]
  type error = Unify.error [@@deriving eq, show]

  let apply subst (t1, t2) = S.Type.apply subst t1, S.Type.apply subst t2

  let rec solve subst cs =
    let open Result.Let_syntax in
    match cs with
    | [] -> return subst
    | (t1, t2) :: cs ->
      let%bind s = Unify.unify t1 t2 in
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

  type 'a t = Context.t -> ('a * Constraint.t list, error) Result.t

  let run m ctx = m ctx

  module Infer_monad = struct
    include Monad.Make (struct
      type nonrec 'a t = 'a t

      let return x _ = Ok (x, [])

      let bind m ~f =
        let open Result.Let_syntax in
        fun ctx ->
          let%bind x1, cs1 = run m ctx in
          let%bind x2, cs2 = run (f x1) ctx in
          return (x2, cs1 @ cs2)


      let map = `Define_using_bind
    end)

    let lift m =
      let open Result.Let_syntax in
      fun _ ->
        let%bind x = m in
        return (x, [])


    let fail err _ = Error err

    (* let gaurd b ~error = if b then fail error else return () *)
    let ask ctx = Ok (ctx, [])
    let local ~f m ctx = run m (f ctx)

    (* let writer (x, cs) _ = Ok (x, cs) *)
    let tell cs _ = Ok ((), cs)

    let listen m =
      let open Result.Let_syntax in
      fun ctx ->
        let%bind x, cs = run m ctx in
        return ((x, cs), cs)

    (* let listens m ~f =
      let open Result.Let_syntax in
      fun ctx ->
        let%bind x, cs = run m ctx in
        return ((x, f cs), cs) *)

    (* let censor m ~f =
      let open Result.Let_syntax in
      fun ctx ->
        let%bind x, cs = run m ctx in
        return (x, f cs) *)
  end

  open Infer_monad
  open Lens.Infix

  let n = ref (-1)

  (* let reset () = n := -1 *)

  let fresh () =
    n := !n + 1;
    !n


  open Scheme
  open Type
  open Kind

  let fresh_var k = Type.TVar (fresh (), k)

  let generalize vctx t =
    let open Scheme in
    let vctx_fvs = S.Context.Variable.free_vars vctx in
    let tvs =
      S.Type.free_vars t |> List.filter ~f:(fun tv -> not (List.mem ~equal:Var.equal vctx_fvs tv))
    in
    Forall (tvs, t)


  let instantiate =
    let open Scheme in
    fun (Forall (tvs, t)) ->
      let subst =
        List.fold_right
          ~init:Substitution.empty
          ~f:(fun tv acc ->
            let subst = Substitution.singleton tv (fresh_var (Var.kind tv)) in
            Substitution.compose subst acc)
          tvs
      in
      S.Type.apply subst t


  let unify t1 t2 = tell [ t1, t2 ]
  let context ~lens = ask >>| fun ctx -> ctx |. lens
  let ( >| ) m f lens = local m ~f:(fun ctx -> (lens ^%= f) @@ ctx)
  let extend (x, s) m = (m >| fun ctx -> Context.Variable.extend ctx x s) @@ Context.variable
  let extends ctx2 m = (m >| fun ctx1 -> Context.Variable.extends ctx1 ctx2) @@ Context.variable

  let guard_dups ctxs =
    Context.Variable.find_a_dup ctxs
    |> Option.value_map ~default:(return ()) ~f:(fun k -> fail (DuplicateBinding k))


  let gaurd_dup ctx2 =
    let open Infer_monad.Let_syntax in
    let%bind ctx1 = context ~lens:Context.variable in
    guard_dups [ ctx1; ctx2 ]


  let find_variable lid =
    let open Infer_monad.Let_syntax in
    let%bind ctx = context ~lens:Context.variable in
    lift (Context.Variable.find ctx lid |> Result.of_option ~error:(UnboundVariable lid))
    >>| instantiate


  let find_constructor uid =
    let open Infer_monad.Let_syntax in
    let%bind ctx = context ~lens:Context.constructor in
    lift (Context.Constructor.find ctx uid |> Result.of_option ~error:(UnboundConstructor uid))
    >>| instantiate


  let substitution m =
    let open Infer_monad.Let_syntax in
    let%bind x, cs = listen m in
    lift (Constraint.solve Substitution.empty cs |> Result.map_error ~f:(fun err -> SolverError err))
    >>| fun s -> x, s


  let context_of_alist alist =
    lift
      (Context.Variable.of_alist alist
      |> Result.map_error ~f:(fun (`Duplicate_key k) -> DuplicateBinding k))


  let infer_lit (_, lit) =
    let open Literal in
    match lit with
    | LInt _ -> return int
    | LBool _ -> return bool
    | LUnit -> return unit


  let rec infer_pat (_, pat) =
    let open Infer_monad.Let_syntax in
    let open Pattern in
    match pat with
    | PCon (uid, ps) ->
      let%bind ctx, ts = infer_pats ps in
      let%bind t = find_constructor uid in
      let tv = fresh_var KStar in
      let%bind () = unify t (List.fold_right ts ~init:tv ~f:( @-> )) in
      return (ctx, tv)
    | PVar lid ->
      let tv = fresh_var KStar in
      return (Context.Variable.singleton lid (Scheme.from_type tv), tv)
    | PLit lit -> infer_lit lit >>| fun t -> Context.Variable.empty, t
    | PWild -> return (Context.Variable.empty, fresh_var KStar)


  and infer_pats ps =
    let open Infer_monad.Let_syntax in
    let%bind ctx_ts = all (List.map ~f:infer_pat ps) in
    let ctxs, ts = List.unzip ctx_ts in
    let%bind () = guard_dups ctxs in
    let ctx = List.fold_right ~init:Context.Variable.empty ~f:Context.Variable.extends ctxs in
    return (ctx, ts)


  let rec infer_alt (ps, e) =
    let open Infer_monad.Let_syntax in
    let%bind ctx, ts = infer_pats ps in
    let%bind () = gaurd_dup ctx in
    let%bind t = extends ctx (infer_expr e) in
    return (List.fold_right ~init:t ~f:( @-> ) ts)


  and infer_alts alts t =
    let open Infer_monad.Let_syntax in
    let%bind ts = all (List.map ~f:infer_alt alts) in
    ts |> List.map ~f:(unify t) |> all_unit


  and infer_branch (_, (p, e)) = infer_alt ([ p ], e)

  and infer_branches bs t =
    let alts = List.map bs ~f:(fun (_, (p, e)) -> [ p ], e) in
    infer_alts alts t


  and infer_binding (_, b) =
    let open Binding in
    match b with
    | BValue (lid, e) -> infer_expr e >>| fun t -> lid, t
    | BFunction (lid, ps, e) -> infer_alt (ps, e) >>| fun t -> lid, t


  and infer_bindings b_ts =
    let open Infer_monad.Let_syntax in
    let lid_ts =
      List.map b_ts ~f:(fun (b, t2) ->
          let%bind lid, t1 = infer_binding b in
          let%bind () = unify t1 t1 in
          return (lid, t2))
    in
    all lid_ts


  and infer_binder (_, br) =
    let open Infer_monad.Let_syntax in
    let open Binder in
    match br with
    | NonRec b ->
      let%bind (lid, t), subst = substitution (infer_binding b) in
      let%bind ctx = context ~lens:Context.variable in
      let sc = generalize (S.Context.Variable.apply subst ctx) (S.Type.apply subst t) in
      return (Context.Variable.singleton lid sc)
    | Rec bs ->
      let b_tvs = List.map ~f:(fun b -> b, fresh_var KStar) bs in
      let%bind ctx =
        context_of_alist
          (List.map b_tvs ~f:(fun ((_, b), tv) -> Binding.identifier b, Scheme.from_type tv))
      in
      let%bind lid_ts, subst = substitution (extends ctx (infer_bindings b_tvs)) in
      let%bind ctx = context ~lens:Context.variable in
      context_of_alist
        (List.map lid_ts ~f:(fun (lid, t) ->
             lid, generalize (S.Context.Variable.apply subst ctx) (S.Type.apply subst t)))


  and infer_expr (loc, expr) =
    let open Infer_monad.Let_syntax in
    let open Expr in
    match expr with
    | EVar lid -> find_variable lid
    | ECon uid -> find_constructor uid
    | ELit lit -> infer_lit lit
    | EApp (e1, e2) ->
      let%bind t1 = infer_expr e1 in
      let%bind t2 = infer_expr e2 in
      let tv = fresh_var KStar in
      let%bind () = unify t1 (t2 @-> tv) in
      return tv
    | ELam (ps, e) -> infer_alt (ps, e)
    | EFix (f, ps, e) ->
      let tv = fresh_var KStar in
      let sc = Scheme.from_type tv in
      let%bind t = extend (f, sc) (infer_alt (ps, e)) in
      let%bind () = unify tv t in
      return tv
    | ELet (b, e) ->
      let%bind ctx = infer_binder b in
      extends ctx (infer_expr e)
    | ECase (e1, bs) ->
      let%bind t = infer_expr e1 in
      let tv = fresh_var KStar in
      let%bind () = infer_branches bs (t @-> tv) in
      return tv
    | EIf (e1, e2, e3) ->
      let%bind t1 = infer_expr e1 in
      let%bind t2 = infer_expr e2 in
      let%bind t3 = infer_expr e3 in
      let%bind () = unify t1 bool in
      let%bind () = unify t2 t3 in
      return t2
    | EWhile (e1, e2) ->
      let%bind t1 = infer_expr e1 in
      let%bind t2 = infer_expr e2 in
      let%bind () = unify t1 bool in
      let%bind () = unify t2 unit in
      return unit
    | ESeq es ->
      (match List.rev es with
      | [] -> fail (Unknown loc)
      | en :: es ->
        List.fold_left es ~init:(infer_expr en) ~f:(fun m e ->
            infer_expr e >>= unify unit >>= fun () -> m))


  let normalize (Forall (tvs, t)) =
    let tv_map = List.mapi tvs ~f:(fun i tv -> tv, (i, Var.kind tv)) in
    let tvs' = List.map tv_map ~f:snd in
    let subst =
      List.fold_right
        ~init:Substitution.empty
        ~f:(fun (tv, tv') acc ->
          let s = Substitution.singleton tv (TVar tv') in
          Substitution.compose s acc)
        tv_map
    in
    Forall (tvs', S.Type.apply subst t)


  let infer_decl decl =
    let open Infer_monad.Let_syntax in
    let%bind vctx = infer_binder decl in
    (* type decl = binder *)
    return Context.{ variable = Variable.map vctx ~f:normalize; constructor = Constructor.empty }


  let infer_module decls =
    let open Infer_monad.Let_syntax in
    let%bind ctxs = all (List.map ~f:infer_decl decls) in
    return (List.fold_right ctxs ~init:Context.empty ~f:Context.extends)


  let infer m ctx = Result.(run m ctx >>| fst)
end
