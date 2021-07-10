open Base

module Id = struct
  type t = int [@@deriving eq, ord, show, sexp]

  let show i = "v" ^ Int.to_string i
end

module Kind = struct
  type t =
    | KStar
    | KArr of (t * t)
  [@@deriving eq, ord, show, sexp]

  let rec show = function
    | KStar -> "*"
    | KArr (k1, k2) -> "(" ^ show k1 ^ ") -> (" ^ show k2 ^ ")"
end

module Var = struct
  type t = Id.t * Kind.t [@@deriving eq, ord, show, sexp]

  let kind = snd
end

module Constructor = struct
  type t = string * Kind.t [@@deriving eq, ord, show, sexp]

  let kind = snd
end

module Type = struct
  type t =
    | TVar of Var.t
    | TCon of Constructor.t
    | TApp of (t * t)
  [@@deriving eq, ord, show, sexp]

  (* Show -------------------------------------------------------------------------*)

  let rec show = function
    | TVar var -> Var.show var
    | TCon con -> Constructor.show con
    | TApp (t1, t2) -> show t1 ^ " " ^ show t2


  (* Built-in Types ----------------------------------------------------------------*)

  open Kind

  let app arr ts = List.fold_left ~f:(fun acc x -> TApp (acc, x)) ~init:arr ts
  let c_arr = "(->)", KArr (KStar, KArr (KStar, KStar))
  let c_sum = "(+)", KArr (KStar, KArr (KStar, KStar))
  let c_prod = "(*)", KArr (KStar, KArr (KStar, KStar))
  let c_ref = "Ref", KArr (KStar, KStar)
  let ( @-> ) t1 t2 = app (TCon c_arr) [ t1; t2 ]
  let sum t1 t2 = app (TCon c_sum) [ t1; t2 ]
  let prod t1 t2 = app (TCon c_prod) [ t1; t2 ]
  let ref t1 = app (TCon c_ref) [ t1 ]
  let int = TCon ("Int", KStar)
  let bool = TCon ("Bool", KStar)
  let unit = TCon ("Unit", KStar)

  (* Kinds ---------------------------------------------------------------------------*)

  let rec kind = function
    | TCon con -> Some (Constructor.kind con)
    | TVar var -> Some (Var.kind var)
    | TApp (t, _) ->
      Option.(
        kind t
        >>= (function
        | KArr (_, kind) -> Some kind
        | KStar -> None))
end

module Scheme = struct
  type t = Forall of (Var.t list * Type.t) [@@deriving eq, show, ord, sexp]

  (* TODO: Refactor to of_type *)
  let from_type t = Forall ([], t)

  let show (Forall (tvs, t)) =
    "∀ " ^ (List.map ~f:Var.show tvs |> String.concat ~sep:", ") ^ ". " ^ Type.show t
end
