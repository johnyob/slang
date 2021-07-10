module Lid : sig
  type t = string [@@deriving eq, ord, show, sexp]

  and located = t Located.t
end

module Uid : sig
  type t = string [@@deriving eq, ord, show, sexp]

  and located = t Located.t
end

module Literal : sig
  type t =
    | LInt of int
    | LBool of bool
    | LUnit
  [@@deriving eq, show]

  and located = t Located.t
end

module Pattern : sig
  type t =
    | PCon of Uid.located * located list
    | PVar of Lid.located
    | PLit of Literal.located
    | PWild
  [@@deriving eq, show]

  and located = t Located.t
end

(* TODO: Refactor to remove rec modules. Use the trick from substitutable.ml *)

module rec Binder : sig
  type t =
    | NonRec of Binding.located
    | Rec of Binding.located list
  [@@deriving eq, show]

  and located = t Located.t
end

and Binding : sig
  type t =
    | BValue of (Lid.located * Expr.located)
    | BFunction of (Lid.located * Pattern.located list * Expr.located)
  [@@deriving eq, show]

  and located = t Located.t

  val identifier : t -> Lid.located
end

and Expr : sig
  type t =
    | EVar of Lid.located
    | ECon of Uid.located
    | ELit of Literal.located
    | EApp of (located * located)
    | ELam of (Pattern.located list * located)
    | EFix of (Lid.located * Pattern.located list * located)
    | ELet of (Binder.located * located)
    | ECase of (located * located_branch list)
    | EIf of (located * located * located)
    | EWhile of (located * located)
    | ESeq of located list
  [@@deriving eq, show]

  and located = t Located.t

  and branch = Pattern.located * located

  and located_branch = branch Located.t
end

module Declaration : sig
  type t = Binder.t [@@deriving eq, show]

  and located = t Located.t
end

module Module : sig
  type t = Declaration.located list [@@deriving eq, show]
end
