open Base
open Syntax.Frontend.Type
open Syntax.Frontend.Parsed
module Located = Syntax.Frontend.Located
module Location = Located.Location
module Substitution = Substitutable.Substitution

module Unify : sig
  type error = private
    | OccursCheck of (Var.t * Type.t)
    | Unknown of (Type.t * Type.t)
  [@@deriving eq, show]

  val unify : Type.t -> Type.t -> (Substitution.t, error) Result.t
end

module Constraint : sig
  type t = Type.t * Type.t [@@deriving eq, show]
  type error = Unify.error [@@deriving eq, show]

  val apply : Substitution.t -> t -> t
  val solve : Substitution.t -> t list -> (Substitution.t, error) Result.t
end

module Infer : sig
  type error = private
    | UnboundVariable of Lid.located
    | UnboundConstructor of Uid.located
    | DuplicateBinding of Lid.located
    | SolverError of Constraint.error
    | Unknown of Location.t
  [@@deriving eq, show]

  type 'a t

  val infer : 'a t -> Context.t -> ('a, error) Result.t
  val infer_lit : Literal.located -> Type.t t
  val infer_pat : Pattern.located -> (Context.Variable.t * Type.t) t
  val infer_pats : Pattern.located list -> (Context.Variable.t * Type.t list) t
  val infer_branch : Expr.located_branch -> Type.t t
  val infer_branches : Expr.located_branch list -> Type.t -> unit t
  val infer_binding : Binding.located -> (Lid.located * Type.t) t
  val infer_bindings : (Binding.located * Type.t) list -> (Lid.located * Type.t) list t
  val infer_binder : Binder.located -> Context.Variable.t t
  val infer_expr : Expr.located -> Type.t t
  val infer_decl : Declaration.located -> Context.t t
  val infer_module : Module.t -> Context.t t
end
