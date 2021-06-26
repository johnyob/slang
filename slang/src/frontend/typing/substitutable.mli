module T = Syntax.Frontend.Type
module Ctx = Context

module Substitution : sig
  type t

  val empty : t

  val singleton : T.Var.t -> T.Type.t -> t

  val compose : t -> t -> t

  val remove : t -> T.Var.t -> t

  val find : t -> T.Var.t -> T.Type.t option
end

module type S = sig
  type t

  val apply : Substitution.t -> t -> t

  val free_vars : t -> T.Var.t list
end

module Type : S with type t := T.Type.t

module Scheme : S with type t := T.Scheme.t

module Context : sig
  module Make (C : Ctx.S) : S with type t := C.t

  module Variable : S with type t := Ctx.Variable.t

  module Constructor : S with type t := Ctx.Constructor.t
end
