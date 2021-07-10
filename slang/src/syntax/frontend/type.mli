module Kind : sig
  type t =
    | KStar
    | KArr of (t * t)
  [@@deriving eq, ord, show, sexp]
end

module Id : sig
  type t = int [@@deriving eq, ord, show, sexp]
end

module Var : sig
  type t = Id.t * Kind.t [@@deriving eq, ord, show, sexp]

  val kind : t -> Kind.t
end

module Constructor : sig
  type t = string * Kind.t [@@deriving eq, ord, show, sexp]

  val kind : t -> Kind.t
end

module Type : sig
  type t =
    | TVar of Var.t
    | TCon of Constructor.t
    | TApp of (t * t)
  [@@deriving eq, show, ord, sexp]

  val kind : t -> Kind.t option
  val ( @-> ) : t -> t -> t
  val sum : t -> t -> t
  val prod : t -> t -> t
  val ref : t -> t
  val int : t
  val bool : t
  val unit : t
  val app : t -> t list -> t
end

module Scheme : sig
  type t = Forall of (Var.t list * Type.t) [@@deriving eq, show, ord, sexp]

  val from_type : Type.t -> t
end
