open Base

module Position = struct
  type t = Position of (int * int) | NoPosition
  [@@deriving eq, show, sexp]

  let compare p1 p2 =
    match (p1, p2) with
    | NoPosition, _ -> -1
    | _, NoPosition -> 1
    | Position (l1, c1), Position (l2, c2) ->
        [%compare: int * int] (l1, c1) (l2, c2)
end

module Span = struct
  type t = Position.t * Position.t [@@deriving eq, show, ord, sexp]

  let subset_of (s1, e1) (s2, e2) =
    let open Position in
    compare s2 s1 <= 0 && compare e1 e2 <= 0

  let empty = Position.(NoPosition, NoPosition)

  let ( <+> ) (s1, e1) (s2, e2) =
    ( Comparable.min Position.compare s1 s2,
      Comparable.max Position.compare e1 e2 )
end

module Location = struct
  type t = { file_path : string; span : Span.t }
  [@@deriving eq, show, ord, sexp]

  let empty = { file_path = ""; span = Span.empty }

  let ( <+> ) { file_path = fp1; span = s1 } { file_path = fp2; span = s2 } =
    if String.equal fp1 fp2 then { file_path = fp1; span = Span.(s1 <+> s2) }
    else failwith "Differening filepaths"
end

module T = struct
  type 'a t = Location.t * 'a [@@deriving eq, show, ord, sexp]

  let location = fst

  let ( <@> ) l r = Location.(location l <+> location r)
end

include T

let fold ls =
  Location.(ls |> List.map ~f:location |> List.fold_left ~f:( <+> ) ~init:empty)

include Monad.Make (struct
  open Location
  include T

  let return x = (empty, x)

  let bind (l1, x) ~f =
    let l2, y = f x in
    (l1 <+> l2, y)

  let map = `Define_using_bind
end)
