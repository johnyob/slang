open Base
open Syntax.Frontend

val parse : Lexing.lexbuf -> (Parsed.Module.t, string) Result.t