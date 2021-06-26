open Base
open Lexing

open Lexer

let show_position lexbuf = 
  let pos = lexbuf.lex_curr_p in
    "Line: " ^ (Int.to_string pos.pos_lnum) ^ " Position: " ^ (Int.to_string (pos.pos_cnum - pos.pos_bol + 1))  

let parse lexbuf = 
  let open Result in
  try Ok (Parser.program Lexer.token lexbuf) with
    | LexerError msg -> Error (show_position lexbuf ^ ": " ^ msg)
    | Parser.Error -> Error (show_position lexbuf ^ ": syntax error")
