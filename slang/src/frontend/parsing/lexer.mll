{

open Base
open Parser

exception LexerError of string


let slang_bool_of_string s = match s with
  | "True" -> true
  | "False" -> false
  | _ -> assert false


}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper

let newline = "\r\n" | "\r" | "\n"
let whitechar = newline | [' ' '\t']
let whitespace = whitechar+


(** Numerical Literals ********************************************************)

let lid = lower (lower | ['0'-'9' '_' '\''])* 
let uid = upper alpha*

(** Numerical Literals ********************************************************)

let digit = ['0'-'9']
let decimal = digit+
let integer = decimal

(** Boolean Literals ********************************************************)

let boolean = "True" | "False"


rule token = parse
  | [' ' '\t']        { token lexbuf }

  | '('               { LPAREN }
  | ')'               { RPAREN }
  | '{'               { LBRACE }
  | '}'               { RBRACE }
  (* | ","               { COMMA } *)
  (* | ":"               { COLON } *)
  | ";"               { SEMICOLON }

  | "+"               { PLUS }
  | "-"               { MINUS }
  | "*"               { STAR }
  | "/"               { SLASH }
  | ":="              { ASSIGN }
  | "!"               { BANG }
  | "="               { EQUALS }
  | "<"               { LT }
  | "<="              { LEQ }
  | ">"               { GT }
  | ">="              { GEQ }
  | "&&"              { AND }
  | "||"              { OR }
  
  (* | "|"               { BAR }  *)

  | "\\"              { LAMBDA }
  | "->"              { ARROW }
  | "case"            { CASE }
  | "of"              { OF }
  | "if"              { IF }
  | "then"            { THEN }
  | "else"            { ELSE }
  | "let"             { LET }
  | "fix"             { FIX }
  | "rec"             { REC }
  | "in"              { IN }
  | "while"           { WHILE }
  | "do"              { DO }
  
  | integer           { LINT (Int.of_string (Lexing.lexeme lexbuf))     }
  | boolean           { LBOOL (slang_bool_of_string (Lexing.lexeme lexbuf))   }
  | "()"              { LUNIT                                           }

  | lid               { LID (Lexing.lexeme lexbuf) }
  | uid               { UID (Lexing.lexeme lexbuf) }

  | eof               { EOF }
  | _                 { raise (LexerError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }
