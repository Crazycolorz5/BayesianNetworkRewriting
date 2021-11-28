{
open Lexing
open Parser

  let newline lexbuf =
    lexbuf.lex_curr_p <- { (lexeme_end_p lexbuf) with
      pos_lnum = (lexeme_end_p lexbuf).pos_lnum + 1;
      pos_bol = (lexeme_end lexbuf) }
}

(* Declare your aliases (let foo = regex) and rules here. *)
let newline = '\n' | ('\r' '\n') | '\r'
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let character = uppercase | lowercase
let whitespace = ['\t' ' ']
let digit = ['0'-'9']
let hexdigit = ['0'-'9'] | ['a'-'f'] | ['A'-'F']

rule token = parse
  | eof { EOF }

  | lowercase (digit | character | '_')* { IDENT (lexeme lexbuf) }
  | digit+ | "0x" hexdigit+ { INT (int_of_string (lexeme lexbuf)) }
  | newline { newline lexbuf; token lexbuf }
  | whitespace+ { token lexbuf }

  | "->"
    { ARROW }
  | ";" { SEMICOLON }
  | "," { COMMA }
