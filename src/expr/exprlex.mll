(* Lexer specification for the simple expression language. *)
{
open Lexing
open Exprpar

exception SyntaxError of string

(* Scan keywords as identifiers and use this function to distinguish them.
   If the set of keywords is large, use a hashtable instead. *)
let keyword s =
    match s with
    | "let" -> LET
    | "in"  -> IN
    | "end" -> END
    | _     -> NAME s

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- {
    pos with
      pos_bol = lexbuf.lex_curr_pos;
      pos_lnum = pos.pos_lnum + 1
  }

}

let ws = [' ' '\t' '\r']
let nl = '\r' | '\n' | "\r\n"
let int = ['0'-'9']['0'-'9']*
let id = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']*

rule read = parse
  | ws  { read lexbuf }
  | nl  { next_line lexbuf; read lexbuf }
  | int { CSTINT (int_of_string (Lexing.lexeme lexbuf)) }
  | id  { keyword (Lexing.lexeme lexbuf) }
  | '+' { PLUS  }
  | '-' { MINUS }
  | '*' { TIMES }
  | '=' { EQ    }
  | '(' { LPAR  }
  | ')' { RPAR  }
  | _   { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF   }
