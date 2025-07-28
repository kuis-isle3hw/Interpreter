{
open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper

rule main = parse
  | space+ { main lexbuf }
  | "(*" { comment lexbuf; main lexbuf }
  | digit+ { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "fun" { FUN }
  | "print_string" { PRINT_STRING }
  | "proj1" { PROJ1 }
  | "proj2" { PROJ2 }
  | (lower | upper) (alpha | digit | '_')* 
    { ID (Lexing.lexeme lexbuf) }
  | "\"" { string_literal "" lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ";;" { SEMISEMI }
  | "=" { EQ }
  | "," { COMMA }
  | "^" { CONCAT }
  | ".[" { DOT_LBRACKET }
  | "]" { RBRACKET }
  | "->" { RARROW }
  | eof { exit 0 }

and comment = parse
  | "*)" { () }
  | "(*" { comment lexbuf; comment lexbuf }
  | eof { () }
  | _ { comment lexbuf }

and string_literal acc = parse
  | "\"" { STRINGV acc }
  | "\\" "\"" { string_literal (acc ^ "\"") lexbuf }
  | "\\" "\\" { string_literal (acc ^ "\\") lexbuf }
  | "\\" "n" { string_literal (acc ^ "\n") lexbuf }
  | "\\" "t" { string_literal (acc ^ "\t") lexbuf }
  | eof { failwith "Unterminated string literal" }
  | _ as c { string_literal (acc ^ String.make 1 c) lexbuf }

