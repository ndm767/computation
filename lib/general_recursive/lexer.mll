{
open Parser
}
rule read = parse
    [' ' '\t']+ { read lexbuf }
    | ['\n' '\r'] | "\r\n" { Lexing.new_line lexbuf; read lexbuf }
    | "E"  { ERASE_FUNC }
    | "S"  { SUCC_FUNC }
    | "P" { PROJ_FUNC }
    | "min" { MIN }
    | ":=" { DEFINED }
    | "=" { EQUALS }
    | "|" { OR }
    | "_" { UNDERSCORE }
    | "^" { EXP }
    | "," { COMMA }
    | "(" { OPEN_PAREN }
    | ")" { CLOSE_PAREN }
    | "[" { OPEN_BRACKET }
    | "]" { CLOSE_BRACKET }
    | ['0'-'9']+ { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
    | "\"" [^'"']+ "\"" { WORD (Functions.process_word (Lexing.lexeme lexbuf)) }
    | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9']* { IDENTIFIER (Lexing.lexeme lexbuf) }
    | "#" [^ '-' '_' '"' '(' ')' '=' '[' ']' ','] { SYMBOL (Functions.process_symbol (Lexing.lexeme lexbuf)) }
    | eof { EOF }