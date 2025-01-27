{
open Parser
}
rule read = parse
    [' ' '\t']+ { read lexbuf }
    | ['\n' '\r'] | "\r\n" { Lexing.new_line lexbuf; read lexbuf }
    | "->t" { TERMINAL_ARROW }
    | "->" { ARROW }
    | '"' { QUOTE }
    | "var" { VAR }
    | "special" { SPECIAL }
    | ['A'-'Z' 'a'-'z' '0'-'9']* { TERM (Lexing.lexeme lexbuf) }
    | eof { EOF }