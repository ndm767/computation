{
open Parser
}
rule read = parse
    [' ' '\t']+ { read lexbuf }
    | ['\n' '\r'] | "\r\n" { Lexing.new_line lexbuf; read lexbuf }
    | "ADD"  { ADD_OP }
    | "DEL"  { DEL_OP }
    | "CLR" { CLR_OP }
    | "<-" { LEFT_ARROW }
    | "GOTO" { GOTO_OP }
    | "IF FIRST" { IF_FIRST_OP }
    | "STOP" { STOP_OP }
    | 'R' ['0'-'9']+ { REGISTER (Lexing.lexeme lexbuf) }
    | ['A'-'Z' 'a'-'z' '0'-'9']+ { ARG (Lexing.lexeme lexbuf) }
    | eof { EOF }