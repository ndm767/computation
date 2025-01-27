{
open Parser
}
rule read = parse
    [' ' '\t']+ { read lexbuf }
    | ['\n' '\r'] | "\r\n" { Lexing.new_line lexbuf; read lexbuf }
    | "PRINT"  { PRINT_OP }
    | "DEL"  { DEL_OP }
    | "RIGHT" { RIGHT_OP }
    | "LEFT" { LEFT_OP }
    | "GOTO" { GOTO_OP }
    | "IF" { IF_OP }
    | "STOP" { STOP_OP }
    | ['A'-'Z' 'a'-'z' '0'-'9']+ { ARG (Lexing.lexeme lexbuf) }
    | eof { EOF }