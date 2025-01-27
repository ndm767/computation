{
    open Parser
}
rule read = parse 
    [' ' '\t']+ { read lexbuf }
    | ['\n' '\r'] | "\r\n" { Lexing.new_line lexbuf ; read lexbuf }
    | ['A'-'Z' 'a'-'z' '0'-'9']+ { STR (Lexing.lexeme lexbuf) }
    | "*" { WILDCARD (Lexing.lexeme lexbuf) }
    | ',' { COMMA }
    | '(' { OPEN_PAREN }
    | ')' { CLOSE_PAREN }
    | eof { EOF }
