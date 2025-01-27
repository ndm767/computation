%token PRINT_OP 
%token DEL_OP 
%token RIGHT_OP 
%token LEFT_OP 
%token GOTO_OP 
%token IF_OP 
%token STOP_OP 
%token EOF
%token <string> ARG
%type <Instruction.instruction> operation
%start<Instruction.instruction list> program 
%%
program:
    | operation* EOF { $1 }
;
operation:
    | PRINT_OP ARG { Print $2 }
    | STOP_OP { Stop } 
    | DEL_OP { Del }
    | RIGHT_OP { Right }
    | LEFT_OP { Left }
    | GOTO_OP ARG { Goto (int_of_string $2) }
    | IF_OP ARG GOTO_OP ARG { Instruction.cons_ifgoto $2 (int_of_string $4) }
;