%token ADD_OP
%token DEL_OP 
%token CLR_OP 
%token LEFT_ARROW 
%token GOTO_OP 
%token IF_FIRST_OP 
%token STOP_OP 
%token EOF
%token <string> REGISTER
%token <string> ARG
%type <Instruction.instruction> operation
%start<Instruction.instruction list> program 
%%
program:
    | operation* EOF { $1 }
;
operation:
    | ADD_OP ARG REGISTER { Add ($2, (Instruction.reg_from_str $3)) }
    | DEL_OP REGISTER { Del (Instruction.reg_from_str $2) }
    | CLR_OP REGISTER { Clr (Instruction.reg_from_str $2) }
    | REGISTER LEFT_ARROW REGISTER { Copy ((Instruction.reg_from_str $1), (Instruction.reg_from_str $3)) }
    | GOTO_OP ARG { Goto (int_of_string $2) }
    | IF_FIRST_OP REGISTER ARG GOTO_OP ARG { IfFirst ((Instruction.reg_from_str $2), $3, (int_of_string $5)) }
    | STOP_OP { Stop } 
;