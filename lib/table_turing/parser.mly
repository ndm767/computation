%token<string> STR
%token<string> WILDCARD
%token COMMA
%token OPEN_PAREN
%token CLOSE_PAREN
%token EOF
%type <Table.unparsed_transition> transition
%start <Table.unparsed_transition list> program
%% 
program:
    | transition* EOF { $1 }
;
transition:
    | OPEN_PAREN STR COMMA STR COMMA STR COMMA STR COMMA STR CLOSE_PAREN { {in_state= $2 ; read= $4 ; write= $6 ; move = $8 ; out_state = $10} }
    | OPEN_PAREN STR COMMA WILDCARD COMMA STR COMMA STR COMMA STR CLOSE_PAREN { {in_state= $2 ; read= $4 ; write= $6 ; move = $8 ; out_state = $10} }
;