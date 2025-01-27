%token<string> TERM
%token VAR
%token SPECIAL
%token ARROW 
%token TERMINAL_ARROW
%token QUOTE
%token EOF
%type <bool> arrow
%type <Production.variable> variable
%type <Production.special> special
%type <Production.production> production
%start<Production.algorithm> algorithm 
%%
algorithm:
    | variable* special* production* EOF { ( $1, $2, $3 ) }
;
variable: 
    | VAR QUOTE TERM QUOTE { $3.[0] } 
;
special: 
    | SPECIAL QUOTE TERM QUOTE { $3.[0] }
;
production:
    | QUOTE TERM? QUOTE arrow QUOTE TERM? QUOTE { { input = $2 ; output = $6 ; terminal = $4 ;} }
;
arrow:
    | ARROW { false } 
    | TERMINAL_ARROW { true }
;