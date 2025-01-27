%token ERASE_FUNC
%token SUCC_FUNC
%token PROJ_FUNC
%token MIN
%token <int> NUMBER 
%token <string> WORD
%token <string> IDENTIFIER
%token <string> SYMBOL
%token DEFINED
%token EQUALS
%token OR
%token UNDERSCORE
%token EXP
%token COMMA
%token OPEN_PAREN
%token CLOSE_PAREN
%token OPEN_BRACKET
%token CLOSE_BRACKET
%token EOF
%type <Functions.function_body> erase_call succ_call proj_call
%type <Functions.function_body> def_body
%type <Functions.function_body> call_arg
%type <Functions.function_body> function_call 
%type <Functions.function_body> prim_recursion 
%type <Functions.function_body> minimization
%start<Functions.func list> definitions 
%%
definitions:
    | function_def* EOF { $1 }
;
erase_call:
    | ERASE_FUNC; OPEN_PAREN; i = id_or_body; CLOSE_PAREN { Functions.Erase i }
;
succ_call:
    | SUCC_FUNC UNDERSCORE s = SYMBOL; OPEN_PAREN; i = id_or_body; CLOSE_PAREN { Functions.Succ (s, i) }
;
proj_call:
    | PROJ_FUNC UNDERSCORE i = NUMBER; EXP n = NUMBER; OPEN_PAREN l = identifier_list CLOSE_PAREN { Functions.Proj (i, n, l) }
;
identifier_list:
    | separated_nonempty_list (COMMA, id_or_body) { $1 }
;
function_def:
    | name = IDENTIFIER; OPEN_PAREN; args = def_arg_list; CLOSE_PAREN; DEFINED; body = def_body { Functions.make_func name args body } 
;
def_body:
    | erase_call { $1 }
    | succ_call { $1 }
    | proj_call { $1 }
    | function_call { $1 }
    | prim_recursion { $1 }
    | minimization { $1 }
;
id_or_body:
    | def_body { $1 }
    | IDENTIFIER { Functions.Ident $1 }
;
def_arg_list: 
    | separated_nonempty_list(COMMA, def_arg) { $1 }
;
def_arg:
    | IDENTIFIER UNDERSCORE IDENTIFIER { Functions.Composite ($1, $3) }
    | IDENTIFIER { Functions.Simple $1 }
;
call_arg_list:
    | separated_nonempty_list(COMMA, call_arg) { $1 } 
;
call_arg: 
    | IDENTIFIER { Ident $1 }
    | WORD { Word $1 }
    | function_call { $1 }
    | erase_call { $1 }
    | succ_call { $1 }
    | proj_call { $1 }
;
function_call: 
    | i = IDENTIFIER; OPEN_PAREN a = call_arg_list; CLOSE_PAREN { Functions.Call (i, a) } 
;
prim_recursion: 
    | base = def_body; OR; ind = def_body { Functions.Recursion (base, ind) } 
;
minimization: 
    | MIN UNDERSCORE v = IDENTIFIER; OPEN_BRACKET f = def_body; EQUALS w = WORD; CLOSE_BRACKET { Functions.Minimization (v, f, w) }
;
