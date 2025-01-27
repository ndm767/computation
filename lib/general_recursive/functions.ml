type ident = string
type symbol = string
type word = string
type number = int
type arg = Simple of ident | Composite of ident * ident

type function_body =
  | Erase of function_body
  | Succ of symbol * function_body
  | Proj of number * number * function_body list
  | Ident of ident
  | Word of word
  | Call of ident * function_body list
  | Recursion of function_body * function_body
  | Minimization of ident * function_body * word

let process_symbol symb = String.sub symb 1 (String.length symb - 1)

let process_word word =
  let x = String.sub word 0 (String.length word - 1) in
  String.sub x 1 (String.length x - 1)

let rec string_of_func_body body =
  match body with
  | Erase body -> "E(" ^ string_of_func_body body ^ ")"
  | Succ (s, b) -> "S_#" ^ s ^ "(" ^ string_of_func_body b ^ ")"
  | Proj (i, n, bs) ->
      "P_" ^ string_of_int i ^ "^" ^ string_of_int n ^ "("
      ^ String.concat ", " (List.map (fun b -> string_of_func_body b) bs)
      ^ ")"
  | Ident i -> i
  | Word w -> w
  | Call (i, bs) ->
      i ^ "("
      ^ String.concat ", " (List.map (fun b -> string_of_func_body b) bs)
      ^ ")"
  | Recursion (base, ind) ->
      string_of_func_body base ^ " | " ^ string_of_func_body ind
  | Minimization (v, b, w) ->
      "min_" ^ v ^ "[" ^ string_of_func_body b ^ " = " ^ w ^ "]"

type func = { name : ident; args : arg list; body : function_body }

let string_of_arg a =
  match a with Simple i -> i | Composite (i, s) -> i ^ "_" ^ s

let string_of_arg_list args =
  String.concat ", " (List.map (fun a -> string_of_arg a) args)

let string_of_func f =
  f.name ^ "(" ^ string_of_arg_list f.args ^ ") := "
  ^ string_of_func_body f.body

let make_func n a b = { name = n; args = a; body = b }
