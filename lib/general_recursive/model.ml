open Functions

(* Look up functions in our symbol table *)
let func_lookup name funcs =
  match List.find_index (fun f -> f.name = name) funcs with
  | Some fidx -> List.nth funcs fidx
  | None -> raise (Failure ("Function " ^ name ^ " not found!"))

(* Bind argument identifiers to values in an association list *)
let bind_args args inputs =
  List.map2
    (fun arg v ->
      match arg with
      | Simple a -> (a, v)
      | Composite (a, _) -> (a, String.sub v 0 (String.length v - 1)))
    args inputs

let arg_name arg = match arg with Simple x -> x | Composite (x, _) -> x

(* Main run function *)
let rec run_func func_body func_args arg_bindings func_list =
  match func_body with
  (* Erase always returns the empty string *)
  | Erase _ -> ""
  (* Succ concats the symbol to the output of the body *)
  | Succ (s, fb) -> run_func fb func_args arg_bindings func_list ^ s
  | Proj (i, _, fbs) ->
      run_func (List.nth fbs i) func_args arg_bindings func_list
  (* look up identifiers *)
  | Ident i -> List.assoc i arg_bindings
  (* Words are the bottom level of evaluation *)
  | Word w -> w
  | Call (f, args) ->
      let call_func = func_lookup f func_list in
      run_func call_func.body call_func.args
        (bind_args call_func.args
           (List.map
              (fun arg -> run_func arg func_args arg_bindings func_list)
              args))
        func_list
  | Recursion (base, ind) ->
      if List.assoc (arg_name (List.hd func_args)) arg_bindings = "" then
        run_func base func_args arg_bindings func_list
      else run_func ind func_args arg_bindings func_list
  | Minimization (id, body, target_func) ->
      minimize body func_args arg_bindings func_list id "1" target_func

and minimize func_body func_args arg_bindings func_list id curr target_func =
  let out =
    run_func func_body func_args ((id, curr) :: arg_bindings) func_list
  in
  let target = run_func target_func func_args ((id, curr) :: arg_bindings) func_list in
  if out = target then curr
  else
    minimize func_body func_args arg_bindings func_list id (curr ^ "1") target_func

let run_general_recursive_internal funcs curr_func inputs =
  let f = func_lookup curr_func funcs in
  run_func f.body f.args (bind_args f.args inputs) funcs

let colnum (pos : Lexing.position) = pos.pos_cnum - pos.pos_bol - 1

let pos_string (pos : Lexing.position) =
  let l = string_of_int pos.pos_lnum and c = string_of_int (colnum pos + 1) in
  "Line: " ^ l ^ ", Column: " ^ c


let run_general_recursive instructions inputs =
  let _ =
    let lexbuf = Lexing.from_string instructions in
    try
      let parse_res = Parser.definitions Lexer.read lexbuf in
      let _ = List.iter (fun f -> print_endline (string_of_func f)) parse_res in
      let inp = List.rev inputs in
      let out =
        run_general_recursive_internal parse_res (List.hd inp) (List.tl inp)
      in
      print_endline out
    with Parser.Error i ->
      raise
        (Failure
           ("Parse error #" ^ string_of_int i ^ " at "
           ^ pos_string lexbuf.lex_curr_p))
  in
  ()
