open Production
open Str

let has_match pattern str =
  try
    let _ = search_forward pattern str 0 in
    true
  with Not_found -> false

let rec find_first_match prod_lst input =
  match prod_lst with
  | h :: t -> (
      match h.input with
      | Some regex ->
          if has_match regex input then Some h else find_first_match t input
      | None -> Some h)
  | [] -> None

let rec replace_subs input output subs start_idx =
  match subs with
  | (hi, ho) :: t ->
      if List.length hi <> 0 then
        let sub = input.[start_idx + List.hd hi] in
        let new_s =
          String.mapi (fun i c -> if List.mem i ho then sub else c) output
        in
        replace_subs input new_s t start_idx
      else replace_subs input output t start_idx
  | [] -> output

let replace regex_opt template str subs =
  match regex_opt with
  | Some regex ->
      (* if there are no substitutions then just replace *)
      if List.length subs = 0 then replace_first regex template str
      (* otherwise, find the match, get the substitutions are replace them in the template*)
        else
        let start_idx = search_forward regex str 0 in
        replace_first regex (replace_subs str template subs start_idx) str
  | None -> template ^ str

let rec run_markov_algo_internal prods input =
  let () = print_endline input in
  match find_first_match prods input with
  | None -> input
  | Some m ->
      if m.terminal then replace m.input m.output input m.subs
      else
        run_markov_algo_internal prods (replace m.input m.output input m.subs)

(* We provide the input as a list separated by spaces so combine it into a single string *)
let process_input input =
  match input with
  | h :: t -> List.fold_left (fun str x -> str ^ " " ^ x) h t
  | [] -> ""

let colnum (pos : Lexing.position) = pos.pos_cnum - pos.pos_bol - 1

let pos_string (pos : Lexing.position) =
  let l = string_of_int pos.pos_lnum and c = string_of_int (colnum pos + 1) in
  "Line: " ^ l ^ ", Column: " ^ c

let run_markov_algo productions input =
  let _ =
    let lexbuf = Lexing.from_string productions in
    try
      let vars, specs, prods = Parser.algorithm Lexer.read lexbuf in
      List.iter (fun prod -> print_endline (production_str prod)) prods;
      List.iter (fun v -> print_endline ("Variable " ^ String.make 1 v)) vars;
      print_endline
        (run_markov_algo_internal
           (List.map
              (fun p ->
                let cp = compile_production p vars specs in
                let () = print_endline (compiled_production_str cp) in
                cp)
              prods)
           (process_input input))
    with Parser.Error ->
      raise (Failure ("Parse error at " ^ pos_string lexbuf.lex_curr_p))
  in
  ()
