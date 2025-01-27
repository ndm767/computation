open Str

type production = {
  input : string option;
  output : string option;
  terminal : bool;
}

type variable = char
type special = char
type algorithm = variable list * special list * production list

type compiled_production = {
  input : regexp option;
  input_str : string option;
  output : string;
  subs : (int list * int list) list;
      (* simple map of variable in input to variable in output*)
  terminal : bool;
}

(* Print utilities *)
let prod_arrow_str terminal = if terminal then "->t" else "->"
let opt_str str = match str with Some x -> "\"" ^ x ^ "\"" | None -> "\"\""

let production_str (prod : production) =
  opt_str prod.input ^ prod_arrow_str prod.terminal ^ " " ^ opt_str prod.output

let compiled_production_str (prod : compiled_production) =
  opt_str prod.input_str
  ^ prod_arrow_str prod.terminal
  ^ " \"" ^ prod.output ^ "\""

let string_of_int_list lst =
  "[" ^ List.fold_left (fun acc x -> acc ^ string_of_int x ^ ", ") "" lst ^ "]"

let rec print_sub_list sl vars =
  match vars with
  | h :: t -> (
      match sl with
      | (shi, sho) :: st ->
          let _ =
            print_endline
              (String.make 1 h ^ ": (" ^ string_of_int_list shi ^ ", "
             ^ string_of_int_list sho ^ ")")
          in
          print_sub_list st t
      | [] -> ())
  | [] -> ()

(* find the indices of the variables in each production (if they exist)*)
let rec find_all_indices acc idx str c =
  match String.index_from_opt str idx c with
  | Some i when i >= idx -> find_all_indices (i :: acc) (i + 1) str c
  | None | _ -> acc

let rec get_single_sub_list acc str vars =
  match vars with
  | h :: t -> get_single_sub_list (find_all_indices [] 0 str h :: acc) str t
  | [] -> acc

let get_sub_list input output vars =
  List.combine
    (List.rev (get_single_sub_list [] input vars))
    (List.rev (get_single_sub_list [] output vars))

let get_spec_str lst =
  "[^" ^ List.fold_left (fun s c -> s ^ String.make 1 c) "" lst ^ "]"

(* Ascii code for substitute, hopefully won't be used in any productions*)
let spec_stand_in = Char.chr 26

let rec insert_wildcard_internal str subs =
  match subs with
  | (h, _) :: t ->
      let new_s =
        String.mapi
          (fun idx c -> if List.mem idx h then spec_stand_in else c)
          str
      in
      insert_wildcard_internal new_s t
  | [] -> str

let insert_wildcard str subs spec_str =
  let replaced = insert_wildcard_internal str subs in
  String.fold_left
    (fun acc c ->
      if c = spec_stand_in then acc ^ spec_str else acc ^ String.make 1 c)
    "" replaced

(* Compile prod into regex and remove option in output *)
let compile_input_regex input sub_list spec_str =
  match input with
  | Some s ->
      let ws = insert_wildcard (quote s) sub_list spec_str in
      (Some (regexp ws), Some ws)
  | None -> (None, None)

let unwrap_else x a = match x with Some y -> y | None -> a

let compile_production (prod : production) (vars : variable list)
    (specs : special list) =
  let output = match prod.output with Some x -> x | None -> "" in
  let subs =
    get_sub_list (unwrap_else prod.input "") (unwrap_else prod.output "") vars
  in
  let input, input_str =
    compile_input_regex prod.input subs (get_spec_str specs)
  in
  { input; input_str; output; subs; terminal = prod.terminal }
