open Instruction

let rec number_list_aux c n =
  if c > n then [] else c :: number_list_aux (c + 1) n

let number_list n = number_list_aux 1 n

let rec remove_last lst =
  match lst with [] -> [] | [ _ ] -> [] | h :: t -> h :: remove_last t

let rec get_last lst =
  match lst with
  | [] -> raise (Failure "empty list")
  | [ x ] -> x
  | _ :: t -> get_last t

(* Note that we are storing the registers backwards. Ie, first element in the list corresponds to the last symbol in the register *)
let run_single_instruction instr regs curr_line =
  match instr with
  | Add (s, r) ->
      let () = regs.(r - 1) <- s :: regs.(r - 1) in
      Some (regs, curr_line + 1)
  | Del r ->
      let () = regs.(r - 1) <- remove_last regs.(r - 1) in
      Some (regs, curr_line + 1)
  | Clr r ->
      let () = regs.(r - 1) <- [] in
      Some (regs, curr_line + 1)
  | Copy (dest, src) ->
      let () = regs.(dest - 1) <- regs.(src - 1) in
      Some (regs, curr_line + 1)
  | Goto l -> Some (regs, l)
  | IfFirst (r, s, l) ->
      if get_last regs.(r - 1) = s then Some (regs, l)
      else Some (regs, curr_line + 1)
  | Stop -> None

let rec run_register_machine_internal instrs (regs : string list array)
    curr_line =
  match
    run_single_instruction (List.nth instrs (curr_line - 1)) regs curr_line
  with
  | Some (new_regs, new_line) ->
      run_register_machine_internal instrs new_regs new_line
  | None -> regs

let rec get_register_list (acc : int list) instructions =
  match instructions with
  | h :: t -> (
      match h with
      | Copy (r1, r2) -> get_register_list (r1 :: r2 :: acc) t
      | _ ->
          let reg = get_instr_reg h in
          let new_l = match reg with Some r -> r :: acc | None -> acc in
          get_register_list new_l t)
  | [] -> acc

let get_register_arr_size instructions =
  List.fold_left (fun acc x -> max acc x) 0 (get_register_list [] instructions)

let colnum (pos : Lexing.position) = pos.pos_cnum - pos.pos_bol - 1

let pos_string (pos : Lexing.position) =
  let l = string_of_int pos.pos_lnum and c = string_of_int (colnum pos + 1) in
  "Line: " ^ l ^ ", Column: " ^ c

let string_of_registers regs =
  List.mapi
    (fun idx reg ->
      "R" ^ string_of_int (idx + 1) ^ ": \"" ^ String.concat "" reg ^ "\"")
    (Array.to_list regs)
  |> String.concat ", "
  |> fun s -> "Registers: [" ^ s ^ "]"

let rec string_to_register str idx aux =
  if idx >= String.length str then aux
  else
    string_to_register str (idx + 1) (String.make 1 (String.get str idx) :: aux)

let rec populate_registers input arr idx =
  match input with
  | [] -> arr
  | h :: t ->
      let () = arr.(idx) <- string_to_register h 0 [] in
      populate_registers t arr (idx + 1)

let make_registers input len = populate_registers input (Array.make len []) 0

let run_register_machine instructions input =
  let _ =
    let lexbuf = Lexing.from_string instructions in
    try
      let parse_res = Parser.program Lexer.read lexbuf in
      let arr_size =
        max (get_register_arr_size parse_res) (List.length input)
      in
      let _ = print_endline ("Max reg: " ^ string_of_int arr_size) in
      let _ =
        List.iter (fun i -> print_endline (Instruction.print_instr i)) parse_res
      in
      let end_regs =
        run_register_machine_internal parse_res
          (make_registers input arr_size)
          1
      in
      print_endline (string_of_registers end_regs)
    with Parser.Error ->
      raise (Failure ("Parse error at " ^ pos_string lexbuf.lex_curr_p))
  in
  ()
