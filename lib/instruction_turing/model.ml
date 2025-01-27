open Instruction

let rec number_list_aux c n =
  if c > n then [] else c :: number_list_aux (c + 1) n

let number_list n = number_list_aux 1 n

type state = { tape : (int * string) list; head : int; instr_ptr : int }

(* TODO tail recursion? *)
let rec change_tape (tape : (int * string) list) pos symb =
  match tape with
  | [] -> []
  | (i, s) :: tape' ->
      if i = pos then (i, symb) :: tape'
      else (i, s) :: change_tape tape' pos symb

let move_right (tape : (int * string) list) head =
  if head = List.length tape then
    (tape @ [ (1 + List.length tape, "0") ], head + 1)
  else (tape, head + 1)

let move_left (tape : (int * string) list) head =
  if head = 1 then ((1, "0") :: List.map (fun (i, s) -> (i + 1, s)) tape, head)
  else (tape, head - 1)

let rec should_goto (tape : (int * string) list) head symb =
  match tape with
  | [] -> false
  | (i, s) :: tape' ->
      if head = i then s = symb else should_goto tape' head symb

let run_single_instruction instr curr_state =
  match instr with
  | Del ->
      Some
        {
          curr_state with
          tape = change_tape curr_state.tape curr_state.head "0";
          instr_ptr = curr_state.instr_ptr + 1;
        }
  | Print symb ->
      Some
        {
          curr_state with
          tape = change_tape curr_state.tape curr_state.head symb;
          instr_ptr = curr_state.instr_ptr + 1;
        }
  | Right ->
      let new_tape, new_head = move_right curr_state.tape curr_state.head in
      Some
        {
          tape = new_tape;
          head = new_head;
          instr_ptr = curr_state.instr_ptr + 1;
        }
  | Left ->
      let new_tape, new_head = move_left curr_state.tape curr_state.head in
      Some
        {
          tape = new_tape;
          head = new_head;
          instr_ptr = curr_state.instr_ptr + 1;
        }
  | Goto line -> Some { curr_state with instr_ptr = line }
  | IfGoto (symb, line) ->
      if should_goto curr_state.tape curr_state.head symb then
        Some { curr_state with instr_ptr = line }
      else Some { curr_state with instr_ptr = curr_state.instr_ptr + 1 }
  | Stop -> None

let rec run_instruction_turing_internal instrs curr_state =
  let output =
    run_single_instruction
      (List.nth instrs (curr_state.instr_ptr - 1))
      curr_state
  in
  match output with
  | None -> (curr_state.head, curr_state.tape)
  | Some new_state -> run_instruction_turing_internal instrs new_state

let colnum (pos : Lexing.position) = pos.pos_cnum - pos.pos_bol - 1

let pos_string (pos : Lexing.position) =
  let l = string_of_int pos.pos_lnum and c = string_of_int (colnum pos + 1) in
  "Line: " ^ l ^ ", Column: " ^ c

let run_instruction_turing instructions input =
  let _ =
    let lexbuf = Lexing.from_string instructions in
    try
      let parse_res = Parser.program Lexer.read lexbuf in
      let final_head, final_tape =
        run_instruction_turing_internal parse_res
          {
            tape = List.combine (number_list (List.length input)) input;
            head = 1;
            instr_ptr = 1;
          }
      in
      print_endline ("Head: " ^ string_of_int final_head);
      print_string "Output tape: ";
      List.iter (fun (_, s) -> print_string (s ^ ", ")) final_tape;
      print_char '\n'
    with Parser.Error ->
      raise (Failure ("Parse error at " ^ pos_string lexbuf.lex_curr_p))
  in
  ()
