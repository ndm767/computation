open Table

let parse_transition (input : unparsed_transition) =
  {
    in_state = input.in_state;
    read = (if input.read = "*" then None else Some input.read);
    write = input.write;
    move =
      (if input.move = "L" then Left
       else if input.move = "R" then Right
       else Stay);
    out_state = input.out_state;
  }

let make_transition_graph (input : unparsed_transition list) =
  List.fold_left
    (fun tbl (entry : unparsed_transition) ->
      Hashtbl.add tbl entry.in_state (parse_transition entry);
      tbl)
    (Hashtbl.create (List.length input))
    input

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

let rec get_symb (tape : (int * string) list) head =
  match tape with
  | [] -> None
  | (i, s) :: tape' -> if head = i then Some s else get_symb tape' head

let find_transition graph state symb =
  let results = Hashtbl.find_all graph state in
  match List.find_opt (fun x -> x.read = Some symb) results with
  | Some t -> Some t
  | None -> List.find_opt (fun x -> x.read = None) results

let expect x =
  match x with
  | Some y -> y
  | None -> raise (Failure "Expected Some but got None!")

let step = ref 0

let rec run_table_turing_internal graph curr_state head tape =
  if Hashtbl.find_opt graph curr_state = None then (head, tape)
  else
    let _ = step := 1 + !step in
    let curr_symb = expect (get_symb tape head) in
    let curr_transition = find_transition graph curr_state curr_symb in
    match curr_transition with
    | None -> (head, tape)
    | Some t ->
        let new_tape = change_tape tape head t.write in
        let tape', head' =
          match t.move with
          | Left -> move_left new_tape head
          | Right -> move_right new_tape head
          | Stay -> (new_tape, head)
        in
        run_table_turing_internal graph t.out_state head' tape'

let colnum (pos : Lexing.position) = pos.pos_cnum - pos.pos_bol - 1

let pos_string (pos : Lexing.position) =
  let l = string_of_int pos.pos_lnum and c = string_of_int (colnum pos + 1) in
  "Line: " ^ l ^ ", Column: " ^ c

let rec number_list_aux c n =
  if c > n then [] else c :: number_list_aux (c + 1) n

let number_list n = number_list_aux 1 n

let run_table_turing instructions input =
  let _ =
    let lexbuf = Lexing.from_string instructions in
    try
      let parse_res = Parser.program Lexer.read lexbuf in
      let out_head, out_tape =
        run_table_turing_internal
          (make_transition_graph parse_res)
          (List.hd parse_res).in_state 1
          (List.combine (number_list (List.length input)) input)
      in
      print_endline ("Head: " ^ string_of_int out_head);
      print_endline ("Number of steps: " ^ string_of_int !step);
      print_string "Output tape: ";
      List.iter (fun (_, s) -> print_string (s ^ ", ")) out_tape;
      print_char '\n'
    with Parser.Error ->
      raise (Failure ("Parse error at " ^ pos_string lexbuf.lex_curr_p))
  in
  ()
