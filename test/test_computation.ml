open Instruction_turing.Model

let print_state_noopt state =
  print_endline ("Instruction pointer: " ^ string_of_int state.instr_ptr);
  print_endline ("Head: " ^ string_of_int state.head);
  List.iter (fun (_, s) -> print_string (s ^ ", ")) state.tape;
  print_string "\n"

let print_state state =
  match state with None -> () | Some s -> print_state_noopt s

let () = print_endline "Print test: "

let () =
  print_state
    (run_single_instruction (Print "1")
       { tape = [ (1, "0"); (2, "0"); (3, "0") ]; head = 2; instr_ptr = 1 })

let () = print_endline "Right overflow: "

let () =
  print_state
    (run_single_instruction Right
       { tape = [ (1, "0"); (2, "0"); (3, "0") ]; head = 3; instr_ptr = 1 })

let () = print_endline "Right normal: "

let () =
  print_state
    (run_single_instruction Right
       { tape = [ (1, "0"); (2, "0"); (3, "0") ]; head = 1; instr_ptr = 1 })

let () = print_endline "Left underflow: "

let () =
  print_state
    (run_single_instruction Left
       { tape = [ (1, "0"); (2, "0"); (3, "0") ]; head = 1; instr_ptr = 1 })

let () = print_endline "Left normal: "

let () =
  print_state
    (run_single_instruction Left
       { tape = [ (1, "0"); (2, "0"); (3, "0") ]; head = 3; instr_ptr = 1 })
