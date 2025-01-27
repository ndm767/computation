open Instruction_turing.Model
open Table_turing.Model
open Markov_algo.Model
open Register_machine.Model
open General_recursive.Model

(* Parse arguments *)
let help_msg = "Usage: computation.exe [file] [input]"
let input_file = ref ""
let input = ref []

let arg_fun arg =
  match !input_file with "" -> input_file := arg | _ -> input := arg :: !input

let () = Arg.parse [] arg_fun help_msg

(* Determine which model we're using *)
type languages =
  | InstructionTuring
  | TableTuring
  | MarkovAlgo
  | RegisterMachine
  | GeneralRecursive

let unit_to_none () = None

let lang =
  if !input_file = "" then unit_to_none (print_endline help_msg)
  else
    let () = print_endline ("Input file: " ^ !input_file) in
    let () = print_string "Input data: " in
    let () = List.iter (fun s -> print_string (s ^ ", ")) !input in
    let () = print_string "\n" in
    match String.split_on_char '.' !input_file with
    | [ _; "trn" ] -> Some InstructionTuring
    | [ _; "ttn" ] -> Some TableTuring
    | [ _; "mkv" ] -> Some MarkovAlgo
    | [ _; "rgm" ] -> Some RegisterMachine
    | [ _; "grf" ] -> Some GeneralRecursive
    | _ -> None

(* read input file *)
let input_text =
  let input_channel = open_in !input_file in
  try
    let read_text =
      really_input_string input_channel (in_channel_length input_channel)
    in
    close_in input_channel;
    read_text
  with e ->
    close_in_noerr input_channel;
    raise e

(* run model *)
let () =
  match lang with
  | Some InstructionTuring -> run_instruction_turing input_text !input
  | Some TableTuring -> run_table_turing input_text !input
  | Some MarkovAlgo -> run_markov_algo input_text !input
  | Some RegisterMachine -> run_register_machine input_text !input
  | Some GeneralRecursive -> run_general_recursive input_text !input
  | None -> print_endline "Error"
