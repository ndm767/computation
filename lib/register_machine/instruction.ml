type instruction =
  | Add of string * int
  | Del of int
  | Clr of int
  | Copy of int * int
  | Goto of int
  | IfFirst of int * string * int
  | Stop

let print_instr instr =
  match instr with
  | Add (s, i) -> "ADD " ^ s ^ " R" ^ string_of_int i
  | Del i -> "DEL R" ^ string_of_int i
  | Clr i -> "CLR R" ^ string_of_int i
  | Copy (m, n) -> "R" ^ string_of_int m ^ " <- R" ^ string_of_int n
  | Goto l -> "GOTO " ^ string_of_int l
  | IfFirst (r, s, l) ->
      "IF FIRST R" ^ string_of_int r ^ " " ^ s ^ " GOTO " ^ string_of_int l
  | Stop -> "STOP"

let get_instr_reg instr =
  match instr with
  | Add (_, r) -> Some r
  | Del r -> Some r
  | Clr r -> Some r
  | IfFirst (r, _, _) -> Some r
  | Copy _ | Goto _ | Stop -> None

let reg_from_str str = int_of_string (String.sub str 1 (String.length str - 1))
let cons_pair a b = (a, b)
let cons_pair3 a b c = (a, b, c)
