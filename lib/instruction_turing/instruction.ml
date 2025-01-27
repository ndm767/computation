type instruction =
  | Del
  | Right
  | Left
  | Stop
  | Print of string
  | Goto of int
  | IfGoto of string * int

let cons_ifgoto symb line = IfGoto (symb, line)

let print_instr instr =
  match instr with
  | Del -> "DEL"
  | Right -> "RIGHT"
  | Left -> "LEFT"
  | Stop -> "STOP"
  | Print s -> "PRINT " ^ s
  | Goto l -> "GOTO " ^ string_of_int l
  | IfGoto (s, l) -> "IF " ^ s ^ " GOTO " ^ string_of_int l
