type unparsed_transition = {
  in_state : string;
  read : string;
  write : string;
  move : string;
  out_state : string;
}

type move = Left | Right | Stay

type transition = {
  in_state : string;
  read : string option;
  write : string; (* could this be string option? *)
  move : move;
  out_state : string;
}
