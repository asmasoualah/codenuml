type token =
  | MINUS
  | PARBEG
  | PAREND
  | SAT
  | UNSAT
  | INTEGER
  | DEF
  | MODEL
  | EOF
  | ID of (string)
  | INT of (int)

val z3 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  bool 
