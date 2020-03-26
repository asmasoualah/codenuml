type token =
  | PLUS
  | MINUS
  | MULT
  | DIV
  | IF
  | THEN
  | ELSE
  | BRABEG
  | BRAEND
  | PARBEG
  | PAREND
  | SHARP
  | LAMBDA
  | DOT
  | COMMA
  | FST
  | SND
  | EXIT
  | VERBOSE
  | BINARY
  | EOF
  | LT
  | GT
  | EQ
  | LTE
  | GTE
  | NEQ
  | COLON
  | STOP
  | LET
  | IN
  | REC
  | TRUE
  | FALSE
  | IPLUS
  | IMINUS
  | IMULT
  | IDIV
  | IMAX
  | IOTA
  | ACCBEG
  | ACCEND
  | CONS
  | CAR
  | CDR
  | SEMICOLON
  | ID of (string)
  | INT of (int)
  | FLOAT of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Types.toplevel 
