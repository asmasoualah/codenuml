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

open Parsing;;
let _ = parse_error;;
# 1 "./z3parser.mly"


open Prelude ;;

# 22 "./z3parser.ml"
let yytransl_const = [|
  257 (* MINUS *);
  258 (* PARBEG *);
  259 (* PAREND *);
  260 (* SAT *);
  261 (* UNSAT *);
  262 (* INTEGER *);
  263 (* DEF *);
  264 (* MODEL *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  265 (* ID *);
  266 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\002\000\002\000\003\000\004\000\004\000\
\000\000"

let yylen = "\002\000\
\006\000\005\000\001\000\001\000\002\000\008\000\001\000\004\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\003\000\009\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\002\000\000\000\005\000\000\000\
\001\000\000\000\000\000\000\000\000\000\007\000\000\000\000\000\
\006\000\000\000\008\000"

let yydgoto = "\002\000\
\005\000\010\000\011\000\023\000"

let yysindex = "\004\000\
\253\254\000\000\004\255\000\000\000\000\255\254\001\255\002\255\
\010\000\008\255\010\255\005\255\000\000\013\000\000\000\013\255\
\000\000\014\255\012\255\254\254\015\255\000\000\016\255\011\255\
\000\000\017\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\012\000\000\000\000\000"

let yytablesize = 23
let yytable = "\021\000\
\003\000\004\000\008\000\009\000\001\000\006\000\007\000\022\000\
\012\000\013\000\014\000\008\000\017\000\016\000\018\000\024\000\
\019\000\020\000\025\000\027\000\026\000\004\000\015\000"

let yycheck = "\002\001\
\004\001\005\001\002\001\003\001\001\000\002\001\008\001\010\001\
\007\001\000\000\003\001\002\001\000\000\009\001\002\001\001\001\
\003\001\006\001\003\001\003\001\010\001\003\001\011\000"

let yynames_const = "\
  MINUS\000\
  PARBEG\000\
  PAREND\000\
  SAT\000\
  UNSAT\000\
  INTEGER\000\
  DEF\000\
  MODEL\000\
  EOF\000\
  "

let yynames_block = "\
  ID\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'defs) in
    Obj.repr(
# 18 "./z3parser.mly"
                                                ( true )
# 107 "./z3parser.ml"
               :  bool ))
; (fun __caml_parser_env ->
    Obj.repr(
# 19 "./z3parser.mly"
                                                 ( true )
# 113 "./z3parser.ml"
               :  bool ))
; (fun __caml_parser_env ->
    Obj.repr(
# 20 "./z3parser.mly"
                              ( false )
# 119 "./z3parser.ml"
               :  bool ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'def) in
    Obj.repr(
# 22 "./z3parser.mly"
                                                                    ( () )
# 126 "./z3parser.ml"
               : 'defs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'def) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'defs) in
    Obj.repr(
# 23 "./z3parser.mly"
                                                                    ( () )
# 134 "./z3parser.ml"
               : 'defs))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'inte) in
    Obj.repr(
# 25 "./z3parser.mly"
                                                                 ( intEnv := setEnv _3 _7 !intEnv )
# 142 "./z3parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 27 "./z3parser.mly"
                                   ( _1 )
# 149 "./z3parser.ml"
               : 'inte))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 28 "./z3parser.mly"
                                      ( (-1) * _3 )
# 156 "./z3parser.ml"
               : 'inte))
(* Entry z3 *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let z3 (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  bool )
;;
