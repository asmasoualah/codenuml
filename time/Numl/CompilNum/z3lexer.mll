

{

open Types;;
open Z3parser ;;
open Prelude ;;

}

rule token = parse
  ' '																	{ token lexbuf }
| '\t'                                                      	       	{ token lexbuf }
| '\n'                                									{ codeLine := !codeLine+1; token lexbuf }
| '\r'																	{ token lexbuf }
| eof                                                        			{ EOF }
| ('-'?)(['0'-'9']+)	 	{   let s = Lexing.lexeme lexbuf 
																			in INT(int_of_string s)
		 																}	
| '('                                                              		{ PARBEG }
| ')'                                                              		{ PAREND }
| '-'                                                              		{ MINUS }
| "sat"																	{ SAT }
| "model"																{ MODEL }
| "define-fun"															{ DEF }
| "Int"																	{ INTEGER }
| "unsat"																{ UNSAT }
| (['a'-'z']|['A'-'Z']|'_'|'!')((['a'-'z']|['A'-'Z']|'_'|'!'|['0'-'9'])*) as word { ID(word) }


