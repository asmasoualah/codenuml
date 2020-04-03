{

open Types ;;
open Prelude ;;
open Parseurcomp ;;


}

rule token = parse
  ' '									{ token lexbuf }
| '\t'                                                      	       	{ token lexbuf }
| '\n'                                					{ codeLine := !codeLine+1; token lexbuf }
| '\r'									{ token lexbuf }
| eof                                                  			{ EOF }
| ('-'?)(['0'-'9']*)('.')(['0'-'9']*)((('e'|'E')(('-'?)('+'?)(['0'-'9'])+))?)  	
									{ let s = Lexing.lexeme lexbuf 
									  in FLOAT(s)
	 								}				
| ('-'?)(['0'-'9']+)(('e'|'E')(('-'?)('+'?)(['0'-'9'])+))	  						
									{   let s = Lexing.lexeme lexbuf 
									    in FLOAT(s)
									}	
| ('-'?)(['0'-'9']+)	  						{   let s = Lexing.lexeme lexbuf 
									    in INT(int_of_string s)
		 							}	
| '('                                                              	{ PARBEG }
| ')'                                                           	{ PAREND }
| '#'                                                                   { SHARP }
| '+'                                                          		{ PLUS }
| '-'                                                         		{ MINUS }
| '*'			                                                { MULT }
| '/'			                                                { DIV }
| "sqrt"                                                                { SQRT } 
| ";;"                                                                  { STOP }
| ';'                                                                   { SEMICOLON }
| '<'                                                          		{ LT }
| '>'                                                          		{ GT }
| "<="                                                                 	{ LTE }
| ">="                                                         		{ GTE }
| "!="                                                         		{ NEQ }
| "if"									{ IF }
| "else"								{ ELSE }
| "then"								{ THEN }
| "let"			 					        { LET }
| "rec"			 					        { REC }
| "true"			 				        { TRUE }
| "false"			 				        { FALSE }
| "in"			 					        { IN }
| "fst"			 					        { FST }
| "snd"			 					        { SND }
| "exit"			 					{ EXIT }
| "quit"			 					{ EXIT }
| '='                                                              	{ EQ }
| ','                                                              	{ COMMA }
| "->"                                                              	{ DOT }
| "fun"                                                              	{ LAMBDA }
| "+_"                                                                  { IPLUS }
| "-_"                                                                  { IMINUS }
| "*_"                                                                  { IMULT }
| "/_"                                                                  { IDIV } 
| "max"                                                                 { IMAX }
| "iota"                                                                { IOTA }
| '['                                                                   { BRABEG }
| ']'                                                                   { BRAEND }
| '{'                                                                   { ACCBEG }
| '}'                                                                   { ACCEND }
| "::"                                                                  { CONS }
| "hd"                                                                  { CAR }
| "tl"                                                                  { CDR }
| "List.hd"                                                             { CAR }
| "List.tl"                                                             { CDR }
| "verbose"                                                             { VERBOSE }
| "binary"                                                              { BINARY }
| (['a'-'z']|['A'-'Z']|'_')((['a'-'z']|['A'-'Z']|'_'|['0'-'9'])*) as word { ID(word) }





