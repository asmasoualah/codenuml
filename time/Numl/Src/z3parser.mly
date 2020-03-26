%{

open Prelude ;;

%}

%type < bool > z3
%start z3


%token MINUS 
%token PARBEG PAREND SAT UNSAT INTEGER DEF MODEL
%token EOF 
%token <string> ID 
%token <int> INT

%%
z3	    : SAT PARBEG MODEL defs PAREND EOF			    { true }
            | SAT PARBEG MODEL PAREND EOF	 		    { true }				
            | UNSAT	 					    { false }
;
defs        : def                                                   { () }
            | def defs                                              { () }
;
def 	    : PARBEG DEF ID PARBEG PAREND INTEGER inte PAREND       { intEnv := setEnv $3 $7 !intEnv }
;  
inte        : INT 					            { $1 }
	    | PARBEG MINUS INT PAREND				    { (-1) * $3 }
;
%%