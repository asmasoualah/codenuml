

%{

open Types ;;
open Prelude ;;
open Primitives ;;
open Gmp ;;
open Print ;; 

%}

%type < Types.toplevel > program
%start program


%token PLUS MINUS MULT DIV IF THEN ELSE BRABEG BRAEND
%token PARBEG PAREND SHARP LAMBDA DOT COMMA FST SND EXIT VERBOSE BINARY
%token EOF LT GT EQ LTE GTE NEQ COLON STOP LET IN REC TRUE FALSE
%token IPLUS IMINUS IMULT IDIV IMAX IOTA ACCBEG ACCEND
%token BRABEG BRAEND CONS CAR CDR SEMICOLON
%token <string> ID 
%token <int> INT
%token <string> FLOAT
%left PLUS MINUS
%left MULT 
%left IPLUS IMINUS
%left IMULT IDIV

%%
program		: expr STOP		                       { TopExpr($1) }
                | LET ID EQ expr STOP                          { TopAssign($2,$4) }	
                | LET ID idlist EQ expr STOP                   { let _ = typeEnv := setEnv $2 (makeTypeFun (fst $3)) !typeEnv in
TopAssign($2,makeFun (snd $3) $5) }
                | LET REC ID idlist EQ expr STOP               { let _ = typeEnv := setEnv $3 (makeTypeFun (fst $4)) !typeEnv in TopRecAssign($3,makeFun (snd $4) $6) }
                | LET REC ID ACCBEG INT COMMA INT ACCEND idlist EQ expr STOP               { let _ = typeEnv := setEnv $3 (makeTypeFun2 (fst $9) (TFloat(ref (Int 2),ref (Int $5),ref (Int $7)))) !typeEnv in 
let _ = err ("\n"^$3^"="^(printExpr (makeTypeFun (fst $9)))^"\n") in
TopRecAssign($3,makeFun (snd $9) $11) }
                | EXIT STOP                                    { Exit }
                | VERBOSE bexpr STOP                           { TopVerbose($2) }
                | BINARY bexpr STOP                            { TopBinary($2) }
;
idlist          : ID                                           { ([newTypeVar ()],[$1]) }
                | ID ACCBEG INT COMMA INT ACCEND               { ([TFloat(ref (Int 2),ref (Int $3),ref (Int $5))],[$1]) }
                | ID idlist                                    { ((newTypeVar ())::(fst $2),$1::(snd $2)) }
                | ID ACCBEG INT COMMA INT ACCEND idlist        { ((TFloat(ref (Int 2),ref (Int $3),ref (Int $5)))::(fst $7),$1::(snd $7)) }
;
exprNoApp       : expr PLUS expr                               { let i = newInt () in 
                                                                 let _ = varEnv := setEnv ("+__"^(string_of_int i)) (newPlusVar 0 0) !varEnv in
                                                                 let _ = typeEnv := setEnv ("+__"^(string_of_int i)) (newPlusType ()) !typeEnv in
                                                                   App(App(Id("+__"^(string_of_int i)),$1),$3) }
                | expr MINUS expr                              { let i = newInt () in 
                                                                 let _ = varEnv := setEnv ("-__"^(string_of_int i)) (newMinusVar 0 0) !varEnv in
                                                                 let _ = typeEnv := setEnv ("-__"^(string_of_int i)) (newMinusType ()) !typeEnv in
                                                                   App(App(Id("-__"^(string_of_int i)),$1),$3) }
                | expr MULT expr                               { let i = newInt () in 
                                                                 let _ = varEnv := setEnv ("*__"^(string_of_int i)) (newMultVar 0 0) !varEnv in
                                                                 let _ = typeEnv := setEnv ("*__"^(string_of_int i)) (newMultType ()) !typeEnv in
                                                                   App(App(Id("*__"^(string_of_int i)),$1),$3) }
                | expr DIV expr                                { let i = newInt () in 
                                                                 let _ = varEnv := setEnv ("/__"^(string_of_int i)) (newDivVar 0 0) !varEnv in
                                                                 let _ = typeEnv := setEnv ("/__"^(string_of_int i)) (newDivType ()) !typeEnv in
                                                                   App(App(Id("/__"^(string_of_int i)),$1),$3) }
                | expr IPLUS expr                              { App(App(Id("+I_"),$1),$3) }
                | expr IMINUS expr                             { App(App(Id("-I_"),$1),$3) }
                | expr IMULT expr                              { App(App(Id("*I_"),$1),$3) }
                | expr IDIV expr                               { App(App(Id("/I_"),$1),$3) }
                | LAMBDA idlist DOT expr                       { makeFun (snd $2) $4 }
                | IF bexpr THEN expr ELSE expr                 { Cond($2,$4,$6) }
                | PARBEG expr PAREND                           { $2 }
                | LET ID EQ expr IN expr                       { Let($2,$4,$6) }
                | expr COMMA expr                              { Pair($1,$3) } 
                | FST expr                                     { First($2) }
                | SND expr                                     { Second($2) }

                | BRABEG BRAEND                                { Nil }
                | expr CONS expr                               { Cons($1,$3) }
		| CAR expr                                     { Car($2) }
		        | CDR expr                                     { Cdr($2) }
		        | BRABEG listElts BRAEND                       { $2 }

                | bexpr                                        { $1 }
                | FLOAT                                        { Float(my_f_of_string_prec $1 53,ufp (float_of_string $1),53) }
				| FLOAT ACCBEG INT ACCEND                      { Float(my_f_of_string_prec $1 $3,ufp (float_of_string $1),$3) }
       				| FLOAT ACCBEG INT COMMA INT ACCEND            { if ($3-(ufp (float_of_string $1))+1<=$5) then Float(my_f_of_string_prec $1 $3,$3,$5) 
				                                                 else raise (Error ("Zero accuracy value: "^$1^" has ufp "^(string_of_int (ufp (float_of_string $1)))^
																 " but type Float{"^(string_of_int $3)^","^(string_of_int $5)^"} has ulp "^(string_of_int ($3-$5+1))))
															   }
                | INT                                          { Int($1) }
                | ID                                           { Id($1) }
;
expr            : expr exprNoApp                               { App($1,$2) }           
                | exprNoApp                                    { $1 }
;
bexpr           : TRUE                                         { Bool(true) }
                | FALSE                                        { Bool(false) } 
                | expr EQ expr                                 { App(App(Id("=B_"),$1),$3) }
                | expr LT expr                                 { App(App(Id("<B_"),$1),$3) }
                | expr GT expr                                 { App(App(Id(">B_"),$1),$3) }
                | expr LTE expr                                { App(App(Id("<=_"),$1),$3) }
                | expr GTE expr                                { App(App(Id(">=_"),$1),$3) }
                | expr NEQ expr                                { App(App(Id("!=_"),$1),$3) }
                | expr EQ ACCBEG INT COMMA INT ACCEND expr     { let i = newInt () in
                                                                 let _ = varEnv := setEnv ("=F_"^(string_of_int i)) (newEqFloatVar $4 $6) !varEnv in
                                                                 let _ = typeEnv := setEnv ("=F_"^(string_of_int i)) (newEqFloatType 2 $4 $6) !typeEnv in
                                                                   App(App(Id("=F_"^(string_of_int i)),$1),$8) }
                | expr LT ACCBEG INT COMMA INT ACCEND expr     { let i = newInt () in
                                                                 let _ = varEnv := setEnv ("<F_"^(string_of_int i)) (newLtFloatVar $4 $6) !varEnv in
                                                                 let _ = typeEnv := setEnv ("<F_"^(string_of_int i)) (newLtFloatType 2 $4 $6) !typeEnv in
                                                                   App(App(Id("<F_"^(string_of_int i)),$1),$8) }
                | expr LTE ACCBEG INT COMMA INT ACCEND expr     { let i = newInt () in
                                                                 let _ = varEnv := setEnv ("<=F"^(string_of_int i)) (newLteFloatVar $4 $6) !varEnv in
                                                                 let _ = typeEnv := setEnv ("<=F"^(string_of_int i)) (newLteFloatType 2 $4 $6) !typeEnv in
                                                                   App(App(Id("<=F"^(string_of_int i)),$1),$8) }
                | expr GT ACCBEG INT COMMA INT ACCEND expr     { let i = newInt () in
                                                                 let _ = varEnv := setEnv (">F_"^(string_of_int i)) (newGtFloatVar $4 $6) !varEnv in
                                                                 let _ = typeEnv := setEnv (">F_"^(string_of_int i)) (newGtFloatType 2 $4 $6) !typeEnv in
                                                                   App(App(Id(">F_"^(string_of_int i)),$1),$8) }
                | expr GTE ACCBEG INT COMMA INT ACCEND expr     { let i = newInt () in
                                                                 let _ = varEnv := setEnv (">=F"^(string_of_int i)) (newGteFloatVar $4 $6) !varEnv in
                                                                 let _ = typeEnv := setEnv (">=F"^(string_of_int i)) (newGteFloatType 2 $4 $6) !typeEnv in
                                                                   App(App(Id(">=F"^(string_of_int i)),$1),$8) }
                | expr NEQ ACCBEG INT COMMA INT ACCEND expr     { let i = newInt () in
                                                                 let _ = varEnv := setEnv ("!=F"^(string_of_int i)) (newNeqFloatVar $4 $6) !varEnv in
                                                                 let _ = typeEnv := setEnv ("!=F"^(string_of_int i)) (newNeqFloatType 2 $4 $6) !typeEnv in
                                                                   App(App(Id("!=F"^(string_of_int i)),$1),$8) }



                | PARBEG bexpr PAREND                          { $2 }
;
listElts        : expr                                         { Cons($1,Nil) }
                | expr SEMICOLON listElts                      { Cons($1,$3) }
;
%% 

