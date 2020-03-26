
open Gmp ;;
type expression =
  Float of F.t * int * int * int (* valeur * signe * ufp * prec *)
| Int of int
| Bool of bool
| Id of string
| Lambda of string * expression * expression (* lambda x:t.e *)
| Pi of string * expression * expression
| App of expression * expression
| Cond of expression * expression * expression
| Let of string * expression * expression
| Pair of expression * expression
| First of expression
| Second of expression
| Cons of expression * expression (* cons e1 e 2 = e1::e2 *)
| Car of expression (* head *)
| Cdr of expression (* tail *) 
| Nil
| Plus of expression * expression * int * int * int (* float *)
| Minus of expression * expression * int * int * int
| Mult of expression * expression * int * int * int
| Div of expression * expression * int * int * int
| Sqrt of expression * int * int * int 
| IPlus of expression * expression (* int *)
| IMinus of expression * expression
| IMult of expression * expression
| IDiv of expression * expression
| IMax of expression * expression

| Iota of expression * expression
| SigPlus of expression * expression * expression * expression
| SigMinus of expression * expression * expression * expression
| SigMult of expression * expression 
| SigmaPlus of expression * expression
| SigmaMinus of expression * expression

| BEq of expression * expression (* int *)
| BNeq of expression * expression 
| BLt of expression * expression 
| BGt of expression * expression 
| BLte of expression * expression 
| BGte of expression * expression 

| FEq of expression * expression * int * int (* float *)
| FNeq of expression * expression * int * int 
| FLt of expression * expression * int * int
| FGt of expression * expression * int * int 
| FLte of expression * expression * int * int 
| FGte of expression * expression * int * int

| TInt (* types *)
| TBool 
| TFloat of (expression ref) * (expression ref) * (expression ref)
| TypeVar of (typeOption ref) * string
| TProduct of expression * expression
| TList of expression

and typeOption = (* pour variables de types *)
  None
| Some of expression ;;


type toplevel = (* boucle interactive dans le main *)
  TopExpr of expression
| TopAssign of string * expression 
| TopRecAssign of string * expression 
| TopVerbose of expression
| TopBinary of expression
| Exit
;;


