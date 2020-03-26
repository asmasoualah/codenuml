
open Gmp ;;

(*type sign = 
  Zero
| Pos
| Neg
| Top
;;*)

type expression =
  Float of F.t * int * int
| Int of int
| Bool of bool
| Id of string
| Lambda of string * expression * expression
| Pi of string * expression * expression
| App of expression * expression
| Cond of expression * expression * expression
| Let of string * expression * expression
| Pair of expression * expression
| First of expression
| Second of expression
| Cons of expression * expression
| Car of expression
| Cdr of expression
| Nil
| Plus of expression * expression * int * int
| Minus of expression * expression * int * int
| Mult of expression * expression * int * int
| Div of expression * expression * int * int
| IPlus of expression * expression
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

| BEq of expression * expression 
| BNeq of expression * expression 
| BLt of expression * expression 
| BGt of expression * expression 
| BLte of expression * expression 
| BGte of expression * expression 

| FEq of expression * expression * int * int
| FNeq of expression * expression * int * int 
| FLt of expression * expression * int * int
| FGt of expression * expression * int * int 
| FLte of expression * expression * int * int 
| FGte of expression * expression * int * int

| TInt 
| TBool 
| TFloat of (expression ref) * (expression ref) * (expression ref)
| TypeVar of typeOption ref * string
| TProduct of expression * expression
| TList of expression

and typeOption =
  None
| Some of expression ;;


type toplevel =
  TopExpr of expression
| TopAssign of string * expression 
| TopRecAssign of string * expression 
| TopVerbose of expression
| TopBinary of expression
| Exit
;;

(*
let addSign s1 s2 = match s1 s2 with
  (Zero,Zero) -> Zero
| (Zero,Pos) -> Pos
| (Zero,Neg) -> Neg
| (Zero,Top) -> Top
| (Pos,Zero) -> Pos
| (Pos,Pos) -> Pos
| (Pos,

*)

