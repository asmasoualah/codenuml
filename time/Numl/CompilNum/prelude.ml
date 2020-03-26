
open Types ;;
open Gmp ;;

let codeLine = ref 1 ;;

let symb = ref 0 ;;

let prettySymb = ref 0 ;;

let symbE = ref 0 ;;

let intGen = ref 0 ;;

let verbose = ref false ;;

let binary = ref false;;

let intEnv = ref (("type gen",0)::[]) ;; 

let cmpTypEnv = ref (("type gen","INT")::[]) ;; (* type d'env pour les operations de comparaison *) 

let opEnvUP = ref (("type gen",0)::[]) ;; (* type d'env pour les operations arithémtiques (float) p  *) 

exception Error of string ;;
exception SolveError ;;
exception RecTypeError of expression * string ;;

let err s = (print_string s; flush stdout)

let newVar () = let _ = symb := !symb + 1 in ("'a_"^(string_of_int !symb)) ;;

let newTypeVar () = let v = newVar () in TypeVar(ref None,v) ;;

let newExprVar () = let _ = symbE := !symbE + 1 in ("a_"^(string_of_int !symbE)) ;;

let newInt () = let _ = intGen := !intGen + 1 in !intGen ;;

let rec pow x i = if (i=0) then 1.0 else 
  if (i<0) then ((pow x (i+1) )/. x) else ((pow x (i-1))*. x) ;;

let rec ufp1 x i = if (x<(pow 2.0 i)) then (i-1) else ufp1 x (i+1);;

let rec ufp0 x i = if (x>=(pow 2.0 i)) then i else ufp0 x (i-1);;

let ufpp x = if (x>=1.0) then ufp1 x 0 else ufp0 x 0 ;;

let ufp x = match (classify_float x) with
  FP_normal -> ufpp (if (x>=0.0) then x else (x *. (-1.0))) 
| FP_subnormal -> ufpp (if (x>=0.0) then x else (x *. (-1.0))) 
| FP_zero -> 0
| FP_infinite -> raise (Error ("ufp error: infinity")) 
| FP_nan -> raise (Error ("ufp error: nan"))  
;;

let ulp x p = (ufp x)-p ;;

let eps x p = pow 2.0 (ulp x p) ;;

let round x = 
  let f = floor x in
  let c = ceil x in
    int_of_float (if ((x -. f) < (c -. x)) then f else c) ;;


let rec rev s = if ((String.length s)=0) then
                  s
                else (rev (String.sub s 1 ((String.length s)-1)))^(String.sub s 0 1) 
;;


let rec ent_of_string s res = 
  if ((String.length s)=0) then res
  else
    let c = String.get s 0 in
    let x = F.mul res (F.from_float 2.0) in
    let y = if (c='0') then x
            else F.add x (F.from_float 1.0) 
    in ent_of_string (String.sub s 1 ((String.length s)-1)) y ;;


let rec frac_of_string s res =
  if ((String.length s)=0) then res 
  else
    let c = String.get s 0 in
    let x = F.mul res (F.from_float 0.5) in
    let y = if (c='0') then x
            else F.add x (F.from_float 0.5) 
    in frac_of_string (String.sub s 1 ((String.length s)-1)) y ;;


let my_f_of_string_prec s p = F.from_string s ;;


let getName t defaultName = match t with
  TypeVar(_,n) -> n
| _ -> defaultName ;;


let getExprInt e = match e with
  Int(i) -> i
| _ -> raise (Error("In getExprInt (for compile) : int expected")) ;;


let getSign x =
  let f = float_of_string x 
  in if (f<0.0) then -1 else
     if (f>0.0) then 1 else 0 ;;


let rec getEnv id env = match env with
  [] -> raise (Error ("unbound value "^id))
| (id',v)::env' -> if (id = id') then v else (getEnv id env') ;;


let rec getEnvComplete id env default = match env with
  [] -> default
| (id',v)::env' -> if (id = id') then v else (getEnvComplete id env' default) ;;


let rec setEnv id v env = match env with
  [] -> (id,v)::[]
| (id',v')::env' -> if (id=id') then (id,v)::env' else (id',v')::(setEnv id v env') ;;

 
let rec makeFun idl e = match idl with
  [] -> raise (Error "fun without arg???")
| x::[] -> Lambda(x,newTypeVar (),e)
| x::xs -> Lambda(x,newTypeVar (),makeFun xs e) ;;


let rec makeTypeFun tl = match tl with
  [] -> newTypeVar ()
| t::ts -> Pi("x",t,makeTypeFun ts) ;;


let rec makeTypeFun2 tl tf = match tl with
  [] -> tf
| t::ts -> Pi("x",t,makeTypeFun2 ts tf) ;;


let rec buildApp e el = match el with
  [] -> e
| e'::es -> buildApp (App(e,e')) es ;;


let fst3 (a,b,c) = a ;;


let rec isRec t l = match t with
  TFloat(r0,r1,r2) -> (isRec !r0 l) || (isRec !r1 l) || (isRec !r2 l)
| Pi(_,t1,t2) -> (isRec t1 l) || (isRec t2 l)
| TypeVar(r,s) -> isRecVar r s l
| TProduct(t1,t2) -> (isRec t1 l) || (isRec t2 l) 
| TList(t1) -> (isRec t1 l)  
| IPlus(t1,t2) -> (isRec t1 l) || (isRec t2 l)
| IMinus(t1,t2) -> (isRec t1 l) || (isRec t2 l) 
| IMult(t1,t2) -> (isRec t1 l) || (isRec t2 l) 
| IDiv(t1,t2) -> (isRec t1 l) || (isRec t2 l) 
| IMax(t1,t2) -> (isRec t1 l) || (isRec t2 l) 
| Iota(t1,t2) -> (isRec t1 l) || (isRec t2 l) 
| SigPlus(t1,t2,t3,t4) -> (isRec t1 l) || (isRec t2 l) || (isRec t3 l) || (isRec t4 l) 
| SigMinus(t1,t2,t3,t4) -> (isRec t1 l) || (isRec t2 l) || (isRec t3 l) || (isRec t4 l) 
| SigMult(t1,t2) -> (isRec t1 l) || (isRec t2 l) 
| SigmaPlus(t1,t2) -> (isRec t1 l) || (isRec t2 l) 
| SigmaMinus(t1,t2) -> (isRec t1 l) || (isRec t2 l) 
| _ -> false

and isRecVar r s l = match !r with
  None -> false
| Some(e) -> if (List.mem r l) then true else (isRec e (r::l)) ;;


let rec unRec t l = match t with
  TFloat(r0,r1,r2) -> TFloat(ref (unRec !r0 l),ref (unRec !r1 l),ref (unRec !r2 l))
| Pi(s,t1,t2) -> Pi(s,unRec t1 l,unRec t2 l)
| TypeVar(r,s) -> unRecVar r s l
| TProduct(t1,t2) -> TProduct(unRec t1 l,unRec t2 l) 
| TList(t1) -> TList(unRec t1 l)  
| IPlus(t1,t2) -> IPlus(unRec t1 l,unRec t2 l)
| IMinus(t1,t2) -> IMinus(unRec t1 l,unRec t2 l) 
| IMult(t1,t2) -> IMult(unRec t1 l,unRec t2 l) 
| IDiv(t1,t2) -> IDiv(unRec t1 l,unRec t2 l)
| IMax(t1,t2) -> IMax(unRec t1 l,unRec t2 l) 
| Iota(t1,t2) -> Iota(unRec t1 l,unRec t2 l)
| SigPlus(t1,t2,t3,t4) -> SigPlus(unRec t1 l,unRec t2 l,unRec t3 l,unRec t4 l)
| SigMinus(t1,t2,t3,t4) -> SigPlus(unRec t1 l,unRec t2 l,unRec t3 l,unRec t4 l)
| SigMult(t1,t2) -> SigMult(unRec t1 l,unRec t2 l)
| SigmaPlus(t1,t2) -> SigMult(unRec t1 l,unRec t2 l)
| SigmaMinus(t1,t2) -> SigMult(unRec t1 l,unRec t2 l)
| _ -> t

and unRecVar r s l = match !r with
  None -> TypeVar(r,s)
| Some(e) -> if (List.mem r l) then TypeVar(ref None,s) else TypeVar(ref (Some(unRec e (r::l))),s) ;;
  

