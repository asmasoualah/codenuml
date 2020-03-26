
(* todo

printf des float
gestion des signes

*)


open Types ;;
open Prelude ;;
open Primitives ;;
open Parser ;;
open Print ;;
open Eval ;;
open Checker ;;
open Gmp ;;


print_string "\nWelcome to NuML!" ;;
print_string "\nStrongly typed numerical computations in ML..." ;;

let continue = ref true ;;


let rec eqType t1 t2 env =
print_string ("\nEqtype: "^(printExpr t1)^" != "^(printExpr t2)    ) ;
 match (t1,t2) with
  (TInt,TInt) -> true
| (TBool,TBool) -> true
| (TFloat(er1,er1',er1''),TFloat(er2,er2',er2'')) -> (eqType !er1 !er2 env) && (eqType !er1' !er2' env) && (eqType !er1'' !er2'' env)
| (TypeVar(to1,_),TypeVar(to2,_)) -> eqTypeOption !to1 !to2 env
| (TypeVar(to1,_),e2) -> (match !to1 with
			    None -> false
		          | Some(e1) -> eqType e1 e2 env
                         )
| (e1,TypeVar(to2,_)) -> (match !to2 with
	     	            None -> false
		          | Some(e2) -> eqType e1 e2 env
                         )
| (TProduct(e1,e1'),TProduct(e2,e2')) -> (eqType e1 e2 env) && (eqType e1' e2' env)
| (Float(f1,i1,j1,k1),Float(f2,i2,j2,k2)) -> (f1=f2) && (i1=i2) && (j1=j2) && (k1=k2)
| (Int(i1),Int(i2)) -> i1=i2
| (Bool(b1),Bool(b2)) -> b1=b2
| (Id(s1),Id(s2)) -> ((String.compare s1 s2)=0) || ((String.compare s2 (getEnv s1 env))=0) || ((String.compare s1 (getEnv s2 env))=0)
| (Pi(x,e1,e2),Pi(y,e1',e2')) -> (eqType e1 e1' (setEnv x y env)) && (eqType e2 e2' (setEnv y x env))
| (App(e1,e1'),App(e2,e2')) -> (eqType e1 e2 env) && (eqType e1' e2' env)
| (Cond(e1,e1',e1''),Cond(e2,e2',e2'')) -> (eqType e1 e2 env) && (eqType e1' e2' env) && (eqType e1'' e2'' env)
| (Pair(e1,e1'),Pair(e2,e2')) -> (eqType e1 e2 env) && (eqType e1' e2' env)
| (First(e1),First(e2)) -> (eqType e1 e2 env) 
| (Second(e1),Second(e2)) -> (eqType e1 e2 env) 
| _ -> false


and eqTypeOption to1 to2 env = match (to1,to2) with
  (None,None) -> true
| (Some(e1),Some(e2)) -> eqType e1 e2 env
| _ -> false;;


let rec copyTypeEnv env1 env2 = match env1 with
  [] -> env2
| (id,t)::env1' -> 
let t' = fst3 (copyType t [] [] []) 
                   in copyTypeEnv env1' ((id,t')::env2) ;;

(*
let rec modifyType tf t'' = match t'' with
  Pi(x,t1,Pi(y,t2,t3)) -> Pi(x,t1,modifyType tf (Pi(y,t2,t3)))
| Pi(x,t1,t2) -> tf

;;
*)

let rec recTypeCheck id e typeEnv varEnv = 
  let t' = typeCheck e typeEnv varEnv in
  let t'' = unRec (evalPart t') [] in
let _ = err ("\nt''="^(printExpr t'')) in
  let _ = match t'' with
            Pi(_,t2,t1) ->  ()   
          | _ -> (print_string ("Error: This expression has type "^(printExpr t')^
                                "\n       This is not a function; it cannot be applied."); flush stdout
                 ) 
  in let t3 = typeCheck e (setEnv id t'' typeEnv) varEnv 
in t3(*
let t4 = if (isRec t3 []) then unRec t3 [] else t3 in
let _ = err ("\nt4="^(printExpr t4)) in *)
(*    try typeCheck e (setEnv id t'' typeEnv) varEnv   
    with RecTypeError(tf,s) -> 
let _ = err ("\n !!!!!!!!!!!!!!!!!!!!\n ") in 
let _ = err ("\ntf="^(printExpr tf)) in 
                               typeCheck e (setEnv id tf typeEnv) varEnv 
(*                               (let t' = typeCheck e (setEnv id tf typeEnv) varEnv in
                               let t'' = evalPart t' in 
                               let _ = match t'' with
                                 Pi(_,t2,t1) -> () (*  (unify t1 t2) *) 
                               | _ -> (print_string ("Error: This expression has type "^(printExpr t')^ "\n       This is not a function; it cannot be applied."); flush stdout
                 ) 
                               in typeCheck e (setEnv id t'' typeEnv) varEnv   
) *)
       | Error(s) -> raise (Error s) *)
;;


let _ = 
  try
  while !continue do
(*   let t1 = Sys.time () in*)
        let _ = symb := 0 ; symbE := 0 in 
        let lexbuf = Lexing.from_channel stdin in
        let _ = print_string "\n\n> " ; flush stdout in
        try 
          let c = Parser.program Lexer.token lexbuf in
(*let _ = print_string (compile c) in*)
            (match c with
		   TopExpr(e) -> 
		             let typeEnv' = copyTypeEnv !typeEnv [] in
			     let t = typeCheck e typeEnv' varEnv in
                             let t' = evalPart t in 
                             let v = eval e !varEnv in 
                               print_string ("- : "^(printExpr t')^" = "^(printExpr2 v t')) ; flush stdout 
             | TopAssign(x,e) -> let typeEnv' = copyTypeEnv !typeEnv [] in
			         let t = typeCheck e typeEnv' varEnv in
                                 let t' = evalPart t in
                                 let v = eval e !varEnv in
                                   varEnv := (setEnv x v !varEnv) ; 
                                   typeEnv := (setEnv x t' !typeEnv) ;
                                   print_string ("val "^x^" : "^(printExpr t')^" = "^(printExpr2 v t')) ; flush stdout
             | TopRecAssign(x,e) -> (*  !!! e est la fonction fun x -> if x then x else f x et non pas le body *)
                                    let typeEnv' = copyTypeEnv !typeEnv [] in
				    let typeEnv'' = typeEnv'(*setEnv x (newTypeVar ()) typeEnv'*)  in
                                    let t = recTypeCheck x e typeEnv'' varEnv in 
                                    let t' = if (isRec t []) then unRec t [] else t in
                                       let t'' = evalPart t' 
                                    in (match t'' with
					 Pi(_,t1,t2) -> ((*  unify t1 t2 ; *) 
                                                         let v = eval e !varEnv 
                                                         in (varEnv := (setEnv x v !varEnv) ; 
                                                             typeEnv := (setEnv x t'' !typeEnv) ;
                                                             print_string ("val "^x^" : "^(printExpr t'')^" = "^(printExpr2 v t'')) ; 
                                                             flush stdout;
                                                            )
                                                        )
                                        | _ -> print_string ("Error: This expression has type "^(printExpr t'')^ 
                                                            "\n       This is not a function; it cannot be applied."); flush stdout
                                       ) 
             | Exit -> continue := false 
             | TopVerbose(e) -> let typeEnv' = copyTypeEnv !typeEnv [] in
                                let t = typeCheck e typeEnv' varEnv in
                                let t' = evalPart t 
                                in if (t' != TBool) then raise (Error ("This expression has type "^(printExpr t')^" but an expression was expected of type bool."))
                                   else
                                     let v = eval e !varEnv 
                                     in ((match v with
                                            Bool(b) -> verbose :=b
                                          | _ -> raise (Error "impossible in verbose top level.")
                                         ) ; print_string ("- : unit = ()") ; flush stdout)
             | TopBinary(e) ->  let typeEnv' = copyTypeEnv !typeEnv [] in
                                let t = typeCheck e typeEnv' varEnv in
                                let t' = evalPart t 
                                in if (t' != TBool) then raise (Error ("This expression has type "^(printExpr t')^" but an expression was expected of type bool."))
                                   else
                                     let v = eval e !varEnv 
                                     in ((match v with
                                            Bool(b) -> binary :=b
                                          | _ -> raise (Error "impossible in binary top level.")
                                         ) ; print_string ("- : unit = ()") ; flush stdout)

            )
        with
          Failure(s) -> print_string ("Parse error: "^s)
        | Parsing.Parse_error -> print_string ("Parse error")
        | Error(e) -> print_string ("Error: "^e) 
        | RecTypeError(_,e) -> print_string ("\nError: "^e)
(*;    let t2 = Sys.time () in
		     err ("\nt= "^(string_of_float (t2-.t1))^"\n") 

  *)    done
  with  Error(e) -> print_string ("\nFatal error: "^e^"\n")
      | RecTypeError(_,e) -> print_string ("\nFatal error: "^e^"\n")
;;


     
