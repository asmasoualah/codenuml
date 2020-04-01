open Types ;;
open Prelude ;;
open Primitives ;;
open Parseurcomp ;;
open Print ;;
open Eval ;;
open Checker ;;
open Gmp ;;
open Compile ;;
open Compilegmp;;
open Nana;;
open Printf ;;

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
in t3
;;

let _ = 
  try
        let _ = symb := 0 ; symbE := 0 in 
        let lexbuf = Lexing.from_channel stdin in
        try 
          let c = Parser.compile Lexer.token lexbuf in
          let fic = open_out "comp.ml" in                   
          let fig = open_out "gomp.ml" in
          let _ = iterCmdList c fic fig in              
          let _ = close_out fic in 
          let _ = close_out fig in 
            

         let rec iterCmdList cl fic fig = match cl with
                [] -> ()
                | c::cs -> 
            (match c with
		   TopExpr(e) -> 
		              let typeEnv' = copyTypeEnv !typeEnv [] in
			      let t = typeCheck e typeEnv' varEnv in	
                             	    let t' = evalPart t in 
                                    let s = genCaml e in
                                    let _ = output_string fic s in
                                    let k = genGmp e in
                                    let _ = output_string fig k in
                                      iterCmdList cs fic fig 
                              
             | TopAssign(x,e) -> let typeEnv' = copyTypeEnv !typeEnv [] in
			         let t = typeCheck e typeEnv' varEnv in
                                            let t' = evalPart t in
                                            let s = "let "^x^" = "^(genCaml e) in
                                            let _ = output_string fic s in
                                            let k = "let "^x^" = "^(genGmp e) in
                                            let _ = output_string fig k in
                                               iterCmdList cs fic fig

             | TopRecAssign(x,e) -> let typeEnv' = copyTypeEnv !typeEnv [] in
                                    let typeEnv'' = typeEnv' in
                                            let t = recTypeCheck x e typeEnv'' varEnv in 
                                            let t' = if (isRec t []) then unRec t [] else t in
                                            let t'' = evalPart t' in
                                            let s = "let rec"^x^" = "^(genCaml e) in
                                            let _ = output_string fic s in 
                                            let k = "let rec"^x^" = "^(genGmp e) in
                                            let _ = output_string fig k in 
                                               iterCmdList cs fic fig
                                               (*| _ -> print_string ("Error: This expression has type "^(printExpr t'')^ 
                                                            "\n       This is not a function; it cannot be applied."); *)
                                                                                       
            )
        with
          Failure(s) -> print_string ("Parse errori: "^s)
        | Parsing.Parse_error -> print_string ("Parse error")
        | Error(e) -> print_string ("Error: "^e) 
        | RecTypeError(_,e) -> print_string ("\nError: "^e)
  with  Error(e) -> print_string ("\nFatal error: "^e^"\n")
      | RecTypeError(_,e) -> print_string ("\nFatal error: "^e^"\n")
;;
