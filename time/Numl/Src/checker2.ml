
open Types ;;
open Prelude ;;
open Primitives ;;
open Print ;;
open Eval ;;
open Gmp ;;




let rec isPrimitive e = match e with (* makes sense for float op only *)
Id(op) -> (try
            let s = String.sub op 0 3 in
              ((String.compare s "+__")=0) || ((String.compare s "-__")=0) ||
              ((String.compare s "*__")=0) || ((String.compare s "/__")=0) ||
              ((String.compare s "<F_")=0) 
           with _ -> false
	  )
| TypeVar(r,_) -> isPrimitiveVar !r
| App(e1,e2) -> isPrimitive e1
| _ -> false 


and isPrimitiveVar r = match r with
  Some(t) -> isPrimitive t
| _ -> false ;;


let rec getPrimitiveName e = match e with (* makes sense for float op only *)
  Id(op) -> op
| TypeVar(r,_) -> getPrimitiveVarName !r
| App(e1,e2) -> getPrimitiveName e1
| _ -> raise (Error "???getPrimitiveName")


and getPrimitiveVarName r = match r with
  Some(t) -> getPrimitiveName t
| _ -> raise (Error "???getPrimitiveVarName") ;;


let rec getS t = match t with
  TFloat(s,u,p) -> let s' = evalPart !s in
                     (match s' with
                        TypeVar(_) -> getS s' 
                      | Int(ss) -> ss 
                      | _ -> 0
                     )
| TypeVar(r,n) -> getSVar r n
| _ -> 0

and getSVar r n = match !r with
  None -> 0
| Some(t) -> getS t 
;;

let rec getU t = match t with
  TFloat(s,u,p) -> let u' = evalPart !u in
                     (match u' with
                        TypeVar(_) -> getU u' 
                      | Int(uu) -> uu 
                      | _ -> 0
                     )
| TypeVar(r,n) -> getUVar r n
| _ -> 0

and getUVar r n = match !r with
  None -> 0
| Some(t) -> getU t 
;;

let rec getP t = match t with
  TFloat(s,u,p) -> let p' = evalPart !p in 
                     (match p' with
                        TypeVar(_) -> getU p' 
                      | Int(pp) -> pp 
                      | _ -> 0
                     )
| TypeVar(r,n) -> getPVar r n
| _ -> 0

and getPVar r n = match !r with
  None -> 0
| Some(t) -> getP t 
;;



let updateValEnv id vEnv t = 
  let s = getS t in
  let u = getU t in
  let p = getP t in
  let x = String.sub id 0 3 in
  let v = if ((String.compare x "+__")=0) then newPlusVar s u p else
          if ((String.compare x "-__")=0) then newMinusVar s u p else
          if ((String.compare x "*__")=0) then newMultVar s u p else
          if ((String.compare x "/__")=0) then newDivVar s u p else 
          if ((String.compare x "<F_")=0) then newLtFloatType 2 u p else 
          if ((String.compare x ">F_")=0) then newGtFloatType 2 u p else 
          if ((String.compare x "=F_")=0) then newEqFloatType 2 u p else 
          if ((String.compare x "<=F")=0) then newLteFloatType 2 u p else 
          if ((String.compare x ">=F")=0) then newGteFloatType 2 u p else 
          if ((String.compare x "!=F")=0) then newNeqFloatType 2 u p else 
            raise (Error "???updateValEnv") 
  in vEnv := setEnv id v !vEnv ;;


let rec getArgType t env = match t with
  Pi(_,t1,t2) -> t1
(* | Id(x) -> getEnv x env ??? plutot getArgType (getEnv x env) ??? *)
| TypeVar(r,s) -> getArgTypeVar !r s env
| _ -> raise (Error ("Error: This expression has type "^(printExpr t)^"\nThis is not a function; it cannot be applied."))


and getArgTypeVar r s env = match r with
  None -> TypeVar(ref r,s)
| Some(t) -> getArgType t env
;;

let rec getRetType t env = match t with
  Pi(_,t1,t2) -> t2
(*| Id(x) -> getEnv x env  ??? plutot getRetType (getEnv x env) ??? *)
| TypeVar(r,s) -> getRetTypeVar !r s env
| _ -> raise (Error ("Error: This expression has type "^(printExpr t)^"\nThis is not a function; it cannot be applied."))


and getRetTypeVar r s env = match r with
  None -> TypeVar(ref r,s)
| Some(t) -> getRetType t env
;;


let getFunId e = match e with
  Id(s) -> s
| _ -> raise (Error ("unexpected case in getFunId: "^(printExpr e)))
;;


(*
let rec len l = if (l=[]) then 0 else len (tl l) ;;
let rec len l = if (l=[]) then 0 else len (hd l) ;;
let rec f x = if (x>10000.0) then x else f (x*1.2);;
let rec f x = f (x+0.1);; 
 *)


let rec copyType t env rEnv bl = match t with
  Float(_) -> (t,env,rEnv)
| Int(_) -> (t,env,rEnv)
| Bool(_) -> (t,env,rEnv)
| Pi(s,t1,t2) -> let s' = newVar () in
                 let env' = setEnv s s' env in
		 let rEnv' = setEnv s' (ref None) rEnv in
                 let (t1',env1,rEnv1) = copyType t1 env' rEnv' bl in
                 let (t2',env2,rEnv2) = copyType t2 env1 rEnv1 bl in
                   (Pi(s',t1',t2'),env2,rEnv2)
| IPlus(t1,t2) -> let (t1',env1,rEnv1) = copyType t1 env rEnv bl in
                  let (t2',env2,rEnv2) = copyType t2 env1 rEnv1 bl in
                      (IPlus(t1',t2'),env2,rEnv2) 
| IMinus(t1,t2) -> let (t1',env1,rEnv1) = copyType t1 env rEnv bl in
                   let (t2',env2,rEnv2) = copyType t2 env1 rEnv1 bl in
                       (IMinus(t1',t2'),env2,rEnv2) 
| IMult(t1,t2) -> let (t1',env1,rEnv1) = copyType t1 env rEnv bl in
                  let (t2',env2,rEnv2) = copyType t2 env1 rEnv1 bl in
                      (IMult(t1',t2'),env2,rEnv2) 
| IDiv(t1,t2) -> let (t1',env1,rEnv1) = copyType t1 env rEnv bl in
                 let (t2',env2,rEnv2) = copyType t2 env1 rEnv1 bl in
                     (IDiv(t1',t2'),env2,rEnv2) 
| IMax(t1,t2) -> let (t1',env1,rEnv1) = copyType t1 env rEnv bl in
                 let (t2',env2,rEnv2) = copyType t2 env1 rEnv1 bl in
                     (IMax(t1',t2'),env2,rEnv2) 
| Iota(t1,t2) -> let (t1',env1,rEnv1) = copyType t1 env rEnv bl in
                 let (t2',env2,rEnv2) = copyType t2 env1 rEnv1 bl in
                     (Iota(t1',t2'),env2,rEnv2) 
| SigPlus(t1,t2,t3,t4) -> let (t1',env1,rEnv1) = copyType t1 env rEnv bl in
                          let (t2',env2,rEnv2) = copyType t2 env1 rEnv1 bl in
                          let (t3',env3,rEnv3) = copyType t3 env2 rEnv2 bl in
                          let (t4',env4,rEnv4) = copyType t4 env3 rEnv3 bl in
                            (SigPlus(t1',t2',t3',t4'),env4,rEnv4) 
| SigMinus(t1,t2,t3,t4) -> let (t1',env1,rEnv1) = copyType t1 env rEnv bl in
                           let (t2',env2,rEnv2) = copyType t2 env1 rEnv1 bl in
                           let (t3',env3,rEnv3) = copyType t3 env2 rEnv2 bl in
                           let (t4',env4,rEnv4) = copyType t4 env3 rEnv3 bl in
                             (SigMinus(t1',t2',t3',t4'),env4,rEnv4) 
| SigMult(t1,t2) -> let (t1',env1,rEnv1) = copyType t1 env rEnv bl in
                    let (t2',env2,rEnv2) = copyType t2 env1 rEnv1 bl in
                      (SigMult(t1',t2'),env2,rEnv2) 
| SigmaPlus(t1,t2) -> let (t1',env1,rEnv1) = copyType t1 env rEnv bl in
                      let (t2',env2,rEnv2) = copyType t2 env1 rEnv1 bl in
                        (SigmaPlus(t1',t2'),env2,rEnv2) 
| SigmaMinus(t1,t2) -> let (t1',env1,rEnv1) = copyType t1 env rEnv bl in
                      let (t2',env2,rEnv2) = copyType t2 env1 rEnv1 bl in
                        (SigmaMinus(t1',t2'),env2,rEnv2) 
| TFloat(r0,r1,r2) -> let (t0,env0,rEnv0) = copyType !r0 env rEnv bl in
                      let (t1,env1,rEnv1) = copyType !r1 env0 rEnv0 bl in
                      let (t2,env2,rEnv2) = copyType !r2 env1 rEnv1 bl in
                        (TFloat(ref t0,ref t1,ref t2),env2,rEnv2) 
| TInt -> (t,env,rEnv)
| TBool -> (t,env,rEnv)
| TypeVar(r,s) -> if (List.mem s bl) then 
                    let _ = r := None 
					in (TypeVar(r,s),env,rEnv)
                  else
                  let (s',env',rEnv') = (try (getEnv s env,env,rEnv) 
                                         with _ -> let s'' = newVar () in (s'',setEnv s s'' env,setEnv s'' (ref None) rEnv)
								        )
                  in copyTypeVar r s' env' rEnv' bl
| TProduct(t1,t2) -> let (t1',env1,rEnv1) = copyType t1 env rEnv bl in
                     let (t2',env2,rEnv2) = copyType t2 env1 rEnv1 bl in
                       (TProduct(t1',t2'),env2,rEnv2) 
| TList(t1) -> let (t1',env1,rEnv1) = copyType t1 env rEnv bl
               in (TList(t1'),env1,rEnv1) 
| Let(r0,r1,r2) ->  let (t1,env1,rEnv1) = copyType r1 env rEnv bl in
                    let (t2,env2,rEnv2) = copyType r2 env1 rEnv1 bl in
                      (Let(r0,t1,t2),env2,rEnv2)  
| Cond(r0,r1,r2) ->  let (t0,env0,rEnv0) = copyType r0 env rEnv bl in
                     let (t1,env1,rEnv1) = copyType r1 env0 rEnv0 bl in
                     let (t2,env2,rEnv2) = copyType r2 env1 rEnv1 bl in
                       (Cond(t0,t1,t2),env2,rEnv2)  
| _ -> raise (Error (":-// "^(printExpr t)))

(*
 | _ -> (t,env,rEnv)
 *)


and copyTypeVar r s env rEnv bl = match !r with
  None -> let r' = getEnv s rEnv in 
          let tv = TypeVar(r',s) 
          in (tv,env,rEnv)
| Some(e) -> let (e',env',rEnv') = copyType e env rEnv (s::bl) in
             let r' = getEnv s rEnv' in
             let _ = r' := Some(e') in
             let tv = TypeVar(r',s) 
             in (tv,env',rEnv')
			   
;;
(*
			   (match !r' with
			      Some(TypeVar(r'',_)) -> if (r=r'') then
				                            let _ =  r := None in
								            let tv = TypeVar(r,s)
								            in (tv,env',rEnv')
				                          else
								            let _ = r' := Some(e') in
                                            let tv = TypeVar(r',s) 
                                            in (tv,env',rEnv')

   *************
 ******

0 53
2 10

*)
(*
((u2' >= u1') && (p2' >= u2'-u1'+1))
  ************
 *****



 ((u2' >= u1') && (p2' >= u1'-u2'+p1')) 
  ************
 **************
*)


let rec ltType t1 t2 = match (t1,t2) with
  (TFloat(s1,u1,p1),TFloat(s2,u2,p2)) -> 
     (match (!u1,!p1) with
        (Int(u1'),Int(p1')) -> (match (!u2,!p2) with
                                  (Int(u2'),Int(p2')) ->
			             if ((u1' <= u2') && (p1' >= p2' + u1' - u2')) then () 
(*	if ((u2' >= u1') && (p2' >= u1'-u2'+p1')) then ()		    				            17 sept*)
                                     else 
                                       raise (Error ("This expression has type "^(printExpr (TFloat(s1,u1,p1)))^" but an expression was expected of type "^(printExpr (TFloat(s2,u2,p2))) ) )
                                | _ -> ()
			       )
     | _ -> ()
    )
| (TypeVar(r1,_),_) -> (match !r1 with
                          None -> ()
                        | Some(t1') -> ltType t1' t2
                       )
| (_,TypeVar(r2,_)) -> (match !r2 with
                          None -> ()
                        | Some(t2') -> ltType t1 t2'
                       )
| _ -> ()
;;


let rec doesNotOccurs v t = match t with
  Float(_) -> true
| Int(_) -> true
| Bool(_) -> true
| Id(x) -> true
| Lambda(x,e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| Pi(x,e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| App(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| Cond(e1,e2,e3) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2) && (doesNotOccurs v e3))
| Let(x,e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| Pair(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| First(e) -> doesNotOccurs v e
| Second(e) -> doesNotOccurs v e
| Cons(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| Car(e) -> doesNotOccurs v e
| Cdr(e) -> doesNotOccurs v e
| Nil -> true
| Plus(e1,e2,_,_,_) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| Minus(e1,e2,_,_,_) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| Mult(e1,e2,_,_,_) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| Div(e1,e2,_,_,_) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| IPlus(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| IMinus(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| IMult(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| IDiv(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| IMax(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| Iota(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| SigPlus(e1,e2,e3,e4) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2) && (doesNotOccurs v e3) && (doesNotOccurs v e4))
| SigMinus(e1,e2,e3,e4) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2) && (doesNotOccurs v e3) && (doesNotOccurs v e4))
| SigMult(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| SigmaPlus(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| SigmaMinus(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| BEq(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| BNeq(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2)) 
| BLt(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| BGt(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2)) 
| BLte(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2))
| BGte(e1,e2) -> ((doesNotOccurs v e1) && (doesNotOccurs v e2)) 

| TInt -> true
| TBool -> true
| TFloat(er0,er1,er2) -> (doesNotOccurs v !er0) && (doesNotOccurs v !er1) && (doesNotOccurs v !er2)
| TypeVar(r,s) -> if ((String.compare v s)=0) then false else true
| TProduct(e1,e2) -> (doesNotOccurs v e1) && (doesNotOccurs v e2)
| TList(e) -> doesNotOccurs v e
| _ -> raise (Error "unexpected case in doesNotOccurs")
;;


let rec eqExpr e1 e2 = true;;


let rec unifyFloat s1 u1 p1 s2 u2 p2 = match (!s1,!u1,!p1) with
  (Int(s1'),Int(u1'),Int(p1')) ->  
    (match (!s2,!u2,!p2) with
       (Int(s2'),Int(u2'),Int(p2')) -> 
         let s = if (s1'=s2') then s1' else 2 in
         let u = max u1' u2' in
         let p = if (u1'>=u2') then
                   min p1' (u1' - u2' + p2') 
                 else
                   min p2' (u2' - u1' + p1') 
         in
            if (p>0) then 
              (s1 := Int(s) ; s2 := Int(s) ; u1 := Int(u) ; u2 := Int(u) ; p1 := Int(p) ; p2 := Int(p) )
            else raise (Error ("Type "^(printExpr (TFloat(s1,u1,p1)))^" is not compatible with type "^(printExpr (TFloat(s2,u2,p2))) ) )

(*
          if ((u1' >= u2') && (p1' >= p2')) then () 
          else raise (Error ("This expression has type "^(printExpr (TFloat(u1,p1)))^" but an expression was expected of type "^(printExpr (TFloat(u2,p2))) ) )
*)
     | (TypeVar(rs,ss),TypeVar(ru,su),TypeVar(rp,sp)) -> rs := Some(!s1) ; ru := Some(!u1) ; rp := Some(!p1) 
     | _ -> solveLT !s1 !s2 !u1 !u2 !p1 !p2 

 (*match (x,y) with
			      (TypeVar(r1,s1),TypeVar(r2,s2)) -> r1 := Int(v); r2 := Int(w)
				| _ -> raise (Error "should not happen in unifyFloat")
	           *) 
(*			raise (Error ("Cannot unify Real types: "^(printExpr (TFloat(u1,p1)))^" and "^(printExpr (TFloat(u2,p2)))))
  *)      
    )
| (TypeVar(rs,ss),TypeVar(ru,su),TypeVar(rp,sp)) -> 
    ((match !rs with
        None -> rs := Some(!s2) (*Some(fst3 (copyType !s2 [] [] []))*) (*??? 30/01/18*)
      | Some(s1) -> unify s1 !s2
     ) ;
     (match !ru with
        None -> ru := Some(!u2) (*Some(fst3 (copyType !u2 [] [] []))*)
      | Some(u1) -> unify u1 !u2
     ) ; 
     (match !rp with
        None -> rp := Some(!p2) (*Some(fst3 (copyType !p2 [] [] []))*)
      | Some(p1) -> unify p1 !p2
     ) 
) 
| _ -> (match (!s2,!u2,!p2) with
     (TypeVar(rs,ss),TypeVar(ru,su),TypeVar(rp,sp)) ->   
      ((match !rs with
	  None -> rs := Some(!s1)
	| Some(s2) -> unify !s1 s2
       ) ;
       (match !ru with
	  None -> ru := Some(!u1)
	| Some(u2) -> unify !u1 u2
       ) ; 
       (match !rp with
	  None -> rp := Some(!p1) 
	| Some(p2) -> unify !p1 p2
      )) 
 | _ -> if ((s1=s2)&&(u1=u2)&&(p1=p2)) then () 
		else (*
		      u1 := (IMax(!u1,!u2)) ;
		      p1 := (IMax(IPlus(IMinus(!u1,!p2),!p1),
				  IPlus(IMinus(!u2,!p1),!p2)
				    )	 );
		      u2 := (IMax(!u1,!u2)) ;
		      p2 := (IMax(IPlus(IMinus(!u1,!p2),!p1),
				  IPlus(IMinus(!u2,!p1),!p2)
				    )	 )			*)   
	    solve !s1 !s2 !u1 !u2 !p1 !p2
    (*		    raise (Error ("Cannot unify Real types: "^(printExpr (TFloat(u1,p1)))^" and "^(printExpr (TFloat(u2,p2)))))
  *)          
		       )


and unify tt1 tt2 = 

let t1 = evalPart tt1 in
let t2 = evalPart tt2 in

match t1 with
TypeVar(r,name) ->  unifyVar t2 r name
| TInt -> (match t2 with
	 TInt -> ()
       | TypeVar(r,name) -> unifyVar t1 r name
       | _ -> raise (Error ("This expression has type "^(printExpr t2)^" but an expression was expected of type "^(printExpr TInt)))
      )
| TBool -> (match t2 with
	  TBool -> ()
	| TypeVar(r,name) -> unifyVar t1 r name
	| _ -> raise (Error ("This expression has type "^(printExpr t2)^" but an expression was expected of type "^(printExpr TBool)))
      )
| TFloat(s,u,p) -> (match t2 with
		  TFloat(s',u',p') -> s := evalPart !s ; u := evalPart !u ; p := evalPart !p ; s' := evalPart !s' ; u' := evalPart !u' ; p' := evalPart !p' ;
				   unifyFloat s u p s' u' p'; 
		| TypeVar(r,name) -> unifyVar t1 r name
		| _ -> raise (Error ("This expression has type "^(printExpr t2)^" but an expression was expected of type "^(printExpr t1)))
	       )
| Pi(x,t3,t4) -> (match t2 with
		  Pi(y,t3',t4') -> unify t3 t3'; unify t4 t4'
		| TypeVar(r,name) -> unifyVar t1 r name
		| _ -> raise (Error ("This expression has type "^(printExpr t2)^" but an expression was expected of type 'a -> 'b"))
	       )
| TProduct(t3,t4) -> (match t2 with
		    TProduct(t3',t4') -> unify t3 t3'; unify t4 t4'
		  | TypeVar(r,name) -> unifyVar t1 r name
		  | _ -> raise (Error ("This expression has type "^(printExpr t2)^" but an expression was expected of type 'a * 'b"))
	       ) 
| TList(t1') -> (match t2 with
		    TList(t2') -> unify t1' t2'
		  | TypeVar(r,s) -> if (doesNotOccurs s t1') then unifyVar t1 r s
				    else
				      raise (Error ("This expression has type "^(printExpr t2)^" but an expression was expected of type 'a list"))
		  | _ -> raise (Error ("This expression has type "^(printExpr t2)^" but an expression was expected of type 'a list"))
	       ) 
| _ -> if (t1=t2) then () else raise (Error("hhhhhhhhh "^(printExpr t1)^" "^(printExpr t2)))


and unifyVar t r name = match !r with
  None -> (match t with
             TypeVar(r',name') -> r := Some(t) 
           | _ -> if (doesNotOccurs name t) then r := Some(t)
                  else raise (Error ("This expression has type 'a but an expression was expected of type "^(printExpr t)))
          )

| Some(t') -> if (doesNotOccurs name t) then unify t t' 
              else 
			    if (eqExpr t t') then () else raise (Error ("This expression "^(printExpr t')^"has type 'a but an expression was expected of type "^(printExpr t)))
;;


let rec typeApp e1 e2 env valEnv =
  let t = newTypeVar () in
  let t' = newTypeVar () in 
  let t1 = typeCheck e1 env valEnv in
  let t2 = typeCheck e2 env valEnv in
  let t1' = evalPart t1 in
  let t2' = evalPart t2 in
  let argType = getArgType t1' env in
    (try (ltType t2' argType ;
          unify t2' t ;
          unify t1' (Pi(newVar (),t,t')) ;
(*          if (isPrimitive e1) then updateValEnv (getPrimitiveName e1) valEnv t' else () ;*)
          t') 
     with Error(s) -> let tf = Pi(newVar (),t2',t') 
                      in raise (RecTypeError(tf,s))
    )


and typeCheck e env valEnv = match e with
  Float(f,s,u,p) -> (*let sf = if ((F.to_float f)>0.0) then 1 else
                            if ((F.to_float f)<0.0) then -1 else
                            if ((F.to_float f)=0.0) then 0 else 2 
                    in*)
                    let uf = ufp (F.to_float f) in
                    let _ = if (uf>u) then raise (Error ("The ufp of this real number is "^(string_of_int uf)^" but the maximal ufp allowed is "^(string_of_int u)))
                  in TFloat(ref (Int(s)),ref (Int(u)),ref (Int(p))) 
| Int(_) -> TInt
| Bool(_) -> TBool
| Id(x) -> getEnv x env 
| Lambda(x,t,e1) -> let s = newVar () 
                    in Pi(s,t,typeCheck e1 (setEnv x t env) valEnv)
(*| Pi(x,t,e1) -> Pi(x,t,typeCheck e1 (setEnv x t env) valEnv) *)
| App(e1, e2) -> typeApp e1 e2 env valEnv
(*    (match e1 with
       Id(s) -> if ((String.length s)<3) then typeApp e1 e2 env valEnv 
                else
                if (((String.compare (String.sub s 0 3) "<F_")=0) ||
                    ((String.compare (String.sub s 0 3) ">F_")=0) ||
                    ((String.compare (String.sub s 0 3) "=F_")=0) ||
                    ((String.compare (String.sub s 0 3) ">=F")=0) ||
                    ((String.compare (String.sub s 0 3) "<=F")=0) ||
                    ((String.compare (String.sub s 0 3) "!=F")=0)) 
                then let _ = err "1111" in
                  let t = newTypeVar () in
                  let t' = newTypeVar () in 
                  let t1 = getEnv s env in
                  let t2 = typeCheck e2 env valEnv in
                  let t2' = evalPart t2 in
                  let argType = getArgType t1 env in
                    (try (ltType t2' argType ;
                          unify t1 (Pi(newVar (),t,t')) ; 
                          t') 
                     with Error(s) -> let tf = Pi(newVar (),t2',t') 
                       in raise (RecTypeError(tf,s))
                    )
                else typeApp e1 e2 env valEnv
       | App(Id(s),e') -> 
              if ((String.length s)<3) then typeApp e1 e2 env valEnv 
              else
                if (((String.compare (String.sub s 0 3) "<F_")=0) ||
                    ((String.compare (String.sub s 0 3) ">F_")=0) ||
                    ((String.compare (String.sub s 0 3) "=F_")=0) ||
                    ((String.compare (String.sub s 0 3) ">=F")=0) ||
                    ((String.compare (String.sub s 0 3) "<=F")=0) ||
                    ((String.compare (String.sub s 0 3) "!=F")=0)) 
                then let _ = err "2222" in
                  let t = newTypeVar () in
                  let t' = newTypeVar () in 
                  let t1 = getEnv s env in
                  let t2 = typeCheck e2 env valEnv in
                  let t2' = evalPart t2 in
                  let argType = getArgType t1 env in
                    (try (ltType t2' argType ;
                          unify t1 (Pi(newVar (),t,t')) ; 
                          t') 
                     with Error(s) -> let tf = Pi(newVar (),t2',t') 
                       in raise (RecTypeError(tf,s))
                    )
                else typeApp e1 e2 env valEnv
     | _ -> typeApp e1 e2 env valEnv
    ) *)
| Cond(be, e1, e2) -> 
    let t1 = typeCheck e1 env valEnv in
    let t2 = typeCheck e2 env valEnv in
    let t3 = typeCheck be env valEnv 
    in unify t3 TBool ;
       unify t1 t2 ; t2
| Let(x,e1,e2) ->
    let t = typeCheck e1 env valEnv
    in typeCheck e2 (setEnv x t env) valEnv 
| Pair(e1,e2) -> TProduct(typeCheck e1 env valEnv,typeCheck e2 env valEnv)
| First(e) -> let t1 = newTypeVar () in
              let t2 = newTypeVar () in
                unify (TProduct(t1,t2)) (typeCheck e env valEnv); t1
| Second(e) -> let t1 = newTypeVar () in
               let t2 = newTypeVar () in
                 unify (TProduct(t1,t2)) (typeCheck e env valEnv); t2
| Cons(e1,e2) ->
    let t = newTypeVar () in
    let t1 = typeCheck e1 env valEnv in
    let t2 = typeCheck e2 env valEnv in
      unify t1 t; unify t2 (TList(t)); TList(t)
| Car(e1) -> let t = newTypeVar () in
               unify (TList(t)) (typeCheck e1 env valEnv); t
| Cdr(e1) -> let t = newTypeVar () in
               unify (TList(t)) (typeCheck e1 env valEnv); (TList(t))
| Nil -> let t = newTypeVar () in TList(t) 
(*| TBool -> TBool
| TInt -> TInt
| TFloat(_) -> e
| TypeVar(_) -> e
*)
| _ -> raise (Error ("unexpected case in typeCheck: "^printExpr e))
;;


  
