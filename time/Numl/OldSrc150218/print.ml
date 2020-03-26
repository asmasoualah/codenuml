
open Types ;;
open Prelude ;;
open Gmp ;;


let rec occurs x e = match e with
  Id(y) -> if ((String.compare x y)=0) then true else false 
| Lambda(y,e1,e2) -> if ((String.compare x y)=0) then false else (occurs x e1)||(occurs x e2)
| Pi(y,e1,e2) -> if ((String.compare x y)=0) then false else (occurs x e1)||(occurs x e2)
| App(e1,e2) -> (occurs x e1)||(occurs x e2)
| Cond(e1,e2,e3) -> (occurs x e1)||(occurs x e2)||(occurs x e3)
| Let(y,e1,e2) -> if ((String.compare x y)=0) then (occurs x e1) else (occurs x e1)||(occurs x e2)
| Pair(e1,e2) -> (occurs x e1)||(occurs x e2)
| First(e1) -> (occurs x e1)
| Second(e1) -> (occurs x e1)
| Cons(e1,e2) -> (occurs x e1)||(occurs x e2)
| Car(e1) -> (occurs x e1)
| Cdr(e1) -> (occurs x e1)
| Nil -> false
| Plus(e1,e2,_,_,_) -> (occurs x e1)||(occurs x e2)
| Minus(e1,e2,_,_,_) -> (occurs x e1)||(occurs x e2)
| Mult(e1,e2,_,_,_) -> (occurs x e1)||(occurs x e2)
| Div(e1,e2,_,_,_) -> (occurs x e1)||(occurs x e2)
| IPlus(e1,e2) -> (occurs x e1)||(occurs x e2)
| IMinus(e1,e2) -> (occurs x e1)||(occurs x e2)
| IMult(e1,e2) -> (occurs x e1)||(occurs x e2)
| IDiv(e1,e2) -> (occurs x e1)||(occurs x e2)
| IMax(e1,e2) -> (occurs x e1)||(occurs x e2)
| Iota(e1,e2) -> (occurs x e1)||(occurs x e2)
| SigPlus(e1,e2,e3,e4) -> (occurs x e1)||(occurs x e2)||(occurs x e3)||(occurs x e4)
| SigMinus(e1,e2,e3,e4) -> (occurs x e1)||(occurs x e2)||(occurs x e3)||(occurs x e4)
| SigMult(e1,e2) -> (occurs x e1)||(occurs x e2)
| SigmaPlus(e1,e2) -> (occurs x e1)||(occurs x e2)
| SigmaMinus(e1,e2) -> (occurs x e1)||(occurs x e2)
| TypeVar(r,_) -> (match !r with
                     None -> false
                   | Some(e1) -> occurs x e1
                  )
| _ -> false ;;



let rec printExpression e env = match e with
  Float(f,s,u,p) -> let ff = (F.to_float f) in 
                    let uf = ufp ff 
                    in if ((u-p+1)>uf) then 
                         raise (Error ("The computed value has no significant digit. Its ufp is "^(string_of_int uf)^" but the ulp of the certified value is "^(string_of_int (u-p+1))))  
                       else 
                         if (uf>u) then 
                           raise (Error("The computed value is out of the range of the certified values. Its ufp is "^(string_of_int uf)^" which is greater than the ufp "^(string_of_int u)^" in the type of the result")) 
                         else 
                           if ff=0.0 then 
                             let err = pow 2.0 (u-p) in
                             let s''' = "0.0 +/- "^(string_of_float err) 
                             in (s''',env)
                           else
                             let ff' = if (ff>=0.0) then ff else (ff *. (-1.0)) in
                             let a = int_of_float (ceil (log10 ff')) in
                             let b = int_of_float (ceil (log10 (pow 2.0 (u-p)))) in
                             let (s0,i) = F.to_string_exp_base_digits 10 (a-b+1) f in 
                             let signe = String.get s0 0 in
                             let s = if (signe='-') then String.sub s0 1 ((String.length s0)-1) else s0 in  
                             let s' = s^(String.make p '0') in
                             let se = if (i>0) then (String.sub s' 0 i) else "0" in
                             let sd = if (i>0) then (String.sub s' i (a-b-i+1))
                                               else (String.sub ((String.make (abs i) '0')^s') 0 (a-b+1))   
                             in
                               let s'' = se^"."^sd in
                               let err = pow 2.0 (u-p) in
                               let s''' = (if (signe='-') then "-" else "")^s''^" +/- "^(string_of_float err) 
                               in (s''',env)
(*                    let (s',i') = F.to_string_exp_base_digits 2 p f in
                    let s'' = s'^(String.make p '0') in
                  let t = (String.length (string_of_int u))+(String.length (string_of_int p)) + 13 in
                  let tt = (String.make t ' ')^"= " 
                  in if !binary then
                       ((F.to_string f)
                        ^"\n"^tt
                        ^((String.sub s'' 0 i')^"."^(String.sub s'' i' (p-i'))) ,env)
                     else (F.to_string f,env)*)
| Int(i) -> (string_of_int i,env)
| Bool(b) -> ((if b then "true" else "false"),env) 
| Id(x) -> (x,env)
| Lambda(x,t,e1) -> ("<fun>",env)
| Pi(x,t,e1) -> let (s1,env1) = printExpression t env in
                let (s2,env2) = printExpression e1 env1 in
                       if (occurs x e1) then
                         ("(forall "^x^" : "^s1^", "^s2^")",env2)
                       else
                         (match t with
                            Pi(_) -> ("("^s1^") -> "^s2,env2)        
                          | _ -> (s1^" -> "^s2,env2)
                         )
| App(e1,e2) -> let (s1,env1) = printExpression e1 env in
                let (s2,env2) = printExpression e2 env1 in
                  ("("^s1^" "^s2^")",env2)
| Cond(be,e1,e2) -> let (s1,env1) = printExpression be env in
                    let (s2,env2) = printExpression e1 env1 in
                    let (s3,env3) = printExpression e2 env2 in
                      ("if "^s1^" then "^s2^" else "^s3,env3)
| Let(x,e1,e2) -> let (s1,env1) = printExpression e1 env in
                       let (s2,env2) = printExpression e2 env1 in
                         ("let "^x^" = "^s1^" in "^s2,env2)
| Pair(e1,e2) -> let (s1,env1) = printExpression e1 env in
                 let (s2,env2) = printExpression e2 env1 in
                   ("("^s1^" , "^s2^")",env2)
| First(e1) -> let (s1,env1) = printExpression e1 env in ("fst "^s1,env1)
| Second(e1) -> let (s1,env1) = printExpression e1 env in ("snd "^s1,env1)
| Cons(e1,e2) -> let (s1,env1) = printList e1 e2 env in ("["^s1^"]",env1)
| Car(e1) -> let (s1,env1) = printExpression e1 env in ("hd "^s1,env1)
| Cdr(e1) -> let (s1,env1) = printExpression e1 env in ("tl "^s1,env1)
| Nil -> ("[]",env)
| Plus(e1,e2,_,_,_) -> let (s1,env1) = printExpression e1 env in
                       let (s2,env2) = printExpression e2 env1 in
                         ("("^s1^" + "^s2^")",env2)
| Minus(e1,e2,_,_,_) -> let (s1,env1) = printExpression e1 env in
                        let (s2,env2) = printExpression e2 env1 in
                          ("("^s1^" - "^s2^")",env2)
| Mult(e1,e2,_,_,_) -> let (s1,env1) = printExpression e1 env in
                       let (s2,env2) = printExpression e2 env1 in
                         ("("^s1^" * "^s2^")",env2)
| Div(e1,e2,_,_,_) -> let (s1,env1) = printExpression e1 env in
                      let (s2,env2) = printExpression e2 env1 in
                        ("("^s1^" / "^s2^")",env2)
| IPlus(e1,e2) -> let (s1,env1) = printExpression e1 env in
                  let (s2,env2) = printExpression e2 env1 in
                    ("("^s1^" +_ "^s2^")",env2)
| IMinus(e1,e2) -> let (s1,env1) = printExpression e1 env in
                  let (s2,env2) = printExpression e2 env1 in
                    ("("^s1^" -_ "^s2^")",env2)
| IMult(e1,e2) -> let (s1,env1) = printExpression e1 env in
                  let (s2,env2) = printExpression e2 env1 in
                    ("("^s1^" *_ "^s2^")",env2)
| IDiv(e1,e2) -> let (s1,env1) = printExpression e1 env in
                  let (s2,env2) = printExpression e2 env1 in
                    ("("^s1^" /_ "^s2^")",env2)
| IMax(e1,e2) -> let (s1,env1) = printExpression e1 env in
                  let (s2,env2) = printExpression e2 env1 in
                    ("(max "^s1^" "^s2^")",env2)
| Iota(e1,e2) -> let (s1,env1) = printExpression e1 env in
                  let (s2,env2) = printExpression e2 env1 in
                    ("(iota "^s1^" "^s2^")",env2)
| SigPlus(e1,e2,e3,e4) -> let (s1,env1) = printExpression e1 env in
                          let (s2,env2) = printExpression e2 env1 in
                          let (s3,env3) = printExpression e3 env2 in
                          let (s4,env4) = printExpression e4 env3 in
                            ("(sigPlus "^s1^" "^s2^" "^s3^" "^s4^")",env4)
| SigMinus(e1,e2,e3,e4) -> let (s1,env1) = printExpression e1 env in
                           let (s2,env2) = printExpression e2 env1 in
                           let (s3,env3) = printExpression e3 env2 in
                           let (s4,env4) = printExpression e4 env3 in
                             ("(sigMinus "^s1^" "^s2^" "^s3^" "^s4^")",env4)
| SigMult(e1,e2) -> let (s1,env1) = printExpression e1 env in
                    let (s2,env2) = printExpression e2 env1 in
                      ("(sigMult "^s1^" "^s2^")",env2)
| SigmaPlus(e1,e2) -> let (s1,env1) = printExpression e1 env in
                      let (s2,env2) = printExpression e2 env1 in
                        ("(sigma+ "^s1^" "^s2^")",env2)
| SigmaMinus(e1,e2) -> let (s1,env1) = printExpression e1 env in
                       let (s2,env2) = printExpression e2 env1 in
                         ("(sigma- "^s1^" "^s2^")",env2)

| TInt -> ("int",env) 
| TBool -> ("bool",env)
| TFloat(s,u,p) -> let (s0,env0) = (match !s with
                                      TypeVar(_) -> printExpression !s env
                                    | Int(i) -> if (i=(-1)) then ("-",env) else
                                                if (i=1) then ("+",env) else
                                                if (i=0) then ("0",env) else ("*",env) 
                                    | _ -> if !verbose then printExpression !u env else ("<expr>",env)
                                   ) 
                   in
                   let (s1,env1) = (match !u with
                                      TypeVar(_) -> printExpression !u env0
                                    | Int(_) -> printExpression !u env0
                                    | _ -> if !verbose then printExpression !u env0 else ("<expr>",env0)
                                   ) 
                   in
                   let (s2,env2) = (match !p with
                                      TypeVar(_) -> printExpression !p env1 
                                    | Int(_) -> printExpression !p env1
                                    | _ -> if !verbose then printExpression !p env1 else ("<expr>",env1)
                                   )     
                   in ("real{"^s0^","^s1^","^s2^"}",env2)
| TypeVar(r,n) -> printTypeVar !r n env
| TProduct(e1,e2) -> let (s1,env1) = printExpression e1 env in
                     let (s2,env2) = printExpression e2 env1 in
                       (s1^" * "^s2,env2)
| TList(e1) -> let (s1,env1) = printExpression e1 env in (s1^" list",env1)
| _ -> ("?",env) (*raise (Error "unexpected case in printExpression") *)


and printTypeVar r n env = match r with
  None -> (try 
             (getEnv n env,env)
           with _ -> let _ = prettySymb := !prettySymb + 1 in
                     let n' = ("'"^(String.make 1 (char_of_int (96 + !prettySymb))))
                     in (n',setEnv n n' env) 
(*				    in  (n,setEnv n n env)  *)
          )
| Some(t) -> printExpression t env



and printList e1 e2 env = 
  let (s1,env1) = printExpression e1 env in
    (match e2 with
       Nil -> (s1,env1)
     | Cons(e1',e2') -> let (s2,env2) = printList e1' e2' env1 in (s1^"; "^s2,env2)
     | _ -> raise (Error "this is not a valid list")
    )
;;


let printExpr t = let _ = prettySymb := 0 in fst (printExpression t []) 
;;



let printExpr2 t typ = 
  match (t,typ) with
    (Float(f,ss,_,_),TFloat(s,u,p)) -> (match (!u,!p) with
                                         (Int(u'),Int(p')) -> let _ = prettySymb := 0 in fst (printExpression (Float(f,ss,u',p')) [])
                                         | _ -> "?" (* raise (Error "unexpected expression in printExpr for float type") *)
                                    )
  | (Float(_),t) -> raise (Error ("impossible in printExpr (float):"^(printExpr t)))
  | _ ->  let _ = prettySymb := 0 in fst (printExpression t []) 
;;

