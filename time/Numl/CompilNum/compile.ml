open Gmp;;
open Types;;
open Prelude;;
open Print;;

let rec genCaml e = match e with 

|(App(App(Id(f),e1),e2)) -> let v1 =(genCaml e1) in
                            let v2 =(genCaml e2)in                                                                     
                            if ((String.compare (String.sub f 0 3) "+__")=0) then
                                                                "("^v1^" +. "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) "-__")=0) then
                                                                "("^v1^" -. "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) "*__")=0) then
                                                                "("^v1^" *. "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) "/__")=0) then
                                                                "("^v1^" /. "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) "+I_")=0) then
                                                                "("^v1^" + "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) "-I_")=0) then
                                                                "("^v1^" - "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) "*I_")=0) then
                                                                "("^v1^" * "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) "/I_")=0) then
                                                                "("^v1^" / "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) ">B_")=0) then
                                                                "("^v1^" > "^v2^")"
                                                       
                                                        else
                                                        if ((String.compare (String.sub f 0 3) "<B_")=0) then
                                                                "("^v1^" < "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) ">=_")=0) then
                                                                "("^v1^" >= "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) "<=_")=0) then
                                                                "("^v1^" <= "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) "=B_")=0) then
                                                                "("^v1^" = "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) "!=_")=0) then

                                                                "("^v1^" != "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) ">F_")=0) then
                                                                "("^v1^" > "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) "<F_")=0) then
                                                                "("^v1^" < "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) ">=F")=0) then
                                                                "("^v1^" >= "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) "<=F")=0) then
                                                                "("^v1^" <= "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) "=F_")=0) then
                                                                "("^v1^" = "^v2^")"
                                                        else
                                                        if ((String.compare (String.sub f 0 3) "!=F")=0) then
                                                                "("^v1^" != "^v2^")"
                                                        else
                                                                "("^f^" "^v1^" "^v2^")"
(* "let v1 ="^v1^ "in let v2="^v2^ "in ("^f^" v1  v2 )" *)
                                             

| Float(f,s,u,p) -> let ff = (F.to_float f) in 
                    let uf = ufp ff 
                    in if ((u-p+1)>uf) then 
                         raise (Error ("The computed value has no significant digit. Its ufp is "^(string_of_int uf)^" but the ulp of the certified value is "^(string_of_int (u-p+1))))  
                         else 
                         if (uf>u) then 
                           raise (Error("!! Should not occur: The computed value is out of the range of the certified values. Its ufp is "^(string_of_int uf)^" which is greater than the ufp "^(string_of_int u)^" in the type of the result")) 
                         else 
                          if ff=0.0 then 
                             let err = pow 2.0 (u-p) in
                             let s''' = "0.0 +/- "^(string_of_float err) 
                             in (s''')
                           else
                             let ff' = if (ff>=0.0) then ff else (ff *. (-1.0)) in
                             let a = int_of_float (ceil (log10 ff')) in
                             let b = int_of_float (ceil (log10 (pow 2.0 (u-p)))) in 
                             let (s0,i) = F.to_string_exp_base_digits 10 (a-b+1) f in 
                             let signe = String.get s0 0 in
                             let s = if (signe='-') then String.sub s0 1 ((String.length s0)-1) 
                                     else s0  
                             in  
                             let s' = s^(String.make p '0') in
                             let se = if (i>0) then (String.sub s' 0 i) else "0" in
                             let sd = if (i>0) then (String.sub s' i (a-b-i+1))
                                               else (String.sub ((String.make (abs i) '0')^s') 0 (a-b+1))   
                             in
                               let s'' = se^"."^(String.sub sd 0 ((String.length sd)-1)) in
                               let err = pow 2.0 (u-p) in
                               let s''' = (if (signe='-') then "-" else "")^s''(*^" +/- "^(string_of_float err)*) 
                               in (s''')

| Int(e) -> string_of_int(e)
| Bool(e) -> (if e then "true" else "false\n")
| Id(x) ->  x
| Lambda(x,t,e) -> "(fun "^ x ^" -> "^(genCaml e)^")\n"

| Cond(be,e1,e2) -> "let v1 =" ^(genCaml be)^ " in\nlet v2 = "^(genCaml e1)^" in\nlet v3 = "^(genCaml e2)^ " in\n(if v1 then\n v2\n else\n v3)\n" 

| Let(x,e1,e2) -> let v1 = (genCaml e1) in
                  let v2 = (genCaml e2) in
                        ("let "^x^" = "^v1^" in\n "^v2^"\n")
| Pair(e1,e2) -> let v1= (genCaml e1) in 
                 let v2= (genCaml e2) in
                     "("^v1^","^v2^ ")"
                                    
| First(e) -> "let v1 = " ^ (genCaml e)^ " in (fst v1)\n"
| Second(e) -> "let v1 = "^(genCaml e)^ " in (snd v1)\n"

| App(Id(f),e) -> "let v1 = "^(genCaml e)^ " in ( sqrt v1)\n"
(*| Cons(e1,e2) -> let v1 = (genCaml e1) in
                   let v2 = (genCaml e2) in
                    "["^v1^";"^v2^ "]"*) 

| Cons(e1,e2) -> "let v1 =" ^(genCaml e1)^ " in\nlet v2 = "^(genCaml e2)^" in\n (v1 :: v2)"                 
| Car(e) -> "let v1 = " ^ (genCaml e)^ " in (List.hd v1)\n"
| Cdr(e) -> "let v1 = " ^ (genCaml e)^ " in (List.tl v1)\n"

| Nil -> "[]"
| _ -> "error : "^(fst (printExpression e []))




;;














































(*|Lambda(_,TFloat(_,_,_),
           Lambda(_,TFloat(_,_,_),
   
 App(App(App(App(App(App(Pi(_,_,
                                          Pi(_,_,
                                          Pi(_,_,
                                          Pi(_,_,
                                          Pi(_,_,
                                          Pi(_,_,Plus(_,_,_,_,_)))))))
                    ,_),_),_),_),_),_)  )) -> "erreur H app y"*)



(*|(App(App(Pi(_,_,
                                          Pi(_,_,
                                          Pi(_,_,
                                          Pi(_,_,
                                          Pi(_,_,
                                          Pi(_,_,Plus(_,_,_,_,_)))))))
                    ,_),_)) -> "H app Y"*)

(* App(App(App(App(App(App(Pi("ss1",TInt,
                                          Pi("uu1",TInt,
                                          Pi("pp1",TInt,
                                          Pi("ss2",TInt,
                                          Pi("uu2",TInt,
                                          Pi("pp2",TInt,Plus(Id("x"),Id("y"),s,u,p)))))))
                    ,s1),u1),p1),s2),u2),p2)  )) ;;
					
( App(App(App(Pi(_,_,
                                          Pi(_,_,
                                          Pi(_,_,
                                          Pi(_,_,
                                          Pi(_,_,
                                          Pi(_,_,Plus(_,_,s,u,p)))))))
                    ,_),_),_)  ) ;;*)



                    
(*"let v1 ="^(genCaml e1)^ "in\n let v2="^(genCaml e2)^ "in ("^f^" v1  v2 )"in*)
                             (*"let v1 ="^(genCaml e1)^ "in\n let v2="^(genCaml e2)^ "in (("^f^ "v1) v2^)\n" "in"*)




