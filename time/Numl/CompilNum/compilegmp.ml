open Gmp;;
open Prelude;;
open Print;;
open Types;;
open Primitives;;
open Checker;;

                            

let rec genGmp e = match e with 

|(App(App(Id(f),e1),e2)) -> let v1 = (genGmp e1) in
                            let v2 = (genGmp e2) in
			    (* let typOp = getEnvComplete f !cmpTypEnv "pas trouve" in
			       if ((String.compare typOp "FLOAT")=0) then      *)                               
                               if ((String.compare (String.sub f 0 3) ">F_")=0) then
                                   " (F.compare("^(v1)^") ("^( v2)^")>0)"
                               else if ((String.compare (String.sub f 0 3) "<F_")=0) then
                                      " (F.compare("^(v1)^") ("^( v2)^")<0)"
                                    else if ((String.compare (String.sub f 0 3) ">=F")=0) then
                                           " (F.compare("^(v1)^") ("^( v2)^")>=0)"
                                         else if ((String.compare (String.sub f 0 3) "<=F")=0) then
                                                " (F.compare("^(v1)^") ("^( v2)^")<=0)"
                                              else if ((String.compare (String.sub f 0 3) "=F_")=0) then
                                                     " (F.compare("^(v1)^") ("^( v2)^")=0)"
                                                   else if ((String.compare (String.sub f 0 3) "!=F")=0) then
                                                          " (F.compare("^(v1)^") ("^( v2)^" )!=0)"
                              (*                          else ("????? unkwnow op :"^f) *)
                            else
                              let p = getEnvComplete f !opEnvUP 0 in 
                                if ((String.compare (String.sub f 0 3) "+__")=0) then
                                  "F.add_prec " ^ (string_of_int(p))^  "(" ^(v1)^") (" ^(v2)^")"        
                                else if ((String.compare (String.sub f 0 3) "-__")=0) then
                                       "F.sub_prec " ^ (string_of_int(p))^  "(" ^(v1)^") (" ^(v2)^")"        
                                     else if ((String.compare (String.sub f 0 3) "*__")=0) then
                                       "F.mul_prec " ^ (string_of_int(p))^  "(" ^(v1)^") (" ^(v2)^")"
                                          else if ((String.compare (String.sub f 0 3) "/__")=0) then
                                                 "F.div_prec " ^ (string_of_int(p))^  "(" ^(v1)^") (" ^(v2)^")"
                                               else if ((String.compare (String.sub f 0 3) "+I_")=0) then
                                                      "("^v1^" + "^v2^")"
                                                    else if ((String.compare (String.sub f 0 3) "+I_")=0) then
                                                           "("^v1^" + "^v2^")"
                                                         else if ((String.compare (String.sub f 0 3) "-I_")=0) then
                                                                "("^v1^" - "^v2^")"
                                                              else if ((String.compare (String.sub f 0 3) "*I_")=0) then
                                                                     "("^v1^" * "^v2^")"
                                                                   else if ((String.compare (String.sub f 0 3) "/I_")=0) then
                                                                            "("^v1^" / "^v2^")" 
                                                                        else if ((String.compare (String.sub f 0 3) ">B_")=0) then
                                                                               let typOp = getEnvComplete f !cmpTypEnv "pas trouve" in
			                                                          if ((String.compare typOp "FLOAT")=0) then
                                                                                     " (F.compare("^(v1)^") ("^( v2)^")>0)" 
                                                                                    else
                                                                                        "("^v1^" > "^v2^")"
                                                                              else if ((String.compare (String.sub f 0 3) "<B_")=0) then
                                                                                    let typOp = getEnvComplete f !cmpTypEnv "pas trouve" in
			                                                              if ((String.compare typOp "FLOAT")=0) then
                                                                                         " (F.compare("^(v1)^") ("^( v2)^")<0)" 
                                                                                       else
                                                                                           "("^v1^" < "^v2^")"
                                                                                    else if ((String.compare (String.sub f 0 3) ">=_")=0) then
                                                                                          let typOp = getEnvComplete f !cmpTypEnv "pas trouve" in
			                                                                    if ((String.compare typOp "FLOAT")=0) then
                                                                                              " (F.compare("^(v1)^") ("^( v2)^")>=0)" 
                                                                                             else
                                                                                                 "("^v1^" >= "^v2^")"                                                       
                                                                                          else if ((String.compare (String.sub f 0 3) "<=_")=0) then
                                                                                                let typOp = getEnvComplete f !cmpTypEnv "pas trouve" in
			                                                                          if ((String.compare typOp "FLOAT")=0) then
                                                                                                     " (F.compare("^(v1)^") ("^( v2)^")<=0)" 
                                                                                                   else
                                                                                                       "("^v1^" <= "^v2^")" 
                                                                                                else if ((String.compare (String.sub f 0 3) "=B_")=0) then
                                                                                                      let typOp = getEnvComplete f !cmpTypEnv "pas trouve" in
			                                                                                if ((String.compare typOp "FLOAT")=0) then
                                                                                                           " (F.compare("^(v1)^") ("^( v2)^")=0)" 
                                                                                                         else
                                                                                                             "("^v1^" = "^v2^")"                                      
                                                                                                     else if ((String.compare (String.sub f 0 3) "!=_")=0) then
                                                                                                           let typOp = getEnvComplete f !cmpTypEnv "pas trouve" in
			                                                                                     if ((String.compare typOp "FLOAT")=0) then
                                                                                                                " (F.compare("^(v1)^") ("^( v2)^")!=0)" 
                                                                                                               else
                                                                                                                   "("^v1^" != "^v2^")"                                         
                                                                                                           else "(("^f^" "^v1^") "^v2^")"  
                                                       
| Float(f,s,u,p) -> let ff = (F.to_float f) in 
                             let ff' = if (ff>=0.0) then ff else (ff *. (-1.0)) in
                             let a = int_of_float (ceil (log10 ff')) in
                             let b = int_of_float (ceil (log10 (pow 2.0 (u-p)))) in 
                             let (s0,i) =F.to_string_exp_base_digits 10 (a-b+1) f in 
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
                             in ("(F.from_float "^s'''^")")
                               
                  

| Int(e) -> string_of_int(e)
| Bool(e) -> (if e then "true" else "false")
| Id(x) ->  x
| Cond(be,e1,e2) -> let v1 =(genGmp be) in
                    let v2= (genGmp e1) in
                    let v3= (genGmp e2) in
                        ("if" ^v1^ "then\n"^ v2^"\n else\n" ^v3)

| Let(x,e1,e2) -> let v1 = (genGmp e1) in
                  let v2 = (genGmp e2) in
                      ("let "^x^" = "^v1^" in "^v2)

| Pair(e1,e2) -> let v1= (genGmp e1) in 
                 let v2= (genGmp e2) in
                     "("^v1^","^v2^ ")"

| First(e) -> let v1= (genGmp e) in("fst "^v1)
| Second(e) -> let v1= (genGmp e) in ("snd "^v1)
| App(Id(f),e) -> "let v1 = "^(genGmp e)^ " in ( sqrt v1)"
| _ -> "error : "^(fst (printExpression e [])) 


;;




(*let (u,p) = getEnv f !opEnvUp in*)
 
