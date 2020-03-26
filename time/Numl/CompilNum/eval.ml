
open Types ;;
open Prelude ;;
open Primitives ;;
open Print ;;
open Gmp ;;

let getFP x = match x with
  Float(f,_,_,_) -> f
| _ -> raise (Error ("error in getFP:"^(printExpr x))) ;;


let getI x = match x with
  Int(f) -> f
| _ -> raise (Error "error in getI") ;;


let rec subst x e0 e = match e with
  Float(_) -> e
| Int(_) -> e
| Bool(_) -> e
| Id(y) -> if ((String.compare x y)=0) then e0 else e 
| Lambda(y,t,e1) -> if ((String.compare x y)=0) then e else Lambda(y,(*subst x e0*) t,subst x e0 e1)
| Pi(y,t,e1) -> if ((String.compare x y)=0) then e else Lambda(y,(*subst x e0*) t,subst x e0 e1)
| App(e1,e2) -> App(subst x e0 e1,subst x e0 e2)
| Cond(be,e1,e2) -> Cond(subst x e0 be,subst x e0 e1,subst x e0 e2)
| Let(y,e1,e2) -> if ((String.compare x y)=0) then e else Let(y,subst x e0 e1,subst x e0 e2)
| Pair(e1,e2) -> Pair(subst x e0 e1,subst x e0 e2)
| First(e1) -> First(subst x e0 e1)
| Second(e1) -> Second(subst x e0 e1)
| Cons(e1,e2) -> Cons(subst x e0 e1,subst x e0 e2)
| Car(e1) -> Car(subst x e0 e1)
| Cdr(e1) -> Cdr(subst x e0 e1)
| Nil -> e
| Plus(e1,e2,s,u,p) -> Plus(subst x e0 e1,subst x e0 e2,s,u,p)
| Minus(e1,e2,s,u,p) -> Minus(subst x e0 e1,subst x e0 e2,s,u,p)
| Mult(e1,e2,s,u,p) -> Mult(subst x e0 e1,subst x e0 e2,s,u,p)
| Div(e1,e2,s,u,p) -> Div(subst x e0 e1,subst x e0 e2,s,u,p)
| Sqrt(e1,s,u,p) -> Sqrt(subst x e0 e1,s,u,p)
| IPlus(e1,e2) -> IPlus(subst x e0 e1,subst x e0 e2)
| IMinus(e1,e2) -> IMinus(subst x e0 e1,subst x e0 e2)
| IMult(e1,e2) -> IMult(subst x e0 e1,subst x e0 e2)
| IDiv(e1,e2) -> IDiv(subst x e0 e1,subst x e0 e2)
| IMax(e1,e2) -> IMax(subst x e0 e1,subst x e0 e2)

| BEq(e1,e2) -> BEq(subst x e0 e1,subst x e0 e2)
| BLt(e1,e2) -> BLt(subst x e0 e1,subst x e0 e2)
| BGt(e1,e2) -> BGt(subst x e0 e1,subst x e0 e2)
| BLte(e1,e2) -> BLte(subst x e0 e1,subst x e0 e2)
| BGte(e1,e2) -> BGte(subst x e0 e1,subst x e0 e2)
| BNeq(e1,e2) -> BNeq(subst x e0 e1,subst x e0 e2)

| FEq(e1,e2,u,p) -> FEq(subst x e0 e1,subst x e0 e2,u,p)
| FLt(e1,e2,u,p) -> FLt(subst x e0 e1,subst x e0 e2,u,p)
| FGt(e1,e2,u,p) -> FGt(subst x e0 e1,subst x e0 e2,u,p)
| FLte(e1,e2,u,p) -> FLte(subst x e0 e1,subst x e0 e2,u,p)
| FGte(e1,e2,u,p) -> FGte(subst x e0 e1,subst x e0 e2,u,p)
| FNeq(e1,e2,u,p) -> FNeq(subst x e0 e1,subst x e0 e2,u,p)

| _ -> e 
;;



let rec eval e env = match e with
  Float(f,_,_,_) -> e
| Int(i) -> e
| Bool(b) -> e
| Id(x) -> getEnv x env 
| Lambda(_) -> e
| Pi(_) -> e
| App(e1,e2) -> let v1 = eval e1 env in
                let v2 = eval e2 env in
                  (match v1 with
		     Lambda(x,t,e1') -> eval (subst x v2 e1') env
                   | Pi(x,t,e1') -> eval (subst x v2 e1') env
                   | _ -> raise (Error "eval: fun expected in app")
                  )
| Let(x,e1,e2) -> let v1 = eval e1 env in eval e2 (setEnv x v1 env)
| Cond(be,e1,e2) -> let b = eval be env in
                      if (b=Bool(true)) then (eval e1 env) else (eval e2 env)
| Pair(e1,e2) -> let v1 = eval e1 env in
                 let v2 = eval e2 env in Pair(v1,v2)
| First(e1) -> let v1 = eval e1 env in
                 (match v1 with
		    Pair(v1,_) -> v1
                  | _ -> raise (Error "eval: pair expected in fst")
                 )
| Second(e1) -> let v1 = eval e1 env in
                  (match v1 with
		     Pair(_,v2) -> v2
                   | _ -> raise (Error "eval: pair expected in snd")
                  )
| Cons(e1,e2) -> let v1 = eval e1 env in
                 let v2 = eval e2 env in Cons(v1,v2)
| Car(e1) -> let v1 = eval e1 env in
               (match v1 with
                  Cons(v1,_) -> v1
                | _ -> raise (Error "eval: non-empty list expected in hd")
               )
| Cdr(e1) -> let v1 = eval e1 env in
               (match v1 with
                  Cons(_,v2) -> v2
                | _ -> raise (Error "eval: non-empty list expected in tl")
               )
| Nil -> Nil
| Plus(e1,e2,s,u,p) -> let v1 = eval e1 env in
                       let v2 = eval e2 env in Float(F.add_prec (p+2) (getFP v1)  (getFP v2),s,u,p)
| Minus(e1,e2,s,u,p) -> let v1 = eval e1 env in
                        let v2 = eval e2 env in Float(F.sub_prec (p+2) (getFP v1) (getFP v2),s,u,p)
| Mult(e1,e2,s,u,p) -> let v1 = eval e1 env in
                       let v2 = eval e2 env in Float(F.mul_prec (p+2) (getFP v1) (getFP v2),s,u,p)
| Div(e1,e2,s,u,p) -> let v1 = eval e1 env in
                      let v2 = eval e2 env in Float(F.div_prec (p+2) (getFP v1) (getFP v2),s,u,p)
| Sqrt(e1,s,u,p) -> let v1 = eval e1 env in 
                    let res = F.from_float (sqrt (F.to_float (getFP v1))) 
                    in Float(res ,s,u,p)                                (* :-(((( on evalue dans les floats au lieu de... dans Gmp *) 
| IPlus(e1,e2) -> let v1 = eval e1 env in
                  let v2 = eval e2 env in Int((getI v1) + (getI v2))
| IMinus(e1,e2) -> let v1 = eval e1 env in
                   let v2 = eval e2 env in Int((getI v1) - (getI v2))
| IMult(e1,e2) -> let v1 = eval e1 env in
                  let v2 = eval e2 env in Int((getI v1) * (getI v2))
| IDiv(e1,e2) -> let v1 = eval e1 env in
                 let v2 = eval e2 env in Int((getI v1) / (getI v2))
| IMax(e1,e2) -> let v1 = eval e1 env in
                 let v2 = eval e2 env in Int(max (getI v1) (getI v2))

| Iota(e1,e2) -> let v1 = eval e1 env in
                 let v2 = eval e2 env in Int(if ((getI v1)=(getI v2)) then 1 else 0)
| SigPlus(e1,e2,e3,e4) -> let v1 = getI (eval e1 env) in
                          let v2 = getI (eval e2 env) in 
                          let v3 = getI (eval e3 env) in 
                          let v4 = getI (eval e4 env) in 
                            Int(sigPlus v1 v2 v3 v4)
| SigMinus(e1,e2,e3,e4) -> let v1 = getI (eval e1 env) in
                           let v2 = getI (eval e2 env) in 
                           let v3 = getI (eval e3 env) in 
                           let v4 = getI (eval e4 env) in 
                             Int(sigMinus v1 v2 v3 v4)
| SigMult(e1,e2) -> let v1 = getI (eval e1 env) in
                    let v2 = getI (eval e2 env) in 
                      Int(sigMult v1 v2)
| SigmaPlus(e1,e2) -> let v1 = getI (eval e1 env) in
                      let v2 = getI (eval e2 env) in 
                        Int(sigmaPlus v1 v2)
| SigmaMinus(e1,e2) -> let v1 = getI (eval e1 env) in
                       let v2 = getI (eval e2 env) in 
                         Int(sigmaMinus v1 v2)

| BEq(e1,e2) -> let v1 = eval e1 env in
                let v2 = eval e2 env in 
				  (match (v1,v2) with
				     (Float(_),Float(_)) -> Bool((getFP v1)=(getFP v2))
				   | _ -> Bool(v1=v2)
				  )
| BLt(e1,e2) -> let v1 = eval e1 env in
                let v2 = eval e2 env in 
                  (match (v1,v2) with
                     (Float(x1,_,_,_),Float(x2,_,_,_)) -> Bool(x1 < x2)
                   | (Int(x1),Int(x2)) -> Bool(x1 < x2) 
                   | _ -> raise (Error ("eval: expression should be prohibited by types: "^(printExpr e1)^" < "^(printExpr e2)))
                  )
| BGt(e1,e2) -> let v1 = eval e1 env in
                let v2 = eval e2 env in 
                  (match (v1,v2) with
                     (Float(x1,_,_,_),Float(x2,_,_,_)) -> Bool(x1 > x2)
                   | (Int(x1),Int(x2)) -> Bool(x1 > x2) 
                   | _ -> raise (Error ("eval: bool expression should be prohibited by types"))
                  )
| BLte(e1,e2) -> let v1 = eval e1 env in
                 let v2 = eval e2 env in 
                   (match (v1,v2) with
                      (Float(x1,_,_,_),Float(x2,_,_,_)) -> Bool(x1 <= x2)
                    | (Int(x1),Int(x2)) -> Bool(x1 <= x2) 
                    | _ -> raise (Error ("eval: bool expression should be prohibited by types"))
                   )
| BGte(e1,e2) -> let v1 = eval e1 env in
                 let v2 = eval e2 env in 
                   (match (v1,v2) with
                      (Float(x1,_,_,_),Float(x2,_,_,_)) -> Bool(x1 >= x2)
                    | (Int(x1),Int(x2)) -> Bool(x1 >= x2) 
                    | _ -> raise (Error ("eval: bool expression should be prohibited by types"))
                   )
| BNeq(e1,e2) -> let v1 = eval e1 env in
                 let v2 = eval e2 env in 
                   (match (v1,v2) with
                      (Float(x1,_,_,_),Float(x2,_,_,_)) -> Bool(x1 != x2)
                    | (Int(x1),Int(x2)) -> Bool(x1 != x2) 
                    | (Bool(x1),Bool(x2)) -> Bool(x1 != x2) 
                    | _ -> raise (Error ("eval: bool expression should be prohibited by types"))
                   )
| FEq(e1,e2,u,p) -> let v1 = eval e1 env in
                    let v2 = eval e2 env in 
                      (match (v1,v2) with
                         (Float(x1,_,_,_),Float(x2,_,_,_)) -> Bool((F.compare x1 x2)=0)
                       | _ -> raise (Error ("eval: float expression expected for comparison: "^(printExpr e1)^" = "^(printExpr e2)))
                      )
| FLt(e1,e2,u,p) -> let v1 = eval e1 env in
                    let v2 = eval e2 env in 
                      (match (v1,v2) with
                         (Float(x1,_,_,_),Float(x2,_,_,_)) -> Bool((F.compare x1 x2)<0)
                       | _ -> raise (Error ("eval: float expression expected for comparison: "^(printExpr e1)^" < "^(printExpr e2)))
                      )
| FLte(e1,e2,u,p) -> let v1 = eval e1 env in
                     let v2 = eval e2 env in 
                       (match (v1,v2) with
                          (Float(x1,_,_,_),Float(x2,_,_,_)) -> Bool((F.compare x1 x2)<=0)
                        | _ -> raise (Error ("eval: float expression expected for comparison: "^(printExpr e1)^" <= "^(printExpr e2)))
                       )
| FGt(e1,e2,u,p) -> let v1 = eval e1 env in
                    let v2 = eval e2 env in 
                      (match (v1,v2) with
                         (Float(x1,_,_,_),Float(x2,_,_,_)) -> Bool((F.compare x1 x2)>0)
                       | _ -> raise (Error ("eval: float expression expected for comparison: "^(printExpr e1)^" > "^(printExpr e2)))
                      )
| FGte(e1,e2,u,p) -> let v1 = eval e1 env in
                     let v2 = eval e2 env in 
                       (match (v1,v2) with
                          (Float(x1,_,_,_),Float(x2,_,_,_)) -> Bool((F.compare x1 x2)>=0)
                        | _ -> raise (Error ("eval: float expression expected for comparison: "^(printExpr e1)^" >= "^(printExpr e2)))
                       )
| FNeq(e1,e2,u,p) -> let v1 = eval e1 env in
                     let v2 = eval e2 env in 
                       (match (v1,v2) with
                          (Float(x1,_,_,_),Float(x2,_,_,_)) -> Bool((F.compare x1 x2)!=0)
                        | _ -> raise (Error ("eval: float expression expected for comparison: "^(printExpr e1)^" != "^(printExpr e2)))
                       )
| TInt -> e
| TBool -> e
| TFloat(r0,r1,r2) -> let r0' = eval !r0 env in
                      let r1' = eval !r1 env in
                      let r2' = eval !r2 env in TFloat(ref r0',ref r1',ref r2')
| TypeVar(tor,s) -> (match !tor with
                       None -> e
                     | Some e -> tor := Some (eval e env) ; e
                    ) 
| TProduct(e1,e2) -> TProduct(eval e1 env,eval e2 env)
| TList(e1) -> let r1 = eval e1 env in TList(r1)
;;


let rec evalPart e = match e with
  Int(i) -> e
| TFloat(r0,r1,r2) -> let e0 = evalPart !r0 in
                      let e1 = evalPart !r1 in 
                      let e2 = evalPart !r2 in TFloat(ref e0,ref e1,ref e2) 
| TypeVar(r,s) -> (match !r with
                     None -> e
                   | Some(e1) -> if (isRec e1 []) then evalPart (unRec e1 []) else evalPart e1 
                  )
| Id(x) -> e
| Pi(x,e1,e2) -> Pi(x,evalPart e1,evalPart e2)
| IPlus(e1,e2) -> let i1 = evalPart e1 in
                  let i2 = evalPart e2 in  
                    (match (i1,i2) with
                       (Int(j1),Int(j2)) -> Int(j1+j2)
                     | (_,Int(j)) -> if (j=0) then i1 else IPlus(i1,i2)
                     | (Int(j),_) -> if (j=0) then i2 else IPlus(i1,i2)
                     | _ -> IPlus(i1,i2)
                    )
| IMinus(e1,e2) -> let i1 = evalPart e1 in
                   let i2 = evalPart e2 in 
                     (match (i1,i2) with
                        (Int(j1),Int(j2)) -> Int(j1-j2)
                      | (_,Int(j)) -> if (j=0) then i1 else IMinus(i1,i2)
                      | _ -> IMinus(i1,i2)
                     )
| IMult(e1,e2) -> let i1 = evalPart e1 in
                  let i2 = evalPart e2 in 
                    (match (i1,i2) with
                       (Int(j1),Int(j2)) -> Int(j1+j2)
                     | (_,Int(j)) -> if (j=0) then (Int(0)) else if (j=1) then i1 else IMult(i1,i2)
                     | (Int(j),_) -> if (j=0) then (Int(0)) else if (j=1) then i2 else IMult(i1,i2)
                     | _ -> IMult(i1,i2)
                    )
| IMax(e1,e2) ->  let i1 = evalPart e1 in
                  let i2 = evalPart e2 in 
                    (match (i1,i2) with
                       (Int(j1),Int(j2)) -> Int(max j1 j2)
                     | _ -> IMax(i1,i2)
                    )
| Iota(e1,e2) ->  let i1 = evalPart e1 in
                  let i2 = evalPart e2 in 
                    (match (i1,i2) with
                       (Int(j1),Int(j2)) -> Int(if (j1=j2) then 1 else 0)
                     | _ -> Iota(i1,i2)
                    )
| SigPlus(e1,e2,e3,e4) ->  let i1 = evalPart e1 in
                           let i2 = evalPart e2 in
                           let i3 = evalPart e3 in
                           let i4 = evalPart e4 in 
                             (match (i1,i2,i3,i4) with
                                (Int(j1),Int(j2),Int(j3),Int(j4)) -> Int(sigPlus j1 j2 j3 j4)
                              | _ -> SigPlus(i1,i2,i3,i4)
                    )
| SigMinus(e1,e2,e3,e4) ->  let i1 = evalPart e1 in
                            let i2 = evalPart e2 in
                            let i3 = evalPart e3 in
                            let i4 = evalPart e4 in 
                              (match (i1,i2,i3,i4) with
                                 (Int(j1),Int(j2),Int(j3),Int(j4)) -> Int(sigMinus j1 j2 j3 j4)
                               | _ -> SigMinus(i1,i2,i3,i4)
                     )
| SigMult(e1,e2) ->  let i1 = evalPart e1 in
                     let i2 = evalPart e2 in
                       (match (i1,i2) with
                          (Int(j1),Int(j2)) -> Int(sigMult j1 j2)
                               | _ -> SigMult(i1,i2)
                     )
| SigmaPlus(e1,e2) ->  let i1 = evalPart e1 in
                       let i2 = evalPart e2 in
                         (match (i1,i2) with
                            (Int(j1),Int(j2)) -> Int(sigmaPlus j1 j2)
                          | _ -> SigmaPlus(i1,i2)
                         )
| SigmaMinus(e1,e2) ->  let i1 = evalPart e1 in
                        let i2 = evalPart e2 in
                          (match (i1,i2) with
                             (Int(j1),Int(j2)) -> Int(sigmaMinus j1 j2)
                           | _ -> SigmaMinus(i1,i2)
                          )

| BEq(e1,e2) ->  let b1 = evalPart e1 in
                 let b2 = evalPart e2 in 
                    (match (b1,b2) with
                       (Int(j1),Int(j2)) -> Bool(j1 = j2)
                     | (Float(j1,_,_,_),Float(j2,_,_,_)) -> Bool(j1 = j2)
                     | _ -> BEq(b1,b2)
                    )
| BLt(e1,e2) ->  let b1 = evalPart e1 in
                 let b2 = evalPart e2 in 
                    (match (b1,b2) with
                       (Int(j1),Int(j2)) -> Bool(j1 < j2)
                     | (Float(j1,_,_,_),Float(j2,_,_,_)) -> Bool(j1 < j2)
                     | _ -> BLt(b1,b2)
                    )
| BLte(e1,e2) ->  let b1 = evalPart e1 in
                  let b2 = evalPart e2 in 
                     (match (b1,b2) with
                        (Int(j1),Int(j2)) -> Bool(j1 <= j2)
                      | (Float(j1,_,_,_),Float(j2,_,_,_)) -> Bool(j1 <= j2)
                      | _ -> BLte(b1,b2)
                     )
| BNeq(e1,e2) ->  let b1 = evalPart e1 in
                  let b2 = evalPart e2 in 
                     (match (b1,b2) with
                        (Int(j1),Int(j2)) -> Bool(j1 != j2)
                      | (Float(j1,_,_,_),Float(j2,_,_,_)) -> Bool(j1 != j2)
                      | _ -> BNeq(b1,b2)
                     )
| BGt(e1,e2) ->  let b1 = evalPart e1 in
                 let b2 = evalPart e2 in 
                    (match (b1,b2) with
                       (Int(j1),Int(j2)) -> Bool(j1 > j2)
                     | (Float(j1,_,_,_),Float(j2,_,_,_)) -> Bool(j1 > j2)
                     | _ -> BGt(b1,b2)
                    )
| BGte(e1,e2) ->  let b1 = evalPart e1 in
                  let b2 = evalPart e2 in 
                     (match (b1,b2) with
                        (Int(j1),Int(j2)) -> Bool(j1 >= j2)
                      | (Float(j1,_,_,_),Float(j2,_,_,_)) -> Bool(j1 >= j2)
                      | _ -> BGte(b1,b2)
                     )

| Pair(e1,e2) -> let e1' = evalPart e1 in
                 let e2' = evalPart e2 in Pair(e1',e2')
| First(e1) -> let e1' = evalPart e1 
               in (match e1' with
                     Pair(a,b) -> a
                   | _ -> First(e1')
                  )
| Second(e1) -> let e1' = evalPart e1 
                in (match e1' with
                     Pair(a,b) -> b
                   | _ -> Second(e1')
                  )                     
| Cons(e1,e2) -> let e1' = evalPart e1 in
                 let e2' = evalPart e2 in Cons(e1',e2')
| Car(e1) -> let e1' = evalPart e1 in
               (match e1' with
                  Cons(a,_) -> a
                | _ -> Car(e1')
               )
| Cdr(e1) -> let e1' = evalPart e1 in
               (match e1' with
                  Cons(_,b) -> b
                | _ -> Cdr(e1')
               )
| Nil -> Nil

| TInt -> TInt
| TBool -> TBool
| TProduct(e1,e2) -> let e1' = evalPart e1 in
                     let e2' = evalPart e2 in TProduct(e1',e2')
| TList(e1) -> let e1' = evalPart e1 in TList(e1')
| Let(id,e1,e2) -> let e2' = evalPart e2
                   in if (occurs id e2') then
                        let e1' = evalPart e1 in Let(id,e1',e2')
                      else e2'
| Cond(e0,e1,e2) -> let e0' = evalPart e0 in
                    let e1' = evalPart e1 in
                    let e2' = evalPart e2 in 
                      (match e0' with
                         Bool(true) -> e1'
                       | Bool(false) -> e2'
                       | _ -> Cond(e0',e1',e2')
                      )

| _ -> (raise (Error (">>>>>"^(printExpr e)^"<<<")));;


let rec getVars e lTV ls = match e with
  TypeVar(r,s) -> (match !r with
                     None -> if (List.mem s ls) then (lTV,ls) else (e::lTV,s::ls)
                   | _ -> raise (Error ("should not occur in getVars: "^(printExpr e)))
                  )				   
| IPlus(e1,e2) -> let (lTV1,ls1) = getVars e1 lTV ls 
                  in getVars e2  lTV1 ls1
| IMinus(e1,e2) -> let (lTV1,ls1) = getVars e1 lTV ls 
                   in getVars e2  lTV1 ls1
| IMult(e1,e2) -> let (lTV1,ls1) = getVars e1 lTV ls 
                  in getVars e2  lTV1 ls1
| IDiv(e1,e2) -> let (lTV1,ls1) = getVars e1 lTV ls 
                 in getVars e2  lTV1 ls1
| IMax(e1,e2) -> let (lTV1,ls1) = getVars e1 lTV ls 
                 in getVars e2  lTV1 ls1
| Iota(e1,e2) -> let (lTV1,ls1) = getVars e1 lTV ls 
                 in getVars e2  lTV1 ls1
| SigPlus(e1,e2,e3,e4) -> let (lTV1,ls1) = getVars e1 lTV ls in
                          let (lTV2,ls2) = getVars e2 lTV1 ls1 in
                          let (lTV3,ls3) = getVars e3 lTV2 ls2
                          in getVars e4  lTV3 ls3
| SigMinus(e1,e2,e3,e4) -> let (lTV1,ls1) = getVars e1 lTV ls in
                           let (lTV2,ls2) = getVars e2 lTV1 ls1 in
                           let (lTV3,ls3) = getVars e3 lTV2 ls2
                           in getVars e4  lTV3 ls3
| SigMult(e1,e2) -> let (lTV1,ls1) = getVars e1 lTV ls 
                    in getVars e2  lTV1 ls1
| SigmaPlus(e1,e2) -> let (lTV1,ls1) = getVars e1 lTV ls 
                      in getVars e2  lTV1 ls1
| SigmaMinus(e1,e2) -> let (lTV1,ls1) = getVars e1 lTV ls 
                       in getVars e2  lTV1 ls1
| _ -> (lTV,ls) ;;

let removePrime s = String.sub s 1 ((String.length s)-1) ;;

let rec vl2Z3 vl = match vl with
  [] -> ""
| x::xs -> ("(declare-const "^x^" Int)\n"^(vl2Z3 xs))
;;

let rec expr2Z3 e = match e with
  Int(i) -> string_of_int i
| Id(s) -> s
| TypeVar(r,s) -> removePrime s 
| IPlus(e1,e2) -> let s1 = expr2Z3 e1 in
                  let s2 = expr2Z3 e2 
                  in "(+ "^s1^" "^s2^")"
| IMinus(e1,e2) -> let s1 = expr2Z3 e1 in
                   let s2 = expr2Z3 e2 
                   in "(- "^s1^" "^s2^")"
| IMult(e1,e2) -> let s1 = expr2Z3 e1 in
                  let s2 = expr2Z3 e2 
                  in "(* "^s1^" "^s2^")"
| IMax(e1,e2) -> let s1 = expr2Z3 e1 in
                 let s2 = expr2Z3 e2 
                 in "(ite (>= "^s1^" "^s2^") "^s1^" "^s2^")"
| Iota(e1,e2) -> let s1 = expr2Z3 e1 in
                 let s2 = expr2Z3 e2 
                 in "(ite (= "^s1^" "^s2^") 1 0)"
| SigmaPlus(e1,e2) -> let s1 = expr2Z3 e1 in
                      let s2 = expr2Z3 e2 
                      in ("(ite (= "^s1^" 0) 0
                           (ite (= "^s2^" 0) 0 
                           (ite (and (= "^s1^" 1) (= "^s2^" -1)) 0 
                           (ite (and (= "^s1^" -1) (= "^s2^" 1)) 0 
                           1))))")  
| SigmaMinus(e1,e2) -> let s1 = expr2Z3 e1 in
                       let s2 = expr2Z3 e2 
                       in ("(ite (= "^s1^" 0) 0
                            (ite (= "^s2^" 0) 0
                            (ite (and (= "^s1^" 1) (= "^s2^" 1)) 0 
                            (ite (and (= "^s1^" -1) (= "^s2^" -1)) 0 
                            1))))") 
| SigPlus(e1,e2,e3,e4) -> let s1 = expr2Z3 e1 in
                          let u1 = expr2Z3 e2 in
                          let s2 = expr2Z3 e3 in
                          let u2 = expr2Z3 e4 in
                          ("(ite (and (= "^s1^" 0) (= "^s2^" 0)) 0 
                            (ite (and (= "^s1^" 1) (= "^s2^" 1)) 1 
                            (ite (and (= "^s1^" -1) (= "^s2^" -1)) -1 
                            (ite (and (= "^s1^" 1) (= "^s2^" 0)) 1 
                            (ite (and (= "^s1^" 0) (= "^s2^" 1)) 1
                            (ite (and (= "^s1^" -1) (= "^s2^" 0)) -1 
                            (ite (and (= "^s1^" 0) (= "^s2^" -1)) -1 
                            (ite (and (= "^s1^" 1) (= "^s2^" -1)) (ite (> "^u1^" "^u2^") 1 (ite (< "^u1^" "^u2^") -1 2)) 
                            (ite (and (= "^s1^" -1) (= "^s2^" 1)) (ite (< "^u1^" "^u2^") 1 (ite (> "^u1^" "^u2^") -1 2))
                            2)))))))))") 
| SigMinus(e1,e2,e3,e4) -> let s1 = expr2Z3 e1 in
                          let u1 = expr2Z3 e2 in
                          let s2 = expr2Z3 e3 in
                          let u2 = expr2Z3 e4 in
                          ("(ite (and (= "^s1^" 0) (= "^s2^" 0)) 0 
                            (ite (and (= "^s1^" 1) (= "^s2^" -1)) 1 
                            (ite (and (= "^s1^" -1) (= "^s2^" 1)) -1 
                            (ite (and (= "^s1^" 1) (= "^s2^" 0)) 1 
                            (ite (and (= "^s1^" 0) (= "^s2^" 1)) -1
                            (ite (and (= "^s1^" -1) (= "^s2^" 0)) -1 
                            (ite (and (= "^s1^" 0) (= "^s2^" -1)) 1 
                            (ite (and (= "^s1^" 1) (= "^s2^" 1)) (ite (> "^u1^" "^u2^") 1 (ite (< "^u1^" "^u2^") -1 2)) 
                            (ite (and (= "^s1^" -1) (= "^s2^" -1)) (ite (< "^u1^" "^u2^") 1 (ite (> "^u1^" "^u2^") -1 2))
                            2)))))))))") 
| SigMult(e1,e2) -> let s1 = expr2Z3 e1 in
                    let s2 = expr2Z3 e2 in
                          ("(ite (and (= "^s1^" 0) (= "^s2^" 0)) 0 
                            (ite (and (= "^s1^" 1) (= "^s2^" -1)) -1 
                            (ite (and (= "^s1^" -1) (= "^s2^" 1)) -1 
                            (ite (and (= "^s1^" -1) (= "^s2^" -1)) 1 
                            (ite (and (= "^s1^" 1) (= "^s2^" 1)) 1 
                            (ite (and (= "^s1^" 1) (= "^s2^" 0)) 0 
                            (ite (and (= "^s1^" 0) (= "^s2^" 1)) 0
                            (ite (and (= "^s1^" -1) (= "^s2^" 0)) 0 
                            (ite (and (= "^s1^" 0) (= "^s2^" -1)) 0
                            2)))))))))") 
| _ -> (print_string (":-[[[ "^(printExpr e)); "??") ;;


let rec z32expr e = match e with
  Int(i) -> ()
| Id(s) -> ()
| TypeVar(r,s) -> (try 
                     let v = getEnv (removePrime s) !intEnv
                     in (match !r with
                           None -> r := Some(Int(v))
                         | _ -> raise (Error "unxepected case in z32expr")
                        )
                   with _ -> ()
                  ) 
| IPlus(e1,e2) -> let _ = z32expr e1 in
                  let _ = z32expr e2 in ()
| IMinus(e1,e2) -> let _ = z32expr e1 in
                   let _ = z32expr e2 in ()
| IMult(e1,e2) -> let _ = z32expr e1 in
                  let _ = z32expr e2 in ()
| IMax(e1,e2) -> let _ = z32expr e1 in
                 let _ = z32expr e2 in ()
| Iota(e1,e2) -> let _ = z32expr e1 in
                 let _ = z32expr e2 in ()
| SigmaPlus(e1,e2) -> let _ = z32expr e1 in
                      let _ = z32expr e2 in ()
| SigmaMinus(e1,e2) -> let _ = z32expr e1 in
                       let _ = z32expr e2 in ()
| SigPlus(e1,e2,e3,e4) -> let _ = z32expr e1 in
                          let _ = z32expr e2 in
                          let _ = z32expr e3 in
                          let _ = z32expr e4 in ()
| SigMinus(e1,e2,e3,e4) -> let _ = z32expr e1 in
                           let _ = z32expr e2 in
                           let _ = z32expr e3 in
                           let _ = z32expr e4 in ()
| SigMult(e1,e2) -> let _ = z32expr e1 in
                    let _ = z32expr e2 in ()
| _ -> raise (Error "unexpected case in z32expr")
;;


let solveOneEq e v =
  let ez3 = expr2Z3 e in
  let vz3 = expr2Z3 v in
    (ez3,vz3)
;;
  
let solve e1 v1 e2 v2 e3 v3 = 

  let e1' = evalPart e1 in
  let v1' = evalPart v1 in
  let e2' = evalPart e2 in
  let v2' = evalPart v2 in
  let e3' = evalPart e3 in
  let v3' = evalPart v3 in
  let (lTV1,ls1) = getVars e1' [] [] in
  let (lTV2,ls2) = getVars v1' lTV1 ls1 in
  let (lTV3,ls3) = getVars e2' lTV2 ls2 in
  let (lTV4,ls4) = getVars v2' lTV3 ls3 in
  let (lTV5,ls5) = getVars e3' lTV4 ls4 in
  let (lTV6,ls6) = getVars v3' lTV5 ls5 in

  let evl' = List.map removePrime ls6 in
  let decl = vl2Z3 evl' in

  let (lhs1,rhs1) = solveOneEq e1' v1' in    
  let (lhs2,rhs2) = solveOneEq e2' v2' in    
  let (lhs3,rhs3) = solveOneEq e3' v3' in    
  let runZ3 = "(check-sat)\n(get-model)\n" in 
  let a1 = ("(assert (= "^lhs1^" "^rhs1^"))\n") in
  let a2 = ("(assert (= "^lhs2^" "^rhs2^"))\n") in 
  let a3 = ("(assert (= "^lhs3^" "^rhs3^"))\n") in 
  let z3 = decl^a1^a2^a3^runZ3 in
  let f = open_out "numl.z3" in 
  let _ = output_string f z3 in
  let _ = close_out f in 
  let sh = "z3 numl.z3 > numl.res" in 
  let _ = Sys.command sh in 
  let _ = codeLine := 0 in 
  let f = open_in "numl.res" in
  let lexbuf = Lexing.from_channel f in 
  let _ = intEnv := [] in
  let z3 = Z3parser.z3 Z3lexer.token lexbuf in 
  let _ = close_in f in
  let _ = codeLine := -1 in
  let _ = if (not z3) then
              raise (Error "Z3 failed!") 
          else
            ()
  in (z32expr e1' ; z32expr v1'; z32expr e2' ; z32expr v2'; z32expr e3' ; z32expr v3') 
;; 


let solveLT e1 v1 e2 v2 e3 v3 = 

  let e1' = evalPart e1 in
  let v1' = evalPart v1 in
  let e2' = evalPart e2 in
  let v2' = evalPart v2 in
  let e3' = evalPart e3 in
  let v3' = evalPart v3 in

  let (lTV1,ls1) = getVars e1' [] [] in
  let (lTV2,ls2) = getVars v1' lTV1 ls1 in
  let (lTV3,ls3) = getVars e2' lTV2 ls2 in
  let (lTV4,ls4) = getVars v2' lTV3 ls3 in
  let (lTV5,ls5) = getVars e3' lTV4 ls4 in
  let (lTV6,ls6) = getVars v3' lTV5 ls5 in

  let evl' = List.map removePrime ls6 in
  let decl = vl2Z3 evl' in

  let (lhs1,rhs1) = solveOneEq e1' v1' in    
  let (lhs2,rhs2) = solveOneEq e2' v2' in    
  let (lhs3,rhs3) = solveOneEq e3' v3' in    
  let runZ3 = "(check-sat)\n(get-model)\n" in 
  let a1 = ("(assert (= "^lhs1^" "^rhs1^"))\n") in
  let a2 = ("(assert (= "^lhs2^" "^rhs2^"))\n") in 
  let a3 = ("(assert (>= "^lhs3^" "^rhs3^"))\n") in 
  let z3 = decl^a1^a2^a3^runZ3 in
  let f = open_out "numl.z3" in 
  let _ = output_string f z3 in
  let _ = close_out f in 
  let sh = "z3 numl.z3 > numl.res" in 
  let _ = Sys.command sh in 
  let _ = codeLine := 0 in 
  let f = open_in "numl.res" in
  let lexbuf = Lexing.from_channel f in 
  let _ = intEnv := [] in
  let z3 = Z3parser.z3 Z3lexer.token lexbuf in 
  let _ = close_in f in
  let _ = codeLine := -1 in
  let _ = if (not z3) then
              raise (Error "Z3 failed!") 
          else
            ()
  in (z32expr e1' ; z32expr v1'; z32expr e2' ; z32expr v2'; z32expr e3' ; z32expr v3') 
;; 


