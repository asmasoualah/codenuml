
open Types;;
open Prelude ;;


let sigPlus s1 u1 s2 u2 = match (s1,s2) with
  (0,0) -> 0
| (1,1) -> 1
| (-1,-1) -> -1
| (1,0) -> 1
| (0,1) -> 1
| (-1,0) -> -1
| (0,-1) -> -1
| (1,-1) -> if (u1>u2) then 1 else 
            if (u1<u2) then -1 else 2
| (-1,1) -> if (u1<u2) then 1 else 
            if (u1>u2) then -1 else 2
| _ -> 2 ;;

let sigMinus s1 u1 s2 u2 = match (s1,s2) with
  (0,0) -> 0
| (1,-1) -> 1
| (-1,1) -> -1
| (1,0) -> 1
| (0,1) -> -1
| (-1,0) -> -1
| (0,-1) -> 1
| (1,1) -> if (u1>u2) then 1 else 
            if (u1<u2) then -1 else 2
| (-1,-1) -> if (u1<u2) then 1 else 
             if (u1>u2) then -1 else 2
| _ -> 2 ;;

let sigMult s1 s2 = match (s1,s2) with
  (0,0) -> 0
| (1,1) -> 1
| (-1,-1) -> 1
| (1,0) -> 0
| (0,1) -> 0
| (-1,0) -> 0
| (0,-1) -> 0
| (1,-1) -> -1
| (-1,1) -> -1
| _ -> 2 ;;

let sigmaPlus s1 s2 = match (s1,s2) with
  (0,_) -> 0
| (_,0) -> 0
| (1,1) -> 1
| (1,-1) -> 0
| (-1,1) -> 0
| (-1,-1) -> 1
| _ -> 1 ;;

let sigmaMinus s1 s2 = match (s1,s2) with
  (0,_) -> 0
| (_,0) -> 0
| (1,1) -> 0
| (1,-1) -> 1
| (-1,1) -> 1
| (-1,-1) -> 0
| _ -> 1 ;;


(*
(*  a * b = c *) 
let solveSigMult a c = match (a,c) with
  (0,0) -> 1
| (0,1) -> 1 
| (0,-1) -> -1
| (0,2) -> 2
| (1,0) -> 0
| (1,1) -> 1
| (1,-1) -> -1
| (1,2) -> 2
| (-1,0) -> 0
| (-1,1) -> -1
| (-1,-1) -> 1
| (-1,2) -> 2
| (2,0) -> 0
| (2,1) -> 2
| (2,-1) -> 2
| (2,2) -> 2 ;;


(* x1 + x2 = x3) *)
let solveSigPlus s1 s3 = match (s1,s3) with
  (0,0) -> 0
| (0,1) -> 1 
| (0,-1) -> -1
| (0,2) -> 2
| (1,0) -> -1
| (1,1) -> 1
| (1,-1) -> -1
| (1,2) -> -1
| (-1,0) -> 1
| (-1,1) -> 1
| (-1,-1) -> -1
| (-1,2) -> 1
| (2,0) -> 2
| (2,1) -> 2
| (2,-1) -> 2
| (2,2) -> 2 ;;


(* x1 - x2 = x3) *)
let solveSigMinus s1 s3 = match (s1,s3) with
  (0,0) -> 0      
| (0,1) -> -1     
| (0,-1) -> 1     
| (0,2) -> 2      
| (1,0) -> 1      
| (1,1) -> -1      
| (1,-1) -> 1      
| (1,2) -> 1       
| (-1,0) -> -1     
| (-1,1) -> -1     
| (-1,-1) -> 1     
| (-1,2) -> -1   
| (2,0) -> 2
| (2,1) -> 2
| (2,-1) -> 2
| (2,2) -> 2 ;;
*)

(************************ Integers *************************************************************)

let iPlusVar =
  Lambda("x",TInt,Lambda("y",TInt,IPlus(Id("x"),Id("y")))) ;;


let iPlusType = 
    Pi("x",TInt,Pi("y",TInt,TInt)) ;;


let iMinusVar =
  Lambda("x",TInt,Lambda("y",TInt,IMinus(Id("x"),Id("y")))) ;;


let iMinusType = 
    Pi("x",TInt,Pi("y",TInt,TInt)) ;;


let iMultVar =
  Lambda("x",TInt,Lambda("y",TInt,IMult(Id("x"),Id("y")))) ;;


let iMultType = 
    Pi("x",TInt,Pi("y",TInt,TInt)) ;;


let iDivVar =
  Lambda("x",TInt,Lambda("y",TInt,IDiv(Id("x"),Id("y")))) ;;


let iDivType = 
    Pi("x",TInt,Pi("y",TInt,TInt)) ;;


(******************************** Booleans *****************************************************)

(*
let boolEqVar =
  let a = newTypeVar () in
    Lambda("x",a,Lambda("y",a,BEq(Id("x"),Id("y")))) ;;



let enforceUnify a b = Cond(Bool(true),a,b) ;;


let boolEqType = 
  let a = newTypeVar () in
  let b = newTypeVar () in
    Pi("x",a,Pi("y",b,Let("#dummy",enforceUnify a b,TBool))) ;;


let boolLtVar =
  let a = newTypeVar () in
    Lambda("x",a,Lambda("y",a,BLt(Id("x"),Id("y")))) ;;


let boolLtType = 
  let a = newTypeVar () in
  let b = newTypeVar () in
    Pi("x",a,Pi("y",b,Let("#dummy",enforceUnify a b,TBool))) ;;



let boolGtVar =
  let a = newTypeVar () in
    Lambda("x",a,Lambda("y",a,BGt(Id("x"),Id("y")))) ;;


let boolGtType = 
  let a = newTypeVar () in
  let b = newTypeVar () in
    Pi("x",a,Pi("y",b,Let("#dummy",enforceUnify a b,TBool))) ;;


let boolLteVar =
  let a = newTypeVar () in
    Lambda("x",a,Lambda("y",a,BLte(Id("x"),Id("y")))) ;;


let boolLteType = 
  let a = newTypeVar () in
  let b = newTypeVar () in
    Pi("x",a,Pi("y",b,Let("#dummy",enforceUnify a b,TBool))) ;;


let boolGteVar =
  let a = newTypeVar () in
    Lambda("x",a,Lambda("y",a,BGte(Id("x"),Id("y")))) ;;


let boolGteType = 
  let a = newTypeVar () in
  let b = newTypeVar () in
    Pi("x",a,Pi("y",b,Let("#dummy",enforceUnify a b,TBool))) ;;


let boolNeqVar =
  let a = newTypeVar () in
    Lambda("x",a,Lambda("y",a,BNeq(Id("x"),Id("y")))) ;;


let boolNeqType = 
  let a = newTypeVar () in
  let b = newTypeVar () in
    Pi("x",a,Pi("y",b,Let("#dummy",enforceUnify a b,TBool))) ;;
*)

let boolEqVar =
  let a = newTypeVar () in
    Lambda("x",a,Lambda("y",a,BEq(Id("x"),Id("y")))) ;;


let boolEqType = 
  let a = newTypeVar () in
    Pi("x",a,Pi("y",a,TBool)) ;;


let boolLtVar =
  let a = newTypeVar () in
    Lambda("x",a,Lambda("y",a,BLt(Id("x"),Id("y")))) ;;


let boolLtType = 
  let a = newTypeVar () in
    Pi("x",a,Pi("y",a,TBool)) ;;


let boolGtVar =
  let a = newTypeVar () in
    Lambda("x",a,Lambda("y",a,BGt(Id("x"),Id("y")))) ;;


let boolGtType = 
  let a = newTypeVar () in
    Pi("x",a,Pi("y",a,TBool)) ;;


let boolLteVar =
  let a = newTypeVar () in
    Lambda("x",a,Lambda("y",a,BLte(Id("x"),Id("y")))) ;;


let boolLteType = 
  let a = newTypeVar () in
    Pi("x",a,Pi("y",a,TBool)) ;;


let boolGteVar =
  let a = newTypeVar () in
    Lambda("x",a,Lambda("y",a,BGte(Id("x"),Id("y")))) ;;


let boolGteType = 
  let a = newTypeVar () in
    Pi("x",a,Pi("y",a,TBool)) ;;


let boolNeqVar =
  let a = newTypeVar () in
    Lambda("x",a,Lambda("y",a,BNeq(Id("x"),Id("y")))) ;;


let boolNeqType = 
  let a = newTypeVar () in
    Pi("x",a,Pi("y",a,TBool)) ;;



let newEqFloatType s u p =
  let r = TFloat(ref (Int s),ref (Int u),ref (Int p))
  in Pi("x",r,Pi("y",r,TBool)) ;;


let newEqFloatVar s u p = 
  let a = newTypeVar () in
  let b = newTypeVar () in
    Lambda("x",a,Lambda("y",b,FEq(Id("x"),Id("y"),u,p))) ;;


let newNeqFloatType s u p =
  let r = TFloat(ref (Int s),ref (Int u),ref (Int p))
  in Pi("x",r,Pi("y",r,TBool)) ;;


let newNeqFloatVar s u p = 
  let a = newTypeVar () in
  let b = newTypeVar () in
    Lambda("x",a,Lambda("y",b,FNeq(Id("x"),Id("y"),u,p))) ;;


let newLtFloatType s u p =
  let r = TFloat(ref (Int s),ref (Int u),ref (Int p))
  in Pi("x",r,Pi("y",r,TBool)) ;;


let newLtFloatVar s u p = 
  let a = newTypeVar () in
  let b = newTypeVar () in
    Lambda("x",a,Lambda("y",b,FLt(Id("x"),Id("y"),u,p))) ;;


let newLteFloatType s u p =
  let r = TFloat(ref (Int s),ref (Int u),ref (Int p))
  in Pi("x",r,Pi("y",r,TBool)) ;;


let newLteFloatVar s u p = 
  let a = newTypeVar () in
  let b = newTypeVar () in
    Lambda("x",a,Lambda("y",b,FLte(Id("x"),Id("y"),u,p))) ;;


let newGtFloatType s u p =
  let r = TFloat(ref (Int s),ref (Int u),ref (Int p))
  in Pi("x",r,Pi("y",r,TBool)) ;;


let newGtFloatVar s u p = 
  let a = newTypeVar () in
  let b = newTypeVar () in
    Lambda("x",a,Lambda("y",b,FGt(Id("x"),Id("y"),u,p))) ;;


let newGteFloatType s u p =
  let r = TFloat(ref (Int s),ref (Int u),ref (Int p))
  in Pi("x",r,Pi("y",r,TBool)) ;;


let newGteFloatVar s u p = 
  let a = newTypeVar () in
  let b = newTypeVar () in
    Lambda("x",a,Lambda("y",b,FGte(Id("x"),Id("y"),u,p))) ;;




(********************************** Envs ***************************************************)

let varEnv  = ref [("+I_",iPlusVar);("-I_",iMinusVar);("*I_",iMultVar);("/I_",iDivVar);
                   ("=B_",boolEqVar);("<B_",boolLtVar);(">B_",boolGtVar);("!=_",boolNeqVar);("<=_",boolLteVar);(">=_",boolGteVar)] ;;


let typeEnv = ref [("+I_",iPlusType);("-I_",iMinusType);("*I_",iMultType);("/I_",iDivType);
                   ("=B_",boolEqType);("<B_",boolLtType);(">B_",boolGtType);("!=_",boolNeqType);("<=_",boolLteType);(">=_",boolGteType)] ;;


(********************************** Floats ***************************************************)

let newPlusVar s u p = 
  let s1 = newTypeVar () in
  let u1 = newTypeVar () in
  let p1 = newTypeVar () in
  let s2 = newTypeVar () in
  let u2 = newTypeVar () in
  let p2 = newTypeVar () in 
    Lambda("x",TFloat(ref s1,ref u1,ref p1),
           Lambda("y",TFloat(ref s2,ref u2,ref p2),
                  App(App(App(App(App(App(Pi("ss1",TInt,
                                          Pi("uu1",TInt,
                                          Pi("pp1",TInt,
                                          Pi("ss2",TInt,
                                          Pi("uu2",TInt,
                                          Pi("pp2",TInt,Plus(Id("x"),Id("y"),s,u,p)))))))
                    ,s1),u1),p1),s2),u2),p2)  )) ;;


let newMinusVar s u p = 
  let s1 = newTypeVar () in
  let u1 = newTypeVar () in
  let p1 = newTypeVar () in
  let s2 = newTypeVar () in
  let u2 = newTypeVar () in
  let p2 = newTypeVar () in 
    Lambda("x",TFloat(ref s1,ref u1,ref p1),
           Lambda("y",TFloat(ref s2,ref u2,ref p2),
                  App(App(App(App(App(App(Pi("ss1",TInt,
                                          Pi("uu1",TInt,
                                          Pi("pp1",TInt,
                                          Pi("ss2",TInt,
                                          Pi("uu2",TInt,
                                          Pi("pp2",TInt,Minus(Id("x"),Id("y"),s,u,p)))))))
                    ,s1),u1),p1),s2),u2),p2)  )) ;;
					
					
					
let newMultVar s u p = 
  let s1 = newTypeVar () in
  let u1 = newTypeVar () in
  let p1 = newTypeVar () in
  let s2 = newTypeVar () in
  let u2 = newTypeVar () in
  let p2 = newTypeVar () in 
    Lambda("x",TFloat(ref s1,ref u1,ref p1),
           Lambda("y",TFloat(ref s2,ref u2,ref p2),
                  App(App(App(App(App(App(Pi("ss1",TInt,
                                          Pi("uu1",TInt,
                                          Pi("pp1",TInt,
                                          Pi("ss2",TInt,
                                          Pi("uu2",TInt,
                                          Pi("pp2",TInt,Mult(Id("x"),Id("y"),s,u,p)))))))
                    ,s1),u1),p1),s2),u2),p2)  )) ;;


let newDivVar s u p = 
  let s1 = newTypeVar () in
  let u1 = newTypeVar () in
  let p1 = newTypeVar () in
  let s2 = newTypeVar () in
  let u2 = newTypeVar () in
  let p2 = newTypeVar () in 
    Lambda("x",TFloat(ref s1,ref u1,ref p1),
           Lambda("y",TFloat(ref s2,ref u2,ref p2),
                  App(App(App(App(App(App(Pi("ss1",TInt,
                                          Pi("uu1",TInt,
                                          Pi("pp1",TInt,
                                          Pi("ss2",TInt,
                                          Pi("uu2",TInt,
                                          Pi("pp2",TInt,Div(Id("x"),Id("y"),s,u,p)))))))
                    ,s1),u1),p1),s2),u2),p2)  )) ;;

		
					
let newPlusType () = 
  let s1 = newTypeVar () in
  let u1 = newTypeVar () in
  let p1 = newTypeVar () in
  let s2 = newTypeVar () in
  let u2 = newTypeVar () in
  let p2 = newTypeVar () in 
    Pi("x",TFloat(ref s1,ref u1,ref p1),Pi("y",TFloat(ref s2,ref u2,ref p2),
                                        TFloat(ref (SigPlus(s1,u1,s2,u2)),
                                               ref (IPlus(IMax(u1,u2),SigmaPlus(s1,s2))),
                                               ref (IMinus(IMinus(IPlus(IMax(u1,u2),SigmaPlus(s1,s2)),
                                                                        IMax(IMinus(u1,p1),IMinus(u2,p2))
                                                                 ),
                                                           Iota(IMinus(u1,p1),IMinus(u2,p2))
                                                          )
                                                   )
                                              ) 
                                   )
      ) ;;


					
let newMinusType () = 
  let s1 = newTypeVar () in
  let u1 = newTypeVar () in
  let p1 = newTypeVar () in
  let s2 = newTypeVar () in
  let u2 = newTypeVar () in
let p2 = newTypeVar () in
    Pi("x",TFloat(ref s1,ref u1,ref p1),Pi("y",TFloat(ref s2,ref u2,ref p2),
                                        TFloat(ref (SigMinus(s1,u1,s2,u2)),
                                               ref (IPlus(IMax(u1,u2),SigmaMinus(s1,s2))),
                                               ref (IMinus(IMinus(IPlus(IMax(u1,u2),SigmaMinus(s1,s2)),
                                                                        IMax(IMinus(u1,p1),IMinus(u2,p2))
                                                                 ),
                                                           Iota(IMinus(u1,p1),IMinus(u2,p2))
                                                          )
                                                   )
                                              ) 
                                   )
      ) ;;


	  (* 18/9/17 
let newMinusType () = 
  let s1 = newTypeVar () in
  let u1 = newTypeVar () in
  let p1 = newTypeVar () in
  let s2 = newTypeVar () in
  let u2 = newTypeVar () in
  let p2 = newTypeVar () in 
    Pi("x",TFloat(ref s1,ref u1,ref p1),Pi("y",TFloat(ref s2,ref u2,ref p2),
                                        TFloat(ref (SigMinus(s1,u1,s2,u2)),
                                               ref (IPlus(IMax(u1,u2),SigmaMinus(s1,s2))),
                                               ref (IPlus(IMax(IMinus(u1,p1),IMinus(p1,u1))
											             ,Int(1)))
                                              ) 
                                   )
      ) ;;
*)


let newMultType () = 
  let s1 = newTypeVar () in
  let u1 = newTypeVar () in
  let p1 = newTypeVar () in
  let s2 = newTypeVar () in
  let u2 = newTypeVar () in
  let p2 = newTypeVar () in 
    Pi("x",TFloat(ref s1,ref u1,ref p1),Pi("y",TFloat(ref s2,ref u2,ref p2),
                                        TFloat(ref (SigMult(s1,s2)),
                                               ref (IPlus(IPlus(u1,u2),(Int(1)))),
                                               ref (IMinus(IMinus(IPlus(IPlus(u1,u2),Int(1)),
                                                                  IMax(IMinus(IPlus(IPlus(u1,u2),Int(1)),p1),
                                                                       IMinus(IPlus(IPlus(u1,u2),Int(1)),p2)
                                                                      )
                                                                 ),
                                                           Iota(p1,p2)
                                                          )
                                                   )
                                              ) 
                                   )
      ) ;;

let newDivType () = 
  let s1 = newTypeVar () in
  let u1 = newTypeVar () in
  let p1 = newTypeVar () in
  let s2 = newTypeVar () in
  let u2 = newTypeVar () in
  let p2 = newTypeVar () in 
    Pi("x",TFloat(ref s1,ref u1,ref p1),Pi("y",TFloat(ref s2,ref u2,ref p2),
                                        TFloat(ref (SigMult(s1,s2)),
                                               ref (IPlus(IMinus(u1,u2),(Int(1)))),
                                               ref (IMinus(IMinus(IPlus(IMinus(u1,u2),Int(1)),
                                                                  IMax(IMinus(IPlus(IMinus(u1,u2),Int(1)),p1),
                                                                       IMinus(IPlus(IMinus(u1,u2),Int(1)),p2)
                                                                      )
                                                                 ),
                                                           Iota(p1,p2)
                                                          )
                                                   )
                                              ) 
                                   )
      ) ;;



(* 18/9/17 
let newDivType () = 
  let s1 = newTypeVar () in
  let u1 = newTypeVar () in
  let p1 = newTypeVar () in
  let s2 = newTypeVar () in
  let u2 = newTypeVar () in
  let p2 = newTypeVar () in 
    Pi("x",TFloat(ref s1,ref u1,ref p1),Pi("y",TFloat(ref s2,ref u2,ref p2),
                                        TFloat(ref (SigMult(s1,s2)),
                                               ref (IPlus(IMinus(u1,u2),Int(1))),
                                               ref (IPlus(IMinus(IMinus(IMinus(u1,u2),p2),p2),Int(3)))
                                              ) 
                                   )
      ) ;;
*)
  


