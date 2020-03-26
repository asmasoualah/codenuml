(declare-const a_52 Int)
(declare-const a_48 Int)
(declare-const a_51 Int)
(declare-const a_47 Int)
(declare-const a_50 Int)
(declare-const a_46 Int)
(assert (= 1 (ite (and (= a_46 0) (= a_50 0)) 0 
                            (ite (and (= a_46 -1) (= a_50 -1)) 1 
                            (ite (and (= a_46 1) (= a_50 1)) 1 
                            (ite (and (= a_46 1) (= a_50 0)) 0 
                            (ite (and (= a_46 0) (= a_50 1)) 0
                            (ite (and (= a_46 -1) (= a_50 0)) 0 
                            (ite (and (= a_46 0) (= a_50 -1)) 0
                            (ite (and (= a_46 1) (= a_50 -1)) -1 
                            (ite (and (= a_46 -1) (= a_50 1)) -1 
                            2)))))))))))
(assert (= 5 (+ (+ a_47 a_51) 1)))
(assert (= 24 (- (- (+ (+ a_47 a_51) 1) (ite (>= (- (+ (+ a_47 a_51) 1) a_48) (- (+ (+ a_47 a_51) 1) a_52)) (- (+ (+ a_47 a_51) 1) a_48) (- (+ (+ a_47 a_51) 1) a_52))) (ite (= a_48 a_52) 1 0))))
(check-sat)
(get-model)
