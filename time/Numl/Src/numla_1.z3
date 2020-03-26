(declare-const a_84 Int)
(declare-const a_80 Int)
(declare-const a_83 Int)
(declare-const a_82 Int)
(declare-const a_79 Int)
(declare-const a_78 Int)
(assert (<= 10 (+ (ite (>= a_79 a_83) a_79 a_83) (ite (= a_78 0) 0
                           (ite (= a_82 0) 0 
                           (ite (and (= a_78 1) (= a_82 -1)) 0 
                           (ite (and (= a_78 -1) (= a_82 1)) 0 
                           1)))))))
(assert (>= 20 (- (- (+ (ite (>= a_79 a_83) a_79 a_83) (ite (= a_78 0) 0
                           (ite (= a_82 0) 0 
                           (ite (and (= a_78 1) (= a_82 -1)) 0 
                           (ite (and (= a_78 -1) (= a_82 1)) 0 
                           1))))) (ite (>= (- a_79 a_80) (- a_83 a_84)) (- a_79 a_80) (- a_83 a_84))) (ite (= (- a_79 a_80) (- a_83 a_84)) 1 0))))
(check-sat)
(get-model)
