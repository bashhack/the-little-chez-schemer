(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define ozero?
  (lambda (n)
    (= n 0)))

(define o+
  (lambda (n m)
    (cond
     ((ozero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

;;          4 + 5 =>
;;         (add1 (+ 4 (sub1 5)))
;;         (add1 (+ 4 4)) => ...now recurse, replacing (+ 4 4) expression with the else condition of the fn (o+/+) `(add1 (+ n (sub1 m)))`
;;         (add1 (add1 (+ 4 3))) =>
;;         (add1 (add1 (add1 (+ 4 2)))) =>
;;         (add1 (add1 (add1 (add1 (+ 4 1))))) =>
;;         (add1 (add1 (add1 (add1 (add1 (+ 4 0)))))) =>
;;         (add1 (add1 (add1 (add1 (add1 4))))) =>
;;         (add1 (add1 (add1 (add1 5)))) =>
;;         (add1 (add1 (add1 6))) =>
;;         (add1 (add1 7)) =>
;;         (add1 8) =>
;;         9


(define o-
  (lambda (n m)
    (cond
     ((ozero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

;;          4 - 5 =>
;;          (sub1 (- 4 (sub1 5)))
;;          (sub1 (- 4 4)) => ...now recurse, replacing (- 4 4) expression with the else condition of the fn (o-/-) `(sub1 (- n (sub1 m)))`
;;          (sub1 (sub1 (- 4 3))) =>
;;          (sub1 (sub1 (sub1 (- 4 2)))) =>
;;          (sub1 (sub1 (sub1 (sub1 (-4 1))))) =>
;;          (sub1 (sub1 (sub1 (sub1 (sub1 (- 4 0)))))) =>
;;          (sub1 (sub1 (sub1 (sub1 (sub1 4))))) =>
;;          (sub1 (sub1 (sub1 (sub1 3)))) =>
;;          (sub1 (sub1 (sub1 2))) =>
;;          (sub1 (sub1 1)) =>
;;          (sub1 0) =>
;;          -1
