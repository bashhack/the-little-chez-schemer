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

(define o-
  (lambda (n m)
    (cond
     ((ozero? m) n)
     (else (sub1 (o- n (sub1 m)))))))
