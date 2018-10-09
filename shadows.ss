;; 6. Shadows

;; (define numbered?
;;   (lambda (aexp)
;;     (cond
;;      ((atom? aexp) (number? aexp))
;;      ((eq? (car (cdr aexp)) 'x)
;;       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
;;      ((eq? (car (cdr aexp)) '+)
;;       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
;;      ((eq? (car (cdr aexp)) '^)
;;       (and numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))))

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

(numbered? '(1 + 3))

(define x
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (+ n (x n (sub1 m)))))))

(define ^
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (x n (^ n (sub1 m)))))))

;; (define value
;;   (lambda (nexp)
;;     (cond
;;      ((atom? nexp) nexp)
;;      ((eq? (car (cdr nexp)) 'x)
;;       (x (value (car nexp) (value (car (cdr (cdr nexp)))))))
;;      ((eq? (car (cdr nexp)) '+)
;;       (+ (value (car nexp) (value (car (cdr (cdr nexp)))))))
;;      (else
;;       (^ (value (car nexp)) (value (car (cdr (cdr nexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(1st-sub-exp '(+ 2 3))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(2nd-sub-exp '(+ 2 3))

(define operator
  (lambda (aexp)
    (car aexp)))

(operator '(+ 2 3))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) '+)
      (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) 'x)
      (x (value (1st-sub-exp nexp)) (value 2nd-sub-exp nexp)))
     (else
      (^ (value (1st-sub-exp nexp)) (value 2nd-sub-exp nexp))))))

(value (^ 5 5))
(value (- 10 (* 2 (^ 2 2) 8)))

(define sero?
  (lambda (n)
    (null? n)))

(sero? '())
;; '()
;; '(())
;; '((()) (()()) (()()())) => (1 2 3)

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define plus
  (lambda (n m)
    (cond
     ((sero? m) n)
     (else (edd1 (+ n (zub1 m)))))))

(sero? '())
(sero? '(()))
(edd1 '())
(sero? (zub1 '(())))

(define one (edd1 '()))
(define two (edd1 one))
(plus one two)

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l))(lat? (cdr l)))
     (else #f))))

(lat? '(one two))  ;; #t
(lat? '((()) (()()) (()()())))  ;; #f
