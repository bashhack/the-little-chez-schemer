;; 4. Number Games

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

(o+ 4 5)
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

(o- 4 5)
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

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (+ (car tup) (addtup (cdr tup)))))))

(define x
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (+ n (x n (sub1 m)))))))

(x 4 5)
;;          4 * 5 =>
;;          (+ 4 (x 4 (sub1 5)))
;;          (+ 4 (x 4 4))
;;          (+ 4 (+ 4 (x 4 3)))
;;          (+ 4 (+ 4 (+ 4 (x 4 2))))
;;          (+ 4 (+ 4 (+ 4 (+ 4 (x 4 1)))))
;;          (+ 4 (+ 4 (+ 4 (+ 4 (+ 4 (x 4 0))))))
;;          (+ 4 (+ 4 (+ 4 (+ 4 (+ 4 0)))))
;;          (+ 4 (+ 4 (+ 4 (+ 4 4))))
;;          (+ 4 (+ 4 (+ 4 8)))
;;          (+ 4 (+ 4 12))
;;          (+ 4 16)
;;          20

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '() '(1 2 3))  ;; (1 2 3)
(tup+ '(4 6) '(1 2 3))  ;; (5 8 3)

(define >
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (> (sub1 n) (sub1 m))))))

(> 3 3)
(> 5 0)

(define <
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (< (sub1 n) (sub1 m))))))

(< 3 3)
(< 5 0)

;; Original implementation...
;; (define =
;;   (lambda (n m)
;;     (cond
;;      ((zero? m) (zero? n))
;;      ((zero? n) #f)
;;      (else (= (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond
     ((or (> n m) (< n m)) #f)
     (else #t))))

(define ^
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (x n (^ n (sub1 m)))))))

(^ 5 3)  ;; 125

(define quotient
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (quotient (- n m) m))))))

(quotient 15 3)  ;; 5

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

(length '(1 2 3 4 5 6))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(pick 4 '(lasagna spaghetti ravioli macaroni meatball))  ;; macaroni

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 3 '(hotdogs with hot mustard))  ;; (hotdogs with mustard)

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((number? (car lat))
            (no-nums (cdr lat)))
            (else (cons (car lat) (no-nums (cdr lat)))))))))

(no-nums '(5 pears 6 prunes 9 dates))  ;; (pears prunes dates)

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
            (else (all-nums (cdr lat))))))))

(all-nums '(5 pears 6 prunes 9 dates))  ;; (5 6 9)

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2)))))

(eqan? 'apple 'apple)  ;; #t
(eqan? 5 43)  ;; #f

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else (cond
            ((eq? (car lat) a)
             (add1 (occur a (cdr lat))))
            (else (occur a (cdr lat))))))))

(occur 5 '(5 pears 6 prunes 5 dates))  ;; 2

(define one?
  (lambda (n)
    (= n 1)))

(one? 5)  ;; #f

(define pick-opt
  (lambda (n lat)
    (cond
     ((one? n) (car lat))
     (else (pick-opt (sub1 n) (cdr lat))))))

(pick-opt 4 '(lasagna spaghetti ravioli macaroni meatball))  ;; macaroni

(define rempick-opt
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat) (rempick-opt (sub1 n) (cdr lat)))))))

(rempick-opt 3 '(hotdogs with hot mustard))  ;; (hotdogs with mustard)
