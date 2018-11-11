;; 8. Lambda the Ultimate

(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     (else (cond
            ((test? (car l) a) (cdr l))
            (else (cons (car l) (rember-f test? a (cdr l)))))))))

(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? (car l) a) (cdr l))
     (else (cons (car l) (rember-f test? a (cdr l)))))))

(define even?
  (lambda (n)
     (eq? (remainder n 2) 0)))

(rember-f = 1 '(2 9 2 3 1))
(rember-f equal? '(1) '(2 9 2 3 1 (1)))
(rember-f eq? '1 '(2 9 2 3 1 (1) ))

(define eq?-c
 (lambda (a)
   (lambda (x)
     (eq? a x))))

(define eq?-salad (eq?-c 'salad))
(eq?-salad 'salad)
(eq?-salad 'pizza)

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define rember-eq? (rember-f eq?))

(rember-eq? 'tuna '(shrimp tuna whisky))
(rember-eq? 'tuna '(tuna salad is good))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (cons old (cdr lat))))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

(insertL 'bird 'mouse '(cat mouse dog))

(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) old) (cons new (cons old (cdr lat))))
       (else (cons (car lat) ((insertL-f test?) new old (cdr lat))))))))

((insertL-f eq?) 'dog 'bird '(bird))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) old) (cons old (cons new (cdr lat))))
       (else (cons (car lat) ((insertR-f test?) new old (cdr lat))))))))

((insertR-f eq?) 'dog 'bird '(bird))

(define seqL
  (lambda (new old lat)
    (cons new (cons old lat))))

(define seqR
  (lambda (new old lat)
    (cons old (cons new lat))))

(define insert-g
  (lambda (seq)
    (lambda (new old lat)
      (cond
       ((null? lat) '())
       ((eq? (car lat) old) (seq new old (cdr lat)))
       (else (cons (car lat) ((insert-g seq) new old (cdr lat))))))))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

;; ...using the seqL/seqR func definitions in insertL/insertR - better!
(define insertL
  (insert-g
   (lambda (new old lat)
     (cons new (cons old lat)))))

(define insertR
  (insert-g
   (lambda (new old lat)
     (cons old (cons new lat)))))

(insertR 'dog 'bird '(bird))
(insertL 'dog 'bird '(bird))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

(subst 'mouse 'bird '(bird mouse bird))

(define seqS
  (lambda (new old lat)
    (cons new lat)))

(define subst (insert-g seqS))

(subst 'bird 'mouse '(bird mouse bird))

(define seqrem
  (lambda (new old lat)
    lat))

;; using insert-g to define rember
(define yyy
  (lambda (a lat)
    ((insert-g seqrem) #f a lat)))

(yyy 'sausage '(pizza with sausage and bacon))

(define operator
  (lambda (aexp)
    (car aexp)))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) '+)
      (+ (value (1st-sub-exp nexp))
         (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) '*)
      (* (value (1st-sub-exp nexp))
         (value (2nd-sub-exp nexp))))
     (else
      (expt (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp)))))))

(value '(+ 1 2))
(value '(* 2 2))
(value '(expt 2 6))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? '+ x) +)
     ((eq? '* x) *)
     (else expt))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else
      ((atom-to-function (operator nexp))
       (value (1st-sub-exp nexp))
       (value (2nd-sub-exp nexp)))))))

(value '(+ 1 2))
(value '(* 2 2))
(value '(expt 2 6))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

(multirember 'bird '(cat mouse bird mouse bird cat bird))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
       (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

((multirember-f eq?) 'bird '(cat mouse bird mouse bird cat bird))

(define multirember-eq? (multirember-f eq?))

(multirember-eq? 'bird '(cat mouse bird mouse bird cat bird))

(define eq?-c
  (lambda (x)
    (lambda (y)
      (eq? x y))))

;; ...what if we combine the test func with the value to test against?
(define eq?-bird
  (eq?-c 'bird))

(define multiremberT
  (lambda (test?)
    (lambda (lat)
      (cond
       ((null? lat) '())
       ((test? (car lat)) ((multiremberT test?) (cdr lat)))
       (else (cons (car lat) ((multiremberT test?) (cdr lat))))))))

((multiremberT eq?-bird) '(cat mouse bird mouse bird cat bird))

(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat) (col '() '()))
     ((eq? (car lat) a)
      (multirember&co a (cdr lat)
                      (lambda (newlat seen)
                        (display "newlat: ")
                        (newline)
                        (display newlat)
                        (newline)
                        (display "seen: ")
                        (newline)
                        (display newlat)
                        (newline)
                        (col newlat (cons (car lat) seen)))))
     (else
      (multirember&co a (cdr lat)
                      (lambda (newlat seen)
                        (col (cons (car lat) newlat) seen)))))))

(define 2nd-of-pair-empty-list
  (lambda (x y)
    (display "x was: ")
    (display x)
    (newline)
    (display "y was: ")
    (display y)
    (newline)
    (null? y)))

(multirember&co 'tuna '(strawberries tuna and swordfish) 2nd-of-pair-empty-list)
(multirember&co 'tuna '() 2nd-of-pair-empty-list)
(multirember&co 'tuna '(tuna) 2nd-of-pair-empty-list)
(multirember&co 'tuna '(space tuna) 2nd-of-pair-empty-list)
