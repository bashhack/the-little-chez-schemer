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

(define a-friend
  (lambda (x y)
    (display "x was: ")
    (display x)
    (newline)
    (display "y was: ")
    (display y)
    (newline)
    (null? y)))

;; (multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)
;; (multirember&co 'tuna '() a-friend)
;; (multirember&co 'tuna '(tuna) a-friend)
;; (multirember&co 'tuna '(space tuna) a-friend)

;; ...behind the scenes:
;; given an atom a and a list of atoms lat, the lat is iterated over -
;; a list ls1 is created where some predicate test? against the atom a is false,
;; al second list s2 is created where the predicate test? against the atom a is true,
;; finally the lists ls1 and ls2 are passed to a func f
;; (where f is a col or collector function, known as a continuation)
;;
;; Ex.
;; eq? '2 '(2 3 4) => '(3 4) '(2) => (col '(3 4) '(2))

(define new-friend
  (lambda (newlat seen)
    (col newlat (cons (car lat) seen))))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat (cons 'tuna seen))))

(multirember&co 'tuna '(strawberries tuna and swordfish) new-friend)
(multirember&co 'tuna '() new-friend)
(multirember&co 'tuna '(tuna) new-friend)
(multirember&co 'tuna '(space tuna) new-friend)
(multirember&co 'tuna '(and tuna) a-friend)

(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons 'and newlat) seen)))

(multirember&co 'tuna '(tuna) a-friend)
(multirember&co 'tuna '(tuna) new-friend)
(multirember&co 'tuna '(tuna) latest-friend)

(define last-friend
  (lambda (x y)
    (length x)))

(multirember&co 'tuna '(strawberries tuna and swordfish) last-friend)

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(multiinsertL 'bird 'mouse '(mouse cat mouse cat))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(multiinsertR 'bird 'mouse '(mouse cat mouse cat))

(define multiinsertLR
  ;; Inserts new to the left of oldL and to the right of oldR
  ;; in lat if oldL and oldR are different
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL)
      (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
     (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(multiinsertLR 'bird 'mouse 'cat '(mouse fish mouse dog cat mouse))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col '() 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R) (col (cons new (cons oldL newlat)) (add1 L) R))))
     ((eq? (car lat) oldR)
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R) (col (cons oldR (cons new newlat)) L (add1 R)))))
     (else (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat L R) (col (cons (car lat) newlat) L R)))))))

(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) list)

(define even?
  (lambda (n)
    (= (* (div n 2) 2) n)))

(define even?-alt
  (lambda (n)
    (= (remainder n 2) 0)))

(eq? (even?-alt 0) (even? 0))
(eq? (even?-alt 1) (even? 1))
(eq? (even?-alt 2) (even? 2))
(eq? (even?-alt 250832) (even? 250832))
(eq? (even?-alt 2508329) (even? 2508329))

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
       (else (evens-only* (cdr l)))))
     (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
