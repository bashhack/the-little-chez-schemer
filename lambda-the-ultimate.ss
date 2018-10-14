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
     (eq? x a))))

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


