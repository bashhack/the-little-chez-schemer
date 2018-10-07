;; 5. *Oh My Gawd*: It's Full of Stars

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (rember* a (cdr l)))
       (else (cons (car l) (rember* a (cdr l))))))
     (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(rember* 'cup '((coffee cup cup) cup ((tea) cup) (and (hick)) cup))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) old) (cons old (cons new (cdr lat))))
            (else (cons (car lat) (insertR new old (cdr lat)))))))))

(insertR 'ubuntu 'linux '(The linux distribution is useful))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) old) (cons new lat))
            (else (cons (car lat) (insertL new old (cdr lat)))))))))

(insertL 'ubuntu 'linux '(The linux distribution is useful))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
     (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(insertR* 'ubuntu 'linux '((linux distribution) apple linux (apple ((linux distribution)))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
       (else (cons (car l) (insertL* new old (cdr l))))))
     (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(insertL* 'ubuntu 'linux '((linux distribution) apple linux (apple ((linux distribution)))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons new (subst* new old (cdr l))))
       (else (cons (car l) (subst* new old (cdr l))))))
     (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(subst* 'birds 'cats '(We love (cats (cats)) cats (cats)))

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (or (eq? (car l) a) (member* a (cdr l))))
     (else (or (member*  a (car l)) (member* a (cdr l)))))))

(member* 'dog '(cats cats cat dog))
(member* 'chips '((potato) (chips ((with) fish) (chips))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(leftmost '(((hot) (tuna (and))) cheese))
(leftmost '(((() four)) 17 (seventeen)))

(define eqan?
 (lambda (a1 a2)
   (cond
    ((and (number? a1) (number? a2)) (= a1 a2))
    ((or (number? a1) (number? a2)) #f)
    (else (eq? a1 a2)))))

(eqan? 15 3)
(eqan? 'dog 'dog)

;; (define eqlist?
;;   (lambda (l1 l2)
;;     (cond
;;      ((and (null? l1) (null? l2)) #t)
;;      ((or (null? l1) (null? l2)) #f)
;;      ((and (atom? (car l1)) (atom? (car l2)))
;;       (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
;;      ((or (atom? (car l1)) (atom? (car l2))) #f)
;;      (else
;;       (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else (eqlist? s1 s2)))))

(equal? 15 15)
(equal? 15 '())
(equal? '((bird) cat (dog (dog))) '((bird) cat (dog (dog))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))))))

(eqlist? '(cat dog dog) '(cat dog dog))
(eqlist? '() '())
(eqlist? '(15) '(5 3))

;; (define rember
;;   (lambda (s l)
;;     (cond
;;      ((null? l) '())
;;      ((atom? (car l))
;;       (cond
;;        ((equal? (car l) s) (cdr l))
;;        (else (cons (car l) (rember s (cdr l))))))
;;      (else (cond
;;             ((equal? (car l) s) (cdr l))
;;             (else (cons (car l) (rember s (cdr l)))))))))

;; (define rember
;;   (lambda (s l)
;;     (cond
;;      ((null? l) '())
;;      (else (cond
;;             ((equal? (car l) s) (cdr l))
;;             (else (cons (car l) (rember s (cdr l)))))))))

(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? (car l) s) (cdr l))
     (else (cons (car l) (rember s (cdr l)))))))

(rember '(dog) '((dog) dog))
