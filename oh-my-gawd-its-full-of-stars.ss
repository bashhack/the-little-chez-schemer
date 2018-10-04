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
       ((eq? (car l) a) (add1 (occur* a (cdr l)))))))))

(define subst*
  (lambda (new old l)))

(define member*
  (lambda (a l)))

(define leftmost
  (lambda (l)))
