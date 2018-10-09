;; 7. Friends and Relations

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     ((eq? (car lat) a) #t)
     (else (member? a (cdr lat))))))

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(set? '(dog dog))
(set? '(dog cat bird))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
     (else (cons (car lat) (makeset (cdr lat)))))))

(makeset '(bird cat dog))
;; (cons 'bird (cons 'cat (makeset '(dog))))
;; (cons 'bird (cons 'cat (cons 'dog (makeset '()))))
;; (cons 'bird (cons 'cat (cons 'dog '())))

(makeset '(dog bird cat dog))
(makeset '(bird cat dog))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) a) (multirember a (cdr lat)))
            (else (cons (car lat) (multirember a (cdr lat)))))))))


(multirember 'dog '(dog dog bird cat dog))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cons (car lat) (multirember (car lat) (cdr lat)))))))

(makeset '(dog cat dog bird dog dog))
