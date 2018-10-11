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
(makeset '(dog cat dog bird dog dog))
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

;; Compared to the first definition of `makeset`, this function will
;; `cons` the first atom of the `lat` - or remember it - before recurring.
;; The resulting effect of this compared to the first is that a repeated
;; atom's first instance will be kept while all subsequent instances will
;; be removed.
;;
;; In practice, this means that the definition of `makeset` using
;; `multirember`, given a `lat` '(dog cat dog bird dog) will return:
;; '(dog cat bird)
;; Whereas, the earlier definition of `makeset` given the same `lat`
;; would return:
;; '(cat bird dog)

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else (cond
            ((member? (car set1) set2)
             (subset? (cdr set1) set2))
            (else #f))))))

;; Looking at the above, the pattern of a cond into an else
;; with another cond can be refactored into:

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     ((member? (car set1) set2)
      (subset? (cdr set1) set2))
     (else #f))))

;; Another refactor using `(and...)` to get rid of the trailing else,
;; since a call to `and` will return a boolean value, anyway

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else (and (member? (car set1) set2)
                (subset? (cdr set1) set2))))))

(subset? '(5 hamburgers 2 pieces of fried chicken and light duckling wings) '(5 chicken wings))

(subset?  '(5 chicken wings) '(5 hamburgers 2 pieces of fried chicken and light duckling wings))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(eqset? '(5 chicken wings 4 apple pies) '(5 chicken wings 4 apple pie))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(intersect? '(chicken in wings) '(5 ducks in a row))

(define intersect
  (lambda (set1 set2)
    ((null? set1) '())
    ...))
(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))
