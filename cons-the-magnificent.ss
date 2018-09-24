;; 3. Cons the Magnificient

;; NOTE: Original implementation, rewritten below in simpler form
;; (define rember
;;   (lambda (a lat)
;;     (cond
;;      ((null? lat?) '())
;;      (else (cond
;;             ((eq? (car lat) a) (cdr lat))
;;             (else (cons (car lat)
;;                         (rember a (cdr lat)))))))))

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) ;; typical element
                  (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) old) (cons old (cons new (cdr lat))))
            (else (cons (car lat) (insertR new old (cdr lat)))))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) old) (cons new lat))
            (else (cons (car lat) (insertL new old (cdr lat)))))))))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) old) (cons new (cdr lat)))
            (else (cons (car lat) (subst new old (cdr lat)))))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
            (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) a) (multirember a (cdr lat)))
            (else (cons (car lat) (multirember a (cdr lat)))))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
            (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) old) (cons new (multiinsertL new old (cdr lat))))
            (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
            (else (cons (car lat) (multisubst new old (cdr lat)))))))))

(insertR 'topping 'fudge '(ice cream with fudge for dessert))  ;; (ice cream with fudge topping for dessert)
(insertL 'topping 'fudge '(ice cream with fudge for dessert))  ;; (ice cream with topping fudge for dessert)
(subst 'topping 'fudge '(ice cream with fudge for dessert))  ;; (ice cream with topping for dessert)
(subst2 'topping 'cat 'fudge '(ice cream with fudge for dessert))  ;; (ice cream with topping for dessert)
(subst2 'topping 'cat 'cat '(ice cream with fudge for dessert))  ;; (ice cream with fudge for dessert)
(multirember 'topping '(topping topping))  ;; ()
(multiinsertR 'topping 'fudge '(fudge ice cream with fudge for dessert))  ;; (fudge topping ice cream with fudge topping for dessert)
(multiinsertL 'topping 'fudge '(fudge ice cream with fudge for dessert))  ;; (topping fudge ice cream with topping fudge for dessert)
(multisubst 'topping 'fudge '(fudge ice cream with fudge for dessert))  ;; topping ice cream with topping for dessert
