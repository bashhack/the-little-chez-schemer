;; 2. Again, and Again...

;; NOTE TO SELF: In debugging, I added the `and` check in the recursive logic to prevent unhandled errors
(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((and (pair? l) (atom? (car l))) (lat? (cdr l)))
     (else #f))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))
