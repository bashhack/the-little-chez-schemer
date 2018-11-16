;; 10. What is the Value of All of This?

;; examples of entries...
;; ((cities states abbrev)
 ;; ("San Francisco" "California" "CA"))

;; ((num-of-members band-name)
 ;; (2 "Simon and Garfunkel"))

;; a mapping of a list of names to a list values,
;; where the first list is a set and both lists are of equal length

(define build
  ;; builds a list of s-expressions
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define new-entry build)

(new-entry '(appetizer entree beverage) '(pate boeuf vin))

(define first
  (lambda (pair)
    (car pair)))

(define second
  (lambda (pair)
    (car (cdr pair))))

(define third
  (lambda (pair)
    (car (cdr (cdr s)))))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry) entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f names))
     ((eq? (car names) name)
      (car values))
     (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

(lookup-in-entry 'appetizer '((appetizer entree beverage) (food tastes good)) '())

;; tables are lists of entries
;; (((cities teams)
  ;; ("San Antonio" "49ers"))
 ;; ((make model)
  ;; ("Honda" "Accord")))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else
      (lookup-in-entry
       name
       (car table)
       (lambda (name)
         (lookup-in-table name (cdr table) table-f)))))))

;; Types (Actions):
;; * const
;; * quote
;; * identifier
;; * lambda
;; * lambda
;; * cond
;; * application

;; we previously worked with a definition of value where we used an atom-to-function
;; (define atom-to-function
;;   (lambda (x)
;;     (cond
;;      ((eq? '+ x) +)
;;      ((eq? '* x) *)
;;      (else expt))))

;; (define value
;;   (lambda (nexp)
;;     (cond
;;      ((atom? nexp) nexp)
;;      (else
;;       ((atom-to-function (operator nexp))
;;        (value (1st-sub-exp nexp))
;;        (value (2nd-sub-exp nexp)))))))

;; what if we have an atom-to-action

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e 'cons) *const)
     ((eq? e 'car) *const)
     ((eq? e 'cdr) *const)
     ((eq? e 'null?) *const)
     ((eq? e 'eq?) *const)
     ((eq? e 'atom?) *const)
     ((eq? e 'zero?) *const)
     ((eq? e 'add1) *const)
     ((eq? e 'sub1) *const)
     ((eq? e 'number?) *const)
     (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) 'lambda) *lambda)
       ((eq? (car e) 'cond) *cond)
       (else *application)))
     (else *application))))

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

;; and now, to create the interpreter
;; approximating our default Scheme (and Lisp) `eval'
(define value
  (lambda (e)
    (meaning e '())))

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build 'primitive e)))))

(define *quote
  (lambda (e table)
    (test-of e)))

(define test-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '())))

(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines)) table)
      (meaning (answer-of (car lines)) table))
     (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x 'else))
     (else #f))))

(define question-of first)

(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

;; (*cond
;;  (cond
;;   (coffee klatsch)
;;   (else party))
;;  (((coffee) '(#t))
;;   ((klatsch party) '(5 (6)))))

(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else
      (cons (meaning (car args) table)
            (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)

;; there are two representations of functions, primitive and non-primitive
;; in the first, there is (primitive primitive-name)
;; and in the second, there is (non-primitive (table formals body))
(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define apply
  (lambda (fun vals)
    (cond
     ((primitive? fun)
      (apply-primitive (second fun) vals))
     ((non-primitive? fun)
      (apply-closure
       (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name 'cons)
      (cons (first vals) (second vals)))
     ((eq? name 'car)
      (car (first vals)))
     ((eq? name 'cdr)
      (cdr (first vals)))
     ((eq? name 'null?)
      (null? (first vals)))
     ((eq? name 'eq?)
      (eq? (first vals) (second vals)))
     ((eq? name 'atom?)
      (:atom? (first vals)))
     ((eq? name 'zero?)
      (zero? (first vals)))
     ((eq? name 'add1)
      (add1 (first vals)))
     ((eq? name 'sub1)
      (sub1 (first vals)))
     ((eq? name 'number?)
      (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) 'primitive) #t)
     ((eq? (car x) 'non-primitive) #t)
     (else #f))))

;; 'applying a non-primitive function - a closure - to a list of values
;; is the same as finding the meaning of the closure's body with its
;; table extended by an entry of the form
;;   (formals values)
;; where in this entry, formals is the formals of the closure
;; and values is the result of evlis'

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry (formals-of closure) vals)
              (table-of closure)))))

(apply-closure ((((u v w)
                  (1 2 3))
                 ((x y z)
                  (4 5 6)))
                (x y)
                (cons z x))
               ((a b c) (d e f)))

;; closure
;; ((((u v w)
;;    (1 2 3))
;;   ((x y z)
;;    (4 5 6)))
;;  (x y)
;;  (cons z x))

;; vals
;; ((a b c) (d e f))

;; (1)
(meaning (body-of closure) ;; body-of (get third)
         (extend-table
          ;; substitute vals, formals-of (get second)
          (new-entry (formals-of closure) vals)
          ;; table-of (get first)
          (table-of closure)))

;; (2) substitute values
(meaning (cons z x)
         (extend-table
          (new-entry (x y) ((a b c) (d e f)))
          (((u v w)
            (1 2 3))
           ((x y z)
            (4 5 6)))))

;; (3) evaluate new-entry (build)
(meaning (cons z x)
         (extend-table
          ((x y) ((a b c) (d e f)))
          (((u v w)
            (1 2 3))
           ((x y z)
            (4 5 6)))))

;; (4) evaluate extend-table (cons)
(meaning (cons z x)
         (((x y)
           ((a b c) (d e f)))
          ((u v w)
           (1 2 3))
          ((x y z)
           (4 5 6))))

;; (5) evaluate meaning
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(meaning
 ;; e (expression)
 (cons z x)
 ;; table
 (((x y)
   ((a b c) (d e f)))
  ((u v w)
   (1 2 3))
  ((x y z)
   (4 5 6))))

;; (5) cont.
(lambda ((cons z x) (((x y) ((a b c) (d e f))) ((u v w) (1 2 3)) ((x y z) (4 5 6))))
  ((expression-to-action (cons z x))
   (cons z x)
   (((x y)
     ((a b c) (d e f)))
    ((u v w)
     (1 2 3))
    ((x y z)
     (4 5 6)))))

;; (6) evaluate expression-to-action
(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

(lambda (cons z x)
  (cond
   ((atom? (cons z x)) (atom-to-action (cons z x)))
   (else (list-to-action (cons z x)))))

(lambda (cons z x)
  (cond
   (#f (atom-to-action (cons z x)))
   (else (list-to-action (cons z x)))))

(lambda (cons z x)
  (cond
   (else (list-to-action (cons z x)))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) 'lambda) *lambda)
       ((eq? (car e) 'cond) *cond)
       (else *application)))
     (else *application))))

(lambda (cons z x)
  (list-to-action (cons z x)))

(lambda (cons z x)
  (cond
   ((atom? (car (cons z x))) ;; #t
    (cond
     ((eq? (car (cons z x)) 'quote) *quote) ;; #f
     ((eq? (car (cons z x)) 'lambda) *lambda) ;; #f
     ((eq? (car (cons z x)) 'cond) *cond) ;; #f
     (else *application))) ;; returns *application
   (else *application)))

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

;; (7) replace (expression-to-action (cons z x)) with *application
(lambda ((cons z x) (((x y) ((a b c) (d e f))) ((u v w) (1 2 3)) ((x y z) (4 5 6))))
  (*application
   (cons z x)
   (((x y)
     ((a b c) (d e f)))
    ((u v w)
     (1 2 3))
    ((x y z)
     (4 5 6)))))

;; (8) evaluate *application
(lambda ((cons z x) (((x y)
                      ((a b c) (d e f)))
                     ((u v w)
                      (1 2 3))
                     ((x y z)
                      (4 5 6))))
  (apply
   (meaning (function-of (cons z x))
            (((x y)
              ((a b c) (d e f)))
             ((u v w)
              (1 2 3))
             ((x y z)
              (4 5 6))))
   (evlis (arguments-of (cons z x))
          (((x y)
            ((a b c) (d e f)))
           ((u v w)
            (1 2 3))
           ((x y z)
            (4 5 6))))))

;; (9) evaluate function-of (car) and arguments-of (cdr)
(lambda ((cons z x) (((x y)
                      ((a b c) (d e f)))
                     ((u v w)
                      (1 2 3))
                     ((x y z)
                      (4 5 6))))
  (apply
   (meaning cons
            (((x y)
              ((a b c) (d e f)))
             ((u v w)
              (1 2 3))
             ((x y z)
              (4 5 6))))
   (evlis (z x)
          (((x y)
            ((a b c) (d e f)))
           ((u v w)
            (1 2 3))
           ((x y z)
            (4 5 6))))))

;; (10) evaluate meaning of (meaning cons (((x y) ((a b c) (d e f))) ((u v w) (1 2 3)) ((x y z) (4 5 6))))
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(lambda (car (((x y)
               ((a b c) (d e f)))
              ((u v w)
               (1 2 3))
              ((x y z)
               (4 5 6))))
  ((expression-to-action cons) cons (((x y)
                                    ((a b c) (d e f)))
                                   ((u v w)
                                    (1 2 3))
                                   ((x y z)
                                    (4 5 6)))))

;; (11) evaluate expression-to-action
(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

(lambda (cons)
  (cond
   ((atom? cons) (atom-to-action cons))
   (else (list-to-action cons))))

(lambda (cons)
  (cond
   (#t (atom-to-action cons))
   (else (list-to-action cons))))

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e 'cons) *const)
     ((eq? e 'car) *const)
     ((eq? e 'cdr) *const)
     ((eq? e 'null?) *const)
     ((eq? e 'eq?) *const)
     ((eq? e 'atom?) *const)
     ((eq? e 'zero?) *const)
     ((eq? e 'add1) *const)
     ((eq? e 'sub1) *const)
     ((eq? e 'number?) *const)
     (else *identifier))))

(lambda (cons)
  (cond
   ((number? cons) *const)
   ((eq? cons #t) *const)
   ((eq? cons #f) *const)
   ((eq? cons 'cons) *const) ;; #t, returns *const
   ((eq? cons 'car) *const)
   ((eq? cons 'cdr) *const)
   ((eq? cons 'null?) *const)
   ((eq? cons 'eq?) *const)
   ((eq? cons 'atom?) *const)
   ((eq? cons 'zero?) *const)
   ((eq? cons 'add1) *const)
   ((eq? cons 'sub1) *const)
   ((eq? cons 'number?) *const)
   (else *identifier)))

;; (12) (expression-to-action cons) replace with its value, *const
(lambda (cons (((x y)
               ((a b c) (d e f)))
              ((u v w)
               (1 2 3))
              ((x y z)
               (4 5 6))))
  (*const cons (((x y)
                 ((a b c) (d e f)))
                ((u v w)
                 (1 2 3))
                ((x y z)
                 (4 5 6)))))

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build 'primitive e)))))

;; (13) evaluate *const, returning ('primitive cons)
(lambda (cons (((x y)
                ((a b c) (d e f)))
               ((u v w)
                (1 2 3))
               ((x y z)
                (4 5 6))))
  (cond
   ((number? cons) cons) ;; #f
   ((eq? cons #t) #t) ;; #f
   ((eq? cons #f) #f) ;; #f
   (else (build 'primitive cons)))) ;; #t, return ('primitive cons)

;; (14) put the value ('primitive cons) in place of (meaning cons (((x y) ((a b c) (d e f))) ((u v w) (1 2 3)) ((x y z) (4 5 6)))) ... next, we need to evaluate evlis
(lambda ((cons z x) (((x y)
                      ((a b c) (d e f)))
                     ((u v w)
                      (1 2 3))
                     ((x y z)
                      (4 5 6))))
  (apply
   ('primitive cons) ;; evaluation of call to (meaning (e table)...)
   (evlis (z x)
          (((x y)
            ((a b c) (d e f)))
           ((u v w)
            (1 2 3))
           ((x y z)
            (4 5 6))))))

(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else
      (cons (meaning (car args) table)
            (evlis (cdr args) table))))))

;; (15) evaluate evlis with values substituted
(lambda (
         ;; args
         (z x)
         ;; table
         (((x y)
           ((a b c) (d e f)))
          ((u v w)
           (1 2 3))
          ((x y z)
           (4 5 6))))
  (cond
   ((null? (z x)) '())
   (else (cons (meaning z (((x y)
                            ((a b c) (d e f)))
                           ((u v w)
                            (1 2 3))
                           ((x y z)
                            (4 5 6))))
               (evlis (x) (((x y)
                            ((a b c) (d e f)))
                           ((u v w)
                            (1 2 3))
                           ((x y z)
                            (4 5 6))))))))

(lambda ((z x) (((x y) ((a b c) (d e f))) ((u v w) (1 2 3)) ((x y z) (4 5 6))))
  (cond
   ((null? (z x)) '()) ;; evaluates to #f
   (else (cons (meaning z (((x y)
                            ((a b c) (d e f)))
                           ((u v w)
                            (1 2 3))
                           ((x y z)
                            (4 5 6))))
               (evlis (x) (((x y)
                            ((a b c) (d e f)))
                           ((u v w)
                            (1 2 3))
                           ((x y z)
                            (4 5 6))))))))

;; the args were not none, so the else branch is evaluated, meaning and evlis are evaluated in turn
(lambda ((z x) (((x y) ((a b c) (d e f))) ((u v w) (1 2 3)) ((x y z) (4 5 6))))
  (else (cons (meaning z (((x y)
                           ((a b c) (d e f)))
                          ((u v w)
                           (1 2 3))
                          ((x y z)
                           (4 5 6))))
              (evlis (x) (((x y)
                           ((a b c) (d e f)))
                          ((u v w)
                           (1 2 3))
                          ((x y z)
                           (4 5 6)))))))

;; first, meaning
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(lambda (z (((x y)
             ((a b c) (d e f)))
            ((u v w)
             (1 2 3))
            ((x y z)
             (4 5 6))))
  ((expression-to-action z) z table))

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

(lambda (z)
  (cond
   ((atom? z) (atom-to-action z))
   (else (list-to-action z))))

(lambda (z)
  (cond
   (#t (atom-to-action z))))

(atom-to-action z)

(lambda (z)
  (cond
    ((number? z) *const)
    ((eq? z #t) *const)
    ((eq? z #f) *const)
    ((eq? z 'cons) *const)
    ((eq? z 'car) *const)
    ((eq? z 'cdr) *const)
    ((eq? z 'null?) *const)
    ((eq? z 'eq?) *const)
    ((eq? z 'atom?) *const)
    ((eq? z 'zero?) *const)
    ((eq? z 'add1) *const)
    ((eq? z 'sub1) *const)
    ((eq? z 'number?) *const)
    (else *identifier))) ;; #t, (atom-to-action z) returns *identifier

;; (16) we continue evaluating (meaning z (((x y) ((a b c) (d e f))) ((u v w) (1 2 3)) ((x y z) (4 5 6)))) having determined our return value from (expression-to-action z) as *identifier
(lambda (z (((x y)
             ((a b c) (d e f)))
            ((u v w)
             (1 2 3))
            ((x y z)
             (4 5 6))))
  (*identifier z table))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(lambda (z (((x y)
             ((a b c) (d e f)))
            ((u v w)
             (1 2 3))
            ((x y z)
             (4 5 6))))
  (lookup-in-table
   ;; name
   z
   ;; table
   (((x y)
     ((a b c) (d e f)))
    ((u v w)
     (1 2 3))
    ((x y z)
     (4 5 6)))
   ;; initial-table
   (lambda (name)
     (car '()))))

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else
      (lookup-in-entry
       name
       (car table)
       (lambda (name)
         (lookup-in-table name (cdr table) table-f)))))))

(lambda (z
         (((x y)
           ((a b c) (d e f)))
          ((u v w)
           (1 2 3))
          ((x y z)
           (4 5 6)))
         (lambda (name)
           (car '())))
  (cond
   ((null? (((x y)
             ((a b c) (d e f)))
            ((u v w)
             (1 2 3))
            ((x y z)
             (4 5 6)))) ((lambda (name)
                           (car '())) z))
   (else
    (lookup-in-entry
     z
     ((x y)
      ((a b c) (d e f)))
     (lambda (name)
       (lookup-in-table z (((u v w)
                            (1 2 3))
                           ((x y z)
                            (4 5 6))) ((lambda (name)
                                         (car '())))))))))

;; (17) lookup-in-entry is called, and failing to find an entry, we recursively call
;; (lookup-in-table z (((u v w) (1 2 3)) ((x y z) (4 5 6))) ((lambda (name) (car '()))))
;; we do eventually get a value of 6, which we substitute for (meaning z (((x y) ((a b c) (d e f))) ((u v w) (1 2 3)) ((x y z) (4 5 6))))

(lambda ((z x) (((x y) ((a b c) (d e f))) ((u v w) (1 2 3)) ((x y z) (4 5 6))))
  (else (cons 6 ;; our return value from meaning...
              (evlis (x) (((x y)
                           ((a b c) (d e f)))
                          ((u v w)
                           (1 2 3))
                          ((x y z)
                           (4 5 6)))))))

;; (18) second, evlis
(evlis (x) (((x y)
             ((a b c) (d e f)))
            ((u v w)
             (1 2 3))
            ((x y z)
             (4 5 6))))

;; diving into evlis, we get back (a b c)
(lambda ((z x) (((x y) ((a b c) (d e f))) ((u v w) (1 2 3)) ((x y z) (4 5 6))))
  (else (cons 6 (a b c))))

(lambda ((z x) (((x y) ((a b c) (d e f))) ((u v w) (1 2 3)) ((x y z) (4 5 6))))
  (else (6 (a b c))))

;; (19) we can now continue to evaluate *application
(lambda ((cons z x) (((x y)
                      ((a b c) (d e f)))
                     ((u v w)
                      (1 2 3))
                     ((x y z)
                      (4 5 6))))
  (apply ('primitive cons) (6 (a b c))))

(define apply
  (lambda (fun vals)
    (cond
     ((primitive? fun)
      (apply-primitive (second fun) vals))
     ((non-primitive? fun)
      (apply-closure
       (second fun) vals)))))

(lambda (('primitive cons) (6 (a b c)))
  (cond
   ((primitive? ('primitive cons))
    (apply-primitive cons (6 (a b c))))
   ((non-primitive? ('primitive cons))
    (apply-closure cons (6 (a b c))))))

(lambda (('primitive cons) (6 (a b c)))
  (cond
   ((primitive? ('primitive cons)) ;; #t
    (apply-primitive cons (6 (a b c))))
   ((non-primitive? ('primitive cons))
    (apply-closure cons (6 (a b c))))))

;; (20) evaluation of apply-primitive
(apply-primitive cons (6 (a b c)))

(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name 'cons)
      (cons (first vals) (second vals)))
     ((eq? name 'car)
      (car (first vals)))
     ((eq? name 'cdr)
      (cdr (first vals)))
     ((eq? name 'null?)
      (null? (first vals)))
     ((eq? name 'eq?)
      (eq? (first vals) (second vals)))
     ((eq? name 'atom?)
      (:atom? (first vals)))
     ((eq? name 'zero?)
      (zero? (first vals)))
     ((eq? name 'add1)
      (add1 (first vals)))
     ((eq? name 'sub1)
      (sub1 (first vals)))
     ((eq? name 'number?)
      (number? (first vals))))))

(lambda (cons (6 (a b c)))
  (cond
   ((eq? cons 'cons)
    (cons 6 (a b c)))
   ((eq? cons 'car)
    (car 6))
   ((eq? cons 'cdr)
    (cdr 6))
   ((eq? cons 'null?)
    (null? 6))
   ((eq? cons 'eq?)
    (eq? 6 (a b c)))
   ((eq? cons 'atom?)
    (:atom? 6))
   ((eq? cons 'zero?)
    (zero? 6))
   ((eq? cons 'add1)
    (add1 6))
   ((eq? cons 'sub1)
    (sub1 6))
   ((eq? cons 'number?)
    (number? 6))))

;; (21) the evaluation returns its final value
(apply-primitive cons (6 (a b c))) ;; (6 a b c)

;; THE END!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
