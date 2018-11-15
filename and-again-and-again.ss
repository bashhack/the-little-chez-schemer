;; 9. ...and Again, and Again, and Again...

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn)
      (keep-looking a (pick sorn lat) lat))
     (else (eq? sorn a)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(looking 'caviar '(6 2 4 caviar 5 7 3))
(looking 'caviar '(6 2 grits caviar 5 7 3))

;; Functions like looking are partial funtions,
;; as opposed to total functions - in that they
;; do not resolve themselves

;; the most partial function
(define eternity
  (lambda (x)
    (eternity x)))

(define f
  (lambda (x)
    (f x)))

(f 1)

(define first
  (lambda (pair)
    (car pair)))

(define second
  (lambda (pair)
    (car (cdr pair))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair)) (second pair)))))

(shift '((1 2) (3 4)))  ;; (1 (2 (3 4)))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(a-pair? '(1 (1 2)))

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else (build (first pora) (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else (+ (length* (first pora))
              (length* (second pora)))))))

(length* '(1 (2 (1 2))))

(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else (+ (* (weight* (first pora)) 2)
              (weight* (second pora)))))))

(weight* '(((1 1) 2) 3))
(weight* '((1 (1 2)) 3))
(weight* '((1 2) 3))
(weight* '(1 (2 3)))
(weight* '(1 1))
(weight* '1)

(define revpair
  (lambda (p)
    (cons (car (cdr p)) (cons (car p) '()))))

(revpair '(1 2))

;; ...not total, because we swap the components of the pair infinitely
(define shuffle
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (shuffle (revpair pora)))
     (else (build (first pora) (shuffle (second pora)))))))

(define one?
  (lambda (n)
    (= n 1)))

;; Lothar Collatz (1910-1990)
;; doesn't yield a value for 0, but otherwise no one knows if this is total
(define C
  (lambda (n)
    (cond
     ((one? n) 1)
     (else
      (display "n: ")
      (display n)
      (newline)
      (cond
       ((even? n) (C (div n 2)))
       (else (C (add1 (* 3 n)))))))))

;; Woah! That was a trip...
;; (C 1824702707149299828420710741989109819847109829809499992010498208664806209891099942547747472087118797987879697731601469861486982987462598631060541986913865337010730178739498712872963545653652765365176535540159552854851053578507299875341)

;; Wilhelm Ackermann (1853-1946)
(define A
  (lambda (n m)
    (cond
     ((zero? n) (add1 m))
     ((zero? m) (A (sub1 n) 1))
     (else (A (sub1 n) (A n (sub1 m)))))))

;; (A 1 0)



;; Defining a function that checks whether some function
;; stops for just the empty list '()
;; (define will-stop?
;;   (lambda (f)
;;     ...))

;; (define last-try
;;   (lambda (x)
;;     (and (will-stop? last-try)
;;          (eternity x))))

;; The Paradox:

;; If we are able to define will-stop?,
;; then (will-stop? last-try) must by definition be either #f or #t.

;; But it cannot, by definition of will-stop? itself - that is,
;; it aims to answer whether some function will return a value
;; for every argument, ending with the empty list.

;; The answer to this lies with Alan Turing (1912 - 1954) and Kurt Godel (1906-1978)

;; We need a recursive definition to solve this...let's look at the
;; function length again...

;; If this is our length with define
(define length
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (length (cdr l)))))))

;; in the recursive definition, let's imagine we have:
(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 (eternity (cdr l))))))

;; ...this will work for the empty list, but if we pass it a non-empty list
;; we will have no answer...
;; ...so, if we gave this an identifier, like length_0, we could use it
;; to define a new function for one or fewer items in the list by writing:
(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 (length_0 (cdr l))))))

;; or...written as...

(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1
          ((lambda (l)
             (cond
              ((null? l) 0)
              (else (add1 (eternity (cdr l))))))
           (cdr l))))))

;; cool, cool - now we have a function we could name something like
;; length_less_than_equal_to_1...to get our length_less_than_equal_to_2
;; we would continue replacing eternity with the new function infinitely

;; Not very practical!

;; By using our 9th Commandment, we describe the pattern
;; of the function length like...

(define eternity
  (lambda (x)
    eternity x))


;; rewriting length0
((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
 eternity)

;; rewriting length_less_than_equal_to_1
((lambda (length0)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length0 (cdr l)))))))
 ((lambda (length1)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length1 (cdr l)))))))
  eternity))

;; rewriting length_less_than_equal_to_2
((lambda (length0)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length0 (cdr l)))))))
 ((lambda (length1)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length1 (cdr l))))))))
 ((lambda (length2)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length2 (cdr l)))))))
    eternity))


;; If we abstract just a bit...we again get our length0
((lambda (make-length)
   (make-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

;; getting to our length_less_than_or_equal_to_1
((lambda (make-length)
   (make-length
    (make-length eternity))
   (lambda (length)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))))

;; etc...

((lambda (make-length)
   (make-length
    (make-length
     (make-length
      (make-length eternity)))))
 ;; ...our length function
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

;; "recursion is like an infinite tower of applications of make-length to some arbitrary function"

;; ...since it does not matter what function we pass to make-length - we pass make-length to make-length

((lambda (make-length)
   (make-length make-length))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

;; but that inner name length isn't as accurate as it could be - isn't it just make-length?

((lambda (make-length)
   (make-length make-length))
 (lambda (make-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (make-length (cdr l))))))))

;; this reminds us that the first argument to make-length is in fact make-length

;; using an argument to make an additional recursive case
((lambda (make-length)
   (make-length make-length))
 (lambda (make-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 ((make-length eternity) (cdr l))))))))

;; walking through evaluation...
;; ...at start (1)
(((lambda (make-length)
    (make-length make-length))
  (lambda (make-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
        (add1
         ((make-length eternity)
          (cdr l))))))))
 '(apples))

;; (2) ...apply the function (lambda (make-length) (lambda (l) (cond ((null? l) 0) (else (add1 ((make-length eternity) (cdr l))))))) as the arg make-length at line 328, returning
(((lambda (make-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 ((make-length eternity) (cdr l)))))))
  (lambda (make-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 ((make-length eternity) (cdr l))))))))
 '(apples))

;; (3) apply the second function starting at line 343 (lambda (make-length) (lambda (l) (cond ((null? l) 0) (else (add1 ((make-length eternity) (cdr l))))))) as the arg at 338, returning
((lambda (l)
   (cond
    ((null? l) 0)
    (else
     (add1
      (((lambda (make-length)
          (lambda (l)
            (cond
             ((null? l) 0)
             (else
              (add1
               ((make-length eternity)
                (cdr l)))))))
        eternity)
       (cdr l))))))
 '(apples))

;; (4) pass '(apples) as the arg l at line 351
(cond
 ((null? '(apples)) 0)
 (else
  (add1
   (((lambda (make-length)
       (lambda (l)
         (cond
          ((null? l) 0)
          (else
           (add1
            ((make-length eternity)
             (cdr l)))))))
     eternity)
    (cdr '(apples))))))

;; (5) line 370 evaluates to #f (see 386), else branch should be evaluated
(cond
 (#f 0)
 (else
  (add1
   (((lambda (make-length)
       (lambda (l)
         (cond
          ((null? l) 0)
          (else
           (add1
            ((make-length eternity)
             (cdr l)))))))
     eternity)
    (cdr '(apples))))))

;; (6) the truthy branch is ignored, leaving the else
(cond
 (else
  (add1
   (((lambda (make-length)
       (lambda (l)
         (cond
          ((null? l) 0)
          (else
           (add1
            ((make-length eternity)
             (cdr l)))))))
     eternity)
    (cdr '(apples))))))

;; (7) the else branch, on its own, is next to be evaluated (the cond and else scopes are removed)
(add1
 (((lambda (make-length)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else
         (add1
          ((make-length eternity)
           (cdr l)))))))
   eternity)
  (cdr '(apples))))

;; (8) eternity is substituted with its definition at line 438
(add1
 (((lambda (make-length)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else
         (add1
          ((make-length eternity)
           (cdr l)))))))
   (lambda (f) (eternity f)))
  (cdr '(apples))))

;; (9) the definition of eternity is passed as the argument make-length at line 430
(add1
 ((lambda (l)
    (cond
     ((null? l) 0)
     (else
      (add1
       (((lambda (f) (eternity f))
         eternity)
        (cdr l))))))
  (cdr '(apples))))

;; (10) the cdr of '(apples) at line 451 is evaluated to '() at line 463
(add1
 ((lambda (l)
    (cond
     ((null? l) 0)
     (else
      (add1
       (((lambda (f) (eternity f))
         eternity)
        (cdr l))))))
  '()))

;; (11) '() from 463 is passed as the arg l at line 455, leaving the cond, passing the empty list to lines 468 and 473
(add1
 (cond
  ((null? '()) 0)
  (else
   (add1
    (((lambda (f) (eternity f))
      eternity)
     (cdr '()))))))

;; (12) the null? check at line 468 evaluates to #t, ignoring the else branch
(add1
 (cond
  (#t 0)
  (else
   (add1
    (((lambda (f) (eternity f))
      eternity)
     (cdr '()))))))

;; (13) the final evaluation occurs, returning 1
(add1 0) ;; 1

;; starting from this - the natural next step to abstract this further
;; is to replace the call to eternity at line 499
((lambda (make-length)
   (make-length make-length))
 (lambda (make-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1
             ((make-length eternity)
              (cdr l))))))))

;; turning the function into...
((lambda (make-length)
   (make-length make-length))
 (lambda (make-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1
             ((make-length make-length)
              (cdr l))))))))

;; we now return to a function that behaves like our original (define length ...)
;; it continues adding recursive cases by passing make-length to itself as
;; (make-length make-length) at line 511, just as it is about to expire
;;
;; we could perform another abstraction here of (make-length make-length)
;; because it is the very heart of the function length as we have known it to behave

(((lambda (make-length)
   (make-length make-length))
 (lambda (make-length)
   ((lambda (length)
      (lambda (l)
        (cond
         ((null? l) 0)
         (else
          (add1
           (length (cdr l)))))))
    (make-length make-length))))
 '(apples))

;; let's evaluate it...
;; (1)
(((lambda (make-length)
    (make-length make-length))
  (lambda (make-length)
    ((lambda (length)
       (lambda (l)
         (cond
          ((null? l) 0)
          (else
           (add1
            (length (cdr l)))))))
     (make-length make-length))))
 '(apples))

;; (2)
(((lambda (make-length)
    ((lambda (length)
       (lambda (l)
         (cond
          ((null? l) 0)
          (else
           (add1
            (length (cdr l)))))))
     (make-length make-length)))
  (lambda (make-length)
    ((lambda (length)
       (lambda (l)
         (cond
          ((null? l) 0)
          (else
           (add1
            (length (cdr l)))))))
     (make-length make-length))))
 '(apples))

;; (3)
(((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
        (add1
         (length (cdr l)))))))
  ((lambda (make-length)
     ((lambda (length)
        (lambda (l)
          (cond
           ((null? l) 0)
           (else
            (add1
             (length (cdr l)))))))
      (make-length make-length)))
   (lambda (make-length)
     ((lambda (length)
        (lambda (l)
          (cond
           ((null? l) 0)
           (else
            (add1
             (length (cdr l)))))))
      (make-length make-length)))))
 '(apples))

;; (4)
(((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
        (add1
         (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else
         (add1
          (length (cdr l)))))))
   ((lambda (make-length)
      ((lambda (length)
         (lambda (l)
           (cond
            ((null? l) 0)
            (else
             (add1
              (length (cdr l)))))))
       (make-length make-length)))
    (lambda (make-length)
      ((lambda (length)
         (lambda (l)
           (cond
            ((null? l) 0)
            (else
             (add1
              (length (cdr l)))))))
       (make-length make-length))))))
 '(apples))

;; this then continues ad infinitum because of (make-length make-length)
;; when we extracted (make-length make-length) from the function that makes
;; length we cease to return a function
;;
;; so, let's go back to our last correct version:

((lambda (make-length)
   (make-length make-length))
 (lambda (make-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1
             ((make-length make-length)
              (cdr l))))))))

;; how might we turn the application of make-length to itself into a function?

;; we should start with a small truth:

(lambda (x) (f x))

;; if f is a function of one arg, is the entire expression above a function of
;; one argument? Yes, of course!

;; if (make-length make-length) returns a function of one argument,
;; what does the following return?

(lambda (x)
  ((make-length make-length) x))

;; it returns a function!

;; let's use this realization in practice:

((lambda (make-length)
   (make-length make-length))
 (lambda (make-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1
             ((lambda (x)
                ((make-length make-length) x))
              (cdr l))))))))

;; we now have an inner call that returns a function, as we expected

((lambda (make-length)
   (make-length make-length))
 (lambda (make-length)
   ((lambda (length)
      (lambda (l)
        (cond
         ((null? l) 0)
         (else
          (add1
           (length (cdr l)))))))
    (lambda (x)
      ((make-length make-length) x)))))

;; we could take the function that looks like our original length (lines 684 - 690),
;; and extract that - giving it a name

((lambda (le)
   ((lambda (make-length)
      (make-length make-length))
    (lambda (make-length)
      (le (lambda (x)
            ((make-length make-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else
       (add1
        (length (cdr l))))))))

;; we now have a function that looks like length (lines 703-709)
;; and one that makes length (lines 697-702)
;;
;; so, let's give it a name: the applicative-order 'Y' combinator

(define Y
  (lambda (le)
    ((lambda (make-length)
       (make-length make-length))
     (lambda (make-length)
       (le (lambda (x)
             ((make-length make-length) x)))))))

;; in the context of our use of length, the name make-length is reasonable,
;; but what we have is a pattern of recursion that is generic - let's
;; rename things a bit...

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

;; renaming for yet another more generic structure

(define Y
  (lambda (f)
    ((lambda (g) (g g))
     (lambda (g)
       (f (lambda (a) ((g g) a)))))))

;; finally, calling the following returns us our correct answer: 1

;; NOTE: working through the order of evaluation is non-trivial, but
;;       using DrRacket (in Scheme mode) with the language set to
;;       'Intermediate Student with lambda' and using the 'Stepper'
;;       proves to be a great visualization tool!

((Y
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
        (add1
         (length (cdr l))))))))
 '(apples))
