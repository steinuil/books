(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (build s1 s2)
  (cons s1 (cons s2 '())))

;; Table management
(define (first p) (car p))
(define (rest p) (cdr p))
(define (second p) (cadr p))
(define (third p) (caddr p))

(define new-entry build)

(define extend-table cons)

(define (lookup-in-entry name entry entry-f)
  (let help ((name name)
             (names (first entry))
             (values (second entry))
             (entry-f entry-f))
    (cond
      ((null? names) (entry-f name))
      ((eq? (first names) name) (first values))
      (else (help name (rest names)
                  (rest values) entry-f)))))

(define (lookup-in-table name table table-f)
  (if (null? table) (table-f name)
    (lookup-in-entry name (first table)
      (lambda (n)
        (lookup-in-table n (rest table) table-f)))))

;; Interpreter
(define (expression-to-action e)
  (if (atom? e) (atom-to-action e)
    (list-to-action e)))

(define (atom-to-action e)
  (if (or (number? e)
          (eq? e #t)
          (eq? e #f)
          (eq? e 'car)
          (eq? e 'cdr)
          (eq? e 'cons)
          (eq? e 'add1)
          (eq? e 'eq?)
          (eq? e 'null?)
          (eq? e 'atom?)
          (eq? e 'number?)
          (eq? e 'zero?)) *const
    *identifier))

(define (list-to-action e)
  (let ((f (car e)))
    (if (atom? f)
      (cond
        ((eq? f 'quote) *quote)
        ((eq? f 'lambda) *lambda)
        ((eq? f 'cond) *cond)
        (else *application))
      *application)))

(define (*const e table)
  (if (or (number? e)
          (eq? e #t)
          (eq? e #f)) e
    (build 'primitive e)))

(define (*quote e table)
  (text-of e))
(define text-of second)

(define (*identifier e table)
  (lookup-in-table e table initial-table))
(define (initial-table name)
  (car '()))

(define (*lambda e table)
  (build 'non-primitive (cons table (cdr e))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define (evcon lines table)
  (cond
    ((else? (question-of (car lines)))
     (meaning (answer-of (car lines)) table))
    ((meaning (question-of (car lines)))
     (meaning (answer-of (car lines)) table))
    (else (evcon (cdr lines) table))))

(define (else? x)
  (if (atom? x) (eq? x 'else) #f))
(define question-of first)
(define answer-of second)

(define (*cond e table)
  (evcon (cdr e) table))

(define (evlis args table)
  (if (null? args) '()
    (cons (meaning (car args) table)
          (evlis (cdr args) table))))

(define (*application e table)
  (apply (meaning (function-of e) table)
         (evlis (arguments-of e) table)))

(define function-of first)
(define arguments-of rest)

(define (primitive? l)
  (eq? (first l) 'primitive))
(define (non-primitive? l)
  (eq? (first l) 'non-primitive))

(define (apply fun vals)
  (cond
    ((primitive? fun)
     (apply-primitive (second fun) vals))
    ((non-primitive? fun)
     (apply-closure (second fun vals)))))

(define (value e)
  (meaning e '()))

(define (meaning e table)
  ((expression-to-action e) e table))

(define (apply-primitive name vals)
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
     (number? (first vals)))))

(define (:atom? x)
  (cond
    ((atom? x) #t)
    ((null? x) #f)
    ((eq? (car x) 'primitive) #t)
    ((eq? (car x) 'non-primitive) #t)
    (else #f)))

(define (apply-closure closure vals)
  (meaning (body-of closure)
    (extend-table
      (new-entry (formals-of closure) vals)
      (table-of closure))))
