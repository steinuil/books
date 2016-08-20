(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (add1 x)
  (+ x 1))

(define (sub1 x)
  (- x 1))

(define (o> n m)
  (cond
    ((zero? n) #f)
    ((zero? m) #t)
    (else (o> (sub1 n) (sub1 m)))))

(define (o< n m)
  (cond
    ((zero? m) #f)
    ((zero? n) #t)
    (else (o< (sub1 n) (sub1 m)))))

(define (o= n m)
  (and (not (o> n m))
       (not (o< n m))))

(define (eqan? a1 a2)
  (cond
    ((and (number? a1) (number? a2)) (o= a1 a2))
    ((or (number? a1) (number? a2)) #f)
    (else (eq? a1 a2))))

;; Start
(define (rember* a l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (if (eqan? a (car l))
       (rember* a (cdr l))
       (cons (car l) (rember* a (cdr l)))))
    (else (cons (rember* a (car l))
                (rember* a (cdr l))))))

(define (insertR* new old l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (if (eqan? old (car l))
       (cons old (cons new
                 (insertR* new old (cdr l))))
       (cons (car l) (insertR* new old (cdr l)))))
    (else (cons (insertR* new old (car l))
                (insertR* new old (car l))))))

(define (occur* a l)
  (cond
    ((null? l) 0)
    ((atom? (car l))
     (if (eqan? a (car l))
       (add1 (occur* a (cdr l)))
       (occur* a (cdr l))))
    (else (cons (occur* a (car l))
                (occur* a (cdr l))))))

(define (subst* new old l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (if (eqan? old (car l))
       (cons new (subst* new old (cdr l)))
       (cons (car l) (subst* new old (cdr l)))))
    (else (cons (subst* new old (car l))
                (subst* new old (cdr l))))))

(define (insertL* new old l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (if (eqan? old (car l))
       (cons new (cons old
                 (insertR* new old (cdr l))))
       (cons (car l) (insertR* new old (cdr l)))))
    (else (cons (insertR* new old (car l))
                (insertR* new old (car l))))))

(define (member* a l)
  (cond
    ((null? l) #f)
    ((atom? (car l))
     (or (eqan? a (car l))
         (member* a (cdr l))))
    (else (or (member* a (car l))
              (member* a (cdr l))))))

(define (leftmost l)
  (if (atom? (car l)) (car l)
    (leftmost (car l))))

(define (eqlist? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    ((and (atom? (car l1)) (atom? (car l2)))
     (or (eqan? (car l1) (car l2))
       (eqlist? (cdr l1) (cdr l2))))
    ((and (list? (car l1)) (list? (car l2)))
     (and (eqlist? (cdr l1)) (eqlist? (cdr l2))))
    (else #f)))

(define (equal? s1 s2)
  (cond
    ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
    ((or (atom? s1) (atom? s2)) #f)
    (else (eqlist? s1 s2))))

(define (eqlist? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    (else (and (equal? (car l1) (car l2))
               (eqlist? (cdr l1) (cdr l2))))))

(define (rember s l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (if (equal? (car l) s) (cdr l)
       (cons (car l)
             (rember s (cdr l)))))
    (else
      (if (equal? (car l) s) (cdr l)
        (cons (car l)
              (rember s (cdr l)))))))

(define (rember s l)
  (cond
    ((null? l) '())
    ((equal? (car l) s) (cdr l))
    (else (rember s (cdr l)))))
