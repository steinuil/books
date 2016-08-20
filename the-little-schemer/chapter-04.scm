(define (add1 x)
  (+ x 1))

(define (sub1 x)
  (- x 1))

;; End of required functions
(define (o+ n m)
  (if (zero? m) n
    (o+ (add1 n) (sub1 m))))

(define (o- n m)
  (if (zero? m) n
    (o- (sub1 n) (sub1 m))))

(define (addtup tup)
  (if (null? tup) 0
    (o+ (car tup) (addtup (cdr tup)))))

(define (o* n m)
  (if (zero? m) 0
    (o+ n (o* n (sub1 m)))))

(define (tup+ tup1 tup2)
  (cond
    ((null? tup1) tup2)
    ((null? tup2) tup1)
    (else (cons (o+ (car tup1) (car tup2))
                (tup+ (cdr tup1) (cdr tup2))))))

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

(define (o** n m)
  (if (zero? m) 1
    (o* n (o** n (sub1 m)))))

(define (o/ n m)
  (if (o< n m) 0
    (add1 (o/ (o- n m) m))))

(define (length lat)
  (if (null? lat) 0
    (add1 (length (cdr lat)))))

(define (pick n lat)
  (if (zero? (sub1 n)) (car lat)
    (pick (sub1 n) (cdr lat))))

(define (rempick n lat)
  (if (zero? (sub1 n)) (cdr lat)
    (cons (car lat)
          (rempick (sub1 n) (cdr lat)))))

(define (no-nums lat)
  (cond
    ((null? lat) '())
    ((number? (car lat)) (no-nums (cdr lat)))
    (else (cons (car lat)
                (no-nums (cdr lat))))))

(define (all-nums lat)
  (cond
    ((null? lat) '())
    ((number? (car lat))
     (cons (car lat) (all-nums (cdr lat))))
    (else (all-nums (cdr lat)))))

(define (eqan? a1 a2)
  (cond
    ((and (number? a1) (number? a2)) (o= a1 a2))
    ((or (number? a1) (number? a2)) #f)
    (else (eq? a1 a2))))

(define (occur a lat)
  (cond
    ((null? lat) 0)
    ((eqan? a (car lat))
     (add1 (occur a (cdr lat))))
    (else (occur a (cdr lat)))))

(define (one? n)
  (o= 1 n))

(define (rempick n lat)
  (if (one? n) (cdr lat)
    (cons (car lat)
          (rempick (sub1 n) (cdr lat)))))
