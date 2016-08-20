(define (lat? l)
  (cond
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f)))

(define (member? a lat)
  (if (null? lat) #f
    (or (eq? a (car lat))
        (member? a (cdr lat)))))
