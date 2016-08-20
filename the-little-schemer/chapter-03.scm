(define (rember a lat)
  (cond
    ((null? lat) '())
    ((eq? a (car lat)) (cdr lat))
    (else (cons (car lat)
            (rember a (cdr lat))))))

(define (firsts l)
  (if (null? l) '()
    (cons (caar l) (firsts (cdr l)))))

(define (insertR new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat))
     (cons old (cons new (cdr lat))))
    (else (cons (car lat)
            (insertR new old (cdr lat))))))

(define (insertL new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat))
     (cons new (cons old (cdr lat))))
    (else (cons (car lat)
            (insertL new old (cdr lat))))))

(define (subst new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat))
     (cons new (cdr lat)))
    (else (cons (car lat)
            (subst new old (cdr lat))))))

(define (subst2 new o1 o2 lat)
  (cond
    ((null? lat) '())
    ((or (eq? o1 (car lat)) (eq? o2 (car lat)))
     (cons new (cdr lat)))
    (else (cons (car lat)
            (subst2 new o1 o2 (cdr lat))))))

(define (multirember a lat)
  (cond
    ((null? lat) '())
    ((eq? a (car lat))
     (multirember (cdr lat)))
    (else (cons (car lat)
            (multirember (cdr lat))))))

(define (multiinsertR new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat))
     (cons old
       (cons new
         (multiinsertR new old (cdr lat)))))
    (else (cons (car lat)
            (multiinsertR new old (cdr lat))))))

(define (multiinsertL new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat))
     (cons new
       (cons old
         (multiinsertL new old (cdr lat)))))
    (else (cons (car lat)
            (multiinsertL new old (cdr lat))))))

(define (multisubst new old lat)
  (cond
    ((null? lat) '())
    ((eq? old (car lat))
     (cons new (multisubst new old (cdr lat))))
    (else (cons (car lat)
            (multisubst new old (cdr lat))))))

