(define (rember-f test? a l)
  (cond
    ((null? l) '())
    ((test? a (car l)) (cdr l))
    (else (cons (car l)
            (rember-f test? a (cdr l))))))

(define (eq?-c a)
  (lambda (x)
    (eq? x a)))

(define eq?-salad (eq?-c 'salad))

(define (rember-f test?)
  (lambda (a l)
    (cond
      ((null? l) '())
      ((test? (car l) a) (cdr l))
      (else (cons (car l)
              ((rember-f test?) a (cdr l)))))))

(define (insertL-f test?)
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((test? (car l) old)
       (cons new (cons old (cdr l))))
      (else (cons (car l)
                  ((insertL-f test?) new old
                                     (cdr l)))))))

(define (seqL new old l)
  (cons new (cons old l)))

(define (seqR new old l)
  (cons old (cons new l)))

(define (insert-g seq)
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? (car l) old)
       (seq new old (cdr l)))
      (else (cons (car l)
                  ((insert-g seq) new old
                                  (cdr l)))))))

(define insertL
  (insert-g
    (lambda (new old l)
      (cons new (cons old l)))))

(define insertR
  (insert-g
    (lambda (new old l)
      (cons old (cons new l)))))

(define (seqS new old l)
  (cons new l))

(define subst (insert-g seqS))

(define (rember a l)
  ((insert-g (lambda (new old l) l)) #f a l))

;(define (value nexp)
;  (if (atom? nexp) nexp
;    ((atom-to-fun (operator nexp))
;     (value (first-sub-exp nexp)
;            (second-sub-exp nexp)))))
;
;(define (atom-to-fun operator)
;  (cond
;    ((eq? operator '+) o+)
;    ((eq? operator '*) o*)
;    ((eq? operator '**) o**)))

(define (multirember&co a lat cont)
  (cond
    ((null? lat) (cont '() '()))
    ((eq? (car lat) a)
     (multirember&co a (cdr lat)
       (lambda (newlat seen)
         (cont newlat
               (cons (car lat) seen)))))
    (else
      (multirember&co a (cdr lat)
        (lambda (newlat seen)
          (cont (cons (car lat) newlat)
                seen))))))

#| Expansion of the above
(multirember&co 'tuna '(strawberries tuna and swordfish)
  (lambda (x y) (null? y)))

(multirember&co 'tuna '(tuna and swordfish)
  (lambda (newlat seen)
    (a-friend (cons 'strawberries newlat) seen)))

(multirember&co 'tuna '(and swordfish)
  (lambda (newlat seen)
    (a-friend (cons 'strawberries newlat)
              (cons 'tuna seen))))

(multirember&co 'tuna '(swordfish)
  (lambda (newlat seen)
    (a-friend (cons 'strawberries (cons 'and newlat))
              (cons 'tuna seen))))

(multirember&co 'tuna '()
  (lambda (newlat seen)
    (a-friend (cons 'swordfish (cons 'strawberries (cons 'and newlat)))
              (cons 'tuna seen))))

(a-friend (cons 'swordfish (cons 'strawberries (cons 'and '())))
          (cons 'tuna '()))
|#

(define (evens-only*&co l cont)
  (cond
    ((null? l) (cont '() 1 0))
    ((atom? (car l))
     (if (even? (car l))
       (evens-only*&co (cdr l)
         (lambda (newl p s)
           (cont (cons (car l) newl)
                 (* (car l) p) s)))
       (evens-only*&co (cdr l)
         (lambda (newl p s)
           (cont newl p
                 (+ (car l) s))))))
    (else
      (evens-only*&co (car l)
        (lambda (al ap as)
          (evens-only*&co (cdr l)
            (lambda (dl dp ds)
              (cont (cons al dl)
                    (* ap dp)
                    (+ as ds)))))))))
