(define (pick n lat)
  (if (zero? (sub1 n)) (car lat)
    (pick (sub1 n) (cdr lat))))

;; Start
(define (looking a lat)
  (keep-looking a (pick 1 lat) lat))

(define (keep-looking a s-or-n lat)
  (if (number? s-or-n)
    (keep-looking a (pick s-or-n) lat)
    (eq? s-or-n a)))

(define (shift pair)
  (build (first (first pair))
    (build (second (first pair)))
      (second pair)))

(define (align pora) ; pair-or-atom
  (cond
    ((atom? pora) pora)
    ((pair? (first pora))
     (align (shift pora)))
    (else (build (first pora)
                 (align (second pora))))))

(define (C n)
  (cond
    ((one? n) 1)
    ((even? n) (C (/ n 2)))
    (else (C (add1 (* 3 n))))))

(define (A n m)
  (cond
    ((zero? n) (add1 m))
    ((zero? m) (A (sub1 n) 1))
    (else (A (sub1 n)
             (A n (sub1 m))))))

(define (eternity x)
  (eternity x))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (if (null? l) 0
       (add1 ((mk-length mk-length)
              (cdr l)))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (if (null? l) 0
          (add1 (length (cdr l))))))
    (lambda (x)
      ((mk-length mk-length) x)))))

((lambda (len)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (if (null? l) 0
       (add1 (length (cdr l)))))))

#| Expansion of the above (Y combinator)
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (length (lambda (x) ((mk-length mk-length) x)))))

((lambda (mk-length)
   (length (lambda (x) ((mk-length mk-length) x))))
 (lambda (mk-length)
   (length (lambda (x) ((mk-length mk-length) x)))))

(length
  (lambda (x)
    ((lambda (mk-length) (length (lambda (x) ((mk-length mk-length) x))))
     (lambda (mk-length) (length (lambda (x) ((mk-length mk-length) x)))))
    x))

(((lambda (mk-length) (length (lambda (x) ((mk-length mk-length) x))))
  (lambda (mk-length) (length (lambda (x) ((mk-length mk-length) x)))))
 x)

(length
  (lambda (x)
    ((lambda (mk-length) (length (lambda (x) ((mk-length mk-length) x))))
     (lambda (mk-length) (length (lambda (x) ((mk-length mk-length) x)))))
    x))

...
|#

(define (Y lam)
  ((lambda (f) (f f))
   (lambda (f) (lam (lambda (x) ((f f) x))))))
