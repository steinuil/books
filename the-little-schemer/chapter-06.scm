(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

;; Start
(define (numbered? aexp)
  (if (atom? aexp) (number? aexp)
    (and (numbered? (car aexp))
         (numbered? (caddr aexp)))))

(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? (cadr nexp) '+)
     (o+ (value (car nexp))
         (value (caddr nexp))))
    ((eq? (cadr nexp) '*)
     (o* (value (car nexp))
         (value (caddr nexp))))
    ((eq? (cadr nexp) '**)
     (o** (value (car nexp))
          (value (caddr nexp))))))

(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? (car nexp) '+)
     (o+ (value (cadr nexp))
         (value (caddr nexp))))
    ((eq? (car nexp) '*)
     (o* (value (cadr nexp))
         (value (caddr nexp))))
    ((eq? (car nexp) '+)
     (o** (value (cadr nexp))
          (value (caddr nexp))))))

(define (first-sub-exp aexp)
  (cadr aexp))

(define (second-sub-exp aexp)
  (caddr aexp))

(define (operator aexp)
  (car aexp))

(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? (operator nexp) '+)
     (o+ (value (first-sub-exp nexp))
         (value (second-sub-exp nexp))))
    ((eq? (operator nexp) '*)
     (o* (value (first-sub-exp nexp))
         (value (second-sub-exp nexp))))
    ((eq? (operator nexp) '+)
     (o** (value (first-sub-exp nexp))
          (value (second-sub-exp nexp))))))

(define (sero? n)
  (null? n))

(define (edd1 n)
  (cons '() n))

(define (zub1 n)
  (cdr n))

(define (edd n m)
  (if (sero? m) n
    (edd (edd1 n) (zub1 m))))
