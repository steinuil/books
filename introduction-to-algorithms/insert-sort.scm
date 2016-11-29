; Helper array-like interaction functions
(define (at n ls)
  (cond ((null? ls) '())
        ((= n 0) (car ls))
        (else (at (- n 1) (cdr ls)))))
        
(define (slice-to to l)
  (let slice ((from 0) (ls l) (res '()))
    (if (= from to) (reverse res)
      (slice (+ from 1) (cdr ls)
             (cons (car ls) res)))))
            
(define (slice-from from l)
  (cdr (list-tail l from)))

; Insertion sort
(define (insert-sorted key ls)
  (cond ((null? ls) (cons key '()))
        ((< key (car ls)) (cons key ls))
        (else (cons (car ls)
                    (insert-sorted key (cdr ls))))))

(define (sort/insert l)
  (let helper ((n 1) (ls l))
    (let ((key (at n ls)))
      (if (null? key) ls
        (helper (+ n 1)
                (append (insert-sorted key (slice-to n ls))
                        (slice-from n ls)))))))
