(include-book "j-bob-lang" :dir :teachpacks)
(include-book "j-bob" :dir :teachpacks)
(include-book "little-prover" :dir :teachpacks)

;; 15
(J-Bob/step (prelude)
  '(atom (cons a b))
  '((() (atom/cons a b))))

;; 19
(J-Bob/step (prelude)
  '(equal 'flapjack (atom (cons a b)))
  '(((2) (atom/cons a b))
    (()  (equal 'flapjack 'nil))))

;; 43
(J-Bob/step (prelude)
  '(equal 'eggs '(ham))
  '((() (equal 'eggs '(ham)))))

;; 44
(J-Bob/step (prelude)
  '(car (cons (equal (cons x y) (cons x y))
              '(and crumpets)))
  '(((1 1) (equal-same (cons x y)))
    (()    (car/cons 't '(and crumpets)))))

;; 54
(J-Bob/step (prelude)
  '(cons y (equal (car (cons (cdr x) (car y)))
                  (equal (atom x) 'nil)))
  '(((2 1) (car/cons (cdr x) (car y)))))

;; 62
(J-Bob/step (prelude)
  '(atom (car (cons (car a) (cdr b))))
  '(((1) (car/cons (car a) (cdr b)))))
