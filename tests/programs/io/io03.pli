; IO test
(module main)

(define main (pure main-pure))

(define main-pure
  (let
    ([x (+ 1 2 3 x)])
    [double x]))

(define x 5)

(define double [x] (+ x x))
