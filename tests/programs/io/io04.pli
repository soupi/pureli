; IO test

(module main)

(define x (do!
            (pure 5)))

(define main
  (do!
    [let! y x]
    [print! y]
    [id x]))

(define id
  (lambda (x) x))
