(module main)

(define main
  (do!
    [let! x (pure (+ 1 2 3 4 5))]
    [print! x]))
