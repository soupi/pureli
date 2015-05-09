(module main)

(define plus (&rest)
  (+ (car rest) (car (cdr rest))))

(define main
  (do!
    [print! (plus 1 2)]))
