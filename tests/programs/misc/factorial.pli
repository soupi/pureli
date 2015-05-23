(module main)

(define factorial [n]
  (letrec ((go (lambda (n prod)
    (if (zero? n)
      prod
      (go (- n 1) (* prod n))))))
    (go n 1)))

(define main
  (do!
    [let! result (pure (factorial 23700))]
    [print! "factorial 23700 is: "]
    [print! result]))
