;IO tests
(module main)

(define x! (do! (pure 5)))

(define printer! (f x)
   (do! (print! (f x))))

(define main
 (do!
   (let! x x!)
   (let! f (pure (lambda (x) (+ x x))))
   (printer! f x)))
