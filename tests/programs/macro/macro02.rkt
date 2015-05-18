;macro test
(module main)

(defmacro show
  [(f)
   (if (procedure? f)
     "<procedure>"
      (qe f))])

(defmacro qe
  [(expr) (eval (quote expr))])

(define main
   (do!
     [print! (show "hello world")]
     [print! (show +)]
     [print! (show (lambda () 5))]))

(module test)

(require "macro02.rkt" main)

(define x 5)