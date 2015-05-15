;macro test
(module main)

(defmacro show
  [(f)
   (if (procedure? f)
     "<procedure>"
      ((eval (quote f))))])

{~

(defmacro qe
  [(expr) (eval (quote expr))])

~}

(define main
   (do!
     [print! (show "hello world")]
     [print! (show +)]
     [print! (show (lambda () 5))]))

(module test)

(require "macro01.rkt" main)

(define x 5)
