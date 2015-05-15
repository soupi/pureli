;macro test
(module main)

(defmacro show
  [(f)
   (if (procedure? f)
     "<procedure>"
      ((eval (mquote show)) f))])

(define main
   (do!
     [print! (show "hello world")]
     [print! (show +)]
     [print! (show (lambda () 5))]))
