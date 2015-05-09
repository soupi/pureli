;macro test
(module main)

(defmacro show
  ([f]
   (if (procedure? f)
     "<procedure>"
      ((eval (quote show)) f))))

(define main
   (do!
     [print! (show "hello world")]
     [print! (show +)]
     [print! (show (lambda () 5))]))
