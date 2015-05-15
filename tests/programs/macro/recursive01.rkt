(module main)

(defmacro cond
  [params (error "*** error: cond - non-exhuastive patterns")]
  [(clause1 &rest)
     (if (car clause1)
         (car (cdr clause1))
         (cond (cdr rest)))])

(define main
  (do!
    [print! x]))

(define x
  (cond ((#f 1)
         (#f 2)
         (#t 3))))
