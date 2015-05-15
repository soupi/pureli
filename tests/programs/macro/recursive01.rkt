(module main)

(defmacro cond
  [() (error "*** error: cond - non-exhuastive patterns")]
  [(clause1 &rest)
     (if (car clause1)
         (car (cdr clause1))
         (cond &rest))])

(define main
  (do!
    [print! x]))

(define x
  (cond (list #f 1)
        (list #f 2)
        (list #t 3)))
