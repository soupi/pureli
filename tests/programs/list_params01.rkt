(module main)

(define plus rest
  (if (trace rest (empty? rest))
      0
      (+ (car rest) (apply plus (cdr rest)))))

(defmacro apply
  ((op lst)
   (eval (++ (list op) lst))))

(define main
  (do!
    [print! (plus 1 2 3 4 5)]))
