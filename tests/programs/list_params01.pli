(module main)

(define plus rest
  (if (trace rest (empty? rest))
      0
      (+ (car rest) (apply plus (cdr rest)))))

(defmacro apply
  ((op lst)
   (eval (++ (list op) lst))))

(defmacro chain
  ((op) op)
  (params (empty? params)))

(define main
  (do!
    [print! (chain 1)]
    [print! (chain 1 2 3)]
    [print! (plus 1 2 3 4 5)]))
