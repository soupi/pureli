(module std)

(define elem [x xs]
  (if (list? xs)
    (if (empty? xs) #f
      (if (= (car xs) x) #t
        (elem x (cdr xs))))
    (error "not a list")))

(define not [test]
  (if test #f #t))

(define cons [x xs]
  (if (list? xs)
    (++ (list x) xs)
    (++ (list x) (list xs))))

(define map [f xs]
  (if (not (list? xs))
    (error "cannot map on a non list type")
    (if (empty? xs)
      xs
      (cons (f (car xs)) (map f (cdr xs))))))

(define compose (f g)
  (lambda (x) (f (g x))))

(define id (x) x)




(define main
  (do!
    [test-elem]
    [test-cons]
    [test-map]))

(define test-elem ()
  (do!
    [print! (elem 3 (list 1 2 3))]
    [print! (try (elem 3 3) #t)]
    [print! (elem 4 (list 1 2 3))]))

(define test-cons ()
  (do!
    [print! (cons 1 2)]
    [print! (cons 1 (list 2))]
    [print! (cdr (cons 1 2))]
    ))

(define test-map ()
  (do!
    [print! (map show (list))]
    [print! (map show (list 1))]
    [print! (map show (list 1 2))]
    [print! (map show (list 1 2 3))]
    [print! (map (lambda (x) (+ 1 x)) (list 1 2 3))]))
