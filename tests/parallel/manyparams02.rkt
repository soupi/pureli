(module main)

; calling this should generate 20 sparks. 10 for procedure call and 10 for ++
(define append (a b c d e f g h i j)
  (++ a b c d e f g h i j))

; calling this should only generate 10 sparks. because it is not in pure context
(define io-append (a b c d e f g h i j)
  (pure (++ a b c d e f g h i j)))


(define main
  (do!
    (print! "This should create 40 sparks,")
    (print! "1 for each list, 10 for procedure call, 10 for ++, and another 10 for ++ (without procedure call)")
    (let! result (pure (append (list 100000) (list 100001) (list 100002) (list 100003) (list 100004) (list 100005) (list 100006) (list 100007) (list 100008) (list 100009))))
    (print! result)
    (let! result (io-append (list 100000) (list 100001) (list 100002) (list 100003) (list 100004) (list 100005) (list 100006) (list 100007) (list 100008) (list 100009)))
    (print! result)))
