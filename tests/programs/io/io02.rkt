;io test - read print
(module main)

(define main
   (do!
     (print! "tell me your name...")
     (let! name (read!))
     (print! (++ "Hello, " name))))
