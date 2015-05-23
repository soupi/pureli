; echo program with macro test

(module main)

(defmacro bind!
    ((m f)
      (do!
        [let! x (m)]
        [f x])))

(define main
   (do!
     [print! "echo program"]
     [letrec
       ([go!
          (lambda ()
            (do!
              [bind! read! print!]
              [go!]))])
       (go!)]))
