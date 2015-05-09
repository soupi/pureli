(module main)

(require "std.rkt" std (id elem))

(define main
  (do!
    [print! (std/id "module loaded")]
    [print! (std/elem 5 (list 1 2 3))]
    ))
