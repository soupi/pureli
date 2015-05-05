(module main)

(require "run-extra.rkt" run-extra extra (hello-str))

(define main
  (print! extra/hello-str))

