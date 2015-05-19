(module main)


(define exponent (n expn)
  (if (zero? expn)
      1
      (* n (exponent n (- expn 1)))))

(define main
  (pure
    (+
      (exponent 2 20000)
      (exponent 2 20001)
      (exponent 2 20002)
      (exponent 2 20003)
      (exponent 2 20004)
      (exponent 2 20005)
      (exponent 2 20006)
      (exponent 2 20007)
      (exponent 2 20008)
      (exponent 2 20009))))
