(module main)

(define exponent (n expn)
  (letrec
    [(go (lambda (m prod)
      (if (zero? m)
          (trace expn prod)
          (go (- m 1) (* prod n)))))]
    (go expn 1)))


(define test
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
    (exponent 2 20009)
    (exponent 2 20010)
    (exponent 2 20011)
    (exponent 2 20012)
    (exponent 2 20013)
    (exponent 2 20014)
    (exponent 2 20015)
    (exponent 2 20016)
    (exponent 2 20017)
    (exponent 2 20018)
    (exponent 2 20019)))

(define main
  (do!
    [let! x (pure test)]
    [print! x]))
