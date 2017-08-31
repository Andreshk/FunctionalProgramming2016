; Зад.1 - Да се обърнат цифрите на дадено число -
; итеративен вариант
(define (reverse-int n)
  (define (helper n res)
    (if (= n 0)
        res
        (helper (quotient n 10) (+ (* res 10) (remainder n 10)))))
  (helper n 0))

; Зад.2 - Да се провери дали едно число е палиндром
(define (palindrome? n)
  (= n (reverse-int-iter n)))

; Зад.3 - Да се изчисли сумата от делителите на дадено число
(define (divisors-sum n)
  (define (helper i res)
    (cond [(> i n) res]
          [(= (remainder n i) 0) (helper (+ i 1) (+ res i))]
          [else (helper (+ i 1) res)]))
  (helper 1 0))

; Зад.4 - Да се провери дали число е "съвършено"
(define (perfect? n)
  (= (divisors-sum n) (* 2 n)))

; Зад.5 - Да се провери дали число е просто
(define (prime? n)
  (define sqn (sqrt n))
  (define (helper i)
    (cond [(> i sqn) #t]
          [(= (remainder n i) 0) #f]
          [else (helper (+ i 1))]))
  (if (= n 1)
      #f
      (helper 2)))

; Зад.6 - Да се провери дали цифрите на число са в строго нарастващ ред
(define (increasing? n)
  (define (getLast n) (remainder n 10))
  (define (get2ndLast n) (getLast (quotient n 10)))
  (cond [(< n 10) #t]
        [(>= (get2ndLast n) (getLast n)) #f]
        [else (increasing? (quotient n 10))]))

; Зад.7 - Да се превърне число от десетична в двоична бройна система
(define (toBinary n)
  (define (helper n res bit)
    (if (= n 0)
        res
        (helper (quotient n 2) (+ res (* (remainder n 2) (expt 10 bit))) (+ bit 1))))
  (helper n 0 0))

; Зад.8 - Да се превърне число от двоична в десетична бройна система
; Забележете приликата между тази и предишната задача. Как може да се
; напише тогава функция, която променя число от p-ична в q-ична бройна система?
(define (toDecimal n)
  (define (helper n res bit)
    (if (= n 0)
        res
        (helper (quotient n 10) (+ res (* (remainder n 10) (expt 2 bit))) (+ bit 1))))
  (helper n 0 0))
