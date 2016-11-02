#lang racket
; Предефиниране на if чрез and и or
(define (if2 t x y)
  (or (and t x) y))

; Предефиниране на and, or и not чрез if
(define (and2 x y)
  (if x y #f))

(define (or2 x y)
  (if x #t y))

(define (not2 x)
  (if x #f #t))

; Рекурсивна процедура за намиране на n-тото число на Фибоначи
(define (fib n)
  ;(if (< n 2) n
  ;    (+ (fib (- n 1)) (fib (- n 2)))))
  (cond [(or (negative? n) (not (integer? n))) #f]
        [(< n 2) n]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))

; Намиране на брой корени на квадратно уравнение
; по дадени коефициенти
(define (roots a b c)
  (define d (- (* b b) (* 4 a c)))
  (cond [(and (= a 0) (= b 0)) 0]
        [(or (= a 0) (= d 0)) 1]
        [(> d 0) 2]
        [else 0])
)

; Изчисляване на биномен коефициент (n над k)
(define (nchoosek n k)
  (if (or (= k 0) (= k n))
      1
      (+ (nchoosek (- n 1) k)
         (nchoosek (- n 1) (- k 1)))))

; Рекурсивна процедура за изчисляване на n!
(define (fact n)
  (if (< n 2) 1
      (* n (fact (- n 1)))))

; Директна формула за изчисление на биномния коефициент
(define (nchk2 n k)
  (/ (fact n)
     (* (fact k) (fact (- n k)))))

; "Бързо" степенуване: T(n)=O(lgn), работи само за n - естествено число
(define (fast-exp x n)
  (define (sq x) (* x x))
  (define k (quotient n 2))
  (cond [(zero? n) 1]
        [(even? n) (sq (fast-exp x k))]
        [else (* (sq (fast-exp x k)) x)])
)

; Обща функция за бързо степенуване, която работи с всякакви степени
(define (fast-exp2 x n)
  (cond [(and (positive? n) (integer? n)) (fast-exp x n)]
        [(integer? n) (/ 1 (fast-exp x (- n)))]
        [else (* (fast-exp2 x (round n))
                 (expt x (- n (round n))))]))
