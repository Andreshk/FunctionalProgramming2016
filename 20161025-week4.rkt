#lang racket
; Обща функция за произведение в интервал - 
; рекурсивна дефиниция + изполвайки accumulate
(define (product a b term next)
  ;(if (> a b)
  ;    1
  ;    (* (term a) (product (next a) b term next))))
  (accum-iter * 1 a b term next))

; Обща функция за сума в интервал - 
; рекурсивна дефиниция + изполвайки accumulate
(define (sum a b term next)
  ;(if (> a b)
  ;    0
  ;    (+ (term a) (sum (next a) b term next)))
  (accum-iter + 0 a b term next))

; Обща функция за сума в интервал - итеративна дефиниция
(define (sum-iter a b term next)
  (define (helper res curr)
    (if (> curr b)
        res
        (helper (+ res (term curr)) (next curr))))
  (helper 0 a))

; "Стандартна" функция от по-висок ред accumulate - рекурсивна дефиниция
(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

; accumulate - итеративна дефиниция.
; Внимавайте - може да даде различни резултати когато op не е комутативна операция!
(define (accum-iter op nv a b term next)
  (define (helper res i)
    (if (> i b)
        res
        (helper (op (term i) res) (next i))))
  (helper nv a))

; Малки, полезни функцийки
(define (id x) x)
(define (++ x) (+ x 1))

; Зад.1 - Изчисляване на n! с accumulate
(define (fact-accum n)
  (product 1 n id ++))

; Зад.2 - Степенуване с accumulate
(define (expt-accum x n)
  ;(define (term i) x)
  ;(product 1 n term ++)
  (product 1 n (lambda (i) x) ++)) ; анонимна функция вместо вложена дефиниция

; Зад.3 - Намиране на брой делители на число в интервал
(define (count-divisors n a b)
  (define (counter i) (if (= (remainder n i) 0)
                       1
                       0))
  (sum a b counter ++))

; Зад.4 - Изчисляване на x+2x^2+...+nx^n
(define (powers-sum x n)
  (define (term i) (* i (expt x i)))
  (if (and (integer? n)
           (>= n 0))
      (sum 1 n term ++)
      #f))

; Функция от по-висок ред, която намира приближена
; стойност на определен интеграл.
(define (integrate f a b)
  (define h 0.001)
  (sum a
       (- b h)
       (lambda (a) (* (f a) h))
       (lambda (i) (+ i h))))

; Зад.5 - Изчисление на биномен коефициент (3 еквивалентни варианта)
(define (combinations n k)
  (define (term i) (/ (- (+ n 1) i) i))
  (product 1 k term ++))
  
(define (combinations2 n k)
  (define (term i) (/ (- (+ n i) k) i))
  (product 1 k term ++))

(define (combinations3 n k)
  (/ (product (- (+ n 1) k) n id ++)
     (product 1 k id ++)))

; Относно синтаксиса на lambda-функциите
;(define (f x y) (* x y))
;(define f (lambda (x y) (* x y)))
;(f 2 3)
;((lambda (x y) (* x y)) 2 3)

; Зад.6 - проверка дали число е просто, използвайки
; "насъбиране" на логически стойности.
(define (prime-accum n)
  (define (term i) (not (= (remainder n i) 0)))
  (define (and2 x y) (and x y))
  (and (> n 1)
       (accum-iter and2 #t 2 (- n 1) term ++)))

; Зад.7 - изчисляване на n!!
(define (!! n)
  (define (2+ x) (+ x 2))
  (define a (if (odd? n) 1 2))
  ;(define a (- 2 (remainder n 2)))
  (product a n id 2+))

; Зад.8 и Зад.9 са съответно дефинициите на sum-iter и accum-iter по-горе.

; Зад.10 - Производна на функция
(define (derivative f)
  (define h 0.0000001)
  (lambda (x) (/ (- (f (+ x h)) (f x)) h)))

;(define id-prim (derivative id))
;(id-prim 5)  ; т.к. derivative връща функция, то id-prim
              ; е функция и я извикваме като такава
;((derivative id) 5) ; същото като горния ред
;(define (f x) (+ (* x x) (* 2 x)))
;((derivative f) 5)
;(integrate (derivative f) 1 10)

; Зад.11 - Функция-константа (забележете, че върнатият резултат е функция)
(define (constantly c)
  (lambda (x) c))

; Зад.12 - Обръщане на аргументите на функция (отново връщаме нова функция)
(define (flip f)
  (lambda (x y) (f y x)))

; Зад.13 - Композиция на функции (няма смисъл да казваме, че е функция)
(define (compose f g)
  (lambda (x) (f (g x))))
  
;(define 2+ (compose ++ ++))
;(2+ 10) ;-> 12

; Зад.14 - Отрицание на предикат
(define (complement p?)
  (lambda (x) (not (p? x))))

(define (my-even? x) (= (remainder x 2) 0))
(define my-odd? (complement my-even?))
