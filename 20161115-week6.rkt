#lang racket
(define (accumulate op nv term next a b)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv term next (next a) b))))

; Зад.1 (Вар.А) - две решения
; "броене" с accumulate - функцията term връща само единици и нули, после сумираме
(define (meetTwice? f g a b)
  (>= (accumulate + 0 (lambda (i) (if (= (f i) (g i)) 1 0))
                  (lambda (x) (+ x 1))
                  a b)
      2))

; да генерираме списък от числата в интервала...
(define (range a b)
  ;(if (> a b) '() (cons a (range (+ a 1) b))))
  (accumulate cons '() (lambda (i) i) (lambda (x) (+ x 1)) a b))

; и после да видим дължината му
(define (meetTwice?? f g a b)
  (>= (length (filter (lambda (x) (= ((f x) (g x))) (range a b)))
      2)))

; Зад.2 (Вар.А) - също няколко подхода
(define (is-duplicate? x lst) ; сравняваме с equal?, понеже може и да не са числа
  (> (length (filter (lambda (y) (equal? x y)) lst)) 1))

;(define (find-duplicates lst)
  ;(filter (lambda (x) (is-duplicate? x lst)) lst))

; за премахване на всички срещания на число в списък
(define (remove-all x lst)
  (filter (lambda (y) (not (equal? x y))) lst))

; намиране на всички повтарящи се елементи в списък
(define (find-duplicates lst)
  (cond [(null? lst) '()]
        [(is-duplicate? (car lst) lst) (cons (car lst)
                                             (find-duplicates (remove-all (car lst) (cdr lst))))]
        [else (find-duplicates (cdr lst))]))

(define (max-duplicate ll)
  (let [(temp (apply append (map find-duplicates ll)))]
    (if (null? temp) #f (apply max temp))))

; стандартни функции
(define (all? p? lst)
  (cond [(null? lst) #t]
        [(not (p? (car lst))) #f]
        [else (all? p? (cdr lst))]))

(define (any? p? lst)
  (not (all? (lambda (x) (not (p? x))) lst)))

; дали списък съдържа повече от едно число, кратно на k
(define (contains-multiple k lst)
  (any? (lambda (x) (= (remainder x k) 0)) lst))

; Зад.3
(define (checkMatrix? m k)
  (all? (lambda (row) (contains-multiple k row)) m))

; Зад.9.
; ппомощна функция дали списъкът lst1 е префикс на lst2
(define (begins-with? lst1 lst2)
  (cond [(and (null? lst1) (null? lst2)) #t] ; '() '() -> #t
        [(null? lst1) #t]                    ; '() '(1 2 3) -> #t
        [(null? lst2) #f]                    ; '(1 2 3) '() -> #f
        [(equal? (car lst1) (car lst2)) (begins-with? (cdr lst1) (cdr lst2))]
        [else #f]))

(define (sublist? lst1 lst2)
  (cond [(null? lst2) (null? lst1)]
        [(begins-with? lst1 lst2) #t]
        [else (sublist? lst1 (cdr lst2))]))

; Зад.10.
(define (make-set lst)
  (if (null? lst)
      '()
      (cons (car lst) (make-set (remove-all (car lst) (cdr lst))))))

; бонус: най-бързото (за написване) сортиране
(define (quick-sort lst)
  (if (null? lst) '()
      (append (quick-sort (filter (lambda (x) (< x (car lst))) lst))
                          (filter (lambda (x) (= x (car lst))) lst)
              (quick-sort (filter (lambda (x) (> x (car lst))) lst)))))

; Зад.11.
(define (histogram lst)
  ; отново - колко пъти елемент се среща в списък
  (define (count x lst) (length (filter (lambda (y) (equal? x y)) lst)))
  (map (lambda (x) (cons x (count x lst))) (make-set lst)))

; полезни функции за работа с матрици
(define (nth-row n m) (list-ref m n))
(define (nth-col n m) (map (lambda (row) (list-ref row n)) m))
(define (row-count m) (length m))
(define (col-count m) (length (car m)))
(define M '((1 2 3) (4 5 6) (7 8 9)))
; (car m) -> първият ред на матрицата
; (cdr m) -> всичко без първия ред
; (map car m) -> първият стълб на матрицата
; (map cdr m) -> всичко без първия стълб
; (cdr (map cdr m)) -> всичко без първи ред и първи стълб
; (map cdr (cdr m)) -> същото като горното

; Зад.12. - класическа задача
; работи за квадратни и правоъгълни матрици
(define (transpose m)
  (if (null? (car m))
      '()
      (cons (map car m) (transpose (map cdr m)))))

; Зад.13. - триъгълни са:
; ((1 2 3)  , ((1 2 3 4)  , ((1 2)
;  (0 4 5)     (0 5 6 7)     (0 3)
;  (0 0 6))    (0 0 8 9))    (0 0)) и това са всички случаи
(define (triangular? m)
  (define (allZero col) (all? (lambda (x) (= x 0)) col))
  (if (or (null? (car m)) (null? (cdr m))) ; за да обработва коректно и квадратни, и правоъгълни матрици
      #t      ; първата колона, без първия ред
      (and (allZero (cdr (map car m)))
           (triangular? (cdr (map cdr m))))))

; МНОГО полезна функция
(define (null-mat? m)
  (or (null? m) (null? (car m))))

; Зад.14.
(define (main-diag m)
  (if (null-mat? m)
      '()
      (cons (caar m) (main-diag (cdr (map cdr m))))))

; Зад.15. - обръщаме редовете на матрицата огледално и взимаме диагонала на полученото
(define (2nd-diag m)
  (main-diag (map reverse m)))

; Зад.16.
(define (descartes lst1 lst2) ; map-ception
  (apply append (map (lambda (x) (map (lambda (y) (cons x y)) lst2)) lst1)))
