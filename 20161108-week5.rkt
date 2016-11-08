#lang racket
; Зад.1. Дължина на списък - рекурсивен (закоментиран) и итеративен вариант
(define (length* lst)
  ;(if (null? lst) 0
  ;    (+ 1 (length* (cdr lst)))))
  (define (helper lst res)
    (if (null? lst) res
        (helper (cdr lst) (+ res 1))))
  (helper lst 0))

; Зад.1. Обръщане на списък - рекурсивен (О(n^2)) и итеративен (O(n)) вариант
(define (reverse* lst)
  ;(if (null? lst)
  ;    '()
  ;    (append (reverse (cdr lst)) (list (car l)))
  (define (helper lst res)
    (if (null? lst)
        res
        (helper (cdr lst) (cons (car lst) res))))
  (helper lst '()))

; Зад.2. Взимане на индекс от списък
(define (nth n lst)
  (cond [(or (null? lst) (< n 0)) #f]
        [(= n 0) (car lst)]
        [else (nth (- n 1) (cdr lst))]))

; Зад.3. Взимане на индекс от списък
(define (range from to)
  (if (> from to)
      '()
      (cons from (range (+ from 1) to))))

; Зад.4. Списък от цифрите на число
(define (digit-list n)
  (define (helper n res)
    (if (< n 10)
        (cons n res)
        (helper (quotient n 10) (cons (remainder n 10) res))))
  (helper n '()))

; Зад.5. Взимане/премахване на няколко елементи от списък
(define (take* n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst)
            (take* (- n 1) (cdr lst)))))

(define (drop* n lst)
  (if (or (= n 0) (null? lst))
      lst
      (drop* (- n 1) (cdr lst))))

; Зад.6. Разцепване на списък на парчета
(define (chunk n lst)
  (if (null? lst)
      '()
      (cons (take* n lst)
            (chunk n (drop* n lst)))))

; Зад.7. - Стандартни функции за проверка на предикат в списък
(define (all p? lst)
  (cond [(null? lst) #t]
        [(not (p? (car lst))) #f]
        [else (all p? (cdr lst))]))

;(define (any p? lst)
;  (cond [(null? lst) #f]
;        [(p? (car lst)) #t]
;        [else (any p? (cdr lst))]))

(define (any p? lst)
  (not (all (lambda (x) (not (p? x))) lst)))

; Бонус: foldr
(define (foldr* op nv lst)
  (if (null? lst)
      nv
      (op (car lst)
          (foldr* op nv (cdr lst)))))

; Зад.8. zip
(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (cons (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

; Зад.9.
(define (remove-first val lst)
  (cond [(null? lst) '()]
        [(equal? val (car lst)) (cdr lst)]
        [else (cons (car lst) (remove-first val (cdr lst)))]))

(define (remove-all val lst)
  (filter (lambda (x) (not (equal? val x))) lst))

; Зад.10.
(define (sum-of-sums lst)
  (define (sum l) (foldr + 0 l))
  (map sum lst))

; Зад.11.
(define (extract-ints lst)
  (filter integer? lst))

; Зад.12.
(define (insert val lst)
  (if (or (null? lst) (< val (car lst)))
      (cons val lst)
      (cons (car lst) (insert val (cdr lst)))))

; Зад.13.
(define (insertion-sort lst)
  (if (null? lst)
      '()
      (insert (car lst) (insertion-sort (cdr lst)))))

(define (insertion-sort* lst) ; за ентусиастите
  (foldr insert '() lst))

; Зад.14. Функция, която брои с колко аргументи е извикана
(define (my-arity . xs)
  (length xs)) ; тук xs вече е списък

; Зад.15. Композиция на произволен брой функции
(define (compose . fns)
  (if (null? fns)
      (lambda (x) x)
      (lambda (x) ((car fns) ((apply compose (cdr fns)) x)))))

; Зад.16. За ентусиастите
; 1. '(1 2 3 4 5)
; -> '((1 . #f) (2 . #t) (3 . #f) (4 . #t) (5 . #f))
(define (makePairs f lst)
  (map (lambda (x) (cons x (f x))) lst))

; 2. '((1 . #f) (2 . #t) (3 . #f) (4 . #t) (5 . #f))
; -> '( ((1 . #f) (3 . #f) (5 . #f))
;       ((2 . #t) (4 . #t)) )
(define (groupPairs lst)
  (let* ((predicate (lambda (x) (equal? (cdr (car lst)) (cdr x))))
         (firsts (filter predicate lst))
         (rest (filter (lambda (x) (not (predicate x))) lst)))
    (if (null? lst)
        '()
        (cons firsts (groupPairs rest)))))

; 3. '( ((1 . #f) (3 . #f) (5 . #f))
;       ((2 . #t) (4 . #t)) )
; -> '((#f (1 3 5)) (#t (2 4)))
(define (combineGroups lst)
  (map (lambda (l) (list (cdar l) (map car l))) lst))

(define (group-by-f f lst)
  (combineGroups (groupPairs (makePairs f lst))))
; haskell: group-by-f f = combineGroups . groupPairs . makePairs f

; iei
(define (group-by-f* f lst)
  (let* ((predicate (lambda (x) (equal? (f x) (f (car lst)))))
         (firsts (filter predicate lst))
         (rest (filter (lambda (x) (not (predicate x))) lst)))
    (if (null? lst)
        '()
        (cons (list (f (car lst)) firsts)
              (group-by-f* f rest)))))
