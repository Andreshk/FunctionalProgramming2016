#lang racket
; "Стандартни" функции за работа с дървета
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))
(define empty-tree '())
(define (make-tree root left right) (list root left right))      ; не искаме просто (define make-tree list) - защо?
(define (make-leaf root) (make-tree root empty-tree empty-tree)) ; за удобство
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

; примерно дърво, над което ще си тестваме
(define test
  (make-tree 3
             (make-tree 1
                        (make-leaf 2)
                        empty-tree)
             (make-tree 5
                        (make-leaf 9)
                        (make-leaf 3))))

; Зад.1.
(define (tree-sum t)
  (if (empty-tree? t)
      0
      (+ (root-tree t)
         (tree-sum (left-tree t))
         (tree-sum (right-tree t)))))

; Помощна функция
(define (tree-height t)
  (if (empty-tree? t)
      -1
      (+ 1 (max (tree-height (left-tree t))
                (tree-height (right-tree t))))))

; Зад.2.
(define (tree-max t)
  (if (empty-tree? t)
      -inf.0
      (max (root-tree t)
           (tree-max (left-tree t))
           (tree-max (right-tree t)))))

; Зад.3.
(define (tree-level k t)
  (cond [(empty-tree? t) '()]
        [(= k 0) (list (root-tree t))]
        [else (append (tree-level (- k 1) (left-tree t))
                      (tree-level (- k 1) (right-tree t)))]))

; Зад.4.
(define (all-levels t)
  (let [(height (tree-height t))]
    (map (lambda (i) (tree-level i t)) (range 0 (+ height 1)))))

; Зад.5.
(define (tree-map f t)
  (if (empty-tree? t)
      empty-tree
      (make-tree (f (root-tree t))
                 (tree-map f (left-tree t))
                 (tree-map f (right-tree t)))))

; Зад.6.
(define (tree->list t)
  (if (empty-tree? t)
      '()
      (append (tree->list (left-tree t))
              (list (root-tree t))
              (tree->list (right-tree t)))))

; Зад.7.
(define (bst-insert val t)
  (cond [(empty-tree? t) (make-leaf val)]
        [(< val (root-tree t)) (make-tree (root-tree t)
                                          (bst-insert val (left-tree t))
                                          (right-tree t))]
        ;[(= val (root-tree t)) t]
        [else (make-tree (root-tree t)
                         (left-tree t)
                         (bst-insert val (right-tree t)))]))

; Вмъкване на списък от стойности в дърво:
; три варианта - рекурсивен, итеративен и fold
(define (list->tree lst)
  ;(if (null? lst)
  ;    empty-tree
  ;    (bst-insert (car lst)
  ;                (list->tree (cdr lst))))
  ;;(define (helper lst result)
  ;;  (if (null? lst)
  ;;      result
  ;;      (helper (cdr lst) (bst-insert (car lst) result))))
  ;;(helper lst empty-tree)
  (foldr bst-insert empty-tree lst))


; Зад.8.
(define (tree-sort lst) (tree->list (list->tree lst)))
;(define tree-sort (compose tree->list list->tree))
; tree-sort = tree->list . list->tree

; Зад.8+.
; решение с проверка на генерирания списък + с интервал от "допустими" стойности
(define (valid-bst? t)
  (define (isNonDecr? lst) ; дали стойностите в списъка са в ненамаляващ ред
    (cond [(or (null? lst) (null? (cdr lst))) #t]
          [(> (car lst) (cadr lst)) #f]
          [else (isNonDecr? (cdr lst))]))
  (isNonDecr? (tree->list t)))

(define (valid-bst?? t)
  (define (helper t from to)
    (cond [(empty-tree? t) #t]
          [(or (< (root-tree) from)
               (> (root-tree) to)) #f]
          [else (helper (left-tree t) from (root-tree t))
                (helper (right-tree t) (root-tree t) to)]))
  (helper t -inf.0 +inf.0))

        
