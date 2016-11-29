#lang racket
; функция, която гарантирано ни връща булева стойност
(define (member? x lst)
  (if (member x lst) #t #f))

; маък граф, над който ще тестваме всички функции
(define G '((a b c d) ; от а има ребра към b,c,d
            (b e f)   ; може да бъде и ориентиран
            (c a d)
            (d b c g)
            (e)       ; връх без наследници
            (f b e)
            (g a)))

; Зад.1. - взимане на всики върхове в граф
(define (vertices g)
  (map car g))

; взимане на всички наследници на даден връх в граф
(define (successors v g)
  (let [(result (assoc v g))]
    (if result (cdr result) '())))

; проверка дали съществува ребро между два върха в графа
; заради member и assoc работи коректно дори когато върховете са несъществуващи
(define (has-edge? u v g)
  (member? v (successors u g)))

; за да добавяме ребро, първо трябва да добавим върховете му
; естествено, не добавяме един връх повече от веднъж
(define (add-vertex v g)
  (if (member? v (vertices g))
      g
      (cons (list v) g)))

; Зад.2.
(define (add-edge u v g)
  (if (has-edge? u v g)
      g
      (let [(newg (add-vertex u (add-vertex v g)))]
        (map (lambda (l) (if (equal? (car l) u)
                             (append l (list v))
                             l))
             newg))))

; след като можем да добавяме едно ребро, защо да не можем
; да направим граф от цял списък с ребра?
(define (make-from-edges lst)
  ;(if (null? lst)
  ;    '()
  ;    (add-edge (caar lst) (cdar lst)
  ;              (make-from-edges (cdr lst)))))
  (foldr (lambda (e g) (add-edge (car e)
                                 (cdr e)
                                 g))
         '()
         lst))

; Зад.3. - проверка за съдържане на път в графа
(define (contains-path? path g)
  (cond [(null? path) #t]
        [(null? (cdr path)) (member? (car path) (vertices g))]
        [(has-edge? (car path) (cadr path) g) (contains-path? (cdr path) g)]
        [else #f]))

; (1 2 3 4) -> ((1 . 2) (2 . 3) (3 . 4))
; (zip '(1 2 3) '(4 5 6 7 8)) -> '((1 . 4) (2 . 5) (3 . 6))
; (zip path (cdr path))
(define (contains-path?? path g)
  (define (make-pairs path) #f) ; това трябва да е гореописаната функция
  (null? (filter (lambda (e) (not (has-edge? (car e)
                                             (cdr e)
                                             g)))
                 (make-pairs path))))

; бонус - map и filter чрез foldr
(define (map* f lst)
  (foldr (lambda (x l) (cons (f x) l)) '() lst))

(define (filter* p? lst)
  (foldr (lambda (x l) (if (p? x)
                           (cons x l)
                           l))
         '()
         lst))

; Зад.4. - предшественици на връх
(define (predecessors v g)
  (filter (lambda (u) (has-edge? u v g)) (vertices g)))

; Зад.5. - разширение на път с едно ребро.
; Ако пътят е празен, значи връща всички възможни пътищв с дължина 0 (т.е. един връх)
(define (extend-path path g)
  (if (null? path)
      (map list (vertices g))
      (map (lambda (v) (append path (list v)))
           (filter (lambda (v) (not (member? v path)))
                   (successors (last path) g)))))

; "разширяване" на цял списък от пътища - получаваме друг списък от пътища
(define (extend-paths paths g)
  (apply append (map (lambda (p) (extend-path p g)) paths)))

; Зад.7. - списък с всичките ребра на граф
(define (edge-list g)
  (define (make-pairs-single l) (map (lambda (v) (cons (car l) v))
                                     (cdr l)))
  (apply append (map make-pairs-single g)))

; Зад.8. - обръщане на всички ребра в граф
(define (invert g)
  (define (flip p) (cons (cdr p) (car p)))
  (make-from-edges (map flip (edge-list g))))

; Зад.9. - обхождане в широчина, използвайки помощна функция, която на всяка
; "итерация" пази текущото "ниво" и изчислява следващото с друга отделна функция (за удобство)
(define (bfs v g)
  (define (get-next-level current visited)
    (apply append (map (lambda (v) (filter (lambda (v) (not (member? v visited)))
                                   (successors v g)))
                       current)))
  (define (helper current result)
    (let [(next (get-next-level current result))]
      (if (null? next)
          result
          (helper next (append result next)))))
  (helper (list v) (list v)))




