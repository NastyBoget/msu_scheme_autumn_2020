#lang scheme/base

(define (first)
  (define (sum-of-squares x y) (+ (* x x) (* y y)))
  (let((a (read))
       (b (read))
       (c (read)))
    (cond ((and (> a b) (> c b)) (sum-of-squares a c))
          ((and (> a c) (> b c)) (sum-of-squares a b))
          (else (sum-of-squares c b)))
))

(define (second)
  (define (p) (p))

  (define (test x y)
    (if (= x 0)
        0
        y))
  (test 0 (p)))

(define (third)
  (define (new-if predicate then-clause else-clause) (cond (predicate then-clause)
                                                           (else else-clause)))
  (define (square x) (* x x))
  (define (sqrt-iter guess x) (new-if (good-enough? guess x)
                                      guess
                                      (sqrt-iter (improve guess x)
                                                 x)))
  (define (improve guess x) (average guess (/ x guess)))
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (sqrt x) (sqrt-iter 1.0 x))
  (sqrt 9)
  )

(define (fourth)
  (define (new-if predicate then-clause else-clause) (cond (predicate then-clause)
                                                           (else else-clause)))
  (new-if #t (print 1) (print 2))
)

(define (fifth)
  (define (accumulate combiner null-val term a next b)
    (let loop ((a a) (result null-val)) (if (> a b)
                                            result
                                            (loop (next a) (combiner (term a) result))))
    )

  (define (enumerate-interval a b)
    (accumulate (lambda (x y)(append y x)) '() list a (lambda (x)(+ x 1)) b))

  (define (fib n)
    (define (loop i fib-n-1 fib-n-2)
      (if (= i 0) fib-n-2
          (loop (- i 1) (+ fib-n-1 fib-n-2) fib-n-1)))
    (loop n 1 0)
    )

; 1.а) Дайте определение функции (list-fib-squares-a n)
; которое будет порождать линейно-итерационный процесс
; и будет использовать при этом уместные функции высшего порядка для обработки списков: map, foldr, foldl, ...
; (Вы должны выбрать подходящие).
; 1.b) Дайте определение функции (list-fib-squares-b n) которое будет порождать линейно-итерационный процесс
; и будет использовать при этом только свёртку.
  ; неэффективная реализация
  (define (list-fib-squares n)
    (map (lambda (x)
           (let ((temp (fib x))) (* temp temp)))
         (enumerate-interval 1 n)))

  ; итерационная реализация
  (define (list-fib-squares-a n)
    (map (lambda (x) (* x x))
         (let loop ((i n) (fib-n-1 1) (fib-n-2 0) (result null))
           (if (= i 0) (reverse result)
               (loop (- i 1) (+ fib-n-1 fib-n-2) fib-n-1 (cons fib-n-1 result)))
           )
         )
    )

  ; итерационная реализация со сверткой
  (define (list-fib-squares-b n)
    (reverse (cddr (foldl
                    (lambda (x y)
                      (let* ((fib-n-2 (cadr y)) (fib-n-1 (car y)) (fib-n (+ fib-n-1 fib-n-2)))
                        (list* fib-n fib-n-1 (* fib-n-1 fib-n-1) (cddr y))))
                    '(1 0) (build-list n (lambda (x) 1)))))
    )
; Функция (process lst) получает непустой список списков lst и возвращает список,
; составленный из следующих по порядку всех списков-элементов lst,
; сумма элементов которых больше произведения элементов первого списка-элемента lst.
; Пусть сумма пустого списка равна 0. Пусть произведение пустого списка равно 1.
; Реализация должна быть эффективной и использующей уместные функции высшего порядка для обработки списков.
  (define (process lst)
    (if (null? lst) null
        (let ((prod-first (foldl * 1 (car lst))))
          (filter (lambda (x)(< prod-first (foldl + 0 x))) (cdr lst)))
        )
    )
  
  (print (list-fib-squares 10))
  (newline)
  (print (list-fib-squares-a 10))
  (newline)
  (print (list-fib-squares-b 10))
  (newline)
  (print (process '((5) (1 2) () (3 4) (2 3) (2 3 4))))
  )

;Функция (colinear? x1 y1 z1 x2 y2 z2) для целых чисел x1, y1, z1, x2, y2, z2,
;являющихся координатами двух ненулевых векторов v1 и v2 в 3х-мерном пространстве,
;возвращает #t, если они коллинеарны, а иначе – #f. Примеры:
;(colinear? 1 1 1 2 2 2)    => #t
;(colinear? 2 3 4 1 2 3)    => #f
;(colinear? 1 0 1 -2 0 -2)  => #t

(define (sixth)
  (define (colinear? x1 y1 z1 x2 y2 z2)
    (and (= (- (* y1 z2) (* z1 y2)) 0)
         (= (- (* x1 z2) (* z1 x2)) 0)
         (= (- (* x1 y2) (* y1 x2)) 0)) ; векторное произведение = 0
    )
  (print (colinear? 1 1 1 2 2 2))
  (newline)
  (print (colinear? 2 3 4 1 2 3))
  (newline)
  (print (colinear? 1 0 1 -2 0 -2))
  )

;Функция (odd-fib-list n) для положительного натурального n возвращает список из первых n чисел в ряду нечётных чисел Фибоначчи:
;1 1 3 5 … , а для остальных n – пустой список. Числа в списке должны идти по неубыванию.
;Дайте два определения этой функции:
;a) определение, порождающее линейно итеративный процесс, записанное с именованным let;
;b) определение, порождающее линейно рекурсивный процесс.

(define (seventh)
  (define (odd-fib-list-iter n) ; iteration
    (let loop ((i n) (fib-n-1 1) (fib-n-2 0) (result null))
      (if (or (= i 0) (< i 0)) (reverse result)
          (if (odd? fib-n-2) 
              (loop (- i 1) (+ fib-n-1 fib-n-2) fib-n-1 (cons fib-n-2 result))
              (loop i (+ fib-n-1 fib-n-2) fib-n-1 result)))
      )
    )
  (define (odd-fib-list-rec n) ; recursion
    (let loop ((i n) (fib-n-1 1) (fib-n-2 0))
      (if (or (= i 0) (< i 0)) null
          (if (odd? fib-n-2) 
              (cons fib-n-2 (loop (- i 1) (+ fib-n-1 fib-n-2) fib-n-1))
              (loop i (+ fib-n-1 fib-n-2) fib-n-1))
          )
      )
    )
  
  (print (odd-fib-list-iter 10))
  (newline)
  (print (odd-fib-list-rec 10))
  )

;Опишите функцию (task-4-2020 h), которая для целых неотрицательных h возвращает совершенное двоичное дерево высоты h, такое что,
;при каждой его вершине хранится число факториалу числа, равного расстоянию от этой вершины до корня дерева.
;Положим, что высота пустого дерева равна 0, высота дерева из одного листа равна 1 и т. д.. 0!=1, 1!=1, 2!=2б 3!=6 и т. д..
;В решении используйте функции работы с деревьями, введённые на лекции, реализующие векторное представление дерева.
;Решение должно быть эффективным по вычислениям.
;Созданное дерево должно быть таковым согласно стрелочной диаграмме, а не только согласно внешнему представлению, выводимому интерпретатором.
;Примеры:
;(task-4-2020 0) => #()
;(task-4-2020 1) => #(1 #() #())
;(task-4-2020 2) => #(1 #(1 #() #()) #(1 #() #()))
;(task-4-2020 3) => #(1 #(1 #(2 #() #()) #(2 #() #())) #(1 #(2 #() #()) #(2 #() #())))

(require racket/vector)

(define (anketa-3-4-task-1)
  (define empty-tree #())
  (define make-tree vector)
  (define (task-4-2020 h)
    (let loop ((h h) (iter 0) (fact 1))
      (if (> h 0)
          (let* ((next-h (- h 1)) (next-iter (+ iter 1)) (next-fact (* fact next-iter)))
            (make-tree fact (loop next-h next-iter next-fact) (loop next-h next-iter next-fact)))
          empty-tree
          )
      )
    )
  (print (task-4-2020 0))
  (newline)
  (print (task-4-2020 1))
  (newline)
  (print (task-4-2020 2))
  (newline)
  (print (task-4-2020 3))
  (newline)
  (print (task-4-2020 4))
  )

;С помощью уместных функций высшего порядка (map, foldl, foldr, filter, …) опишите функцию (task-03-2020 lst),
;принимающую список из двух или более чем двух непустых подсписков чисел.
;Функция вычисляет среднее геометрическое каждого подсписка, затем получившийся список средних рассматривает как геометрический вектор и возвращает его длину
;[длина понимается в геометрическом смысле, в алгебраическом смысле это норма].
;Пример:
;(task-03-2020 (list (list 2 4 1) (list 2 2) (list 1 1 1 1 1 1 1))) => 3

(define (anketa-3-4-task-2)
  ; плохие случаи не рассматриваю
  (define (task-03-2020 lst)
    (sqrt
     (foldl (lambda (x y) (+ y (* x x))) ; сумма квадратов
            0
            (map (lambda (x) (expt (foldl * 1 x) (/ 1 (length x)))) ; среднее геометрическое
                 lst)))
    )
  (task-03-2020 (list (list 2 4 1) (list 2 2) (list 1 1 1 1 1 1 1)))
  )