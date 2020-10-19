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

; печать всех листьев дерева
; (fringe-cps #(10 #(21 #(31 #() #()) #(32 #() #())) #(22 #() #(34 #() #()))) (lambda (x) x))
(define (fringe-cps t cc)
  (define (tree-data tree) (vector-ref tree 0))
  (define (tree-left tree) (vector-ref tree 1))
  (define (tree-right tree) (vector-ref tree 2))
  (define (tree-empty? t) (equal? t #()))
  (cond ((tree-empty? t) (cc '())) ((and (tree-empty? (tree-left t))
                                         (tree-empty? (tree-right t))) (cc (list (tree-data t))))
        (else (fringe-cps (tree-right t)
                           (lambda (z) (fringe-cps (tree-left t)
                                                    (lambda (y) (begin
                                                                  (print "y")
                                                                  (print y)
                                                                  (print "z")
                                                                  (print z)
                                                                  (newline)
                                                                  (cc (append y z)))))))))
  )

(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (tree-empty? t) (equal? t #()))

; обход дерева в ширину
; (width-walk  #(10 #(21 #(31 #() #()) #(32 #() #())) #(22 #() #(34 #() #()))))
(define (width-walk tree)
  (let walk ((queue (list tree)))
    (if (null? queue)
        null
        (begin
          (if (not (tree-empty? (car queue)))
              (begin (print (tree-data (car queue)))
                     (if (null? (cdr queue)) (newline)
                         (display " "))
                     (walk
                      (append (cdr queue) (list (tree-left (car queue)) (tree-right (car queue))))
                      ))
              (begin
                (walk (cdr queue)))     
              )
          )
        )
    )
  )

; (print-tree  #(10 #(21 #(31 #() #()) #(32 #() #())) #(22 #() #(34 #() #()))))
; (print-tree #(1 #(2 #(4 #(8 #() #()) #(9 #() #())) #(5 #() #())) #(3 #(6 #() #()) #(7 #() #()))))
; проход каждый раз до нужного уровня от корня
; рекурсивная версия
(define (print-tree tree)
  (define (print-list lst)
    (if (null? lst)
        (display "\n")
        (begin (display (car lst))
               (display " ")
               (print-list (cdr lst))))
    )
  (let loop ((depth 0))
    (let ((level-list (let print-level ((tree tree) (depth depth) (cur-depth 0))
                        (cond ((or (> cur-depth depth) (tree-empty? tree)) '())
                              ((= depth cur-depth)(list (tree-data tree)))
                              (else (append (print-level (tree-left tree) depth (+ cur-depth 1))
                                            (print-level (tree-right tree) depth (+ cur-depth 1)))))
                        )
                      ))
      (if (null? level-list) (display "")
          (begin
            (print-list level-list)
            (loop (+ depth 1))))
      )
    )
  )

;Опишите функцию (print-tree-by-level-asc tree).
;Функция получает двоичное дерево в векторном представлении и печатает его по уровням --
;каждый уровень на отдельной строке, значения при вершинах уровня слева направо через пробелы --
;в порядке возрастания номеров уровней. Используйте стиль передачи остаточных вычислений,
;чтобы всюду рекурсия была хвостовой.
;Примеры:
;(print-tree-by-level-asc #()) ->
;
;(print-tree-by-level-asc #(1 #() #())) ->
;1
;(print-tree-by-level-asc #(10 #(21 #() #()) #(22 #() #()))) ->
;10
;21 22
;(print-tree-by-level-asc #(10 #(21 #(31 #() #()) #(32 #() #())) #(22 #() #(34 #() #())))) ->
;10
;21 22
;31 32 34

(require racket/vector)

(define (anketa-5)
  (define (tree-data tree) (vector-ref tree 0))
  (define (tree-left tree) (vector-ref tree 1))
  (define (tree-right tree) (vector-ref tree 2))
  (define (tree-empty? t) (equal? t #()))
  ; в стиле передачи продолжений
  (define (print-tree-by-level-asc tree)
    (define (print-list lst)
      (if (null? lst)
          (display "\n")
          (begin (display (car lst))
                 (display " ")
                 (print-list (cdr lst))))
      )
    (let loop ((depth 0))
      (let ((level-list (let print-level ((tree tree) (cur-depth 0) (cc (lambda (x) x)))
                          (cond ((or (> cur-depth depth) (tree-empty? tree)) (cc '()))
                                ((= depth cur-depth)(cc (list (tree-data tree))))
                                (else (print-level
                                       (tree-left tree) (+ cur-depth 1)
                                       (lambda (y) (print-level (tree-right tree) (+ cur-depth 1)
                                                                (lambda (z) (cc (append y z)))))))))))
        (if (null? level-list) (display "")
            (begin
              (print-list level-list)
              (loop (+ depth 1))))
        )
      )
    )
  
  (print-tree-by-level-asc #())
  (newline)
  (print-tree-by-level-asc #(1 #() #()))
  (newline)
  (print-tree-by-level-asc #(10 #(21 #() #()) #(22 #() #())))
  (newline)
  (print-tree-by-level-asc #(10 #(21 #(31 #() #()) #(32 #() #())) #(22 #() #(34 #() #()))))
  (newline)
  (print-tree-by-level-asc #(1 #(2 #(4 #(8 #() #()) #(9 #() #())) #(5 #() #())) #(3 #(6 #() #()) #(7 #() #()))))
  )


;Релизуйте очередь на основе мутируемых пар:
;конструктор очереди (make-queue)
;селектор очереди (front-queue q)
;мутаторы очереди:
;   (insert-queue! q e)
;   (delete-queue! q)
;чеккеры очереди:
;   (queue? q) 
;   (empty-queue? q)
;Сложность вставки в очередь должна быть константой!
(require scheme/mpair)
;('queue head tail)
(define (anketa-6-1)
  (define (make-queue) (mcons 'queue (mcons null null)))
  (define (front-queue q)
    (if (and (queue? q) (not (empty-queue? q)))
        (mcar (mcar (mcdr q)))
        "empty queue")
    )
  (define (insert-queue! q e)
    (if (queue? q)
        (begin
          {if (empty-queue? q)
              (begin
                (set-mcdr! (mcdr q) (mcons e null))
                (set-mcar! (mcdr q) (mcdr (mcdr q))))
              (begin
                (set-mcdr! (mcdr (mcdr q)) (mcons e null))
                (set-mcdr! (mcdr q) (mcdr (mcdr (mcdr q)))))})
        q))
  (define (delete-queue! q)
    (if (and (queue? q) (not (empty-queue? q)))
        (begin
          (if (null? (mcdr (mcar (mcdr q))))
              (begin
                (set-mcdr! (mcdr q) null)
                (set-mcar! (mcdr q) null))
              (set-mcar! (mcdr q)
                     (mcdr (mcar (mcdr q))))))
        q)
    )
  (define (queue? q) (and (mpair? q) (eq? 'queue (mcar q)) (mpair? (mcdr q))))
  (define (empty-queue? q) (and (queue? q) (null? (mcar (mcdr q)))))
  (define ex (make-queue))
  (print ex)
  (newline)
  (insert-queue! ex 1)
  (print ex)
  (newline)
  (insert-queue! ex 2)
  (print ex)
  (newline)
  (insert-queue! ex 3)
  (print ex)
  (newline)
  (print '(1))
  (print (front-queue ex))
  (newline)
  (delete-queue! ex)
  (print ex)
  (newline)
  (print '(2))
  (print (front-queue ex))
  (newline)
  (delete-queue! ex)
  (print ex)
  (newline)
  (print '(3))
  (print (front-queue ex))
  (newline)
  (delete-queue! ex)
  (print (front-queue ex))
  (newline)
  (print ex)
  (newline)
  (insert-queue! ex 4)
  (print ex)
  (newline)
  (insert-queue! ex 5)
  (print ex)
  (newline)
  (delete-queue! ex)
  (print ex)
  (newline)
  (insert-queue! ex 6)
  (print ex)
  (newline)
  (delete-queue! ex)
  (print ex)
  (newline)
  (print (empty-queue? ex))
  (newline)
  (delete-queue! ex)
  (print ex)
  (newline)
  (print (empty-queue? ex))
  (newline)
  )

;Рассмотрим функцию (odd-abundant n), возвращающую n-й элемент последовательности нечётных избыточных чисел:
;945, 1575, 2205, 2835, 3465, 4095, … .
;Избыточным является любое натуральное число X, у которого сумма всех его натуральных делителей превышает 2*X.
;Составьте код, реализующий (odd-abundant n) без мемоизации и (memo-odd-abundant n) с мемоизацией,
;используя хеш-таблицы Racket-а:
;(make-hash alist) – создание таблицы и заполнение её элементами по ассоциативному списку пар;
;(hash-set! tbl key val) – вставка;
;(hash-ref tbl key #f) – поиск, возвращающий #f при неуспехе.
;Даёт ли мемоизированная версия выигрыш по сравнению с обычной? Если даёт, то какой и при каких условиях?
(define (anketa-6-2)
  ; вариант первый: зная, что первое число = 945,
  ; все остальные получаем прибавляя 2 к предыдущему,
  ; вычисляя разложение числа и сравнивая сумму делителей с числом 
  (define (odd-abundant n) ; n > 0
    ; получение списка делителей нечетного числа (за исключением самого числа)
    (define (get-dividers x)
      (filter (lambda (y) (= (remainder x y) 0)) (build-list (/ (- x 1) 2) add1))
      )
    (let loop ((cur-num 945) (iter n))
      (if (= iter 1)
          cur-num
          (let next-num ((prev-num (+ cur-num 2)))
            (if (> (foldl + 0 (get-dividers prev-num)) prev-num)
                (loop prev-num (- iter 1))
                (next-num (+ prev-num 2))
                )
            )
          )
      )
    )
  ; вариант второй - с мемоизацией:
  ; пытаемся найти номер числа в хеш-таблице,
  ; если не нашли, ищем число с наибольшим номером, который меньше требуемого.
  ; таблица для сохранения нечетных избыточных чисел
  (define odd-abundant-table (make-hash (list (cons 1 945))))
  (define (memo-odd-abundant n) ; n > 0
    ; получение списка делителей нечетного числа (за исключением самого числа)
    (define (get-dividers x)
      (filter (lambda (y) (= (remainder x y) 0)) (build-list (/ (- x 1) 2) add1))
      )
    ; нахождение числа с наибольшим номером, который меньше требуемого
    (define (find-biggest num)
      (let loop ((cur-num num) (res (hash-ref odd-abundant-table num #f)))
        (if res
            (list cur-num res)
            (loop (- cur-num 1) (hash-ref odd-abundant-table (- cur-num 1) #f))
            )
        )
      )
    (let ((res (find-biggest n)))
      (let loop ((cur-num (cadr res)) (iter (car res)))
        (if (= iter n)
            cur-num
            (let next-num ((prev-num (+ cur-num 2)))
              (if (> (foldl + 0 (get-dividers prev-num)) prev-num)
                  (begin
                    (hash-set! odd-abundant-table (add1 iter) prev-num)
                    (loop prev-num (add1 iter)))
                  (next-num (+ prev-num 2))
                  )
              )
            )
        )
      )
    )
  
  (print (odd-abundant 1))
  (newline)
  (print (odd-abundant 2))
  (newline)
  (print (odd-abundant 5))
  (newline)
  (print (odd-abundant 10))
  (newline)
  (print (odd-abundant 12))
  (newline)
  (print (odd-abundant 15))
  (newline)
  (print (odd-abundant 20))
  (newline)
  (newline)
  (print (memo-odd-abundant 1))
  (newline)
  (print (memo-odd-abundant 2))
  (newline)
  (print (memo-odd-abundant 5))
  (newline)
  (print (memo-odd-abundant 10))
  (newline)
  (print (memo-odd-abundant 12))
  (newline)
  (print (memo-odd-abundant 15))
  (newline)
  (print (memo-odd-abundant 20))
  (newline)
  (newline)
  (print (memo-odd-abundant 20))
  (newline)
  (print (memo-odd-abundant 15))
  (newline)
  (print (memo-odd-abundant 12))
  (newline)
  (print (memo-odd-abundant 10))
  (newline)
  (print (memo-odd-abundant 5))
  (newline)
  (print (memo-odd-abundant 2))
  (newline)
  (print (memo-odd-abundant 1))
  )