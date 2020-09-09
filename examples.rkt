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

  ; неэффективная реализация
  (define (list-fib-squares-1 n)
    (map (lambda (x)
           (let ((temp (fib x))) (* temp temp)))
         (enumerate-interval 1 n)))

  ; итерационная реализация
  (define (list-fib-squares-2 n)
    (let loop ((i n) (fib-n-1 1) (fib-n-2 0) (num 1) (result null))
      (if (= i 0) (reverse result)
          (let ((next-num (+ num 1)))
                (loop (- i 1) (fib next-num) fib-n-1 next-num (cons (* fib-n-1 fib-n-1) result))))
          ))

  ; итерационная реализация со сверткой
  (define (list-fib-squares-3 n)
    (foldl (lambda (x result)
             ((if (null? result) (cons 1 result)
                  (begin ()
                  (cons (* fib-n-1 fib-n-1) result)))))
             null (enumerate-interval 1 n))
    )

  (define (process lst)
    (let ((prod-first (foldl * 1 (car lst))))
      (filter (lambda (x)(< prod-first (foldl + 0 x))) lst)))
  
  (print (list-fib-squares-1 10))
  (newline)
  (print (list-fib-squares-2 10))
  (newline)
  (print (process '((5) (1 2) () (3 4) (2 3) (2 3 4))))
  )