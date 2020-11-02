#lang scheme/base

; TODO
; смешанная стратегия генерации ответов
; генерация реплики из нескольких предложений
; сохранение графов в файл и чтение графов из файла

(require racket/string)
(define in (open-input-file "frankl.txt"))

; (n-1-gramma (hash: (next-word counter)))
(define next-graph (make-hash))

; (n-1-gramma (hash: (prev-word counter)))
(define prev-graph (make-hash))

; graph for sentences beginning
; (n-1-gramma frequency)
(define begin-graph (make-hash))

; graph for sentences ending
; (n-1-gramma frequency)
(define end-graph (make-hash))

; (n-1-gramma frequency)
(define frequency-graph (make-hash))

; size of n-gramm
; should be >= 2
(define n 3)

; overall amount of n-1-gramm
(define n-gram-counter 0)

(define (split-line line)
  (foldl (lambda (x y) (let ((sentence (filter (lambda (z) (not (equal? z ""))) (regexp-split #px"\\s+" x)))) ; разбиваем каждое предложение на слова
                         (if (null? sentence) y (cons sentence y)))) ; пустые предложения удаляются
         null
         (regexp-split #px"[\\.\\?!]" ; разбиваем реплику на предложения
                       (regexp-replace* #px"([;,:'])" ; выделяем знаки препинания
                                        (regexp-replace* #px"[^a-z\\.\\?!;,:'\\s\\-]+"
                                                         (string-downcase line)
                                                         "")
                                        " \\1 ")))
  )

; построчно читаем предложения из файла
; каждую строку парсим на предложения и добавляем в граф
(define (read-loop in)
  (let loop ((line (read-line in)))
    (if (eof-object? line) null
        (begin 
          (let ((sentences (split-line line)))
            (if (null? sentences) null
                (begin
                  (map add-first-n-gram sentences)
                  (map add-sentence-to-next-graph sentences)
                  (map add-last-n-gram sentences)
                  (map add-sentence-to-prev-graph sentences))))
          (loop (read-line in))))
    )
  )

; добавляем для данного предложения первую n-1-грамму в граф begin-graph
(define (add-first-n-gram sentence)
  (let ((n-gram (get-n-gram sentence n)))
    (if (< (length n-gram) n)
        (update-frequency-graph begin-graph (reverse n-gram))
        (update-frequency-graph begin-graph (reverse (cdr n-gram)))))
  )

; добавляем для данного предложения последнюю n-1-грамму в граф end-graph
(define (add-last-n-gram sentence)
  (let ((n-gram (get-n-gram (reverse sentence) n)))
    (if (< (length n-gram) n)
        (update-frequency-graph end-graph n-gram)
        (update-frequency-graph end-graph (cdr n-gram))))
  )

; добавляем для данного предложения все его n-1-граммы в граф next-graph
(define (add-sentence-to-next-graph sentence)
  (let ((n-gram (get-n-gram sentence n)))
    (if (< (length n-gram) n)
        (let ((n-1-gram (reverse n-gram)) (next-word "."))
          (update-graph next-graph n-1-gram next-word)
          (update-frequency-graph frequency-graph n-1-gram)
          (set! n-gram-counter (add1 n-gram-counter)))
        (begin
          (let ((next-word (car n-gram)) (n-1-gram (reverse (cdr n-gram))))
            (update-graph next-graph n-1-gram next-word)
            (update-frequency-graph frequency-graph n-1-gram)
            (set! n-gram-counter (add1 n-gram-counter)))
          (add-sentence-to-next-graph (cdr sentence))
          )
        )
    )
  )

; добавляем для данного предложения все его n-1-граммы в граф prev-graph
(define (add-sentence-to-prev-graph sentence)
  (define r-sentence (reverse sentence))
  (let ((n-gram (get-n-gram r-sentence n)))
    (if (< (length n-gram) n)
        (update-graph prev-graph n-gram ".")
        (begin
          (update-graph prev-graph (cdr n-gram) (car n-gram))
          (add-sentence-to-prev-graph (reverse (cdr r-sentence)))
          )
        )
    )
  )

; добавление n-граммы в граф
(define (update-graph graph key value)
  (if (hash-has-key? graph key)
      (hash-set! graph key (let ((word-hash (hash-ref graph key)))
                             (hash-set word-hash value
                                       (if (hash-has-key? word-hash value)
                                           (add1 (hash-ref word-hash value))
                                           1))))
      (hash-set! graph key (make-immutable-hash (list (cons value 1)))))
  )

; обновление счетчика n-грамм
(define (update-frequency-graph graph key)
  (if (hash-has-key? graph key)
      (hash-set! graph key (add1 (hash-ref graph key)))
      (hash-set! graph key 1))
  )

; выделяем n первых элементов (в обратном порядке) из начала списка lst
; если длина списка меньше, выделяем сколько сможем
(define (get-n-gram lst n)
  (let loop ((lst lst) (iter n) (res null))
    (if (or (= iter 0) (null? lst)) res
        (loop (cdr lst) (sub1 iter) (cons (car lst) res))))
  )

; hash-with-weights -- список пар (элемент, вес)
(define (pick-random-from-hash hash-with-weights)
  (let ((weight-sum (foldl + 0 (hash-values hash-with-weights)))) ; находим сумму всех весов
    (let loop ((cur-sum (random weight-sum))
               (lst (hash-keys hash-with-weights)))
      (if (null? lst)
          null
          (let ((cur-weight (hash-ref hash-with-weights (car lst))))
            (if (> cur-weight cur-sum)
                (car lst)
                (loop (- cur-sum cur-weight) (cdr lst))
                )
            )
          )
      )
    )
  )

; прямой способ генерации реплик
(define (direct-generator)
  (let ((n-1-gramma (pick-random-from-hash begin-graph)))
    (let loop ((cur-n-gram n-1-gramma) (result null) (iter 100))
      (if (or (< iter 0) (equal? (car (reverse cur-n-gram)) "."))
          (append (reverse result) cur-n-gram)
          (loop (append (cdr cur-n-gram) (list (pick-random-from-hash (hash-ref next-graph cur-n-gram))))
                (cons (car cur-n-gram) result) (sub1 iter)))
      )
    )
  )

; обратный способ генерации реплик
(define (reverse-generator)
  (let ((n-1-gramma (pick-random-from-hash end-graph)))
    (let loop ((cur-n-gram n-1-gramma) (result null) (iter 200))
      (if (or (< iter 0) (equal? (car cur-n-gram) "."))
          (append (cdr cur-n-gram) result)
          (loop (cons (pick-random-from-hash (hash-ref prev-graph cur-n-gram)) (reverse (cdr (reverse cur-n-gram))))
                (cons (car (reverse cur-n-gram)) result) (sub1 iter)))
      )
    )
  )

; склейка ответа
(define (join-string lst)
  (regexp-replace* #px"(') " (regexp-replace* #px" ([;,:'\\.])" (string-join lst) "\\1") "\\1")
  )

(read-loop in)

(define (test)
  (println (join-string (direct-generator)))
  (newline)
  (println (join-string (reverse-generator))))