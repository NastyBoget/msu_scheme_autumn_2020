#lang scheme/base

(require racket/string)
(provide (all-defined-out))

(define input-file "data/texts.txt")
(define output-file "good_out.txt")

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
(define n 5)

; разбиваем строку на предложения, а предложения на слова
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
(define (read-loop input-file)
  (define in (open-input-file input-file))
  (let loop ((line (read-line in)))
    (if (eof-object? line) (println "all structures created")
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
  (close-input-port in)
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
          (update-frequency-graph frequency-graph n-1-gram))
        (begin
          (let ((next-word (car n-gram)) (n-1-gram (reverse (cdr n-gram))))
            (update-graph next-graph n-1-gram next-word)
            (update-frequency-graph frequency-graph n-1-gram))
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
(define (direct-generator first-n-gramma)
    (let loop ((cur-n-gram first-n-gramma) (result null) (iter 100))
      (if (or (< iter 0) (equal? (car (reverse cur-n-gram)) "."))
          (append (reverse result) cur-n-gram)
          (loop (append (cdr cur-n-gram) (list (pick-random-from-hash (hash-ref next-graph cur-n-gram))))
                (cons (car cur-n-gram) result) (sub1 iter)))
      )
  )

; обратный способ генерации реплик
(define (reverse-generator first-n-gramma)
    (let loop ((cur-n-gram first-n-gramma) (result null) (iter 100))
      (if (or (< iter 0) (equal? (car cur-n-gram) "."))
          (append (cdr cur-n-gram) result)
          (loop (cons (pick-random-from-hash (hash-ref prev-graph cur-n-gram)) (reverse (cdr (reverse cur-n-gram))))
                (cons (car (reverse cur-n-gram)) result) (sub1 iter)))
      )
  )

; смешанный способ генерации реплик
; sentence - предложение (список слов), на основе которого нужно построить реплику
; ищем n-граммы, которые есть в графе всех n-грамм
; если не нашли, ищем n-граммы, в которых встречается максимальное число слов из предложения
; если не нашли - применяем прямой способ генерации
; если нашли - наращиваем ответ с двух сторон с помощью двух графов
(define (compound-generator sentence)
  (define variants (make-hash)) ; варианты для начала построения ответных реплик
  (let loop ((cur-sentence sentence)) ; получение всех n-грамм из предложения, их поиск в графе n-грамм тренировочного текста
    (let ((n-1-gram (reverse (get-n-gram cur-sentence (sub1 n)))))
      (if (< (length n-1-gram) (sub1 n))
          null
          (begin
            (if (hash-has-key? frequency-graph n-1-gram)
                (hash-set! variants n-1-gram (hash-ref frequency-graph n-1-gram))
                null)
            (loop (cdr cur-sentence))
            ))
      ))
  (if (hash-empty? variants)
      ; ищем n-граммы, в которых встречается максимальное число слов из предложения
      (find-best-reply sentence)
      ; генерация предложения на основе рандомно выбранной n-граммы
      (let ((base-n-gramma (pick-random-from-hash variants)))
        (append (reverse-generator base-n-gramma)
                (list-tail (direct-generator base-n-gramma) (sub1 n)))
        ))
  )

; ищем n-граммы, в которых встречается максимальное число слов из предложения
; генерация предложения на основе рандомно выбранной n-граммы
; если не нашли - применяем прямой способ генерации
(define (find-best-reply sentence)
  (define variants (make-hash)) ; варианты для начала построения ответных реплик
  (define (update-variants graph-n-gram n-gram)
    (if (= (foldl (lambda (x y) (if (member x graph-n-gram) (add1 y) y)) 0 n-gram) (length n-gram)) ; если все слова присутствуют в n-1-грамме графа frequency-graph
        (hash-set! variants graph-n-gram (hash-ref frequency-graph graph-n-gram)) ; добавляем к вариатам ответа
        null)
    )
  (let loop-1 ((word-number (sub1 n)))
    (let loop-2 ((cur-sentence sentence))
      (let ((n-gram (get-n-gram cur-sentence word-number))) ; не обращаем внимания на порядок слов
        (if (or (< (length n-gram) word-number) (null? cur-sentence))
            null
            (begin
              (map (lambda (x) (update-variants x n-gram)) (hash-keys frequency-graph))
              (loop-2 (cdr cur-sentence))
              ))
        ))
    (if (hash-empty? variants)
        (if (> word-number 0)
            (loop-1 (sub1 word-number))
            (direct-generator (pick-random-from-hash begin-graph)))
        (let ((base-n-gramma (pick-random-from-hash variants)))
          (append (reverse-generator base-n-gramma)
                  (list-tail (direct-generator base-n-gramma) (sub1 n)))
          )
        )
    )
  )

; склейка ответа
(define (join-string lst)
  (regexp-replace* #px"(') " (regexp-replace* #px" ([;,:'\\.])" (string-join lst) "\\1") "\\1")
  )

; инициализация всех необходимых структур данных - либо формирование, либо чтение из файла
(define (init-structures mode)
  (if (equal? mode 'train)
      ; формируем все необходимые структуры данных
      (begin
        (read-loop input-file)
        (save-structures output-file))
      ; загружаем готовое
      (load-structures output-file)
      )
  )

(define (save-structures output-file)
  (define out (open-output-file output-file #:exists 'replace))
  (write next-graph out)
  (write prev-graph out)
  (write begin-graph out)
  (write end-graph out)
  (write frequency-graph out)
  (close-output-port out)
  )

(define (load-structures input-file)
  (define in (open-input-file input-file))
  (set! next-graph (read in))
  (set! prev-graph (read in))
  (set! begin-graph (read in))
  (set! end-graph (read in))
  (set! frequency-graph (read in))
  (close-input-port in)
  )

; тест для простых генераторов
(define (test-1)
  (println (join-string (direct-generator (pick-random-from-hash begin-graph))))
  (newline)
  (println (join-string (reverse-generator (pick-random-from-hash end-graph)))))

; тест для смешанного генератора
(define (test-2)
  (let ((sentence (read-line)))
    (println (join-string (compound-generator (car (split-line sentence))))) ; основываемся на первом предложении
    )
  )

;(init-structures 'train)