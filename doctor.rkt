#lang scheme/base
; В учебных целях используется базовая версия Scheme
; основная функция, запускающая "Доктора"
; параметр stop-word -- стоп-слово для завершения работы доктора
; параметр max-iter -- максимальное число обслуживаемых пациентов
(define (visit-doctor stop-word max-iter)
  (let loop ((name (ask-patient-name))
             (iter max-iter))
    (cond ((or (= iter 0) (equal? name stop-word)) (print  '(time to go home))) ; завершаем работу если приняли нужное количество пациентов или получили стоп-слово вместо имени
          (else (printf "Hello, ~a!\n" name)
                (print '(what seems to be the trouble?))
                (doctor-driver-loop name)
                (loop (if (= iter 1) name (ask-patient-name)) (- iter 1)) ; принимаем следующего пациента, на последней итерации имя следующего пациента не нужно
                )
          )
    )
  )

; упражнение 5
; ввод имени очередного пациента
(define (ask-patient-name)
  (begin
    (println '(next!))
    (println '(who are you?))
    (print '**)
    (car (read))
    ) 
  )

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
  ; answers - список всех ответов пользователя, keywords - список ключевых слов в templates, strategies - структура данных со сведениями обо всех стратегиях «Доктора»
  (let loop ((name name) (answers null) (keywords get-keywords) (strategies strategies))
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
        ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для завершения работы с данным пациентом
         (printf "Goodbye, ~a!\n" name)
         (print '(see you next week))
         (newline))
        (else (print (reply user-response answers keywords strategies)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
              (loop name (cons user-response answers) keywords strategies)
              )
        )
      )
    )
  )

; генерация ответной реплики по user-response -- реплике от пользователя
; параметр user-response -- ответ пациента
; параметр answers -- список сохраненных реплик пациента
; параметр keywords -- список ключевых слов в templates
; параметр strategies -- структура данных со сведениями обо всех стратегиях «Доктора»
(define (reply user-response answers keywords strategies)
  ; строится список стратегий, применимых в текущей ситуации
  (let ((possible-strategies (foldl (lambda (x result) (if ((car x) user-response answers keywords) (cons (cdr x) result) result)) ; список из (weight, strategy) 
                                    null
                                    strategies)))
    ; если в построенном списке больше одной стратегии, то выбирается одна из них
    (if (and (not (null? possible-strategies)) (not (null? (cdr possible-strategies)))) ; если длина списка > 1
        ; выбранная стратегия применяется и её результат будет ответной репликой
        ((pick-random-with-weight possible-strategies) user-response answers keywords)
        ((car possible-strategies) user-response answers keywords) ; считаю, что стратегия всегда найдется
        )
    )
  )
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
  (append (pick-random '((you seem to think that)
                         (you feel that)
                         (why do you believe that)
                         (why do you say that)
                         (i would like to know more about your story because)
                         (if i am not mistaken you said that)
                         (the way i see it you mean to say that))
                       )
          (change-person user-response)
          )
  )

; случайный выбор одного из элементов списка lst
(define (pick-random lst)
  (list-ref lst (random (length lst)))
  )

; замена лица во фразе			
(define (change-person phrase)
  (many-replace-2 '((am are)
                    (are am)
                    (i you)
                    (me you)
                    (mine yours)
                    (my your)
                    (myself yourself)
                    (you i)
                    (your my)
                    (yours mine)
                    (yourself myself))
                  phrase)
  )

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
  (cond ((null? lst) lst)
        (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                          (car lst) ; иначе в начале ответа помещается начало списка без изменений
                          )
                      (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                      )
                )
              )
        )
  )

; упражнение 2
(define (many-replace-1 replacement-pairs lst)
  (let loop ((lst lst) (result null))
    (cond ((null? lst) (reverse result))
          (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; ищем первый элемент списка в ассоциативном списке замен
                  (loop (cdr lst)
                        (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа пишем замену
                                  (car lst)) ; иначе в начале ответа помещается начало списка без изменений
                              result)
                        )
                  )
                )
          )
    )
  )

; упражнение 3
(define (many-replace-2 replacement-pairs lst)
  (map (lambda (x)(let ((pat-rep (assoc x replacement-pairs)))
                    (if pat-rep (cadr pat-rep) ; если поиск был удачен, то возвращаем замену
                        x))) ; иначе ничего не меняем
       lst)
  )

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
  (pick-random '((please go on)
                 (many people have the same sorts of feelings)
                 (many of my patients have told me the same thing)
                 (please continue)
                 (i completely understand you)
                 (would you like to tell me more about it?)
                 (it is interesting and i should think more about it)
                 )
               )
  )

; упражнение 4
; 3й способ генерации ответной реплики -- возврат к репликам пациента, сказанным ранее
(define (history-answer answers)
  (append '(earlier you said that)
          (change-person (pick-random answers)) ; выбираем произвользую фразу из сохраненных и производим в ней замену лица
          )
  )

; упражнение 6
; 4й способ генерации ответной реплики
(define (template-answer user-response keyword-list)
  (let ((keyword
         (pick-random ; случайный выбор ключевого слова для построения реплики
          (extract-keywords user-response keyword-list) ; получение всех ключевых слов, встретившихся в реплике (с повторениями)))
          )
         )
        )
    (many-replace-2 ; замена * на ключевое слово
     (list (list '* keyword))
     (pick-random ; случайный выбор шаблона
      (make-templates-list keyword) ; получение списка всех шаблонов, куда входит ключевое слово
      )
     )
    )
  )

; получение списка всех ключевых слов, встретившихся в реплике пациента (с повторениями)
(define (extract-keywords user-response keyword-list)
  (filter (lambda (x)(member x keyword-list)) user-response)
  )

; получение объединённого перечня всех шаблонов, относящихся к каждой группе, куда входит ключевое слово
(define (make-templates-list keyword)
  (foldl append
         null
         (map (lambda (x) (cadr x))
              (filter (lambda (y)(member keyword (car y)))
                      templates)))
  )

; функция-предикат, проверяющая, есть ли в реплике пользователя ключевое слово
(define (contains-keyword user-response keyword-list)
  (ormap (lambda (y)
           (ormap (lambda (x) (equal? x y))
                  keyword-list))
         user-response)
  )

; структура данных, хранящая группы ключевых слов и привязанных к ним шаблонов для составления ответных реплик
(define templates
  '( 
    ( ; начало данных 1й группы
     (depressed suicide exams university) ; список ключевых слов 1й группы
     ( ; список шаблонов для составления ответных реплик 1й группы 
      (when you feel depressed, go out for ice cream)
      (depression is a disease that can be treated)
      (your life is more important than studying)
      (do not think about * so much)
      )
     ) ; завершение данных 1й группы
    ( ; начало данных 2й группы
     (mother father parents brother sister uncle ant grandma grandpa)
     (
      (tell me more about your * , i want to know all about your *)
      (why do you feel that way about your * ?)
      (does your * make you unhappy?)
      (have your any difficulties with your * ?)
      )
     )
    ( ; начало данных 3й группы
     (university scheme lections exams)
     (
      (your education is important)
      (how many time do you spend to learning?)
      (you think so much about your *)
      (do you think that * is really important for you?)
      )
     )
    ( ; начало данных 4й группы
     (love passion tenderness affection fondness)
     (
      (* is great feeling)
      (were you happy when you felt * ?)
      (all people love somebody)
      (many people said me that they felt *)
      )
     )
    ( ; начало данных 5й группы
     (hatred disgust aversion contempt)
     (
      (how often do you feel * ?)
      (try not to feel in that way)
      (you should be more patient)
      (all people make some mistakes, try not to judge them)
      )
     )
    )
  )

; получение списка ключевых слов из структуры templates (без повторений)
(define get-keywords
  (foldl (lambda (x y) (append (filter (lambda (z) (not (member z y))) x) y))
         null
         (map car templates)) ; список списков ключевых слов
  )

; упражнение 7
; (pick-random-with-weight '((3 7) (3 8) (2 9) (1 10)))
; lst-with-weights -- список пар (вес, элемент)
(define (pick-random-with-weight lst-with-weights)
  (let ((weight-sum (foldl (lambda (x y) (+ (car x) y)) 0 lst-with-weights))) ; находим сумму всех весов
    (let loop ((cur-sum (random weight-sum))
               (lst lst-with-weights))
      (if (or (null? lst) (null? (car lst)))
          null
          (let ((cur-weight (caar lst)))
            (if (> cur-weight cur-sum)
                (cadar lst)
                (loop (- cur-sum cur-weight) (cdr lst))
                )
            )
          )
      )
    )
  )

; структура данных со сведениями обо всех стратегиях «Доктора»
(define strategies
  (list
   (list (lambda (x y z) #t) 2 (lambda (repl history keywords) (qualifier-answer repl)))
   (list (lambda (x y z) #t) 1 (lambda (repl history keywords) (hedge)))
   (list (lambda (repl answers keywords) (not (null? answers))) 2 (lambda (repl answers keywords) (history-answer answers)))
   (list (lambda (repl answers keywords) (contains-keyword repl keywords)) 3 (lambda (repl answers keywords) (template-answer repl keywords)))
   )
  )