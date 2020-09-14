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
; параметр stop-word -- стоп-слово для завершения работы доктора
(define (doctor-driver-loop name)
  (let loop ((name name) (answers null)) ; answers - список всех ответов пользователя
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
        ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для завершения работы с данным пациентом
         (printf "Goodbye, ~a!\n" name)
         (print '(see you next week))
         (newline))
        (else (print (reply user-response answers)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
              (loop name (cons user-response answers))
              )
        )
      )
    )
  )

; генерация ответной реплики по user-response -- реплике от пользователя
; параметр user-response -- ответ пациента
; параметр answers -- список сохраненных реплик пациента
(define (reply user-response answers)
  (case (if (null? answers) (random 2) (random 3)) ; с равной вероятностью выбирается один из трех (или двух) способов построения ответа
    ((0) (qualifier-answer user-response)) ; 1й способ
    ((1) (hedge))  ; 2й способ
    ((2) (history-answer answers)) ; 3й способ
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