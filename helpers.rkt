#lang racket

(provide (all-defined-out))

(define SECONDS #f)
(define MILLISECONDS #t)


(define (get-time ms?)
  (if ms?
      (current-milliseconds)
      (current-seconds)))
  

;;;; een timer ADT ;;;;
(define (make-timer how-long ms?)
  (let ((current-time (get-time ms?)))

    
    ;; de huidige tijd van de timer resetten
    (define (reset-time!)
       (set! current-time (get-time ms?)))
       

    ;; kijken of de tijd van de timer al gepasseerd is
    (define (time-passed?)
      (let* ((time-now (get-time ms?))
             (passed? (>=  (- time-now current-time) how-long)))
        ;; als de tijd al gepasseerd is, resetten we de timer zodat die opnieuw kan timen.
        (when passed?
          (reset-time!))
        passed?))


    (define (dispatch msg)
      (cond ((eq? msg 'time-passed?) (time-passed?))
            ((eq? msg 'reset-current!) (reset-time!))
            (else (error "unknown message" msg))))
    dispatch))


;;;;;           Timer met een schakeling ADT          ;;;;
;;;      gebaseerd op bovengedefinieerde Timer ADT ;;;;;;;
(define (make-timer-with-a-switch time ms?)
  (let* ((timer (make-timer time ms?)))

    ;; de timer starten
    (define (start-the-count!)
      (timer 'reset-current!))

    (define (dispatch msg)
      (cond ((eq? msg 'start-the-count!) (start-the-count!))
            (else (timer msg))))
    dispatch))

;;; Een aantal vaakvoorkomende timers
(define five-seconds-timer (make-timer 5 SECONDS))

(define (five-seconds-passed?)
  (five-seconds-timer 'time-passed?))

(define one-minute-timer (make-timer 60 SECONDS))

(define (one-minute-passed?)
  (one-minute-timer 'time-passed?))

(define 100-ms-timer (make-timer 100 MILLISECONDS))

(define (100-ms-passed?)
  (100-ms-timer 'time-passed?))

(define 200-ms-timer (make-timer 200 MILLISECONDS))

(define (200-ms-passed?)
  (200-ms-timer 'time-passed?))



;; extra hulpprocedures om te abstraheren

(define (average a b)
  (quotient (+ a b) 2))

(define (half num)
   (quotient num 2))

(define (incr num)
  (+ num 1))

(define (decr num)
  (- num 1))