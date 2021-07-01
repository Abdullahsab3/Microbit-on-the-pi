#lang racket

(provide (all-defined-out))

(require "adt-matrix.rkt")
(require "raspi-gpio.rkt")

(define OFFSET 100)
(define row-pins #(102 103 104 105 106))
(define column-pins #(109 110 111 112 113))

(define number-of-rows 5)
(define number-of-cols 5)

;; een patroon is een vector van vectoren.
;; Deze procedure kan een lijst die 25 elementen heeft
;; omzetten naar een vector van vectoren (een matrix met 5 rijen en 5 kolommen).
(define (make-pattern pattern)
  (if (vector? pattern)
      pattern
      (list->matrix number-of-rows number-of-cols pattern)))

;; initialize zal alle kolommen en rijen initialiseren en uitzetten
(define (initialize)
  (vector-for-each (lambda (col)
                     (gpio-set-pin-mode col 'output)
                     (gpio-digital-write col 0)) column-pins)

  (vector-for-each (lambda (row)
                     (gpio-set-pin-mode row 'output)
                     (gpio-digital-write row 1)) row-pins))
                   

;; draw is de functie die verantwoordelijk is voor patronen tekenen op het scherm.
;; Gegeven een symbool en een delay (doorgaans 1ms om ervoor te zorgen dat alle leds zichtbaar zijn)
;; wordt het patroon op het scherm getekend.
(define (draw pattern delay)
  (for ((row (in-range number-of-rows)))
    (for ((col (in-range number-of-cols)))
      (gpio-digital-write (vector-ref column-pins col)
                          (matrix-ref pattern row col)))
    (gpio-digital-write (vector-ref row-pins row) 0)
    (gpio-delay-ms delay)
    (gpio-digital-write (vector-ref row-pins row) 1)))

;; draw-times zal het patroon oproepen voor x keren.
(define (draw-times pattern delay times)
  (for ((i (in-range times)))
    (draw pattern delay)))

;; draw-inf-loop zal het patroon op het scherm oneindig blijven tekenen 
(define (draw-inf-loop pattern delay)
  (define (loop)
    (draw pattern delay)
    (loop))
  (loop))


;; draw-with-scrolling-effect zal, gegeven een patroon die doorgaans lang is,
;; het patroon op het scherm scrollend tekenen.
;; hoe lager de scrolling delay, hoe sneller
(define scrolling-delay 10)
(define (draw-with-scrolling-effect pattern)  
  (let ((pattern-width (matrix-cols pattern))
        (pos 0))
        
    (define (draw pattern delay) 
      (for ((row (in-range number-of-rows)))
        (for ((col (in-range number-of-cols)))
          (gpio-digital-write (vector-ref column-pins col)
					;; pos wordt elke loop met 1 verhoogd nadat de "huidige" positie
					;; een bepaald aantal keren op het scherm getekend was.
                              (matrix-ref pattern row (+ pos col))))
        (gpio-digital-write (vector-ref row-pins row) 0)
        (gpio-delay-ms delay)
        (gpio-digital-write (vector-ref row-pins row) 1)))
  
    (for ((y (in-range (- pattern-width number-of-cols 1))))
      (for ((x (in-range scrolling-delay)))
        (draw pattern 1))
      (set! pos (+ pos 1)))))

(define (draw-with-pause pattern delay)
  (define pause 300)
  (define (loop)
    (draw pattern (+ delay pause))
    (when (not (= pause 0))
      (if (> pause 100)
          (set! pause (- pause 100))
          (set! pause (- pause 5))))
    (loop))
  (loop))

(define (draw-with-pause-times pattern delay times)
  (define pause 300)
  (define (loop)
    (if (zero? pause)
        (draw-times pattern delay times)
        (begin  (draw pattern (+ delay pause))
                (if (> pause 100)
                    (set! pause (- pause 100))
                    (set! pause (- pause 5)))
                (loop))))
  (loop))

;; een grote matrix van digits.
(define DIGITS
  #(#(0 1 1 1 0 0 1 1 0 0 0 1 1 1 0 0 1 1 1 0 0 1 0 1 0 0 1 1 1 0 0 1 1 1 0 0 1 1 1 0 0 1 1 1 0 0 1 1 1 0)
    #(0 1 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 1 0 1 0 0 1 0 1 0)
    #(0 1 0 1 0 0 0 1 0 0 0 1 1 1 0 0 1 1 1 0 0 1 1 1 0 0 1 1 1 0 0 1 1 1 0 0 0 0 1 0 0 1 1 1 0 0 1 1 1 0)
    #(0 1 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 0 0 0 0 1 0 0 1 0 1 0 0 0 0 1 0)
    #(0 1 1 1 0 0 0 1 0 0 0 1 1 1 0 0 1 1 1 0 0 0 0 1 0 0 1 1 1 0 0 1 1 1 0 0 0 0 1 0 0 1 1 1 0 0 1 1 1 0)))

;; een getal omzetten naar een lijst
;; bvb 5231 --> '(5 2 3 1)
(define (integer->list-of-digits number)
  (define (iter number lst)
    (if (= number 0)
        lst
        (iter (quotient number 10)
              (cons (modulo number 10) lst))))
  (iter number '()))

(define (get-digit-pos digit)
  (* 5 digit))


;; een getal op het scherm tekenen.
;; Dit gebeurt door elke digit op het scherm voor een bepaalde periode te laten tekenen.
;; De digits worden uit de grote matrix gehaald.
;; Er wordt niet gekozen voor een scrolling effect
;; aangezien dat het scrolling effect het een beetje moeilijker te lezen maakt.
(define number-delay 100)
(define (draw-number number)
  (let ((digits-list
         (integer->list-of-digits number)))

    (define (draw digit)
      (let ((pos (get-digit-pos digit)))
        (for ((row (in-range number-of-rows)))
          (for ((col (in-range number-of-cols)))
            (gpio-digital-write (vector-ref column-pins col)
                                (matrix-ref DIGITS row (+ pos col))))
          (gpio-digital-write (vector-ref row-pins row) 0)
          (gpio-delay-ms 1)
          (gpio-digital-write (vector-ref row-pins row) 1))))

    ;; alle digits drawen na elkaar
    (for-each (lambda (digit)
                (for ((x (in-range number-delay)))
                  (draw digit)))
              digits-list)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; een abstractie voor het scherm
(define screen (make-pattern '(0 0 0 0 0
                               0 0 0 0 0
                               0 0 0 0 0
                               0 0 0 0 0
                               0 0 0 0 0)))

;; een bepaalde led aanzetten
(define (turn-on! which-row which-col)
  (matrix-set! screen which-row which-col 1))

;; een bepaalde led uitzetten.
(define (turn-off! which-row which-col)
  (matrix-set! screen which-row which-col 0))

;; het scherm tekenen
(define (draw-screen)
  (draw screen 1))
