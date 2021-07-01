#lang racket
(provide (all-defined-out))


(require "raspi-gpio.rkt")
(require "multiplexing-drawing-lib.rkt")
(require "helpers.rkt")
(require "constants.rkt")

(gpio-setup)

(gpio-mcp23017-setup OFFSET #x20)

(initialize)

(define LEFT-BUTTON 108)
(define RIGHT-BUTTON 107)
(gpio-set-pin-mode LEFT-BUTTON 'input)
(gpio-set-pull-resistor LEFT-BUTTON 'up)
(gpio-set-pin-mode RIGHT-BUTTON 'input)
(gpio-set-pull-resistor RIGHT-BUTTON 'up)


;; oefening 2
(define left-button-pressed? #f)
(define player-col 2)
(define player-row bottom)

(turn-on! player-row player-col)

(define input-disabled? #f)



;; ipv delay te doen, wordt er gebruikgemaakt van een timer
;; om de input voor 50ms uit te zetten.
(define waiting-time 100)
(define timer (make-timer-with-a-switch waiting-time MILLISECONDS))

(define (disabling-time-passed?)
  (timer 'time-passed?))

;; wanneer een knop ingedrukt is, wordt de input voor 50ms uitgezet
(define (disable-input!)
  (set! input-disabled? #t)
  (timer 'start-the-count!))

;; er wordt gecheckt of het tijd is om de input terug aan te zetten (na 50ms dus)
(define (enable-input!)
  (when (disabling-time-passed?)
    (set! input-disabled? #f)))
  
;; luisteren naar eventuele input (knoppen ingedrukt)  
(define (listen-to-input)
  (cond ((= 0 (gpio-digital-read LEFT-BUTTON))
         (when (= left-border player-col) (buttons-loop))
         (set! left-button-pressed? #t))
        ;; rechterknop ingedrukt -> verlaat de cond en ga naar de procedure-oproep button-pressed
        ((= 0 (gpio-digital-read RIGHT-BUTTON))
         (when (= right-border player-col) (buttons-loop)))
        ;; geen enkele knop is ingedrukt --> verder gaan in de spellus
        (else (buttons-loop)))
  (button-pressed))

;; wat er moet gebeuren wanneer een knop ingedrukt is.  
(define (button-pressed)
  (unless input-disabled?
    ;; de vorige positie uitzetten
    (turn-off! player-row player-col)
    (set! player-col ((if left-button-pressed? - +)  player-col 1))
    ;; de nieuwe positie aanzetten
    (turn-on! player-row player-col)
    ;; zet de input uit voor 50 ms
    (disable-input!))
  ;; indien de linkerknop gedrukt was wordt het terug op false gezet.
  (when left-button-pressed?
    (set! left-button-pressed? #f))
  ;; terug de input aanzetten na 50ms
  (enable-input!)
  (buttons-loop))

;; de algemene loop
(define (buttons-loop)
  (draw-screen)
  (listen-to-input))

;; de loop starten.
(buttons-loop)