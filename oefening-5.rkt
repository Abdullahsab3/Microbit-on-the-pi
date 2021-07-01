#lang racket

(provide (all-defined-out))

(require "raspi-gpio.rkt")
(require "songs.rkt")
(require "multiplexing-drawing-lib.rkt")
(require "helpers.rkt")
(require "patterns.rkt")

(gpio-setup)

(define piezo-pin 26)

(gpio-set-pin-mode piezo-pin 'output)

(define OFFSET 100)
(gpio-mcp23017-setup OFFSET #x20)

(initialize)

(define LEFT-BUTTON 108)
(define RIGHT-BUTTON 107)
(gpio-set-pin-mode LEFT-BUTTON 'input)
(gpio-set-pull-resistor LEFT-BUTTON 'up)
(gpio-set-pin-mode RIGHT-BUTTON 'input)
(gpio-set-pull-resistor RIGHT-BUTTON 'up)


;; Dankzij deze website https://www.freva.com/2019/06/05/buzzer-on-raspberry-pi-playing-a-melody/
(define (buzz freq duration)
  (let* ((halve-wave-time (/ 1 (* freq 2)))
         (waves (* freq duration)))
    (for ((i (in-range waves)))
      (gpio-digital-write piezo-pin 1)
      (sleep halve-wave-time)
      (gpio-digital-write piezo-pin 0)
      (sleep halve-wave-time))))

(define get-note car)
(define get-duration cdr)

(define (play notes/duration-assocls tempo)
  (for-each (lambda (assocls)
              (let ((note (get-note assocls))
                    (duration (get-duration assocls)))
                (buzz note duration)
                ;; hoe hoger de tempo, hoe trager (normaal 0.13)
                (sleep (* tempo duration))))
            notes/duration-assocls))

    
;; oefening 5
(define current-pos 0)

(define (next-pos!)
  (if (> current-pos 7)
      (set! current-pos 0)
      (set! current-pos (+ current-pos 1))))

(define (current-pattern)
  (get-pattern current-pos))

(define (current-song)
  (get-song current-pos))

(define input-disabled? #f)


;; ipv delay te doen, wordt er gebruikgemaakt van een timer
;; om de input voor 50ms uit te zetten.
(define waiting-time 100)
(define timer (make-timer-with-a-switch waiting-time MILLISECONDS))

(define (disabling-time-passed?)
  (timer 'time-passed?))

;; wanneer een knop ingedrukt is, wordt de input voor 100ms uitgezet
(define (disable-input!)
  (set! input-disabled? #t)
  (timer 'start-the-count!))

;; er wordt gecheckt of het tijd is om de input terug aan te zetten (na 100ms dus)
(define (enable-input!)
  (when (disabling-time-passed?)
    (set! input-disabled? #f)))
  
;; luisteren naar eventuele input (knoppen ingedrukt)  
(define (listen-to-input)
  (unless input-disabled?
    (disable-input!)
    ;; linkerknop zorgt voor het overgaan naar het volgende lied
    (cond ((= 0 (gpio-digital-read LEFT-BUTTON))
           (next-pos!))
          ;; rechterknop zorgt voor het selecteren van een liedje
          ((= 0 (gpio-digital-read RIGHT-BUTTON))
           (apply play (current-song)))))
  (enable-input!)
  ;; wanneer beide knoppen ingedrukt worden, wordt de lus beeindigd
  (if (and (= 0 (gpio-digital-read LEFT-BUTTON))
           (= 0 (gpio-digital-read RIGHT-BUTTON)))
      'done 
      (piezo-player)))

;; de algemene loop
(define (piezo-player)
  (draw (current-pattern) 1)
  (listen-to-input))

;; de loop starten.
(piezo-player)





