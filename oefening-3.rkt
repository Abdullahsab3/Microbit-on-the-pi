#lang racket
(provide (all-defined-out))


(require "raspi-gpio.rkt")
(require "multiplexing-drawing-lib.rkt")
(require "patterns.rkt")
(require "helpers.rkt")
(require "constants.rkt")

(gpio-setup)

(define OFFSET 100)
(gpio-mcp23017-setup OFFSET #x20)
(define LEFT-BUTTON 108)
(define RIGHT-BUTTON 107)
(gpio-set-pin-mode LEFT-BUTTON 'input)
(gpio-set-pull-resistor LEFT-BUTTON 'up)
(gpio-set-pin-mode RIGHT-BUTTON 'input)
(gpio-set-pull-resistor RIGHT-BUTTON 'up)

  
(initialize)
  
  

;; oefening 3
(define input-disabled? #f)


(define max-snowflakes-to-lose 3)
(define snowflake-speed 20)

(define left-button-pressed? #f)
(define snowflake-refresh-rate 20)
(define snowflake-speeder 0)

; de coordinaten van de speler
(define player-col (half right-border))
(define player-row bottom)

; de coordinaten van het spel
(define snowflake-col (random 5))
;; dit wordt op -1 gezet zodat de 0e rij aangezet kan worden.
(define snowflake-row -1)

(define score 0)
(define lost-snowflakes 0)

;; In het begin de initiele plaats van de speler aanzetten
(turn-on! player-row player-col)

(define (bottom-reached?)
  (>= snowflake-row bottom))

;; een botsing gebeurt wanneer beide coordinaten gelijk zijn.
(define (collision?)
  (and (= snowflake-row player-row)
       (= snowflake-col player-col)))

;; de speler verliest wanneer hij 3 snowflakes niet kon vangen.
(define (game-lost?)
  (>= lost-snowflakes max-snowflakes-to-lose))

;; wat er moet gebeuren wanneer de speler verliest
(define (game-lost)
  (draw-with-scrolling-effect GAME-OVER)
  (draw-number score))

;; een nieuwe snowflake maken
(define (reset-snowflake!)
  (set! snowflake-col (random 5))
  (set! snowflake-row -1))

;; de score met 1 vermeerderen
(define (incr-score!)
  (set! score (+ score 1)))

;; de verloren snowflakes met 1 vermeerderen.
(define (incr-lost-snowflakes!)
  (set! lost-snowflakes
        (+ lost-snowflakes 1)))


(define (update-snowflake!)
  ;; tijd om de snowflake positie te updaten.
  (when (>= snowflake-refresh-rate (- snowflake-speed
                                      snowflake-speeder))
    (set! snowflake-refresh-rate 0)
    ;; de huidige snowflake uitzetten (zodat de volgende aangezet kan worden).
    (unless (or (collision?)
                (negative? snowflake-row))
      ;; zet de vorige positie van de snowflake uit tenzij dat er een botsing is gebeurd
      ;; (want dan is dat ook de positie van de speler)
      ;; of de waarde van de snowflake-row negatief is (want dan zit de snowflake helemaal vanboven)
      (turn-off! snowflake-row snowflake-col))
    ;; kijken of de snowflake
    (cond ((bottom-reached?)
           ;; als de snowflake op het einde zit, kijk of er een botsing is gebeurd
           (cond ((collision?) (incr-score!))
                 (else (incr-lost-snowflakes!)))
           ;; een nieuwe snowflake wordt getekend
           (reset-snowflake!))
          ;; als de snowflake nog niet beneden zit, gaat de snowflake verder omlaag.
          (else  (set! snowflake-row (+ snowflake-row 1))
                 (turn-on! snowflake-row snowflake-col)))))

;; ipv delay te doen, wordt er gebruikgemaakt van een timer
;; om de input voor 50ms uit te zetten.
(define waiting-time 50)
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
         (when (= left-border player-col) (snowflake-catcher))
         (set! left-button-pressed? #t))
        ;; rechterknop ingedrukt -> verlaat de cond en ga naar de procedure-oproep button-pressed
        ((= 0 (gpio-digital-read RIGHT-BUTTON))
         (when (= right-border player-col) (snowflake-catcher))
              'ok)
        ;; geen enkele knop is ingedrukt --> verder gaan in de spellus
        (else (snowflake-catcher)))
  (unless (game-lost?)
    (button-pressed)))
  
;; wat er moet gebeuren wanneer een knop ingedrukt is.  
(define (button-pressed)
  (unless input-disabled?
    ;; de vorige positie uitzetten
    (turn-off! player-row player-col)
    (set! player-col ((if left-button-pressed? - +)  player-col 1))
    ;; de nieuwe positie aanzetten
    (turn-on! player-row player-col)
    ;; zet de input uit voor 50ms
    (disable-input!))
  ;; indien de linkerknop gedrukt was wordt het terug op false gezet.
  (when left-button-pressed?
    (set! left-button-pressed? #f))
  ;; terug de input aanzetten na 50ms
  (enable-input!)
  (snowflake-catcher))

(define (increment-rates!)
  (set! snowflake-refresh-rate
        (+ snowflake-refresh-rate 1)))

;; versnel de snowflakes om de 5 seconden.
(define (speed-up-snowflakes!)
  (when (five-seconds-passed?)
    (set! snowflake-speeder (+ snowflake-speeder 2)))
  ;; de snowflake-speeder mag niet hoger liggen dan 20,
  ;; want anders krijgen we een negatieve waarde in de test van de when in update-snowflake!
  (when (> snowflake-speeder 20)
    (set! snowflake-speeder 20)))

;; de algemene spellus
(define (snowflake-catcher)
  (speed-up-snowflakes!)
  (update-snowflake!)
  (increment-rates!)
  (draw-screen)
  (cond ((game-lost?) (game-lost))
        (else (listen-to-input))))

;; spel opstarten
(snowflake-catcher)