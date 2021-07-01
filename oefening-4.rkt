#lang racket

(provide (all-defined-out))

(require "raspi-gpio.rkt")
(require "adt-matrix.rkt")
(require "multiplexing-drawing-lib.rkt")
(require "patterns.rkt")
(require "helpers.rkt")
(require "constants.rkt")


(gpio-setup)

(gpio-mcp23017-setup OFFSET #x20)

;; uit de pintro
(define X_CH 5)
(define Y_CH 6)
(define Z_CH 7)
(define spi-channel 0)
(define channel-config 8)
(gpio-mcp3008-setup spi-channel)
(define sample-size 10)

(define (read-axis ch)
  (let ((reading 0))
    (gpio-delay-ms 1)
    (for ([i sample-size])
      (set! reading
            (+ reading (gpio-mcp3008-analog-read spi-channel channel-config ch))))
    (/ reading (exact->inexact sample-size))))


(define x-raw-min 512)
(define y-raw-min 512)
(define z-raw-min 512)
(define x-raw-max 512)
(define y-raw-max 512)
(define z-raw-max 512)

(define (auto-calibrate x-raw y-raw z-raw)
  (cond ((< x-raw x-raw-min) (set! x-raw-min x-raw)))
  (cond ((> x-raw x-raw-max) (set! x-raw-max x-raw)))
  (cond ((< y-raw y-raw-min) (set! y-raw-min y-raw)))
  (cond ((> y-raw y-raw-max) (set! y-raw-max y-raw)))
  (cond ((< z-raw z-raw-min) (set! z-raw-min z-raw)))
  (cond ((> z-raw z-raw-max) (set! z-raw-max z-raw))))


(define (value-map x in-min in-max out-min out-max)
  (exact-round (/ (* (- x in-min)
                     (- out-max out-min))
                  (+ (- in-max in-min) out-min))))


(define (calibrate-loop)
  (let ((x-raw (read-axis X_CH))
        (y-raw (read-axis Y_CH))
        (z-raw (read-axis Z_CH)))
    (for ([i 400])
      (set! x-raw (read-axis X_CH))
      (set! y-raw (read-axis Y_CH))
      (set! z-raw (read-axis Z_CH))
      (auto-calibrate x-raw y-raw z-raw)
      (gpio-delay-ms 50))))

;; accelerometer calibreren
(calibrate-loop)

(initialize)

  
;; oefening 4
(define rotated-to-left? #f)
(define rotated-down? #f)

(define player-col 2)
(define player-row 2)

(define star-col (random 5))
(define star-row (random 5))

(define score 0)
(define game-done? #f)

(turn-on! player-row player-col)

;; kijken of er een collision gebeurd is.
(define (collision?)
  (and (= star-row player-row)
       (= star-col player-col)))

;; de actie die er moet gebeuren wanneer het spel klaar is
(define (game-done)
  (set! game-done? #t)
  (draw-with-scrolling-effect SCORE)
  (draw-number score))

;; een nieuwe ster maken
(define (reset-star!)
  (set! star-col (random 5))
  (set! star-row (random 5)))

;; score met 1 vermeerderen
(define (incr-score!)
  (set! score (+ score 1)))

;; inputwachttijd
(define input-disabled? #f)
(define waiting-time 100)
(define timer (make-timer-with-a-switch waiting-time MILLISECONDS))

(define (disabling-time-passed?)
  (timer 'time-passed?))

(define (disable-input!)
  (set! input-disabled? #t)
  (timer 'start-the-count!))

(define (enable-input!)
  (when (disabling-time-passed?)
    (set! input-disabled? #f)))

;; wanneer een collision gebeurt wordt de score vermeerderd
;; een nieuwe ster aangemaakt, en die ster op het scherm getekend.
(define (update-star!)
  (when (collision?)
    (incr-score!)
    (reset-star!))
  (turn-on! star-row star-col))

(define min-scaled 0)
(define max-scaled 10)
(define avg-scaled (average min-scaled max-scaled))

  
(define (listen-to-input)
  ;; de assen uitlezen en hun waarde herschalen
  (let* ((x-raw (read-axis X_CH))
         (y-raw (read-axis Y_CH))
         (x-scaled (value-map x-raw
                              x-raw-min x-raw-max
                              min-scaled max-scaled))
         (y-scaled (value-map y-raw
                              y-raw-min y-raw-max
                              min-scaled max-scaled)))

    ;; checken langs welke as we aan het roteren zijn
    (cond ((>= x-scaled (add1 avg-scaled))
           (when (= left-border player-col) (star-catcher))
           (set! rotated-to-left? #t)
           (x-rotated))
          ((< x-scaled (sub1 avg-scaled))
           (when (= right-border player-col) (star-catcher))
           (x-rotated))
          ((>= y-scaled (add1 avg-scaled))
           (when (= bottom player-row) (star-catcher))
           (set! rotated-down? #t)
           (y-rotated))
          ((< y-scaled (sub1 avg-scaled))
           (when (= top player-row) (star-catcher))
           (y-rotated))
          (else (star-catcher)))))

;; een algemene procedure voor de actie wanneer er rond een as geroteerd wordt.
(define (axis-rotated action)
  (unless input-disabled?
    (turn-off! player-row player-col)
    (action)
    (turn-on! player-row player-col)
    (disable-input!))
  (enable-input!)
  (unless game-done?
    (star-catcher)))

(define (action-when-x-rotated)
  (set! player-col ((if rotated-to-left? - +)  player-col 1))
  (set! rotated-to-left? #f))

(define (action-when-y-rotated)
  (set! player-row ((if rotated-down? + -) player-row 1))
  (set! rotated-down? #f))

(define (x-rotated)
  (axis-rotated action-when-x-rotated))

(define (y-rotated)
  (axis-rotated action-when-y-rotated))
  

;; De algemene spellus
(define (star-catcher)
  (update-star!)
  (draw-screen)
  ;; het spel is gedaan wanneer 1 minuut gepaseerd is.
  (cond ((one-minute-passed?)
         (game-done))
        (else (listen-to-input))))

;; spel opstarten
(star-catcher)