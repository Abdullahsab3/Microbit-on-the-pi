#lang racket
;;;;; OEFENING 1 ;;;;;

(require "raspi-gpio.rkt")
(require "multiplexing-drawing-lib.rkt")
(require "patterns.rkt")

(gpio-setup)

(gpio-mcp23017-setup OFFSET #x20)

;; het scherm initialiseren
(initialize)

;; de patronen tekenen voor ong. 3 seconden per patroon
(draw-times heart-pattern 1 250)
(draw-times phone-pattern 1 250)
(draw-with-scrolling-effect GAME-OVER)