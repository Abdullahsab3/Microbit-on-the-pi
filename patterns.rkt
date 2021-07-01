#lang racket

(provide (all-defined-out))
(require "multiplexing-drawing-lib.rkt")

;; furelise -- beethoven
(define heart-pattern
 (make-pattern '(0  1  0  1  0
                 1  1  1  1  1
                 1  1  1  1  1
                 0  1  1  1  0
                 0  0  1  0  0)))

;; rickroll
(define smile-pattern 
(make-pattern '(1  1  0  1  1
                1  1  0  1  1
                0  0  0  0  0
                1  0  0  0  1
                0  1  1  1  0)))

;; birthday
(define birthday-pattern 
(make-pattern '(0  1  0  1  0
                0  1  0  1  0
                1  1  1  1  1
                1  1  1  1  1
                1  1  1  1  1
                )))

;; brahms
(define melody-pattern 
(make-pattern '(0  0 0 1 1
                0  0 0 1 0
                0  0 1 0 0
                1  1 0 0 0
                1  1 0 0 0)))

;; pinkpanhter
(define pinkpanther-pattern 
(make-pattern '(1 1 0 1 1
                1 1 0 1 1
                1 1 1 1 1
                1 0 0 0 1
                1 1 1 1 1)))

;; super-mario
(define super-mario-pattern 
(make-pattern '(1 1 1 1 0
                1 1 1 1 0
                1 1 1 1 1
                1 1 1 1 1
                0 0 0 0 0)))

;; christmas
(define christmas-tree-pattern 
(make-pattern '(0 0 1 0 0
                0 1 1 1 0
                0 1 1 1 0
                0 0 1 0 0
                0 0 1 0 0)))
;; nokia
(define phone-pattern 
(make-pattern '(1 1 0 0 0
                1 1 0 0 0
                1 0 0 0 0
                0 1 0 1 1
                0 0 1 1 1
                0 0 0 0 0)))

;; calm
(define coffee-pattern
 (make-pattern '(0 0 1 1 1
                 1 1 1 1 1
                 1 0 1 1 1
                 1 1 1 1 1
                 0 0 1 1 1)))

(define patterns (vector heart-pattern
                         smile-pattern
                         birthday-pattern
                         melody-pattern
                         pinkpanther-pattern
                         super-mario-pattern
                         christmas-tree-pattern
                         phone-pattern
                         coffee-pattern))
                              
                             

(define (get-pattern which-pattern)
    (vector-ref patterns which-pattern))


(define GAME-OVER
  (vector
   (vector 0 1 1 1 1 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 1 1 1 0 0 1 0 1 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0 0 0)
   (vector 0 1 0 0 0 0 0 0 1 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 0 1 0 1 0 0 1 0 1 0 0 0 1 0 0 0 0 0 1 0 1 0 0 0 0 0 0)
   (vector 0 1 0 1 1 0 0 0 1 1 1 0 0 0 1 0 1 0 0 0 1 1 1 0 0 0 1 0 1 0 0 1 0 1 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0 0 0)
   (vector 0 1 0 0 1 0 0 0 1 0 1 0 0 0 1 0 1 0 0 0 1 0 0 0 0 0 1 0 1 0 0 1 0 1 0 0 0 1 0 0 0 0 0 1 1 0 0 0 0 0 0 0)
   (vector 0 1 1 1 1 0 0 0 1 0 1 0 0 0 1 0 1 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 1 1 1 0 0 0 1 0 1 0 0 0 0 0 0)))

(define SCORE
  (vector
    (vector 0 1 1 1 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0 0 0)   
    (vector 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 1 0 0 0 1 0 1 0 0 0 1 0 0 0 0 0 0 0 0)
    (vector 0 1 1 1 0 0 0 1 0 0 0 0 0 1 0 1 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0 0 0)
    (vector 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0 1 0 0 0 1 1 0 0 0 0 1 0 0 0 0 0 0 0 0)
    (vector 0 1 1 1 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 1 0 1 0 0 0 1 1 1 0 0 0 0 0 0)))


(define BSSF (vector
              (vector 0 0 0 0 0 1 1 1 0 0 0 1 0 1 0 0 0 1 1 0 0 0 1 1 1 0 0 1 1 1 0 0 1 1 1 0 0 0 0 0 0)
              (vector 0 0 0 0 0 0 1 0 0 0 1 1 1 1 1 0 0 1 0 1 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0)
              (vector 0 0 0 0 0 0 1 0 0 0 1 1 1 1 1 0 0 1 1 1 0 0 1 1 1 0 0 1 1 1 0 0 1 1 0 0 0 0 0 0 0)
              (vector 0 0 0 0 0 0 1 0 0 0 0 1 1 1 0 0 0 1 0 1 0 0 0 0 1 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0)
              (vector 0 0 0 0 0 1 1 1 0 0 0 0 1 0 0 0 0 1 1 0 0 0 1 1 1 0 0 1 1 1 0 0 1 0 0 0 0 0 0 0 0)))




                                    


                                    




