#lang racket

(require "notes.rkt")

(provide (all-defined-out))


(define (make-song list-of-notes/durations)
  (define (iter list-of-notes/durations res)
    (if (null? list-of-notes/durations)
        res
        (iter (cddr list-of-notes/durations)
              (append res
                      (list
                       (cons (car list-of-notes/durations)
                             (let* ((beat (cadr list-of-notes/durations))
                                    (out-beat (exact->inexact (/ 1 beat)))
                                    (half (/ out-beat 2)))
                               (if (negative? beat)
                                   (abs (+ out-beat half))
                                   out-beat))))))))
  (iter list-of-notes/durations '()))

;; omgezet naar scheme uit https://github.com/robsoncouto/arduino-songs/
(define calm-song
  (list
   (cons C4 0.5)
   (cons D4 0.5)
   (cons E4 0.5)
   (cons C4 0.5)
   (cons C4 0.5)
   (cons D4 0.5)
   (cons E4 0.5)
   (cons C4 0.5)
   (cons E4 0.5)
   (cons F4 0.5)
   (cons G4 1)
   (cons E4 0.5)
   (cons F4 0.5)
   (cons G4 1)
   (cons G4 0.25)
   (cons A4 0.25)
   (cons G4 0.25)
   (cons F4 0.25)
   (cons E4 0.5)
   (cons C4 0.5)
   (cons G4 0.25)
   (cons A4 0.25)
   (cons G4 0.25)
   (cons F4 0.25)
   (cons E4 0.5)
   (cons C4 0.5)
   (cons C4 0.5)
   (cons G3 0.5)
   (cons C4 1)
   (cons C4 0.5)
   (cons G3 0.5)
   (cons C4 1)))

(define beethoven-song
  (make-song
   (list
    E5  16   DS5  16  ;1
    E5  16   DS5  16   E5  16   B4  16   D5  16   C5  16 
    A4  -8   C4  16   E4  16   A4  16 
    B4  -8   E4  16   GS4  16   B4  16 
    C5  8   REST  16   E4  16   E5  16    DS5  16 
  
    E5  16   DS5  16   E5  16   B4  16   D5  16   C5  16 ;6
    A4  -8   C4  16   E4  16   A4  16  
    B4  -8   E4  16   C5  16   B4  16  
    A4   4  REST  8  ;9 - 1st ending

    ;repaets from 1 ending on 10
    E5  16   DS5  16  ;1
    E5  16   DS5  16   E5  16   B4  16   D5  16   C5  16 
    A4  -8   C4  16   E4  16   A4  16 
    B4  -8   E4  16   GS4  16   B4  16 
    C5  8   REST  16   E4  16   E5  16    DS5  16 
  
    E5  16   DS5  16   E5  16   B4  16   D5  16   C5  16 ;6
    A4  -8   C4  16   E4  16   A4  16  
    B4  -8   E4  16   C5  16   B4  16  
    A4  8  REST  16   B4  16   C5  16   D5  16  ;10 - 2nd ending
    ;continues from 11
    E5  -8   G4  16   F5  16   E5  16  
    D5  -8   F4  16   E5  16   D5  16  ;12
  
    C5  -8   E4  16   D5  16   C5  16  ;13
    B4  8  REST  16   E4  16   E5  16  REST  16 
    REST  16   E5  16   E6  16  REST  16  REST  16   DS5  16 
    E5  16  REST  16  REST  16   DS5  16   E5  16   DS5  16 
    E5  16   DS5  16   E5  16   B4  16   D5  16   C5  16 
    A4  8  REST  16   C4  16   E4  16   A4  16 
  
    B4  8  REST  16   E4  16   GS4  16   B4  16  ;19
    C5  8  REST  16   E4  16   E5  16    DS5  16 
    E5  16   DS5  16   E5  16   B4  16   D5  16   C5  16 
    A4  8  REST  16   C4  16   E4  16   A4  16 
    B4  8  REST  16   E4  16   C5  16   B4  16 
    A4  8  REST  16   B4  16   C5  16   D5  16  ;24 (1st ending)
  
    ;repeats from 11
    E5  -8   G4  16   F5  16   E5  16  
    D5  -8   F4  16   E5  16   D5  16  ;12
  
    C5  -8   E4  16   D5  16   C5  16  ;13
    B4  8  REST  16   E4  16   E5  16  REST  16 
    REST  16   E5  16   E6  16  REST  16  REST  16   DS5  16 
    E5  16  REST  16  REST  16   DS5  16   E5  16   DS5  16 
    E5  16   DS5  16   E5  16   B4  16   D5  16   C5  16 
    A4  8  REST  16   C4  16   E4  16   A4  16 
  
    B4  8  REST  16   E4  16   GS4  16   B4  16  ;19
    C5  8  REST  16   E4  16   E5  16    DS5  16 
    E5  16   DS5  16   E5  16   B4  16   D5  16   C5  16 
    A4  8  REST  16   C4  16   E4  16   A4  16 
    B4  8  REST  16   E4  16   C5  16   B4  16 
    A4  8  REST  16   C5  16   C5  16   C5  16  ;25 - 2nd ending

    ;continues from 26
    C5   4   F5  -16   E5  32  ;26
    E5  8   D5  8   AS5  -16   A5  32 
    A5  16   G5  16   F5  16   E5  16   D5  16   C5  16 
    AS4  8   A4  8   A4  32   G4  32   A4  32   B4  32 
    C5   4   D5  16   DS5  16 
    E5  -8   E5  16   F5  16   A4  16 
    C5   4    D5  -16   B4  32 
  
  
    C5  32   G5  32   G4  32   G5  32   A4  32   G5  32   B4  32   G5  32   C5  32   G5  32   D5  32   G5  32  ;33
    E5  32   G5  32   C6  32   B5  32   A5  32   G5  32   F5  32   E5  32   D5  32   G5  32   F5  32   D5  32 
    C5  32   G5  32   G4  32   G5  32   A4  32   G5  32   B4  32   G5  32   C5  32   G5  32   D5  32   G5  32 

    E5  32   G5  32   C6  32   B5  32   A5  32   G5  32   F5  32   E5  32   D5  32   G5  32   F5  32   D5  32  ;36
    E5  32   F5  32   E5  32   DS5  32   E5  32   B4  32   E5  32   DS5  32   E5  32   B4  32   E5  32   DS5  32 
    E5  -8   B4  16   E5  16   DS5  16 
    E5  -8   B4  16   E5  16  REST  16 

    REST  16   DS5  16   E5  16  REST  16  REST  16   DS5  16  ;40
    E5  16   DS5  16   E5  16   B4  16   D5  16   C5  16 
    A4  8  REST  16   C4  16   E4  16   A4  16 
    B4  8  REST  16   E4  16   GS4  16   B4  16 
    C5  8  REST  16   E4  16   E5  16   DS5  16 
    E5  16   DS5  16   E5  16   B4  16   D5  16   C5  16 

    A4  8  REST  16   C4  16   E4  16   A4  16  ;46
    B4  8  REST  16   E4  16   C5  16   B4  16 
    A4  8  REST  16   B4  16   C5  16   D5  16 
    E5  -8   G4  16   F5  16   E5  16 
    D5  -8   F4  16   E5  16   D5  16 
    C5  -8   E4  16   D5  16   C5  16 
    B4  8  REST  16   E4  16   E5  16  REST  16 
    REST  16   E5  16   E6  16  REST  16  REST  16   DS5  16 

    E5  16  REST  16  REST  16   DS5  16   E5  16   D5  16  ;54
    E5  16   DS5  16   E5  16   B4  16   D5  16   C5  16 
    A4  8  REST  16   C4  16   E4  16   A4  16 
    B4  8  REST  16   E4  16   GS4  16   B4  16 
    C5  8  REST  16   E4  16   E5  16   DS5  16 
    E5  16   DS5  16   E5  16   B4  16   D5  16   C5  16 
  
    A4  8  REST  16   C4  16   E4  16   A4  16  ;60
    B4  8  REST  16   E4  16   C5  16   B4  16 
    A4  8  REST  16  REST  16  REST  8  
    CS5   -4  
    D5   4   E5  16   F5  16 
    F5   4   F5  8  
    E5   -4 
    D5   4   C5  16   B4  16 
    A4   4   A4  8 
    A4  8   C5  8   B4  8 
    A4   -4 
    CS5   -4 

    D5   4   E5  16   F5  16  ;72
    F5   4   F5  8 
    F5   -4 
    DS5   4   D5  16   C5  16 
    AS4   4   A4  8 
    GS4   4   G4  8 
    A4   -4 
    B4   4  REST  8 
    A3  -32   C4  -32   E4  -32   A4  -32   C5  -32   E5  -32   D5  -32   C5  -32   B4  -32 

    A4  -32   C5  -32   E5  -32   A5  -32   C6  -32   E6  -32   D6  -32   C6  -32   B5  -32  ;80
    A4  -32   C5  -32   E5  -32   A5  -32   C6  -32   E6  -32   D6  -32   C6  -32   B5  -32 
    AS5  -32   A5  -32   GS5  -32   G5  -32   FS5  -32   F5  -32   E5  -32   DS5  -32   D5  -32 

    CS5  -32   C5  -32   B4  -32   AS4  -32   A4  -32   GS4  -32   G4  -32   FS4  -32   F4  -32  ;84
    E4  16   DS5  16   E5  16   B4  16   D5  16   C5  16 
    A4  -8   C4  16   E4  16   A4  16 
    B4  -8   E4  16   GS4  16   B4  16 

    C5  8  REST  16   E4  16   E5  16   DS5  16  ;88
    E5  16   DS5  16   E5  16   B4  16   D5  16   C5  16  
    A4  -8   C4  16   E4  16   A4  16  
    B4  -8   E4  16   C5  16   B4  16  
    A4  -8  REST  -8 
    REST  -8   G4  16   F5  16   E5  16 
    D5   4  REST  8 
    REST  -8   E4  16   D5  16   C5  16 
  
    B4  -8   E4  16   E5  8  ;96
    E5  8   E6  -8   DS5  16 
    E5  16  REST  16  REST  16   DS5  16   E5  16   DS5  16 
    E5  16   DS5  16   E5  16   B4  16   D5  16   C5  16 
    A4  -8   C4  16   E4  16   A4  16 
    B4  -8   E4  16   GS4  16   B4  16 

    C5  8  REST  16   E4  16   E5  16   DS5  16  ;102
    E5  16   DS5  16   E5  16   B4  16   D5  16   C5  16 
    A4  -8   C4  16   E4  16   A4  16 
    B4  -8   E4  16   C5  16   B4  16 
    A4   -4 )))

(define pinkpanther-song
  (make-song
   (list
    REST 2  REST 4  REST 8   DS4 8  
    E4 -4  REST 8   FS4 8   G4 -4  REST 8   DS4 8 
    E4 -8   FS4 8    G4 -8   C5 8   B4 -8   E4 8   G4 -8   B4 8    
    AS4 2   A4 -16   G4 -16   E4 -16   D4 -16  
    E4 2  REST 4  REST 8   DS4 4 

    E4 -4  REST 8   FS4 8   G4 -4  REST 8   DS4 8 
    E4 -8   FS4 8    G4 -8   C5 8   B4 -8   G4 8   B4 -8   E5 8 
    DS5 1    
    D5 2  REST 4  REST 8   DS4 8  
    E4 -4  REST 8   FS4 8   G4 -4  REST 8   DS4 8 
    E4 -8   FS4 8    G4 -8   C5 8   B4 -8   E4 8   G4 -8   B4 8    
  
    AS4 2   A4 -16   G4 -16   E4 -16   D4 -16  
    E4 -4  REST 4 
    REST 4   E5 -8   D5 8   B4 -8   A4 8   G4 -8   E4 -8 
    AS4 16   A4 -8   AS4 16   A4 -8   AS4 16   A4 -8   AS4 16   A4 -8    
    G4 -16   E4 -16   D4 -16   E4 16   E4 16   E4 2)))

(define supermario-song
  (make-song
   (list
    E5 8   E5 8  REST 8   E5 8  REST 8   C5 8   E5 8  ;1
    G5 4  REST 4   G4 8  REST 4  
    C5 -4   G4 8  REST 4   E4 -4  ; 3
    A4 4   B4 4   AS4 8   A4 4 
    G4 -8   E5 -8   G5 -8   A5 4   F5 8   G5 8 
    REST 8   E5 4  C5 8   D5 8   B4 -4 
    C5 -4   G4 8  REST 4   E4 -4  ; repeats from 3
    A4 4   B4 4   AS4 8   A4 4 
    G4 -8   E5 -8   G5 -8   A5 4   F5 8   G5 8 
    REST 8   E5 4  C5 8   D5 8   B4 -4 

  
    REST 4   G5 8   FS5 8   F5 8   DS5 4   E5 8 ;7
    REST 8   GS4 8   A4 8   C4 8  REST 8   A4 8   C5 8   D5 8 
    REST 4   DS5 4  REST 8   D5 -4 
    C5 2  REST 2 

    REST 4   G5 8   FS5 8   F5 8   DS5 4   E5 8 ;repeats from 7
    REST 8   GS4 8   A4 8   C4 8  REST 8   A4 8   C5 8   D5 8 
    REST 4   DS5 4  REST 8   D5 -4 
    C5 2  REST 2 

    C5 8   C5 4   C5 8  REST 8   C5 8   D5 4 ;11
    E5 8   C5 4   A4 8   G4 2 

    C5 8   C5 4   C5 8  REST 8   C5 8   D5 8   E5 8 ;13
    REST 1  
    C5 8   C5 4   C5 8  REST 8   C5 8   D5 4 
    E5 8   C5 4   A4 8   G4 2 
    E5 8   E5 8  REST 8   E5 8  REST 8   C5 8   E5 4 
    G5 4  REST 4   G4 4  REST 4  
    C5 -4   G4 8  REST 4   E4 -4  ; 19
  
    A4 4   B4 4   AS4 8   A4 4 
    G4 -8   E5 -8   G5 -8   A5 4   F5 8   G5 8 
    REST 8   E5 4   C5 8   D5 8   B4 -4 

    C5 -4   G4 8  REST 4   E4 -4  ; repeats from 19
    A4 4   B4 4   AS4 8   A4 4 
    G4 -8   E5 -8   G5 -8   A5 4   F5 8   G5 8 
    REST 8   E5 4   C5 8   D5 8   B4 -4 

    E5 8   C5 4   G4 8  REST 4   GS4 4 ;23
    A4 8   F5 4   F5 8   A4 2 
    D5 -8   A5 -8   A5 -8   A5 -8   G5 -8   F5 -8 
  
    E5 8   C5 4   A4 8   G4 2  ;26
    E5 8   C5 4   G4 8  REST 4   GS4 4 
    A4 8   F5 4   F5 8   A4 2 
    B4 8   F5 4   F5 8   F5 -8   E5 -8   D5 -8 
    C5 8   E4 4   E4 8   C4 2 

    E5 8   C5 4   G4 8  REST 4   GS4 4 ;repeats from 23
    A4 8   F5 4   F5 8   A4 2 
    D5 -8   A5 -8   A5 -8   A5 -8   G5 -8   F5 -8 
  
    E5 8   C5 4   A4 8   G4 2  ;26
    E5 8   C5 4   G4 8  REST 4   GS4 4 
    A4 8   F5 4   F5 8   A4 2 
    B4 8   F5 4   F5 8   F5 -8   E5 -8   D5 -8 
    C5 8   E4 4   E4 8   C4 2 
    C5 8   C5 4   C5 8  REST 8   C5 8   D5 8   E5 8 
    REST 1 

    C5 8   C5 4   C5 8  REST 8   C5 8   D5 4  ;33
    E5 8   C5 4   A4 8   G4 2 
    E5 8   E5 8  REST 8   E5 8  REST 8   C5 8   E5 4 
    G5 4  REST 4   G4 4  REST 4  
    E5 8   C5 4   G4 8  REST 4   GS4 4 
    A4 8   F5 4   F5 8   A4 2 
    D5 -8   A5 -8   A5 -8   A5 -8   G5 -8   F5 -8 
  
    E5 8   C5 4   A4 8   G4 2  ;40
    E5 8   C5 4   G4 8  REST 4   GS4 4 
    A4 8   F5 4   F5 8   A4 2 
    B4 8   F5 4   F5 8   F5 -8   E5 -8   D5 -8 
    C5 8   E4 4   E4 8   C4 2 
  
    ;game over sound
    C5 -4   G4 -4   E4 4  ;45
    A4 -8   B4 -8   A4 -8   GS4 -8   AS4 -8   GS4 -8 
    G4 8   D4 8   E4 -2   )))

(define birthday-song
  (make-song
   (list
    C4 4   C4 8  
    D4 -4   C4 -4   F4 -4 
    E4 -2   C4 4   C4 8  
    D4 -4   C4 -4   G4 -4 
    F4 -2   C4 4   C4 8 

    C5 -4   A4 -4   F4 -4  
    E4 -4   D4 -4   AS4 4   AS4 8 
    A4 -4   F4 -4   G4 -4 
    F4 -2 )))

(define rickrolling-song
  (make-song
   (list
       D5 -4   E5 -4   A4 4  ;1
   E5 -4   FS5 -4   A5 16   G5 16   FS5 8 
   D5 -4   E5 -4   A4 2 
   A4 16   A4 16   B4 16   D5 8   D5 16 
   D5 -4   E5 -4   A4 4  ;repeat from 1
   E5 -4   FS5 -4   A5 16   G5 16   FS5 8 
   D5 -4   E5 -4   A4 2 
   A4 16   A4 16   B4 16   D5 8   D5 16 
  REST 4   B4 8   CS5 8   D5 8   D5 8   E5 8   CS5 -8 
   B4 16   A4 2  REST 4  

  REST 8   B4 8   B4 8   CS5 8   D5 8   B4 4   A4 8  ;7
   A5 8  REST 8   A5 8   E5 -4  REST 4  
   B4 8   B4 8   CS5 8   D5 8   B4 8   D5 8   E5 8  REST 8 
  REST 8   CS5 8   B4 8   A4 -4  REST 4 
  REST 8   B4 8   B4 8   CS5 8   D5 8   B4 8   A4 4 
   E5 8   E5 8   E5 8   FS5 8   E5 4  REST 4 
   
   D5 2   E5 8   FS5 8   D5 8  ;13
   E5 8   E5 8   E5 8   FS5 8   E5 4   A4 4 
  REST 2   B4 8   CS5 8   D5 8   B4 8 
  REST 8   E5 8   FS5 8   E5 -4   A4 16   B4 16   D5 16   B4 16 
   FS5 -8   FS5 -8   E5 -4   A4 16   B4 16   D5 16   B4 16 

   E5 -8   E5 -8   D5 -8   CS5 16   B4 -8   A4 16   B4 16   D5 16   B4 16  ;18
   D5 4   E5 8   CS5 -8   B4 16   A4 8   A4 8   A4 8  
   E5 4   D5 2   A4 16   B4 16   D5 16   B4 16 
   FS5 -8   FS5 -8   E5 -4   A4 16   B4 16   D5 16   B4 16 
   A5 4   CS5 8   D5 -8   CS5 16   B4 8   A4 16   B4 16   D5 16   B4 16 

   D5 4   E5 8   CS5 -8   B4 16   A4 4   A4 8   ;23
   E5 4   D5 2  REST 4 
  REST 8   B4 8   D5 8   B4 8   D5 8   E5 4  REST 8 
  REST 8   CS5 8   B4 8   A4 -4  REST 4 
  REST 8   B4 8   B4 8   CS5 8   D5 8   B4 8   A4 4 
  REST 8   A5 8   A5 8   E5 8   FS5 8   E5 8   D5 8 
  
  REST 8   A4 8   B4 8   CS5 8   D5 8   B4 8  ;29
  REST 8   CS5 8   B4 8   A4 -4  REST 4 
   B4 8   B4 8   CS5 8   D5 8   B4 8   A4 4  REST 8 
  REST 8   E5 8   E5 8   FS5 4   E5 -4  
   D5 2   D5 8   E5 8   FS5 8   E5 4  
   E5 8   E5 8   FS5 8   E5 8   A4 8   A4 4 

  REST -4   A4 8   B4 8   CS5 8   D5 8   B4 8  ;35
  REST 8   E5 8   FS5 8   E5 -4   A4 16   B4 16   D5 16   B4 16 
   FS5 -8   FS5 -8   E5 -4   A4 16   B4 16   D5 16   B4 16 
   E5 -8   E5 -8   D5 -8   CS5 16   B4 8   A4 16   B4 16   D5 16   B4 16 
   D5 4   E5 8   CS5 -8   B4 16   A4 4   A4 8  

    E5 4   D5 2   A4 16   B4 16   D5 16   B4 16  ;40
   FS5 -8   FS5 -8   E5 -4   A4 16   B4 16   D5 16   B4 16 
   A5 4   CS5 8   D5 -8   CS5 16   B4 8   A4 16   B4 16   D5 16   B4 16 
   D5 4   E5 8   CS5 -8   B4 16   A4 4   A4 8   
   E5 4   D5 2   A4 16   B4 16   D5 16   B4 16 
   
   FS5 -8   FS5 -8   E5 -4   A4 16   B4 16   D5 16   B4 16  ;45
   A5 4   CS5 8   D5 -8   CS5 16   B4 8   A4 16   B4 16   D5 16   B4 16 
   D5 4   E5 8   CS5 -8   B4 16   A4 4   A4 8   
   E5 4   D5 2   A4 16   B4 16   D5 16   B4 16 
   FS5 -8   FS5 -8   E5 -4   A4 16   B4 16   D5 16   B4 16  ;45
  
   A5 4   CS5 8   D5 -8   CS5 16   B4 8   A4 16   B4 16   D5 16   B4 16 
   D5 4   E5 8   CS5 -8   B4 16   A4 4   A4 8  

   E5 4   D5 2  REST 4)))

(define merrychristmas-song
  (make-song
   (list
       C5 4  ;1
   F5 4   F5 8   G5 8   F5 8   E5 8 
   D5 4   D5 4   D5 4 
   G5 4   G5 8   A5 8   G5 8   F5 8 
   E5 4   C5 4   C5 4 
   A5 4   A5 8   AS5 8   A5 8   G5 8 
   F5 4   D5 4   C5 8   C5 8 
   D5 4   G5 4   E5 4 

   F5 2   C5 4  ;8 
   F5 4   F5 8   G5 8   F5 8   E5 8 
   D5 4   D5 4   D5 4 
   G5 4   G5 8   A5 8   G5 8   F5 8 
   E5 4   C5 4   C5 4 
   A5 4   A5 8   AS5 8   A5 8   G5 8 
   F5 4   D5 4   C5 8   C5 8 
   D5 4   G5 4   E5 4 
   F5 2   C5 4 

   F5 4   F5 4   F5 4 ;17
   E5 2   E5 4 
   F5 4   E5 4   D5 4 
   C5 2   A5 4 
   AS5 4   A5 4   G5 4 
   C6 4   C5 4   C5 8   C5 8 
   D5 4   G5 4   E5 4 
   F5 2   C5 4  
   F5 4   F5 8   G5 8   F5 8   E5 8 
   D5 4   D5 4   D5 4 
  
   G5 4   G5 8   A5 8   G5 8   F5 8  ;27
   E5 4   C5 4   C5 4 
   A5 4   A5 8   AS5 8   A5 8   G5 8 
   F5 4   D5 4   C5 8   C5 8 
   D5 4   G5 4   E5 4 
   F5 2   C5 4 
   F5 4   F5 4   F5 4 
   E5 2   E5 4 
   F5 4   E5 4   D5 4 
  
   C5 2   A5 4 ;36
   AS5 4   A5 4   G5 4 
   C6 4   C5 4   C5 8   C5 8 
   D5 4   G5 4   E5 4 
   F5 2   C5 4  
   F5 4   F5 8   G5 8   F5 8   E5 8 
   D5 4   D5 4   D5 4 
   G5 4   G5 8   A5 8   G5 8   F5 8  
   E5 4   C5 4   C5 4 
  
   A5 4   A5 8   AS5 8   A5 8   G5 8 ;45
   F5 4   D5 4   C5 8   C5 8 
   D5 4   G5 4   E5 4 
   F5 2   C5 4 
   F5 4   F5 8   G5 8   F5 8   E5 8 
   D5 4   D5 4   D5 4 
   G5 4   G5 8   A5 8   G5 8   F5 8 
   E5 4   C5 4   C5 4 
  
   A5 4   A5 8   AS5 8   A5 8   G5 8  ;53
   F5 4   D5 4   C5 8   C5 8 
   D5 4   G5 4   E5 4 
   F5 2  REST 4)))


(define nokia-song
  (make-song
   (list
       E5  8   D5  8   FS4  4   GS4  4  
   CS5  8   B4  8   D4  4   E4  4  
   B4  8   A4  8   CS4  4   E4  4 
   A4  2  )))


(define brahmslullaby-song
  (make-song
    (list
   G4  4   G4  4  ;1
   AS4  -4   G4  8   G4  4 
   AS4  4  REST  4   G4  8   AS4  8 
   DS5  4   D5  -4   C5  8 
   C5  4   AS4  4   F4  8   G4  8 
   GS4  4   F4  4   F4  8   G4  8 
   GS4  4  REST  4   F4  8   GS4  8 
   D5  8   C5  8   AS4  4   D5  4 

   DS5  4  REST  4   DS4  8   DS4  8  ;8
   DS5  2   C5  8   GS4  8 
   AS4  2   G4  8   DS4  8 
   GS4  4   AS4  4   C5  4 
   AS4  2   DS4  8   DS4  8 
   DS5  2   C5  8   GS4  8 
   AS4  2   G4  8   DS4  8 
   AS4  4   G4  4   DS4  4 
   DS4  2)))


(define songs (vector (list beethoven-song 1)
		      (list rickrolling-song 0.5)
		      (list birthday-song 0.5)
                      (list brahmslullaby-song 0.5)
		      (list pinkpanther-song 0.5)
                      (list supermario-song 0.5)
                      (list merrychristmas-song 0.5)
                      (list nokia-song 0.5)
                      (list calm-song 0.2)))

(define (get-song which-song)
  (vector-ref songs which-song))
                      
                      

                      


