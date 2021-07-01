#lang racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; EEN MATRIX ADT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; Abdullah Sabaa Allil ;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; hulpprocedures
(define (nthcdr lst nth)
  (if (zero? nth)
      lst
      (nthcdr (cdr lst) (- nth 1))))


(define (list-head lst vanaf tem)
  (define (iter idx lst res)
    (if (= idx tem)
        (reverse res)
        (iter (+ idx 1) (cdr lst) (cons (car lst) res))))
  (iter vanaf (nthcdr lst vanaf) '()))

;; een matrix aanmaken
; number number list -> matrix
(define (list->matrix rows cols els)
  (let ((matrix (make-vector rows 'vec-placeholder)))
      
    (let make-matrix-complete ((idx 0))
      (cond ((<= idx (- rows 1))
             (vector-set! matrix idx
                          (list->vector (list-head els
                                                   (* idx cols)
                                                   (+ cols (* idx cols)))))
             (make-matrix-complete (+ idx 1)))))
    matrix))

;; een "lege" matrix maken
(define (make-matrix rows cols fillobj)
  (let ((matrix (make-vector rows 'vec-placeholder)))
    (let make-matrix-loop ((idx 0))
      (cond ((<= idx (- rows 1))
             (vector-set! matrix idx (make-vector cols fillobj))
             (make-matrix-loop (+ idx 1)))))
    matrix))
;; als alternatief kan ook direct (vector (vector ..) (vector ..) ..) uitgevoerd worden, dit is ook een geldige matrix


;; reffen in  de matrix
(define (matrix-ref mat rows cols)
  (vector-ref (vector-ref mat rows) cols))

;; setten in de matrix
(define (matrix-set! mat rows cols new)
  (vector-set! (vector-ref mat rows) cols new))

;; aantal rijen in de matrix
(define (matrix-rows matrix)
  (vector-length matrix))

;; aantal kolommen in de matrix
(define (matrix-cols matrix)
  (vector-length (vector-ref matrix 0)))

;; een element vinden in de vector en zijn idx teruggeven.
(define (find-in-vector vec el ==?)
  (let loop ((idx 0))
    (if (= idx (vector-length vec))
        #f
    (let ((curr (vector-ref vec idx)))
      (if (==? curr el)
          idx
          (loop (+ idx 1)))))))

;; een element vinden in de vector en het element zelf teruggeven.
(define (find-content-in-vector vec el ==?)
  (let loop ((idx 0))
    (if (= idx (vector-length vec))
        #f
    (let ((curr (vector-ref vec idx)))
      (if (==? curr el)
          curr
          (loop (+ idx 1)))))))

;; een element vinden in de matrix en zijn coordinaten teruggeven in een conscel.
(define (find-in-matrix mat el ==?)
  (let outerloop ((row 0))
    (if (= row (matrix-rows mat))
        #f
    (let ((col (find-in-vector (vector-ref mat row) el ==?)))
    (cond (col (cons row col))
          (else (outerloop (+ row 1))))))))
        

;; een element verwijderen uit de matrix
(define (remove-from-matrix! mat el)
  (let* ((el-cdtes (find-in-matrix mat el eq?))
         (row (car el-cdtes))
         (col (cdr el-cdtes)))
    (matrix-set! mat row col 'deleted)))


;; een procedure for-eachen op een vector
(define (vector-for-each f vec)
  (let loop ((idx 0))
    (when (not (= idx (vector-length vec)))
        (let ((orig-el (vector-ref vec idx)))
          (f orig-el)
          (loop (+ idx 1))))))

;; een procedure for-eachen op een matrix
(define (matrix-for-each matrix proc)
  (vector-for-each (lambda (vecjes)
                     (vector-for-each proc vecjes))
                   matrix))

;; een procedure mappen op een vector
(define (vector-map f vec)
  (define vector-out (make-vector (vector-length vec)))
  (let loop ((idx 0))
    (if (= idx (vector-length vec))
        vector-out
        (let ((orig-el (vector-ref vec idx)))
          (vector-set! vector-out idx (f orig-el))
          (loop (+ idx 1))))))

        
;; een procedure mappen op een matrix
(define (matrix-map matrix proc)
  (vector-map (lambda (vecjes)
                (vector-map proc vecjes))
              matrix))