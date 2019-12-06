#lang racket

(define (run v)
  (for ([i (in-range 0 (vector-length v) 4)]
        #:break (= 99 (vector-ref v i)))
    (define op (vector-ref v i))
    (define pos1 (vector-ref v (+ i 1)))
    (define pos2 (vector-ref v (+ i 2)))
    (define dest (vector-ref v (+ i 3)))
    (cond
      [(= op 1)
       (vector-set! v dest (+ (vector-ref v pos1) (vector-ref v pos2)))]
      [(= op 2)
       (vector-set! v dest (* (vector-ref v pos1) (vector-ref v pos2)))]))
  v)

(define program (list->vector (map string->number (string-split (string-trim (file->string "input")) ","))))

(vector-set! program 1 12)
(vector-set! program 2 2)

(display (vector-ref (run program) 0))