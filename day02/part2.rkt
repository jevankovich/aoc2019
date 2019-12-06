#lang racket

(define (run program noun verb)
  (define prog (vector-copy program))
  (vector-set! prog 1 noun)
  (vector-set! prog 2 verb)
  (for ([i (in-range 0 (vector-length prog) 4)]
        #:break (= 99 (vector-ref prog i)))
    (define op (vector-ref prog i))
    (define pos1 (vector-ref prog (+ i 1)))
    (define pos2 (vector-ref prog (+ i 2)))
    (define dest (vector-ref prog (+ i 3)))
    (cond
      [(= op 1)
       (vector-set! prog dest (+ (vector-ref prog pos1) (vector-ref prog pos2)))]
      [(= op 2)
       (vector-set! prog dest (* (vector-ref prog pos1) (vector-ref prog pos2)))]))
  prog)

(define program (list->vector (map string->number (string-split (string-trim (file->string "input")) ","))))

(for* ([i (in-range 100)]
       [j (in-range 100)])
  (define out (vector-ref (run program i j) 0))
  (if (= out 19690720)
      (displayln (+ (* 100 i) j))
      '()))