#lang racket

(define (valid-password? s)
  (and (= 6 (string-length s))
       (monotonically-increasing? s)
       (has-pair? s)))

(define (monotonically-increasing? s)
  (or (< (string-length s) 2)
      (and (char<=? (string-ref s 0) (string-ref s 1))
           (monotonically-increasing? (substring s 1)))))

(define (has-pair? s)
  (and (>= (string-length s) 2)
       (or (char=? (string-ref s 0) (string-ref s 1))
           (has-pair? (substring s 1)))))

(for/sum ([i (in-range 382345 843167)])
  (if (valid-password? (number->string i)) 1 0))