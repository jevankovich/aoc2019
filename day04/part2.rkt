#lang racket

(define (valid-password? s)
  (and (= 6 (string-length s))
       (monotonically-increasing? s)
       (has-exact-pair? s)))

(define (monotonically-increasing? s)
  (or (< (string-length s) 2)
      (and (char<=? (string-ref s 0) (string-ref s 1))
           (monotonically-increasing? (substring s 1)))))

(define (has-pair? s)
  (and (>= (string-length s) 2)
       (or (char=? (string-ref s 0) (string-ref s 1))
           (has-pair? (substring s 1)))))

(define (has-exact-pair? s)
  (define (count-group s)
    (define finger (string-ref s 0))
    (for/sum ([c s]
              #:when (char=? c finger))
      1))
  (if (non-empty-string? s)
      (let ([sz (count-group s)])
        (or (= sz 2) (has-exact-pair? (substring s sz))))
      #f))

(for/sum ([i (in-range 382345 843167)])
  (if (valid-password? (~r i #:min-width 6 #:pad-string "0")) 1 0))