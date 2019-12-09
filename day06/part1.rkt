#lang racket

(define orbits (make-hash))
(define total-orbits 0)

(for ([orbit (in-lines)])
  (define-values (_ com body) (apply values (regexp-match #rx"^(.+)[)](.+)$" orbit)))
  (hash-set! orbits body com))

(define (count-orbits body)
  (if (equal? body "COM")
      0
      (+ 1 (count-orbits (hash-ref orbits body)))))

(for/fold ([sum 0]) ([body (in-hash-keys orbits)])
  (+ sum (count-orbits body)))