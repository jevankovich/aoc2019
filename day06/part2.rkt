#lang racket

(define orbits (make-hash))
(define total-orbits 0)

(for ([orbit (in-lines)])
  (define-values (_ com body) (apply values (regexp-match #rx"^(.+)[)](.+)$" orbit)))
  (hash-set! orbits body com))

(define (full-orbit body)
  (if (equal? body "COM")
      '("COM")
      (cons body (full-orbit (hash-ref orbits body)))))

(displayln (full-orbit "YOU"))
(displayln (full-orbit "SAN"))

(define transfers (set-symmetric-difference (list->set (full-orbit "YOU")) (list->set (full-orbit "SAN"))))

(displayln transfers)
(displayln (- (sequence-length transfers) 2))