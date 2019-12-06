#lang racket

(define (required-fuel mass)
  (define fuel-mass (- (quotient mass 3) 2))
  (if (<= fuel-mass 0)
      0
      (+ fuel-mass (required-fuel fuel-mass))))

(display (foldr + 0 (map required-fuel (file->list "input"))))