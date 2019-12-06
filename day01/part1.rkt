#lang racket

(define (required-fuel mass)
  (- (quotient mass 3) 2))

(display (foldr + 0 (map required-fuel (file->list "input"))))