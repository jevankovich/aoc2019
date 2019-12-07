#lang racket

(define (path dirs)
  (define x 0)
  (define y 0)
  (define steps 0)
  (apply append (for/list ([dir dirs])
    (define direction (first dir))
    (define length (second dir))
    (define-values (dx dy)
      (cond [(equal? #\U direction) (values 0 1)]
            [(equal? #\D direction) (values 0 -1)]
            [(equal? #\R direction) (values 1 0)]
            [(equal? #\L direction) (values -1 0)]))
    (for/list ([_ (in-range length)])
      (set! x (+ x dx))
      (set! y (+ y dy))
      (set! steps (+ steps 1))
      (cons (vector-immutable x y) steps)))))

(define (intersection-distance p1 p2)
  (define worst-case-distance
    (+ (cdr (last p1)) (cdr (last p2))))
  (define path-one (make-hash p1))
  (for/fold ([m worst-case-distance])
             ([y p2])
    (if (hash-has-key? path-one (car y))
        (min m (+ (cdr y) (hash-ref path-one (car y))))
             m)))

(define (parse s)
  (for/list ([d (string-split s ",")])
    (list (string-ref d 0) (string->number (substring d 1)))))

(define wire-one (parse (read-line)))
(define wire-two (parse (read-line)))

(displayln (intersection-distance (path wire-one) (path wire-two)))