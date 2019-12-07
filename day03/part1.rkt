#lang racket

(define (path dirs)
  (define x 0)
  (define y 0)
  (flatten (for/list ([dir (in-list dirs)])
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
      (vector-immutable x y)))))

(define (manhattan-distance v)
  (for/fold ([sum 0]) ([x v])
    (+ sum (abs x))))

(define (intersection-distance p1 p2)
  (define intersections (set-intersect (list->set p1) (list->set p2)))
  (apply min (map manhattan-distance (set->list intersections))))

(define (parse s)
  (for/list ([d (string-split s ",")])
    (list (string-ref d 0) (string->number (substring d 1)))))

(define wire-one (parse (read-line)))
(define wire-two (parse (read-line)))

(displayln (intersection-distance (path wire-one) (path wire-two)))