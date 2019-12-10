#lang racket

(define (read-image w h data)
  (define raw-data (for/vector ([c data])
                     (- (char->integer c) (char->integer #\0))))
  (define layers (quotient (vector-length raw-data) (* w h)))
  (for/vector ([l layers])
    (for/vector ([y h])
      (for/vector ([x w])
        (define index (+ (* w h l) (* w y) x))
        (vector-ref raw-data index)))))

(define (layer-ref layer x y)
  (vector-ref (vector-ref layer y) x))

(define (layer-set! layer x y value)
  (vector-set! (vector-ref layer y) x value))

(define (composite image)
  (define final-layer (vector-ref image 0))
  (for ([layer image]
        #:when #t
        [y (vector-length layer)]
        [row layer]
        #:when #t
        [x (vector-length row)]
        [sample row])
    (if (= (layer-ref final-layer x y) 2)
        (layer-set! final-layer x y sample)
        (void)))
  final-layer)

(define (display-layer layer)
  (for ([row layer])
    (for ([sample row])
      (display sample))
    (newline)))

(display-layer (composite (read-image 25 6 (read-line))))