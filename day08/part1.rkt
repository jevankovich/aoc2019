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

(define (count/image value image)
  (for/list ([layer image])
    (count/layer value layer)))

(define (count/layer value layer)
  (for*/fold ([count 0]) ([row layer] [x row])
      (if (= x value)
          (+ count 1)
          count)))

(define (checksum image)
  (define layer (vector-argmin (lambda (l) (count/layer 0 l)) image))
  (* (count/layer 1 layer) (count/layer 2 layer)))

(checksum (read-image 25 6 (read-line)))