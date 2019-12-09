#lang racket

(define ops
  (hash 1 (list 2 'set +)
        2 (list 2 'set *)
        3 (list 0 'set read)
        4 (list 1 'none displayln)
        5 (list 2 'jump (lambda (x) (not (zero? x))))
        6 (list 2 'jump zero?)
        7 (list 2 'set (lambda (x y) (if (< x y) 1 0)))
        8 (list 2 'set (lambda (x y) (if (= x y) 1 0)))))

(define (get-arg tape index mode)
  (if (= mode 1)
      (vector-ref tape index)
      (vector-ref tape (vector-ref tape index))))

(define (run-instruction tape pc)
  (define opcode (vector-ref tape pc))
  (define mode-one (remainder (quotient opcode 100) 10))
  (define mode-two (remainder (quotient opcode 1000) 10))
  (define-values (in out fn) (apply values (hash-ref ops (remainder opcode 100))))
  (define result (cond [(= in 2)
                        (define arg-one (get-arg tape (+ pc 1) mode-one))
                        (define arg-two (get-arg tape (+ pc 2) mode-two))
                        (fn arg-one arg-two)]
                       [(= in 1)
                        (define arg-one (get-arg tape (+ pc 1) mode-one))
                        (fn arg-one)]
                       [(= in 0) (fn)]))
  (if (= out 1) (vector-set! tape (vector-ref tape (+ pc 1 in)) result) void)
  (+ pc 1 in out))

(define (run program)
  (define prog (vector-copy program))
  (define pc 0)
  (define (run-at p pc)
    (if (= 99 (vector-ref p pc))
        p
        (run-at p (run-instruction p pc))))
  (run-at prog pc))

(define program (list->vector (map string->number (string-split (string-trim (read-line)) ","))))
(run program)
