#lang racket

(require racket/generator)

(define ops
  (hash 1 (list 2 'set +)
        2 (list 2 'set *)
        3 (list 0 'in void)
        4 (list 1 'out identity)
        5 (list 1 'jump (lambda (x) (not (zero? x))))
        6 (list 1 'jump zero?)
        7 (list 2 'set (lambda (x y) (if (< x y) 1 0)))
        8 (list 2 'set (lambda (x y) (if (= x y) 1 0)))))

(define (get-arg tape index mode)
  (if (= mode 1)
      (vector-ref tape index)
      (vector-ref tape (vector-ref tape index))))

(define (run-instruction tape pc inputs)
  (define opcode (vector-ref tape pc))
  (define mode-one (remainder (quotient opcode 100) 10))
  (define mode-two (remainder (quotient opcode 1000) 10))
  (define-values (in type fn) (apply values (hash-ref ops (remainder opcode 100))))
  (define result (cond [(= in 2)
                        (define arg-one (get-arg tape (+ pc 1) mode-one))
                        (define arg-two (get-arg tape (+ pc 2) mode-two))
                        (fn arg-one arg-two)]
                       [(= in 1)
                        (define arg-one (get-arg tape (+ pc 1) mode-one))
                        (fn arg-one)]
                       [(= in 0) (fn)]))
  (cond [(eq? type 'none)
         (values (+ pc 1 in) inputs (void))]
        [(eq? type 'set)
         (vector-set! tape (vector-ref tape (+ pc 1 in)) result)
         (values (+ pc 1 in 1) inputs (void))]
        [(eq? type 'in)
         (vector-set! tape (vector-ref tape (+ pc 1 in)) (first inputs))
         (values (+ pc 1 in 1) (rest inputs) (void))]
        [(eq? type 'out)
         (values (+ pc 1 in) inputs result)]
        [(eq? type 'jump)
         (define target (get-arg tape (+ pc 2) mode-two))
         (values (if result target (+ pc 1 in 1))
                 inputs
                 (void))]))

(define (run-generator program)
  (generator (inputs)
    (define prog (vector-copy program))
    (define pc 0)
    (define (run-at p pc)
      (if (= 99 (vector-ref p pc))
          (void)
          (let-values ([(new-pc new-inputs new-output) (run-instruction p pc inputs)])
            (set! inputs new-inputs)
            (if (not (void? new-output))
                (set! inputs (append inputs (yield new-output)))
                (void))
            (run-at p new-pc))))
    (run-at prog 0)))

(define (run-amplifiers program phases)
  (define signal 0)
  (define amps (for/list ([p phases])
    (define gen (run-generator program))
    (set! signal (gen (list p signal)))
    gen))
  (for ([gen (in-cycle amps)])
    (define out (gen (list signal)))
    #:break (void? out)
    (set! signal out))
  signal)

(define program (list->vector (map string->number (string-split (string-trim (read-line)) ","))))

(for/fold ([m 0]) ([phases (in-permutations '(5 6 7 8 9))])
  (define thrust (run-amplifiers program phases))
  (max m thrust))