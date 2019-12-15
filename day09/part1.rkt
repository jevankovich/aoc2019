#lang typed/racket

(struct intcode-vm
  ([pc : Integer]
   [tape : (Mutable-HashTable Integer Integer)]
   [rel : Integer]
   [in : (Listof Integer)])
  #:mutable
  #:type-name Intcode-VM)

(: make-intcode-vm (-> (Mutable-HashTable Integer Integer) (Listof Integer) Intcode-VM))
(define (make-intcode-vm initial-memory input)
  (intcode-vm 0 (hash-copy initial-memory) 0 input))

(define (default) 0)

(: get-arg (-> Intcode-VM Integer Integer Integer))
(define (get-arg vm pc-offset mode)
  (define t (intcode-vm-tape vm))
  (define index (+ (intcode-vm-pc vm) pc-offset))
  (cond [(= mode 0) (hash-ref t (hash-ref t index default) default)]
        [(= mode 1) (hash-ref t index default)]
        [(= mode 2) (hash-ref t (+ (hash-ref t index default) (intcode-vm-rel vm)) default)]
        [else 0]))

(: set-arg! (-> Intcode-VM Integer Integer Integer Void))
(define (set-arg! vm pc-offset mode value)
  (define t (intcode-vm-tape vm))
  (define index (+ (intcode-vm-pc vm) pc-offset))
  (assert (or (= mode 0) (= mode 2)))
  (cond [(= mode 0) (hash-set! t (hash-ref t index default) value)]
        [(= mode 2) (hash-set! t (+ (hash-ref t index default) (intcode-vm-rel vm)) value)]))

(: binop (-> (-> Integer Integer Integer) (-> Intcode-VM Void)))
(define (binop f)
  (lambda (vm)
    (define pc (intcode-vm-pc vm))
    (define instruction (hash-ref (intcode-vm-tape vm) pc))
    (define mode-one (remainder (quotient instruction 100) 10))
    (define mode-two (remainder (quotient instruction 1000) 10))
    (define mode-three (remainder (quotient instruction 10000) 10))
    (define arg-one (get-arg vm 1 mode-one))
    (define arg-two (get-arg vm 2 mode-two))
    (set-arg! vm 3 mode-three (f arg-one arg-two))
    (set-intcode-vm-pc! vm (+ pc 4))))

(: jump (-> (-> Integer Boolean) (-> Intcode-VM Void)))
(define (jump f)
  (lambda (vm)
    (define pc (intcode-vm-pc vm))
    (define instruction (hash-ref (intcode-vm-tape vm) pc))
    (define mode-one (remainder (quotient instruction 100) 10))
    (define mode-two (remainder (quotient instruction 1000) 10))
    (define arg (get-arg vm 1 mode-one))
    (define target (get-arg vm 2 mode-two))
    (if (f arg)
        (set-intcode-vm-pc! vm target)
        (set-intcode-vm-pc! vm (+ pc 3)))))

(: in (-> Intcode-VM Void))
(define (in vm)
  (define pc (intcode-vm-pc vm))
  (define instruction (hash-ref (intcode-vm-tape vm) pc))
  (define mode (remainder (quotient instruction 100) 10))
  (set-arg! vm 1 mode (first (intcode-vm-in vm)))
  (set-intcode-vm-in! vm (rest (intcode-vm-in vm)))
  (set-intcode-vm-pc! vm (+ pc 2)))

(: out (-> Intcode-VM Integer))
(define (out vm)
  (define pc (intcode-vm-pc vm))
  (define instruction (hash-ref (intcode-vm-tape vm) pc))
  (define mode (remainder (quotient instruction 100) 10))
  (define result (get-arg vm 1 mode))
  (set-intcode-vm-pc! vm (+ pc 2))
  result)

(: set-rel (-> Intcode-VM Void))
(define (set-rel vm)
  (define pc (intcode-vm-pc vm))
  (define instruction (hash-ref (intcode-vm-tape vm) pc))
  (define mode (remainder (quotient instruction 100) 10))
  (set-intcode-vm-rel! vm (+ (intcode-vm-rel vm) (get-arg vm 1 mode)))
  (set-intcode-vm-pc! vm (+ pc 2)))

(define ops
  (hash 1 (binop +)
        2 (binop *)
        3 in
        4 out
        5 (jump (lambda (x) (not (zero? x))))
        6 (jump zero?)
        7 (binop (lambda (x y) (if (< x y) 1 0)))
        8 (binop (lambda (x y) (if (= x y) 1 0)))
        9 set-rel))

(: run-instruction (-> Intcode-VM (U Void Integer)))
(define (run-instruction vm)
  (define pc (intcode-vm-pc vm))
  (define instruction (hash-ref (intcode-vm-tape vm) pc default))
  ((hash-ref ops (remainder instruction 100)) vm))

(: run-program (-> (Mutable-HashTable Integer Integer) (Listof Integer) (Listof Integer)))
(define (run-program mem input)
  (define vm (make-intcode-vm mem input))
  (define out : (Listof Integer) '())
  (define (loop) : Void
    (define result (run-instruction vm))
    (if (void? result) (void) (set! out (cons result out)))
    (if (= (hash-ref (intcode-vm-tape vm) (intcode-vm-pc vm) default) 99)
        (void)
        (loop)))
  (loop)
  (reverse out))

(define (string->program [s : String]) : (Mutable-HashTable Integer Integer)
  (define instr-list
    (map (lambda ([x : String]) (cast (string->number x) Integer)) (string-split (string-trim s) ",")))
  (make-hash (for/list : (Listof (Pairof Integer Integer)) ([i : Integer (in-naturals)]
                        [op : Integer instr-list])
               (cons i op))))

(define program (string->program (assert (read-line) string?)))
(define input (cast (read) (Listof Integer)))

(displayln (run-program program input))