#lang racket

(require "./lib/defmac.rkt")


(struct obj (class values))

(define Root
  (λ (msg . args)
    (match msg
      ['-lookup     (error "message not understood:" (first args))]
      ['-all-fields '()])))

(defmac (CLASS extends superclass
               ([field f init] ...)
               ([method m (param ...) body ...] ...))
  #:keywords field method extends
  #:captures self ? ! ↑
  (let* ([scls superclass]
         [fields (append (scls '-all-fields)
                         (list (cons 'f init) ...))]
         [methods
          (local [(defmac (? fd) #:captures self
                    (vector-ref (obj-values self)
                                (find-last 'fd fields)))
                  (defmac (! fd v) #:captures self
                    (vector-set! (obj-values self)
                                 (find-last 'fd fields)
                                 v))
                  (defmac (↑ md . args) #:captures self
                    ((scls '-lookup 'md) self . args))]
            (list (cons 'm (λ (self param ...) body ...)) ...))])
    (letrec ([class
                 (λ (msg . args)
                   (match msg
                     ['-all-fields fields]
                     ['-create
                      (let ([values (list->vector (map cdr fields))])
                        (obj class values))]
                     ['-lookup
                      (let ([found (assoc (first args) methods)])
                        (if found
                            (cdr found)
                  (scls '-lookup (first args))))]))])
      class)))

(define (new c)
  (c '-create))

(defmac (→ o m arg ...)
  (let ([obj o])
    (((obj-class obj) '-lookup 'm) obj arg ...)))

;; find-last: symbol * (listof (symbol * value)) -> position
;; (find-last 'a ((a . 1) (a . 2) (a . 3))) => 2
;; (find-last 'a ((a . 1) (b . 2) (b . 3))) => 0
(define (find-last s ls)
 (define (h i last ls)
   (cond [(null? ls) last]
         [(let ([e (caar ls)])
           (if (eq? s e)
               (h (add1 i) i (cdr ls))
               (h (add1 i) last (cdr ls))))]))
  (h 0 #f ls))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define Counter
 (CLASS extends Root
  ([field count 0]
   [field step  1])
  ([method inc () (! count (+ (? count) (? step))) (? count)]
   [method dec () (! count (- (? count) (? step))) (? count)]
   [method reset () (! count 0)]
   [method step! (v) (! step v)]
   [method inc-by! (v) (→ self step! v) (→ self inc)])))

(define ReactiveCounter
  (CLASS extends Counter
         ([field predicate (λ (n) #f)]
          [field action    (λ (n) #f)])
         ([method register (p a) (! predicate p) (! action a)]
          [method react () (when ((? predicate) (? count))
                             ((? action) (? count)))]
          [method inc ()
                  (let ([val (↑ inc)])
                    (→ self react)
                    val)])))
 

(define rc (new ReactiveCounter))
(→ rc register even? (λ (v) (printf "reacting to ~a~n" v)))
; => 1
(→ rc inc)
(→ rc inc)
; =>reacting to 2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define A
 (CLASS extends Root
        ([field x "A"])
        ([method ax () (? x)])))
(define B
  (CLASS extends A
         ([field x "B"])
         ([method bx () (? x)])))
 
;; fix: value of fields shoudle be lexical scope

(define b (new B))
(→ b ax)
; => "A"
(→ b bx)
; => "B"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define A1 (CLASS extends Root ()
            ([method m () "A"])))
(define B1 (CLASS extends A1 ()
            ([method m () (string-append "B" (↑ m) "B")])))
(define C1 (CLASS extends B1 () ()))
 

(define c (new C1))
(→ c m)
; => "BAB"
