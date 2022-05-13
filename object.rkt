#lang racket

(require "./lib/defmac.rkt")

(defmac (OBJECT ([field fname fval] ...)
                ([method mname mparams mbody ...] ...))
  #:keywords field method
  (let ([fname fval] ...)
    (let ([methods (list (cons 'mname (λ mparams mbody ...)) ...)])
      (λ (msg . args)
        (let ([found (assoc msg methods)])
          (if found
              (apply (cdr found) args)
              (error "message not understood:" msg)))))))


(defmac (→ o m arg ...)
  (o 'm arg ...))

;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;

(define (make-node l r)
 (OBJECT
  ([field left l]
   [field right r])
  ([method sum () (+ (→ left sum) (→ right sum))])))
 
(define (make-leaf v)
 (OBJECT
  ([field value v])
  ([method sum () value])))

(let ([tree (make-node
               (make-node (make-leaf 3)
                          (make-node (make-leaf 10)
                                     (make-leaf 4)))
               (make-leaf 1))])
   (→ tree sum))
 