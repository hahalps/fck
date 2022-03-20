#lang plait

(require "../arith.rkt")

(module+ test
 (test (interp* `(- (+ (+ 1 2) (* 3 4))
                    10))
       (numV 5)))


