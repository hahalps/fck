#lang plait

(require "../variable.rkt")

(module+ test
  (define (interp* e) (v*s-v (interp (parse e) mt-env mt-store)))
  (test (interp* `(* (+ (+ 1 2) (* 3 4))
                     10))
        (numV 150))
  (test (interp* `(let ([b (box 1)])
                    (begin
                      (set-box! b 2)
                      (unbox b))))
        (numV 2)))



