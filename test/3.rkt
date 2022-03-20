#lang plait

(require "../boolean.rkt")

(module+ test
  (define (interp* e) (interp (parse e) mt-env))
  (test (interp* `(* (+ (+ 1 2) (* 3 4))
                     10))
        (numV 150))
  (test (interp* `((if (< (+ 10 (* 1 2))
                         22)
                       (lambda(x) x)
                       (lambda(x) (+ x 1)))
                   42))
        (numV 42))
  (test (interp* `#t) (trueV)))


