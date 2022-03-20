#lang plait

(require "../clos.rkt")

(module+ test
  (define (interp* e) (interp (parse e) mt-env))
  (test (interp* `(let ([f (lambda(x) (lambda(y) (+ x y)))])
                    ((f 1) 2)))
        (numV 3)))


