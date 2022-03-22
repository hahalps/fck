#lang plait

(require "../lazy.rkt")

(module+ test
  (define (interp* e) (interp (parse e) mt-env))
  (test (interp* `(* (+ (+ 1 2) (* 3 4))
                     10))
        (numV 150))
  (test (interp* `(letrec ([f (lambda(x) (f x))])
                    (let ([g (lambda(x) (lambda(y) y))])
                      ((g (f 1)) 2))))
        (numV 2))
  (test (interp* `(letrec ([f (lambda(n)
                                (if0 n
                                     0
                                     (+ n (f (+ n -1)))))])
                    (f 5)))
        (numV 15))
  (test (interp* `(letrec ([fib (lambda(n)
                                  (if0 (< n 1)
                                       1
                                       (+ (fib (+ n -1))
                                          (fib (+ n -2)))))])
                    (fib 5)))
        (numV 13))
  )



