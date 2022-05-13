#lang plait #:untyped

(require "../encoding.rkt")

(module+ test
  (define (w f) ((f (λ(x) (+ x 1)))
                   0))

  (test (w one) 1)

  (test (w (add1 (add1 one)))
        3)

  (test (w ((add one) one))
        2)

  (test (w ((mult two) two))
        4)

  (define four ((mult two) two))
  (define s ((mult four) four))
  (test (w (sub1 s))
        15)
  
  (define tp ((pair 1) 2))

  (test (car tp) 1)
  (test (cdr tp) 2)
  
  )
