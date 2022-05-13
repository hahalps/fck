#lang plait #:untyped


(define zero (λ(f) (λ(x) x)))
(define one (λ(f) (λ(x) (f x))))
(define two (λ(f) (λ(x) (f (f x)))))

(define add1
  (λ(n)
    (λ(f) (λ(x) (f ((n f) x))))))

(define add
  (λ(n) (λ(m)
          ((n add1) m))))

(define mult
  (λ(n)
    (λ(m)
      ((n (add m)) zero))))

(define true (λ(x) (λ(y) x)))
(define false (λ(x) (λ(y) y)))

(define pair
  (λ(x) (λ(y) (λ(sel) ((sel x) y)))))

(define (car p) (p true))
(define (cdr p) (p false))

(define iszero?
  (λ(n) ((n (λ(x) false))
         true)))

(define shift
  (λ(p)
    ((pair (cdr p))
     (add1 (cdr p)))))

(define (sub1 n)
  (car ((n shift) ((pair zero) zero))))


(define (node v l r)
  (λ(N L)
    (N v (l N L) (r N L))))

(define (leaf)
  (λ(N L)
    L))

(define my-tree
  (node 1 (node 2
                (node 3
                        (leaf)
                        (leaf))
                (node 4
                      (leaf)
                      (leaf)))
        (leaf)))


(define (preorder t)
  (t (λ(v l r)
       (append (append l r)
               (list v)))
     '()))