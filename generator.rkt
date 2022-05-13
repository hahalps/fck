#lang plait

;; code from Matthew Flatt
;; url: https://my.eng.utah.edu/~cs3520/f20/generator.rkt

#|
通过 continuation 来实现 generator
要点
1, 进来之前先保存client-k
2, 返回之前需要保存gen-k
|#

; 问题： 为什么body当中yield为什么是unbound
; fix: let => let*
; yield 的定义在 go 中找不到， 草草草


;; HtDP fucking good !!!!

(define-syntax generator
  (syntax-rules ()
    [(generator yield proc)
     (let* ([yield (λ(v) (error 'yield "never reach here"))]
            [go (λ(v) proc)])
       (λ()
         (let/cc client-k
           (begin
             (set! yield
                   (λ(v)
                     (let/cc gen-k
                       (begin
                         (set! go gen-k)
                         (client-k v)))))
             (go 'dummy)))))]))



(define (make-numbers start-n)
  (generator yield 
             (local [(define (numbers n)
                       (begin
                         (yield n) 
                         (numbers (+ n 1))))]
               (numbers start-n))))

(define g (make-numbers 0))
(g) ; => 0
(g) ; => 1
(g) ; => 2
; ...


(define (make-fib)
  (generator y (local [(define (fib n m)
                         (begin
                           (y n)
                           (fib m (+ n m))))]
                 (fib 1 1))))


(define f (make-fib))
(f) ; 1
(f) ; 1
(f) ; 2
(f) ; 3
(f) ; 5
(f) ; 8
;  ......