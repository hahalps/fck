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



#|
设计目标：
mixin接受一个class生成一个新的class,新生成的class是在原有class进行拓展
拓展的方式当原有的类所需的函数定义解析不到的时候委托到MIXIN所定义的方法中寻找
有一个问题是，寻找方向无关吗？ class -> MIXIN or MINIX -> class
这里的抉择时 MIXIN 到 class，这样做的原因在于类的层次结构， MINIX -> class -> ... -> root

(MIXIN %
   (func-name (args) body)
   ...)

expand to

fields 为空
carry new function collection
new class
|#

(defmac (MIXIN c body ...)
  #:keywords  method extends
   (CLASS extends c
          ()
          (body ...)))
          
(define Counter
 (CLASS extends Root
  ([field count 0]
   [field step  1])
  ([method dec () (! count (- (? count) (? step))) (? count)]
   [method reset () (! count 0)]
   [method step! (v) (! step v)]
   [method inc-by! (v) (→ self step! v) (→ self inc)])))

(define (m c) (MIXIN c [method test () #f]))
(define new-c (new (m Counter)))
(→ new-c test)
;; => #f

         