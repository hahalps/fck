#lang plait

(define-type-alias Location Number)

(define-type Value
  (numV [n : Number])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env])
  (boxV [l : Location]))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (letE [n : Symbol] 
        [rhs : Exp]
        [body : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (boxE [arg : Exp])
  (unboxE [arg : Exp])
  (setboxE [bx : Exp]
           [val : Exp])
  (beginE [l : Exp]
          [r : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(define-type Storage
  (cell [location : Location] 
        [val : Value]))

(define-type-alias Store (Listof Storage))
(define mt-store empty)
(define override-store cons)

(define-type Result
  (v*s [v : Value] [s : Store]))

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `(+ ANY ANY) s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `(* ANY ANY) s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `(let ([SYMBOL ANY]) ANY) s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `(lambda (SYMBOL) ANY) s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `(box ANY) s)
     (boxE (parse (second (s-exp->list s))))]
    [(s-exp-match? `(unbox ANY) s)
     (unboxE (parse (second (s-exp->list s))))]
    [(s-exp-match? `(set-box! ANY ANY) s)
     (setboxE (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? `(begin ANY ANY) s)
     (beginE (parse (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
    [(s-exp-match? `(ANY ANY) s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))


;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env] [sto : Store]) : Result
  (type-case Exp a
    [(numE n) (v*s (numV n) sto)]
    [(idE s) (v*s (lookup s env) sto)]
    [(plusE l r)
     (type-case Result (interp l env sto)
       [(v*s v-l sto-l)
        (type-case Result (interp r env sto-l)
          [(v*s v-r sto-r)
           (v*s (num+ v-l v-r) sto-r)])])]
    [(multE l r)
     (type-case Result (interp l env sto)
       [(v*s v-l sto-l)
        (type-case Result (interp r env sto-l)
          [(v*s v-r sto-r)
           (v*s (num* v-l v-r) sto-r)])])]
    [(letE n rhs body)
     (type-case Result (interp rhs env sto)
       [(v*s v-rhs sto-rhs)
        (interp body
                (extend-env
                 (bind n v-rhs)
                 env)
                sto-rhs)])]
    [(lamE n body)
     (v*s (closV n body env) sto)]
    [(appE fun arg)
     (type-case Result (interp fun env sto)
       [(v*s v-f sto-f)
        (type-case Result (interp arg env sto-f)
          [(v*s v-a sto-a)
           (type-case Value v-f
             [(closV n body c-env)
              (interp body
                      (extend-env
                       (bind n v-a)
                       c-env)
                      sto-a)]
             [else (error 'interp 
                          "not a function")])])])]
    [(boxE a)
     (type-case Result (interp a env sto)
       [(v*s v sto-v)
        (let ([l (new-loc sto-v)])
          (v*s (boxV l) 
               (override-store (cell l v) 
                               sto-v)))])]
    [(unboxE a)
     (type-case Result (interp a env sto)
       [(v*s v sto-v)
        (type-case Value v
          [(boxV l) (v*s (fetch l sto-v) 
                         sto-v)]
          [else (error 'interp
                       "not a box")])])]
    [(setboxE bx val)
     (type-case Result (interp bx env sto)
       [(v*s v-b sto-b)
        (type-case Result (interp val env sto-b)
          [(v*s v-v sto-v)
           (type-case Value v-b
             [(boxV l)
              (v*s v-v
                   (override-store
                    (cell l v-v)
                    sto-v))]
             [else (error 'interp
                          "not a box")])])])]
    [(beginE l r)
     (type-case Result (interp l env sto)
       [(v*s v-l sto-l)
        (interp r env sto-l)])]))


;; num+ and num* ----------------------------------------
(define (numG op)
  (λ(l r)
    (numV (op (numV-n l) (numV-n r)))))

(define num+ (numG +))
(define num* (numG *))



;; lookup ----------------------------------------
(define (lookup [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
   [empty (error 'lookup "free variable")]
   [(cons b rst-env) (cond
                       [(symbol=? n (bind-name b))
                        (bind-val b)]
                       [else (lookup n rst-env)])]))

;; store operations ----------------------------------------

(define (new-loc [sto : Store]) : Location
  (+ 1 (max-address sto)))

(define (max-address [sto : Store]) : Location
  (type-case (Listof Storage) sto
   [empty 0]
   [(cons c rst-sto) (max (cell-location c)
                          (max-address rst-sto))]))

(define (fetch [l : Location] [sto : Store]) : Value
  (type-case (Listof Storage) sto
   [empty (error 'interp "unallocated location")]
   [(cons c rst-sto) (if (equal? l (cell-location c))
                         (cell-val c)
                         (fetch l rst-sto))]))

