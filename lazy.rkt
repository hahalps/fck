#lang plait

(define-type Value
  (numV [n : Number])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env]))

(define-type-alias Thunk (Boxof (-> Value)))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (if0E [test : Exp]
        [thn : Exp]
        [els : Exp])
  (lessE [l : Exp]
         [r : Exp])
  (letrecE [n : Symbol]
           [rhs : Exp]
           [body : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Thunk]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

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
    [(s-exp-match? `(< ANY ANY) s)
     (lessE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `(let ([SYMBOL ANY]) ANY) s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (s-exp->symbol (first bs))
                   (parse (third (s-exp->list s))))
             (parse (second bs))))]
    [(s-exp-match? `(letrec ([SYMBOL ANY]) ANY) s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letrecE (s-exp->symbol (first bs))
                (parse (second bs))
                (parse (third (s-exp->list s)))))]
    [(s-exp-match? `(lambda (SYMBOL) ANY) s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
     [(s-exp-match? `(if0 ANY ANY ANY) s)
     (let ([bs (s-exp->list s)])
       (if0E (parse (second bs))
            (parse (third bs))
            (parse (fourth bs))))]
    [(s-exp-match? `(ANY ANY) s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))


;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(idE s) (force (lookup s env))]
    [(plusE l r) (num+ (interp l env)
                       (interp r env))]
    [(multE l r) (num* (interp l env)
                       (interp r env))]
    [(lessE l r) (let ([lv (numV-n (interp l env))]
                       [rv (numV-n (interp r env))])
                         (if (< lv rv)
                             (numV 0)
                             (numV 1)))]
    [(lamE n body) (closV n body env)]
    [(appE fun arg) (type-case Value (interp fun env)
                      [(closV n body c-env)
                             (interp body
                                     (extend-env
                                      (bind n (delay arg env))
                                      c-env))]
                      [else (error 'interp "not a function")])]
    [(if0E test thn els) (type-case Value (interp test env)
                           [(numV n) (if (= n 0)
                                         (interp thn env)
                                         (interp els env))]
                           [else (error 'interp "not a number in if position")])]
    [(letrecE s rhs body) (interp body (extend-env (bind s (delayR s rhs env))
                                                   env))]))

(define (delay a e)
  (let ([isCache (box (none))])
    (box (λ() (type-case (Optionof Value) (unbox isCache)
                [(none) (let ([v (interp a e)])
                          (begin (set-box! isCache (some v))
                                 v))]
                [(some v) v])))))

(define (delayR s rhs env)
  (let ([isCache (box (none))])
    (box (λ() (type-case (Optionof Value) (unbox isCache)
                [(none) (let ([f (box (λ() (error 'delayR "not initialize function value")))])
                          (let ([v (interp rhs (extend-env (bind s f) env))])
                            (begin
                              (set-box! isCache (some v))
                              (set-box! f (λ() v))
                              v)))]
                [(some v) v])))))

(define (force [t : Thunk]) : Value
  ((unbox t)))


;; num+ and num* ----------------------------------------
(define (numG op)
  (λ(l r)
    (numV (op (numV-n l) (numV-n r)))))

(define num+ (numG +))
(define num* (numG *))


;; lookup ----------------------------------------
(define (lookup [n : Symbol] [env : Env]) : Thunk
  (type-case (Listof Binding) env
   [empty (error 'lookup "free variable")]
   [(cons b rst-env) (cond
                       [(symbol=? n (bind-name b))
                        (bind-val b)]
                       [else (lookup n rst-env)])]))