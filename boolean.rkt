#lang plait

(define-type Value
  (numV [n : Number])
  (trueV)
  (falseV)
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env]))

(define-type Exp
  (trueE)
  (falseE)
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
  (lessE (l : Exp)
         (r : Exp))
  (ifE (test : Exp)
       (thn : Exp)
       (els : Exp)))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))
(define mt-env empty)
(define extend-env cons)


(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `#t s) (trueE)]
    [(s-exp-match? `#f s) (falseE)]
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
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `(lambda (SYMBOL) ANY) s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `(if ANY ANY ANY) s)
     (let ([bs (s-exp->list s)])
       (ifE (parse (second bs))
            (parse (third bs))
            (parse (fourth bs))))]
    [(s-exp-match? `(ANY ANY) s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))


(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(trueE) (trueV)]
    [(falseE) (falseV)]
    [(numE n) (numV n)]
    [(idE s) (lookup s env)]
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(multE l r) (num* (interp l env) (interp r env))]
    [(letE n rhs body) (interp body
                               (extend-env
                                (bind n (interp rhs env))
                                env))]
    [(lamE n body) (closV n body env)]
    [(appE fun arg) (type-case Value (interp fun env)
                      [(closV n body c-env)
                       (interp body
                               (extend-env
                                (bind n
                                      (interp arg env))
                                c-env))]
                      [else (error 'interp "not a function")])]
    [(lessE l r) (let ([lv (interp l env)]
                       [rv (interp r env)])
                   (if (and (numV? lv) (numV? rv))
                       (let ([ln (numV-n lv)]
                             [rn (numV-n rv)])
                         (if (< ln rn)
                             (trueV)
                             (falseV)))
                       (error 'interp "......")))]
    [(ifE test thn els)
     (let ([rb (interp test env)])
       (type-case Value rb
         [(trueV) (interp thn env)]
         [(falseV) (interp els env)]
         [else (error 'interp "not a boolean value at if")]))]))

(define (numG op)
  (λ(l r)
    (numV (op (numV-n l) (numV-n r)))))

(define num+ (numG +))
(define num* (numG *))

(define (lookup [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
   [empty (error 'lookup "free variable")]
   [(cons b rst-env) (cond
                       [(symbol=? n (bind-name b))
                        (bind-val b)]
                       [else (lookup n rst-env)])]))

