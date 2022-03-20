#lang plait 

(define-type Value
  (numV [n : Number]))


(define-type Exp
  (numE [n : Number])
  [plusE (l : Exp)
         [r : Exp]]
  (multE [l : Exp]
         [r : Exp])
  (subE [l : Exp]
        [r : Exp])
  (divE [l : Exp]
        [r : Exp]))


(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `(+ ANY ANY) s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `(* ANY ANY) s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `(- ANY ANY) s)
     (subE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `(/ ANY ANY) s)
     (divE (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]))


(define (interp [a : Exp]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(plusE l r) (num+ (interp l) (interp r))]
    [(multE l r) (num* (interp l) (interp r))]
    [(subE l r) (num- (interp l) (interp r))]
    [(divE l r) (num/ (interp l) (interp r))]))

(define (numG op)
  (λ(l r)
    (numV (op (numV-n l) (numV-n r)))))

(define num+ (numG +))
(define num* (numG *))
(define num- (numG -))
(define num/ (numG /))


(define (interp* e) (interp (parse e)))




