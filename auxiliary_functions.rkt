#lang racket

(provide (all-defined-out))

; for environment
(define st-lookup dict-ref)
(define st-bound? dict-has-key?)
(define st-set
  (lambda (st x e)
    (dict-set st x (cons 'quote (list e)))))


(define st-empty  #hash())
(define initial-st
  (lambda (vars d)
    (if (equal? (length vars) (length d))
        (for/fold ([st st-empty])
                  ([i vars]
                   [j d])
          (st-set st i j))
        (error "initial-st error: program arity mismatch"))))

; for basic_blocks
(define bb-lookup dict-ref)
(define bb-set    dict-set)
(define bb-empty  #hash())
(define initial-prog
  (lambda (p)
    (for/fold ([bbs bb-empty])
              ([i (cdr p)])
      (bb-set bbs (car i) (cdr i)))))

; eval
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
(define my-eval
  (lambda (e)
    (eval e ns)))

; eval expression in current environment
(define subst
  (lambda (st e)
    (match e
      [`(,x . ,y) `(,(subst st x) . ,(subst st y))]
      [`,x (if (st-bound? st x) (st-lookup st x) x)]
      )))
(define eval-exp
  (lambda (st e)
    (let ([ee (subst st e)])
      (my-eval ee))))

; mix
(define initial-residual
  (lambda (read-data division)
    (let ([init (set-subtract read-data division)])
      (list (append '(read) init)))))

(define initial-code
  (lambda (pp vs)
    ;`((,pp . ,vs) . ())))
    `(,pp . ())))

(define extend append)
(define first_command car)
(define rest_bb cdr)

(define pending-set
  (lambda (pending pair)
    (if (equal? pair '())
        pending
        (let ([m (member pair pending)])
          (if (not m)
              (append (list pair) pending)
              pending)))))

(define listminus
  (lambda (l1 l2)
    (let ([m (member l1 l2)])
      (if (not m)
          l1
          '()
          ))))

; division
; ASSUMPTION: division contains only static variables!
(define is_var_static_by_division
  (lambda (x division)
    (dict-has-key? division x)))
;(define is_exp_static_by_division
;  (lambda (exp division)
;    (match exp
;      [`(quote ,c) (println 'quote) #t]
;      [`(list ,l ...) (println 'list) (= (length (filter (lambda (e) (not (is_exp_static_by_division e division))) l)) 0)]
;      [`(,op ,l) (println 'op-1) (is_exp_static_by_division l division)]
;      [`(,op ,e1 ,e2) (println 'op-2) (and (is_exp_static_by_division e1 division) (is_exp_static_by_division e2 division))]
;      [`,x (println 'var) (is_var_static_by_division x division)]
;      )))

(define (_zipwith acc l1 l2)
  (if (or (empty? l1) (empty? l2)) acc (_zipwith (append acc (list (list (car l1) (car l2)))) (cdr l1) (cdr l2))))
(define (zipwith l1 l2) (_zipwith '() l1 l2))

(define (unzip l) (list (map car l) (map cadr l)))

(define (var-set st x val)
;  (println `(var-set ,st ,x ,val))
  (let ([names (car (unzip st))]
        [values (cadr (unzip st))])
;    (println `(names ,names : values ,values))
    (if (member x names) (zipwith names (list-set values (index-of names x) val)) (append st (list (list x val)))))
)

(define _reduce
  (lambda (exp division)
    (match exp
      [`(quote ,c)
        ; (println `('quote ,c))
        (cons `',c #t)]

      [`(list ,l ...) ; (println `('list ,l))
        (let ([reduced_list (map (lambda (e) (_reduce e division)) l)])
;          (println `(reduced_list ,reduced_list))
          (cons (cons 'list (map car reduced_list)) (andmap cdr reduced_list))
        )]

      [`(,op ,l)
        ; (println `(op1 ,op ,l))
        (let ([red_l (_reduce l division)])
          ; (println 'back-to-op1)
          ; (println red_l)
          (let ([red_exp (car red_l)])
;            (println `(will eval (,op ,red_exp)))
            (if (cdr red_l) (cons `',(my-eval `(,op ,red_exp)) #t) (cons `(,op ,red_exp) #f)) ; TODO do I need ' in #f
          )
        )
      ]
      
      [`(,op ,e1 ,e2) ; (println `(op2 ,op ,e1 ,e2))
        (let ([red_e1 (_reduce e1 division)]
              [red_e2 (_reduce e2 division)])
;          (println red_e1)
;          (println red_e2)
          (let ([red_exp1 (car red_e1)]
                [red_exp2 (car red_e2)])
;            (println `(op2 ,op ,red_exp1 ,red_exp2))
            (if (and (cdr red_e1) (cdr red_e2)) (cons `',(my-eval `(,op ,red_exp1 ,red_exp2)) #t) (cons `(,op ,red_exp1 ,red_exp2) #f)) ; TODO do I need ' in #f
          )
        )
      ]

      [(? number? n) ;(println `('number ,n))
           (cons n #t)]

      [`,x
           ;(println x)
           ; (println `('variable ,x))
           (if (is_var_static_by_division x division) (cons (car (st-lookup division x)) #t) (cons x #f))] ; TODO do I need ' in #f
    )
  )
)

(define reduce
  (lambda (exp division)
    (let ([result (car (_reduce exp division))])
;      (println `(reduced to ,result))
      result
    )
  )
)