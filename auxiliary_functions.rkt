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
(define-namespace-anchor abracadabra)
(define ns (namespace-anchor->namespace abracadabra))
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
  (lambda (x names)
    (if (member x names) #t #f)))
(define is_exp_static_by_division
  (lambda (exp names)
    (match exp
      [`(quote ,c) #t]
      [`(list ,l ...) (= (length (filter (lambda (e) (not (is_exp_static_by_division e names))) l)) 0)]
      [`(,op ,l) (is_exp_static_by_division l names)]
      [`(,op ,e1 ,e2) (and (is_exp_static_by_division e1 names) (is_exp_static_by_division e2 names))]
      [`,x (is_var_static_by_division x names)]
     )))

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
;       (println `('quote ,c))
        (cons `',c #t)]

      [`(list ,l ...)
       ; (println `('list ,l))
        (let ([reduced_list (map (lambda (e) (_reduce e division)) l)])
;         (println `(reduced_list ,reduced_list))
          (cons (cons 'list (map car reduced_list)) (andmap cdr reduced_list))
        )]

      [`(,op ,l)
 ;      (println `(op1 ,op ,l))
        (let ([red_l (_reduce l division)])
;          (println `(back-to-op1 ,red_l))
          (let ([red_exp (car red_l)])
 ;           (println `(so it is op1 ,op ,red_exp))
            (if (cdr red_l) (cons `',(my-eval `(,op ,red_exp)) #t) (cons `(,op ,red_exp) #f)) ; TODO do I need ' in #f
          )
        )
      ]
      
      [`(,op ,e1 ,e2)
       ; (println `(op2 ,op ,e1 ,e2))
        (let ([red_e1 (_reduce e1 division)]
              [red_e2 (_reduce e2 division)])
;          (println `(op2 red1 ,red_e1))
;          (println `(op2 red2 ,red_e2))
          (let ([red_exp1 (car red_e1)]
                [red_exp2 (car red_e2)])
;            (println `(so it is op2 ,op ,red_exp1 ,red_exp2))
            (if (and (cdr red_e1) (cdr red_e2)) (cons `',(my-eval `(,op ,red_exp1 ,red_exp2)) #t) (cons `(,op ,red_exp1 ,red_exp2) #f)) ; TODO do I need ' in #f
          )
        )
      ]

      [`(,op ,e1 ,e2 ,e3)
       ;(println `(op3 ,op ,e1 ,e2 ,e3))
        (let ([red_e1 (_reduce e1 division)]
              [red_e2 (_reduce e2 division)]
              [red_e3 (_reduce e3 division)])
;          (println `(op3 red1 ,red_e1))
;          (println `(op3 red2 ,red_e2))
;          (println `(op3 red3 ,red_e3))
          (let ([red_exp1 (car red_e1)]
                [red_exp2 (car red_e2)]
                [red_exp3 (car red_e3)])
;            (println `(so it is op3 ,op ,red_exp1 ,red_exp2 ,red_exp3))
            (if (and (cdr red_e1) (cdr red_e2)) (cons `',(my-eval `(,op ,red_exp1 ,red_exp2 ,red_exp3)) #t) (cons `(,op ,red_exp1 ,red_exp2 ,red_exp3) #f)) ; TODO do I need ' in #f
          )
        )
      ]

      [(? number? n)
           ;(println `('number ,n))
           (cons `',n #t)]

      [`,x
           ;(println x); with value ,(car (st-lookup division x))
;           (println `('variable ,x))
           (if (is_var_static_by_division x (car (unzip division))) (cons (car (st-lookup division x)) #t) (cons x #f))] ; TODO do I need ' in #f
    )
  )
)

(define reduce
  (lambda (exp division)
 ;   (println `(reducing ,exp))
    (let ([result (car (_reduce exp division))])
;      (println `(reduced to ,result))
      result
    )
  )
)