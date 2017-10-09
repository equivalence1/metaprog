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
(define is_exp_static_by_division
  (lambda (exp division)
    (match exp
      [`(quote ,c) (println 'quote) #t]
      [`(list ,l ...) (println 'list) (= (length (filter (lambda (e) (not (is_exp_static_by_division e division))) l)) 0)]
      [number (println 'number) #t]
      [string (println 'string) #t]
      [`(,op ,l) (println 'op-1) (is_exp_static_by_division l division)]
      [`(,op ,e1 ,e2) (println 'op-2) (and (is_exp_static_by_division e1 division) (is_exp_static_by_division e2 division))]
      [`,x (println 'var) (is_var_static_by_division x division)]
      )))

(define reduce
  (lambda (exp division)
    (match exp
      [`(quote ,c) (println 'quote) c]
      [`(list ,l ...) (println l) (map (lambda (e) (reduce e division)) `,l)]
      [`(,op ,l) (println 'op-1) (let ([red_l (reduce l division)])
                                   (println red_l)
                                   (match red_l
                                     [`,c (println 'my-evaling) (my-eval `(,op ',c))]
                                     ;[`,x (my-eval `(,op ,c))]
                                     [_ `(,op ,red_l)]))]
      [`(,op ,e1 ,e2) (println 'op-2) (let ([red_e1 (reduce e1 division)]
                                            [red_e2 (reduce e2 division)])
                                        (match `(,red_e1 ,red_e2)
                                          [`(,c1 ,c2) (my-eval `(,op ,c1 ,c2))]
                                          [_ `(,op ,red_e1 ,red_e2)]))]
      [`,x (println 'x) (if (is_var_static_by_division x division) (car (st-lookup division x)) x)]
    )
  )
)