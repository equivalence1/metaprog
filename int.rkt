#lang racket

(require "auxiliary_functions.rkt")
(provide int)

(define int
  (lambda (p d)
    (let ([prog (initial-prog p)]
          [st (initial-st (cdar p) d)])
      (int-bb prog st (cdadr p)))))

(define int-bb
  (lambda (prog st bb)
    (match bb
      ['() (error "int: empty basic_block list")]
      [`(,h) ; (println `(current line: ,h)) (println `(current state: ,st)) (println '())
             (int-jump prog st h)]
      [`(,h . ,t) ; (println `(current line: ,h)) (println `(current state: ,st)) (println '())
              (int-bb prog (int-assn st h) t)]
      )))

(define int-jump
  (lambda (prog st jump)
    (match jump
      [`(goto ,l) (int-bb prog st (bb-lookup prog l))]
      [`(if ,e ,l1 ,l2) (int-bb prog st (bb-lookup prog (if (eval-exp st e) l1 l2)))]
      [`(return ,e) (eval-exp st e)]
      )))

(define int-assn
  (lambda (st assn)
    (match assn
      [`(:= ,x ,exp) ; (println `('x- ,x)) (println `('exp- ,exp))
       (let ([nv (eval-exp st exp)]) ; (println 'setting)
                (st-set st x nv))]
      [_ (error "int: assignment expected")]
      )))

(define find_name
  '((read name namelist valuelist)
   (search (if (equal? name (car namelist)) found cont))
   (cont (:= valuelist (cdr valuelist))
         (:= namelist (cdr namelist))
         (goto search))
   (found (return (car valuelist)))
   ))

(define int_machine
'(
 (read Q Right)
 (init (:= Qtail Q)
       (:= Left '())
       (goto loop))
 (loop (if (empty? Qtail) stop cont))

 (cont (:= Instruction (car Qtail)) (:= Qtail (cdr Qtail)) (:= Operator (cadr Instruction))
        (if (eq? Operator 'right) do-right cont1))
 (cont1 (if (eq? Operator 'left)  do-left  cont2))
 (cont2 (if (eq? Operator 'write) do-write cont3))
 (cont3 (if (eq? Operator 'goto)  do-goto  cont4))
 (cont4 (if (eq? Operator 'if)    do-if    error))

 (do-right (:= Left (cons (car Right) Left))         (:= Right (cdr Right))                           (goto loop))
 (do-left  (:= Left (cdr Left))                      (:= Right (cons (car Left) Right))               (goto loop))
 (do-write (:= Symbol (caddr Instruction))           (:= Right (cons Symbol (cdr Right)))             (goto loop))
 (do-goto  (:= Nextlabel (caddr Instruction))        (:= Qtail (list-tail Q Nextlabel))               (goto loop))
 (do-if    (:= Symbol (caddr Instruction))           (:= Nextlabel (caddr (cddr Instruction)))        (if (eq? Symbol (car Right)) jump loop))

 (jump     (:= Qtail (list-tail Q Nextlabel))        (goto loop))

 (error    (return `(syntax-error: ,Instruction)))

 (stop     (return Right))
))


(define mix
'(
 (read program names values)
 (init (:= st0 (zipwith names values))
       (:= pending (list (list (caadr program) st0)))
       (:= marked '())
       (:= dynamic (filter (lambda (x) (not (member x names))) (car program)))
       (:= residual (list dynamic))
       (goto outer-while))

 (outer-while (if (empty? pending) stop cont-pp-1))
 (cont-pp-1 (:= tmp_prog program) (goto cont-pp))
 (cont-pp (:= tmp_prog (cdr tmp_prog))
          (if (eq? (caar pending) (caar tmp_prog)) set-pp cont-pp))
 (set-pp (:= pp (caar tmp_prog)) (goto cont))
 (cont (:= st (cadar pending))
       (:= pending (cdr pending))
       (:= marked (cons (list pp st) marked))
       (:= bb (st-lookup program pp))
       (:= code (list (list pp '- st)))
       (goto inner-while))
   (inner-while (if (empty? bb) update-residual inner-cont))
   (inner-cont (:= command (car bb))
               (:= bb (cdr bb))
               (goto case-expr))
   (case-expr (if (eq? ':=     (car command)) do-assign case-goto))
   (case-goto (if (eq? 'goto   (car command)) do-goto   case-if))
   (case-if   (if (eq? 'if     (car command)) do-if     case-ret))
   (case-ret  (if (eq? 'return (car command)) do-ret    error-case))
   (error-case (return ('illegal-command: command)))

   (do-assign (if (member (cadr command) names) do-assign-static do-assign-dynamic))
   (do-assign-static (:= st (var-set st (cadr command) (reduce (caddr command) st))) (goto inner-while))
   (do-assign-dynamic (:= code (append code (list (list ':= (cadr command) (reduce (caddr command) st))))) (goto inner-while))

   (do-goto (:= bb (st-lookup program (cadr command))) (goto inner-while))

   (do-if (:= reduced_exp (_reduce (cadr command) st)) (goto do-if-1))
   (do-if-1 (if (cdr reduced_exp) do-if-static do-if-dynamic))
   (do-if-static (if (equal? (car reduced_exp) ''#t) do-if-static-1 do-if-static-2))
   (do-if-static-1 (:= bb (st-lookup program (caddr command))) (goto inner-while))
   (do-if-static-2 (:= bb (st-lookup program (cadddr command))) (goto inner-while))

   (do-if-dynamic (if (member (list (caddr command) st) marked) do-if-dynamic-2 do-if-dynamic-1))
   (do-if-dynamic-1 (:= pending (append pending (list (list (caddr command) st)))) (goto do-if-dynamic-2))
   (do-if-dynamic-2 (if (member (list (cadddr command) st) marked) do-if-dynamic-4 do-if-dynamic-3))
   (do-if-dynamic-3 (:= pending (append pending (list (list (cadddr command) st)))) (goto do-if-dynamic-4))
   (do-if-dynamic-4 (:= code (append code (list (list 'if (reduce (cadr command) st) (list (caddr command) '- st) (list (cadddr command) '- st))))) (goto inner-while))
   
   (do-ret (:= code (append code (list (list 'return (reduce (cadr command) st))))) (goto inner-while))

 (update-residual (:= residual (append residual (list code))) (goto outer-while))
 (stop (return residual))
))