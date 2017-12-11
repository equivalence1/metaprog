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
      [`(,h) ; (println `(current line: ,h)); (println `(current state: ,st)) (println '())
             (int-jump prog st h)]
      [`(,h . ,t) ; (println `(current line: ,h)); (println `(current state: ,st)) (println '())
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
      [`(println ,exp) ;(println (eval-exp st exp))
       st]
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
       (:= residual (list (filter (lambda (x) (not (member x names))) (car program))))
       (goto outer-while))

 (outer-while (if (empty? pending) stop init-pp))
 (init-pp (:= tmp_prog program)
          (goto init-pp-1))
 (init-pp-1 (:= tmp_prog (cdr tmp_prog))
            (if (empty? tmp_prog) error init-pp-2))
 (error (return 'WTF))
 (init-pp-2 (if (eq? (caar pending) (caar tmp_prog)) set-pp init-pp-1))
 (set-pp (:= pp (caar tmp_prog)) (:= tmp_prog '()) (goto cont))
 (cont (:= st (cadar pending))
       (:= pending (cdr pending))
       (:= marked (cons (list pp st) marked))
       (:= bb (st-lookup program pp))
       (:= code (list (list pp '- st)))
       (:= pp '())
       (goto inner-while))
   (inner-while (if (empty? bb) update-residual inner-cont))
   (inner-cont (:= command (car bb))
               (println `(Command is ,command))
               (:= bb (cdr bb))
               (goto case-expr))
   (case-expr (if (eq? ':=     (car command)) do-assign case-goto))
   (case-goto (if (eq? 'goto   (car command)) do-goto   case-if))
   (case-if   (if (eq? 'if     (car command)) do-if     case-ret))
   (case-ret  (if (eq? 'return (car command)) do-ret    case-println))
   (case-println  (if (eq? 'println (car command)) do-print error-case))
   (error-case (return `(illegal-command: ,command)))

   (do-print (goto inner-while))

   (do-assign (if (is_exp_static_by_division (cadr command) names) do-assign-static do-assign-dynamic))
   (do-assign-static (println `(assign-static ,(cadr command) from ,(st-lookup st (cadr command)) to ,(reduce (caddr command) st) ))
                     (:= st (var-set st (cadr command) (reduce (caddr command) st))) (goto inner-while))
   (do-assign-dynamic (println 'assign-dynamic) (:= code (append code (list (list ':= (cadr command) (reduce (caddr command) st))))) (goto inner-while))

   (do-goto (:= bb (st-lookup program (cadr command))) (goto inner-while))

   (do-if (if (is_exp_static_by_division (cadr command) names) do-if-static do-if-dynamic))
   (do-if-static
      (println 'do-if-static)
      (println `(reducing expression ,(cadr command) ,(reduce 'st st)))
      (:= reduced_exp (reduce (cadr command) st))
      (println `(reduced to ,reduced_exp))
      (if (equal? reduced_exp ''#t) do-if-static-1 do-if-static-2))
   (do-if-static-1 (:= bb (st-lookup program (caddr command))) (goto inner-while))
   (do-if-static-2 (:= bb (st-lookup program (cadddr command))) (goto inner-while))

   (do-if-dynamic (println 'do-if-dynamic) (if (member (list (caddr command) st) marked) do-if-dynamic-2 do-if-dynamic-1))
   (do-if-dynamic-1 (:= pending (append pending (list (list (caddr command) st)))) (goto do-if-dynamic-2))
   (do-if-dynamic-2 (if (member (list (cadddr command) st) marked) do-if-dynamic-4 do-if-dynamic-3))
   (do-if-dynamic-3 (:= pending (append pending (list (list (cadddr command) st)))) (goto do-if-dynamic-4))
   (do-if-dynamic-4 (:= code (append code (list (list 'if (reduce (cadr command) st) (list (caddr command) '- st) (list (cadddr command) '- st))))) (goto inner-while))

   (do-ret (:= code (append code (list (list 'return (reduce (cadr command) st))))) (goto inner-while))

 (update-residual (:= residual (append residual (list code))) (:= command '()) (goto outer-while))
 (stop (return residual))
))
; in if-dynamic-4 (reduce (cadr command) st) -> (cadr command)



(define mix1
'(
 (read program1 names1 values1)
 (init (:= st01 (zipwith names1 values1))
       (:= pending1 (list (list (caadr program1) st01)))
       (:= marked1 '())
       (:= residual1 (list (filter (lambda (x) (not (member x names1))) (car program1))))
       (goto outer-while))

 (outer-while (if (empty? pending1) stop init-pp))
 (init-pp (:= tmp_prog1 program1)
          (goto init-pp-1))
 (init-pp-1 (:= tmp_prog1 (cdr tmp_prog1))
            (if (empty? tmp_prog1) error init-pp-2))
 (error (return 'WTF))
 (init-pp-2 (if (eq? (caar pending1) (caar tmp_prog1)) set-pp init-pp-1))
 (set-pp (:= pp1 (caar tmp_prog1)) (:= tmp_prog1 '()) (goto cont))
 (cont (:= st1 (cadar pending1))
       (:= pending1 (cdr pending1))
       (:= marked1 (cons (list pp1 st1) marked1))
       (:= bb1 (st-lookup program1 pp1))
       (:= code1 (list (list pp1 '- st1)))
       (:= pp1 '())
       (goto inner-while))
   (inner-while (if (empty? bb1) update-residual inner-cont))
   (inner-cont (:= command1 (car bb1))
               (println `(Command is ,command1))
               (:= bb1 (cdr bb1))
               (goto case-expr))
   (case-expr (if (eq? ':=     (car command1)) do-assign case-goto))
   (case-goto (if (eq? 'goto   (car command1)) do-goto   case-if))
   (case-if   (if (eq? 'if     (car command1)) do-if     case-ret))
   (case-ret  (if (eq? 'return (car command1)) do-ret    case-println))
   (case-println  (if (eq? 'println (car command1)) do-print error-case))
   (error-case (return `(illegal-command: ,command1)))

   (do-print (goto inner-while))

   (do-assign (if (is_exp_static_by_division (cadr command1) names1) do-assign-static do-assign-dynamic))
   (do-assign-static (println `(assign-static ,(cadr command1) from ,(st-lookup st1 (cadr command1)) to ,(reduce (caddr command1) st1) ))
                     (:= st1 (var-set st1 (cadr command1) (reduce (caddr command1) st1))) (goto inner-while))
   (do-assign-dynamic (println 'assign-dynamic) (:= code1 (append code1 (list (list ':= (cadr command1) (reduce (caddr command1) st1))))) (goto inner-while))

   (do-goto (:= bb1 (st-lookup program1 (cadr command1))) (goto inner-while))

   (do-if (if (is_exp_static_by_division (cadr command1) names1) do-if-static do-if-dynamic))
   (do-if-static
      (println 'do-if-static)
      (println `(reducing expression ,(cadr command1) ,(reduce 'st1 st1)))
      (:= reduced_exp1 (reduce (cadr command1) st1))
      (println `(reduced to ,reduced_exp1))
      (if (equal? reduced_exp1 ''#t) do-if-static-1 do-if-static-2))
   (do-if-static-1 (:= bb1 (st-lookup program1 (caddr command1))) (goto inner-while))
   (do-if-static-2 (:= bb1 (st-lookup program1 (cadddr command1))) (goto inner-while))

   (do-if-dynamic (println 'do-if-dynamic) (if (member (list (caddr command1) st1) marked1) do-if-dynamic-2 do-if-dynamic-1))
   (do-if-dynamic-1 (:= pending1 (append pending1 (list (list (caddr command1) st1)))) (goto do-if-dynamic-2))
   (do-if-dynamic-2 (if (member (list (cadddr command1) st1) marked1) do-if-dynamic-4 do-if-dynamic-3))
   (do-if-dynamic-3 (:= pending1 (append pending1 (list (list (cadddr command1) st1)))) (goto do-if-dynamic-4))
   (do-if-dynamic-4 (:= code1 (append code1 (list (list 'if (reduce (cadr command1) st1) (list (caddr command1) '- st1) (list (cadddr command1) '- st1))))) (goto inner-while))

   (do-ret (:= code1 (append code1 (list (list 'return (reduce (cadr command1) st1))))) (goto inner-while))

 (update-residual (:= residual1 (append residual1 (list code1))) (:= command1 '()) (goto outer-while))
 (stop (return residual1))
))



(define mix2
'(
 (read program2 names2 values2)
 (init (:= st02 (zipwith names2 values2))
       (:= pending2 (list (list (caadr program2) st02)))
       (:= marked2 '())
       (:= residual2 (list (filter (lambda (x) (not (member x names2))) (car program2))))
       (goto outer-while))

 (outer-while (if (empty? pending2) stop init-pp))
 (init-pp (:= tmp_prog2 program2)
          (goto init-pp-1))
 (init-pp-1 (:= tmp_prog2 (cdr tmp_prog2))
            (if (empty? tmp_prog2) error init-pp-2))
 (error (return 'WTF))
 (init-pp-2 (if (eq? (caar pending2) (caar tmp_prog2)) set-pp init-pp-1))
 (set-pp (:= pp2 (caar tmp_prog2)) (:= tmp_prog2 '()) (goto cont))
 (cont (:= st2 (cadar pending2))
       (:= pending2 (cdr pending2))
       (:= marked2 (cons (list pp2 st2) marked2))
       (:= bb2 (st-lookup program2 pp2))
       (:= code2 (list (list pp2 '- st2)))
       (:= pp2 '())
       (goto inner-while))
   (inner-while (if (empty? bb2) update-residual inner-cont))
   (inner-cont (:= command2 (car bb2))
               (println `(Command is ,command2))
               (:= bb2 (cdr bb2))
               (goto case-expr))
   (case-expr (if (eq? ':=     (car command2)) do-assign case-goto))
   (case-goto (if (eq? 'goto   (car command2)) do-goto   case-if))
   (case-if   (if (eq? 'if     (car command2)) do-if     case-ret))
   (case-ret  (if (eq? 'return (car command2)) do-ret    case-println))
   (case-println  (if (eq? 'println (car command2)) do-print error-case))
   (error-case (return `(illegal-command: ,command2)))

   (do-print (goto inner-while))

   (do-assign (if (is_exp_static_by_division (cadr command2) names2) do-assign-static do-assign-dynamic))
   (do-assign-static (println `(assign-static ,(cadr command2) from ,(st-lookup st2 (cadr command2)) to ,(reduce (caddr command2) st2) ))
                     (:= st2 (var-set st2 (cadr command2) (reduce (caddr command2) st2))) (goto inner-while))
   (do-assign-dynamic (println 'assign-dynamic) (:= code2 (append code2 (list (list ':= (cadr command2) (reduce (caddr command2) st2))))) (goto inner-while))

   (do-goto (:= bb2 (st-lookup program2 (cadr command2))) (goto inner-while))

   (do-if (if (is_exp_static_by_division (cadr command2) names2) do-if-static do-if-dynamic))
   (do-if-static
      (println 'do-if-static)
      (println `(reducing expression ,(cadr command2) ,(reduce 'st2 st2)))
      (:= reduced_exp2 (reduce (cadr command2) st2))
      (println `(reduced to ,reduced_exp2))
      (if (equal? reduced_exp2 ''#t) do-if-static-1 do-if-static-2))
   (do-if-static-1 (:= bb2 (st-lookup program2 (caddr command2))) (goto inner-while))
   (do-if-static-2 (:= bb2 (st-lookup program2 (cadddr command2))) (goto inner-while))

   (do-if-dynamic (println 'do-if-dynamic) (if (member (list (caddr command2) st2) marked2) do-if-dynamic-2 do-if-dynamic-1))
   (do-if-dynamic-1 (:= pending2 (append pending2 (list (list (caddr command2) st2)))) (goto do-if-dynamic-2))
   (do-if-dynamic-2 (if (member (list (cadddr command2) st2) marked2) do-if-dynamic-4 do-if-dynamic-3))
   (do-if-dynamic-3 (:= pending2 (append pending2 (list (list (cadddr command2) st2)))) (goto do-if-dynamic-4))
   (do-if-dynamic-4 (:= code2 (append code2 (list (list 'if (reduce (cadr command2) st2) (list (caddr command2) '- st2) (list (cadddr command2) '- st2))))) (goto inner-while))

   (do-ret (:= code2 (append code2 (list (list 'return (reduce (cadr command2) st2))))) (goto inner-while))

 (update-residual (:= residual2 (append residual2 (list code2))) (:= command2 '()) (goto outer-while))
 (stop (return residual2))
))





(define change-labels
  (lambda (program)
    (let* ([add-block
            (lambda (block acc)
              (match acc
                [`(,num ,used ,blocks)
                 (if (member (car block) used)
                     `(,num ,used ,(append blocks (list (cons (index-of used (car block)) (cdr block)))))
                     `(,(+ num 1) ,(append used (list (car block))) ,(append blocks (list (cons num (cdr block))))))]))]
           [update-instr
            (lambda (instr ids)
              (match (car instr)
                ['goto (list 'goto (index-of ids (cadr instr)))]
                ['if (list 'if (cadr instr) (index-of ids (caddr instr)) (index-of ids (cadddr instr)))]
                [_ instr]))]
           [folded (foldl add-block `(0 () ()) (cdr program))])
      (cons (car program)
            (map (lambda (b) (cons (car b) (cons `(println ,(car b)) (map (lambda (i) (update-instr i (cadr folded))) (cdr b))))) (caddr folded))))))

; first projection
 ;(change-labels (int mix `(,int_machine (Q Qtail Instruction Operator Nextlabel Symbol) ('((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)) 0 0 0 0 0))))
 ;(change-labels (int mix `(,find_name (name namelist) ('z '(x y z)))))

; second projection
; (change-labels (int mix `(,mix (program names dynamic pp bb command tmp_prog) (',int_machine '(Q Qtail Instruction Operator Nextlabel Symbol) '() '() '() '() '()))))
; output on '(('((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)) '() '() '() '() '()))
; output on '((1 1 0 1 0 1))

; (change-labels (int mix `(,mix (program names dynamic pp bb command tmp_prog) (',find_name '(name namelist) '() '() '() '() '()))))
; output on '(('z '(x y z)))
; output on '((1 2 3))

; third projection
; (change-labels (int mix `(,mix1 (program1 names1 dynamic1 pp1 bb1 command1 tmp_prog1) (',mix2 '(program2 names2 dynamic2 pp2 bb2 command2 tmp_prog2) '() '() '() '() '()))))
; output on `((',int_machine '(Q Qtail Instruction Operator Nextlabel Symbol) '() '() '() '() '()))
; '(('((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)) '() '() '() '() '()))
; '((1 1 0 1 0 1))

; (change-labels (int mix `(,mix (program names dynamic pp tmp_prog) (',int_machine '(Q Qtail Instruction Operator Nextlabel Symbol) '() '() '()))))

;(define mm (change-labels
;            (int
;             mix
;             `(,mix (program names names1 dynamic pp tmp_prog bb command)
;                    (',int_machine '(Q Qtail Instruction Operator Nextlabel Symbol) '() '() '() '() '() '())))))
; (define pr (int mm `(('((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)) '() '() '() '() '()))))
;(int pr '((1 0 1 1 0 1)))