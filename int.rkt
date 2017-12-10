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

; (change-labels (int mix `(,mix (program names dynamic pp tmp_prog) (',int_machine '(Q Qtail Instruction Operator Nextlabel Symbol) '() '() '()))))

;(define mm (change-labels
;            (int
;             mix
;             `(,mix (program names names1 dynamic pp tmp_prog bb command)
;                    (',int_machine '(Q Qtail Instruction Operator Nextlabel Symbol) '() '() '() '() '() '())))))
; (define pr (int mm `(('((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)) '() '() '() '() '()))))
;(int pr '((1 0 1 1 0 1)))