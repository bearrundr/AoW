(require racket/main)
;; Turing machine interpreter written in L
(define tm-interpreter
 (list->vector
  '((read Q Right)
    (init (set! Qtail Q)
           (set! Left (quote ()))
           (goto loop))
    (loop (if (= Qtail (quote ()))
               (goto stop)
               (goto cont)))
    (cont (set! Instruction (first-instruction Qtail))
           (set! Qtail (rest Qtail))
           (set! Operator (hd (tl Instruction)))
           
           (if (= Operator 'right) (goto do-right) (goto cont1)))
    (cont1 (if (= Operator 'left) (goto do-left) (goto cont2)))
    (cont2 (if (= Operator 'write) (goto do-write) (goto cont3)))
    (cont3 (if (= Operator 'goto) (goto do-goto) (goto cont4)))
    (cont4 (if (= Operator 'if) (goto do-if) (goto error)))

    (do-right (set! Left (cons (firstsym Right) Left))
               (set! Right (tl Right))
               (goto loop))

    (do-left (set! Right (cons (firstsym Left) Right))
              (set! Left (tl Left))
              (goto loop))

    (do-write (set! Symbol (hd (tl (tl Instruction))))
               (set! Right (cons Symbol (tl Right)))
               (goto loop))

    (do-goto (set! Nextlabel (hd (tl (tl Instruction))))
              (set! Qtail (new-tail Nextlabel Q))
              (goto loop))

    (do-if (set! Symbol (hd (tl (tl Instruction))))
            (set! Nextlabel (hd (tl (tl (tl (tl Instruction))))))
            (if (= Symbol (firstsym Right)) (goto jump) (goto loop)))

    (jump (set! Qtail (new-tail Nextlabel Q))
           (goto loop))

    (error (return (cons 'syntax-error: Instruction)))

    (stop (return Right)))))

(define (firstsym a-list)
  (if (empty? a-list)
      'B
      (car a-list)))

(define (hd a-list) (car a-list))

(define (tl a-list)
  (if (empty? a-list)
      '()
      (cdr a-list)))

(define (label instruction)
  (when (not (empty? instruction))
    (car instruction)))

(define (new-tail nextlabel Q)
  (let loop ([Q Q])
    (cond [(empty? Q) (error 'new-tail "Could not find label ~a~n" nextlabel)]
          [else
           (define instruction (car Q))
           (if (eq? nextlabel (label instruction))
               Q
               (loop (cdr Q)))])))

(define (first-instruction Q) (car Q))

(define (l-interpreter l-program vals proc-bindings)
  (define N-steps 10000)
  (define current-step 0)
  (define (steps-reached?)
    (>= current-step N-steps))
  (define (incr-steps!) (set! current-step (+ current-step 1)))
  ;; give labels fast access
  (define labels
    (for/hash ([block l-program]
               [i (vector-length l-program)]
               #:when (> i 0))
      (define label (car block))
      (values label i)))
  (define (lookup-label label)
    (hash-ref labels label))

  ;; make a new environment
  (define env '())
  (define (add-var! var val)
    (set! env (cons (cons var val) env)))
  (define (set-var! var val)
    (unless (has-binding? var)
      (add-var! var val)))
  (define (lookup-var var)
    (let loop ([env env])
      (cond [(empty? env) empty]
            [(eq? (caar env) var)
             (cdar env)]
            [else (loop (cdr env))])))
  (define (has-binding? var)
    (let loop ([env env])
      (cond [(empty? env) #f]
            [(eq? (caar env) var) #t]
            [else (loop (cdr env))])))

  (define (eval-proc proc)
    (let loop ([bindings proc-bindings])
      (cond [(empty? bindings)
             (error 'eval-proc "procedure '~a' has no binding." proc)]
            [(eq? proc (caar bindings))
             (cdar bindings)]
            [else (loop (cdr bindings))])))

  (define (eval expr)
    (cond [(list? expr)
           (match (car expr)
             ['set!
              (define var (cadr expr))
              (define val (eval (caddr expr)))
              (set-var! var val)]
             ['quote (cadr expr)]
             ['if
              (if (eval (cadr expr))
                  (eval (caddr expr))
                  (eval (cadddr expr)))]
             ['goto
              (define label (cadr expr))
              (define next-block (lookup-label label))
              (eval-basic-block (vector-ref l-program next-block))]
             ['return
              (cdr expr)]
             [proc ;; procedure
              (apply (eval-proc proc) (map eval (cdr expr)))])]
          [(number? expr) expr]
          [(string? expr) expr]
          [else (lookup-var expr)]))

  (define (eval-basic-block basic-block)
    (incr-steps!)
    (printf "~a~n" (car basic-block))
    (if (steps-reached?)
        (printf "DONE~n")
        (for [(expr (cdr basic-block))]
          (eval expr))))
  
    ;; start evaluation
  (define first-expression (vector-ref l-program 0))
  (printf "~a~n" first-expression)
  (cond [(eq? (car first-expression) 'read)
         (for ([var (cdr first-expression)]
               [val vals])
           (add-var! var val))]
        [else
         (error "Syntax error: expected a 'read' expression, but got: ~a~n"
                first-expression)])

  (eval-basic-block (vector-ref l-program 1)))

(define procs
  (list
   (cons 'firstsym firstsym)
   (cons 'hd hd)
   (cons 'tl tl)
   (cons 'new-tail new-tail)
   (cons 'cons cons)
   (cons 'first-instruction first-instruction)
   (cons '= eq?)
   (cons 'rest rest)
   (cons 'car car)
   (cons 'first first)))

(define p0
  '((0 if 0 goto 3)
    (1 right)
    (2 goto 0)
    (3 write 1)))

(l-interpreter tm-interpreter (list p0 (list 1 1 0 1 0 1)) procs)
