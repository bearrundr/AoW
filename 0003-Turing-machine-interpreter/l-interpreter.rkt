(require racket/main)
;; Turing machine interpreter written in L
(define Turing-machine-interpreter
  '((read Q Right)
    (init: (set! Qtail Q)
          (set! Left (quote ()))
          (goto loop))
    (loop: (if (= Qtail (quote ()))
              (goto stop)
              (goto cont)))
    (cont: (set! Instruction (first-instruction Qtail))
          (set! Qtail (rest Qtail))
          (set! Operator (hd (tl Instruction)))
          
          (if (= Operator 'right) (goto do-right) (goto cont1)))
    (cont1: (if (= Operator 'left) (goto do-left) (goto cont2)))
    (cont2: (if (= Operator 'write) (goto do-write) (goto cont3)))
    (cont3: (if (= Operator 'goto) (goto do-goto) (goto cont4)))
    (cont4: (if (= Operator 'if) (goto do-if) (goto error)))

    (do-right: (set! Left (cons (firstsym Right) Left))
              (set! Right (tl Right))
              (goto loop))

    (do-left: (set! Right (cons (firstsym Left) Right))
             (set! Left (tl Left))
             (goto loop))

    (do-write: (set! Symbol (hd (tl (tl Instruction))))
              (set! Right (cons Symbol (tl Right)))
              (goto loop))

    (do-goto: (set! Nextlabel (hd (tl (tl Instruction))))
             (set! Qtail (new-tail Nextlabel Q))
             (goto loop))

    (do-if: (set! Symbol (hd (tl (tl Instruction))))
           (set! Nextlabel (hd (tl (tl (tl (tl Instruction))))))
           (if (= Symbol (firstsym Right)) (goto jump) (goto loop)))

    (jump: (set! Qtail (new-tail Nextlabel Q))
          (goto loop))

    (error: (return (cons 'syntax-error: Instruction)))

    (stop: (return Right))))

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

(define (L-interpreter l-program vals proc-bindings)
  ;; count number of executed statements
  (define exec-cost 0)
  (define (inc-cost! n)
    (set! exec-cost (+ exec-cost n)))
  
  (set! l-program (list->vector l-program))
  (inc-cost! (vector-length l-program))

  ;; program counter needed for Pascal-style programming
  (define PC 0)
  (define (incr-pc!) (set! PC (+ PC 1)))
  (define (current-expression)
    (if (>= PC (vector-length l-program))
        #f
        (vector-ref l-program PC)))
  (define (current-expression!)
    (if (>= PC (vector-length l-program))
        #f
        (let ([expr (vector-ref l-program PC)])
          (incr-pc!)
          expr)))
  (define finished? #f)
  (define returned? #f)
  (define goto? #f)
  (define return-value 0)
  
  ;; give labels fast access
  (define (label? label)
    (cond [(symbol? label)
           (define label-str (symbol->string label))
           (define last-char
             (string-ref label-str
                         (- (string-length label-str) 1)))
           (char=? last-char #\:)]
          [else #f]))
  (define labels
    (for/hash ([block l-program]
               [i (vector-length l-program)]
               #:when (label? (car block)))
      (inc-cost! 7)
      (define label (car block))
      ;; string the trailing ":" character of each label
      (define label-str (symbol->string label))
      (define stripped-label
        (substring label-str 0 (- (string-length label-str) 1)))
      (set! label (string->symbol stripped-label))
      (values label i)))
  (define (lookup-label label)
    (hash-ref labels label))

  ;; make a new environment
  (define env '())
  
  (define (add-var! var val)
    (inc-cost! 3)
    (set! env (cons (cons var val) env)))
  
  (define (set-var! var val)
    (cond [(has-binding? var) ;; replace binding
           (let loop ([done '()]
                      [todo env])
             (inc-cost! 4)
             (if (eq? (caar todo) var)
                 (begin
                   (inc-cost! 10)
                   (set! env
                         (append done
                                 (cons (cons var val)
                                       (cdr todo)))))
                 (begin
                   (inc-cost! 5)
                   (loop (append done (list (car todo)))
                         (cdr todo)))))]
          [else
           (inc-cost! 1)
           (add-var! var val)]))
  
  (define (lookup-var var)
    (let loop ([env env])
      (inc-cost! 2)
      (cond [(empty? env)
             (inc-cost! 1)
             empty]
            [(eq? (caar env) var)
             (inc-cost! 4)
             (cdar env)]
            [else
             (inc-cost! 2)
             (loop (cdr env))])))
  
  (define (has-binding? var)
    (let loop ([env env])
      (inc-cost! 2)
      (cond [(empty? env)
             (inc-cost! 1)
             #f]
            [(eq? (caar env) var)
             (inc-cost! 3)
             #t]
            [else
             (inc-cost! 2)
             (loop (cdr env))])))

  (define (eval-proc proc)
    (let loop ([bindings proc-bindings])
      (inc-cost! 2)
      (cond [(empty? bindings)
             (inc-cost! 2)
             (error 'eval-proc "procedure '~a' has no binding."
                    proc)]
            [(eq? proc (caar bindings))
             (inc-cost! 5)
             (cdar bindings)]
            [else
             (inc-cost! 2)
             (loop (cdr bindings))])))

  (define (eval expr)
    (inc-cost! 1)
    (set! goto? #f)
    (cond [(list? expr)
           (inc-cost! 2)
           (match (car expr)
             ['set!
              (inc-cost! 9)
              (define var (cadr expr))
              (define val (eval (caddr expr)))
              (set-var! var val)]
             ['quote
              (inc-cost! 2)
              (cadr expr)]
             ['while
              (let loop ()
                (inc-cost! 5)
                (when (eval (cadr expr))
                  (inc-cost! 6)
                  (let inner-loop ([body (cddr expr)])
                    (unless (empty? body)
                      (inc-cost! 4)
                      (eval (car body))
                      (inner-loop (cdr body))))
                  (loop)))]
             ['case
               (inc-cost! 8)
               (define test (eval (cadr expr)))
               (let loop ([cases (cddr expr)])
                 (inc-cost! 1)
                 (cond [(empty? cases)
                        (inc-cost! 1)
                        (error 'case "no default statement.")]
                       [(equal? test (caar cases))
                        (inc-cost! 7)
                        (let inner-loop ([body (cdar cases)])
                          (inc-cost! 1)
                          (cond [(empty? body)
                                 (inc-cost! 1)
                                 (error 'case
                                        "no body in the case: ~a~n"
                                        test)]
                                [(empty? (cdr body))
                                 (inc-cost! 5)
                                 (eval (car body))]
                                [else
                                 (inc-cost! 5)
                                 (eval (car body))
                                 (inner-loop (cdr body))]))]
                       [else
                        (inc-cost! 2)
                        (loop (cdr cases))]))]
             ['if
              (inc-cost! 4)
              (if (eval (cadr expr))
                  (begin
                    (inc-cost! 4)
                    (eval (caddr expr)))
                  (begin
                    (inc-cost! 5)
                    (eval (cadddr expr))))]
             ['goto
              (set! goto? #t)
              (inc-cost! 7)
              (define label (cadr expr))
              (define next-block (lookup-label label))
              (eval-basic-block (vector-ref l-program next-block))]
             ['return
              (set! returned? #t)
              (inc-cost! 3)
              (define result (eval (cadr expr)))
              (printf "return: ~a~n" result)
              (set! return-value result)
              result]
             [proc ;; procedure
              (inc-cost! (+ 3 -1 (length expr) ))
              (apply (eval-proc proc) (map eval (cdr expr)))])]
          [(number? expr)
           (inc-cost! 1)
           expr]
          [(string? expr)
           (inc-cost! 1)
           expr]
          [else
           (inc-cost! 1)
           (lookup-var expr)]))

  (define (eval-basic-block basic-block)
    (define (loop exprs)
      (inc-cost! 3)
      (cond [(empty? exprs)
             (inc-cost! 1)
             (printf "execution cost: ~a~n" exec-cost)
             (error 'eval-basic-block
                    "syntax error: empty basic block.")]
            [(empty? (cdr exprs))
             (inc-cost! 5)
             (define result (eval (car exprs)))
             (cond [finished?
                    (inc-cost! 1)
                    return-value]
                   [returned?
                    (inc-cost! 1)
                    (printf "execution cost: ~a~n" exec-cost)
                    (set! finished? #t)
                    result]
                   [else
                    (inc-cost! 4)
                    (define next-expression (current-expression!))
                    (if next-expression
                        (eval-basic-block next-expression)
                        (begin
                          (printf "execution cost: ~a~n" exec-cost)
                          result))])]
            [else
             (inc-cost! 4)
             (eval (car exprs))
             (loop (cdr exprs))]))
    (inc-cost! 1)
    (cond [(label? (car basic-block)) ;; skip label
           (inc-cost! 4)
           (loop (cdr basic-block))]
          [else
           (inc-cost! 3)
           (loop (list basic-block))]))

  ;; start evaluation
  (inc-cost! 2)
  (define first-expression (current-expression!))
  (cond [(eq? (car first-expression) 'read)
         (inc-cost! 2)
         (for ([var (cdr first-expression)]
               [val vals])
           (inc-cost! 5)
           (add-var! var val))]
        [else
         (inc-cost! 1)
         (error
          "Syntax error: expected a 'read' expression, but got: ~a~n"
          first-expression)])

  (inc-cost! 2)
  (eval-basic-block (current-expression!)))

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
   (cons 'cdr cdr)
   (cons 'first first)
   (cons 'not not)
   (cons '+ +)))

(define p0
  '((0 if 0 goto 3)
    (1 right)
    (2 goto 0)
    (3 write 1)))

(define p1
  '((read Right)
    (lab0: (set! Left (quote ()))
           (if (= 0 (firstsym Right))
               (goto lab2)
               (goto lab1)))
    (lab1: (set! Left (cons (firstsym Right) Left))
           (set! Right (tl Right))
           (if (= 0 (firstsym Right))
               (goto lab2)
               (goto lab1)))
    (lab2: (set! Right (cons 1 (tl Right)))
           (return Right))))

(define p2
  '((read search L)
    (while (not (= search (car L)))
      (set! L (cdr L)))
    (return L)))

(define p3
  '((read c b a)
    (case c
      [a (set! a (+ a 10))
          (return a)]
      [b (set! b (+ b 20))
         (return b)]
      [c (set! c 0)
         (return c)])))

    

(L-interpreter Turing-machine-interpreter
               (list p0 (list 1 1 1 0 1 0)) procs)
(L-interpreter p1 (list (list 1 1 1 0 1 0)) procs)
(L-interpreter p2 (list 'z '(a b c z d)) procs)
(L-interpreter p3 (list 'b 0 0) procs)

