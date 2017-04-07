(require racket/main)

(define (firstsym a-list)
  (if (empty? a-list)
      'B
      (first a-list)))

(define (hd a-list) (first a-list))

(define (tl a-list)
  (if (empty? a-list)
      '()
      (rest a-list)))

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


(define (tm-interpret Q Right)
  ;; tracking the number of operations
  (define execution-time 0)

  (define (add-time! n)
    (set! execution-time (+ execution-time n)))

  (define execution-trace '())
  
  (define (add-execution-trace! instr)
    (set! execution-trace (append execution-trace
                                  (list (cons instr Right)))))

  ;; init
  (add-time! 4)
  (define Qtail Q)
  (define Left '())
  (define Instruction '())
  (define Nextlabel #f)

  ;; Interpreter
  (define (loop)
    (add-time! 2)
    (if (empty? Qtail)
        (stop)
        (cont)))

  (define (cont)
    (add-time! 9)
    (set! Instruction (first-instruction Qtail))
    (set! Qtail (rest Qtail))
    (define Operator (hd (tl Instruction)))

    (add-execution-trace! Instruction)
    (match Operator
      ['right (do-right)]
      ['left (do-left)]
      ['write (do-write)]
      ['goto (do-goto)]
      ['if (do-if)]
      [_ (err)]))

  (define (do-right)
    (add-time! 6)
    (set! Left (cons (firstsym Right) Left))
    (set! Right (tl Right))
    (loop))

  (define (do-left)
    (add-time! 6)
    (set! Right (cons (firstsym Left) Right))
    (set! Left (tl Left))
    (loop))

  (define (do-write)
    (add-time! 8)
    (define Symbol (hd (tl (tl Instruction)))) 
    (set! Right (cons Symbol (tl Right)))
    (loop))

  (define (do-goto)
    (add-time! 7)
    (set! Nextlabel (hd (tl (tl Instruction))))
    (set! Qtail (new-tail Nextlabel Q))
    (loop))

  (define (do-if)
    (add-time! 13)
    (define Symbol (hd (tl (tl Instruction))))
    (set! Nextlabel (hd (tl (tl (tl (tl Instruction))))))
    (if (eq? Symbol (firstsym Right))
        (jump)
        (loop)))

  (define (jump)
    (add-time! 3)
    (set! Qtail (new-tail Nextlabel Q))
    (loop))

  (define (err)
    (add-time! 1)
    (printf "syntax-error: ~a~n" Instruction)
    (values Right execution-time execution-trace))

  (define (stop)
    (add-time! 1)
    (values Right execution-time execution-trace))

  (loop))

(define p0
  '((0 if 0 goto 3)
    (1 right)
    (2 goto 0)
    (3 write 1)))

(define-values (result exec-time exec-trace) (tm-interpret p0 '(1 1 0 1 0 1)))
