(require racket/main)

(define (lookup pp program)
  (let loop ([program program])
    (cond [(empty? program)
           (error 'lookup "Reached the end of the program, but label ~a not found." pp)]
          [(eq? pp (caar program))
           (define result (cdar program))
           (printf "~a~n" result)
           (cdar program)]
          [else (loop (cdr program))])))

;; empty basic block with label pp-vs:
(define (initial-code pp vs)
  (list 
   (string->symbol (~a pp vs ":"))))

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

(define (first-command bb) (car bb))

;; split: list of static vars
(define (var-static? var split)
  (let loop ([split split])
    (cond [(empty? split) #f]
          [(eq? var (car split)) #t]
          [else (loop (cdr split))])))

(define (exp-static? exp split)
  (cond [(empty? exp) #t]
        [(number? exp) #t]
        [(string? exp) #t]
        [(symbol? exp) (var-static? exp split)]
        [else
         (define fn (car exp))
         (cond [(eq? fn 'if)
                (and (exp-static? (cadr exp) split)
                     (exp-static? (caddr exp) split)
                     (exp-static? (cadddr exp) split))]
               [(eq? fn 'set!)
                (and (var-static? (cadr exp) split)
                     (exp-static? (caddr exp) split))]
               [(eq? fn 'goto) #t]
               [(eq? fn 'quote) #t]
               [(eq? fn 'return)
                (exp-static? (cadr exp) split)]
               ;; function application
               [else
                (for/and ([e (cdr exp)])
                  (exp-static? e split))])]))

(exp-static? '(if (= b 20) a c) '(b a)) ;; #f
(exp-static? '(if (= b 20) a c) '(b a c)) ;; #t

(exp-static? '(set! a (+ a b c d (+ (+ (+ 10 e))))) '(b a c)) ;; #f
(exp-static? '(set! a (+ a b c d (+ (+ (+ 10 e))))) '(b a c d e)) ;t

(exp-static? '(set! a '(c d e)) '(c d e)) ;; #f
(exp-static? '(set! a '(c d e)) '(a)) ;; #t

(exp-static? '(return (cons a b)) '(a)) ;; #f
(exp-static? '(return (cons a b)) '(b a)) ;; #t

(exp-static? '(+ a b c (test d)) '(b a)) ;; #f
(exp-static? '(+ a b c (test d)) '(b a c)) ;; #f
(exp-static? '(+ a b c (test d)) '(d b a c)) ;; #t


(define (update-environment env var val)
  (let loop ([done '()]
             [todo env])
    (if (eq? (caar todo) var)
        (set! env
              (append done
                      (cons (cons var val)
                            (cdr todo))))
        (loop (append done (list (car todo)))
              (cdr todo))))
  env)

(define (extend-code code exp)
  (cons code (list exp)))

(define (reduce exp env split)
  (cond [(exp-static? exp split)
         (printf "static: ~a~n" exp)
         (l-eval exp env)]
        [(list? exp)
         (cons (car exp)
               (map (lambda (e)
                      (reduce e env split))
                    (cdr exp)))]
        [else exp]))

(reduce '(+ 1 (+ 2 (+ a 10 (+ 1 2)))) '((a . 20)) '(a b c))
(exp-static? '(+ 1 (+ 2 (+ a 10 (+ 1 2)))) '())
(l-eval '(+ 1 (+ 2 (+ a 10 (+ 1 2)))) '((a . 10)))

(define *procs*
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
   (cons 'cadr cadr)
   (cons 'caddr caddr)
   (cons 'cadddr cadddr)
   (cons 'first first)
   (cons 'not not)
   (cons '+ +)
   (cons 'list list)
   (cons 'empty? empty?)
   (cons 'lookup lookup)
   (cons 'initial-code initial-code)
   (cons 'extend-code extend-code)
   (cons 'update-environment update-environment)
   (cons 'exp-static? exp-static?)
   (cons 'var-static? var-static?)
   (cons 'reduce reduce)
   (cons 'first-command first-command)
   (cons 'printf printf)
   ))

(define L-mix
  '((read program split pp0 vs0)
    (1: (set! pending (list (list pp0 vs0))))
    (2: (set! marked (quote ())))
    (3: (while (not (empty? pending))
          (set! element (car pending))
          (set! pp (car element))
          (set! vs (cadr element))
          (set! pending (cdr pending))
          (set! marked (cons element marked))
          (set! basic-block (lookup pp program))
          (set! code (initial-code pp vs))
          (while (not (empty? basic-block))
            (set! command (first-command basic-block))
            (set! basic-block (rest basic-block))
            (printf "CAR COMMAND: ~a~n" (car command))
            (case (car command)
              [set!
               (printf "SET! ~a~n" (cadr command))
               (set! X (cadr command))
               (set! exp (caddr command))
               (if (var-static? X split)
                   (set! vs
                         (update-environment vs X
                                             (l-eval exp vs)))
                   (set! code (extend-code
                               code (list 'set! X
                                          (reduce exp vs split)))))]
              [goto ;; compress the transition
               (printf "GOTO ~a~n" (cdr command))
               (set! pp* (cadr command))
               (set! basic-block (lookup pp* program))]
              [if
               (printf "IF ~a~n" (cdr command))
               (set! exp (cadr program))
               (set! pp* (caddr program))
               (set! pp** (cadddr program))
               ;;(if (exp-static? exp)
               (goto return)
               ]
              [_
               (printf "_ ~a~n" (cdr command))
               (goto return)
               ]))))
    (return: (return code))))

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

(L-interpreter L-mix (list p1 '() 'lab0: (list 1 1 1 0 1 0)) *procs*)
