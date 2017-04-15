(define (l-eval-proc proc)
  (let loop ([bindings *procs*])
    (cond [(empty? bindings)
           (error 'l-eval-proc "procedure '~a' has no binding." proc)]
          [(eq? proc (caar bindings)) (cdar bindings)]
          [else (loop (cdr bindings))])))

(define (lookup-var var env)
  (let loop ([env env])
    (cond [(empty? env) empty]
          [(eq? (caar env) var) (cdar env)]
          [else (loop (cdr env))])))

(define (l-eval expr env)
  (cond [(list? expr)
         (match (car expr)
           ['quote (cadr expr)]
           [proc
            (apply (l-eval-proc proc)
                   (map (lambda (expr)
                          (l-eval expr env))
                          (cdr expr)))])]
        [(number? expr) expr]
        [(string? expr) expr]
        [else (lookup-var expr env)]))

(l-eval '(+ 10 20 (+ 1 2 3 a)) '((a . 2000)))
(eval-proc '= procs)
(lookup-var 'b '((a . 10) (b . 20) (c . 30) (x . 100)))
