(require racket/main)

(define (forwardMultiplyGate x y) (* x y))
(forwardMultiplyGate -2 3)

;; How to change to output of a circuit?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strategy #1: Random Local Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define x -2)  ;; some input values
(define y 3)

(define tweak-amount 0.01)
(define best-out -inf.0)

(define best-x x)
(define best-y y)
(for ([k 100])
  (define x-try (+ x (* tweak-amount (- (* (random) 2) 1))))
  (define y-try (+ y (* tweak-amount (- (* (random) 2) 1))))
  (define out (forwardMultiplyGate x-try y-try))
  (when (> out best-out)
    (set! best-out out)
    (set! best-x x-try)
    (set! best-y y-try)))

(forwardMultiplyGate best-x best-y)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strategy #2: Numerical Gradient
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define x -2)
(define y 3)
(define out (forwardMultiplyGate x y))
(define h 0.0001)

;; derivative with respect to x
(define xph (+ x h))
(define out2 (forwardMultiplyGate xph y))
(define x-derivative (/ (- out2 out) h))

;; derivative with respect to y
(define yph (+ y h))
(define out3 (forwardMultiplyGate x yph))
(define y-derivative (/ (- out3 out) h))

(define step-size 0.01)
(define out (forwardMultiplyGate x y))
(set! x (+ x (* step-size x-derivative)))
(set! y (+ y (* step-size y-derivative)))
(define out-new (forwardMultiplyGate x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strategy #3: Analytic Gradient
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define x -2)
(define y 3)
(define out (forwardMultiplyGate x y))
(define x-gradient y)
(define y-gradient x)

(define step-size 0.01)
(set! x (+ x (* step-size x-gradient)))
(set! y (+ y (* step-size y-gradient)))
(define out-new (forwardMultiplyGate x y))
