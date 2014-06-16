;; Perceptron convergence algorithm
;; (+ (sum i (* w x)) b)

(require plot)
(plot-new-window? #t)


;; linear combiner

(define (linear-combiner inputs weights)
  (for/fold ([sum 0])
      ([i inputs]
       [w weights])
    (+ sum (* i w))))

(linear-combiner '(1 2 3) '(0 0 1))

;; assume two linearly-separable classes
;; then after training:
;; (lc w x) is > 0 for input vector x belonging to class C1
;; (lc w x) is <= 0 for input vector x belonging to class C2

;; Training algorithm:
;; if x is correctly classified, no change
;; Otherwise update the weight vector
;; w(n+1) = w(n) - Eta(n)*x(n)
;; page 91 from Haykin
(define (generate-half-moons width distance radius nof-samples)
  (for/list ([n nof-samples])
    (define d (+ radius (/ width -2.0)
                 (* width (random))))
    (if (zero? (random 2)) ;; approx. equal distributions
        (let ([angle (* pi (random))])
          (list (* d (cos angle))
                (* d (sin angle))
                -1))
        (let ([angle (* (- pi) (random))])
          (list (+ radius       (* d (cos angle)))
                (+ (- distance) (* d (sin angle)))
                1)))))



(define width 1)
(define distance 1)
(define radius 2)
(define training-N 10000)
(define test-N 10000)

(define training-set
  (generate-half-moons width distance radius training-N))
(define test-set
  (generate-half-moons width distance radius test-N))

(define lim (max (+ (/ width 2.0) (* 2 radius))
                 (+ distance (* 2 radius))))

(define (sign x)
  (if (> x 0) 1 -1))

;; Perceptron training
(define *m* 2)
(define *weights* (build-list (+ *m* 1) (lambda _ (random))))
(define *eta* 0.5)

(define (compute-response weights dataset)
  (map (lambda (x)
         (sign (linear-combiner (cons 1 (take x 2))
                                weights)))
       dataset))

(define (compute-response weights x)
  (sign (linear-combiner weights (cons 1 x))))

(define (decision-boundary weights)
  (lambda (x)
    (/ (+ (car weights)
          (* (cadr weights) x))
       (- (caddr weights)))))

(define fn (decision-boundary *weights*))

(plot (line (decision-boundary *weights*))
      #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1)

(define (compute-error weights dataset)
  (define errors 0)
  (for ([example dataset]
        [i (length dataset)])
    (define d (last example))
    (unless (= (compute-response weights (drop-right example 1))
               d)
      (set! errors (+ errors 1))))
  (/ errors 1.0 (length dataset)))


(for ([example training-set])
  (define x (cons 1 (take example 2)))
  (define desired (caddr example))
  (define diff (- desired (sign (linear-combiner x *weights*))))
  (set! *weights*
        (map (lambda (w xx)
               (+ w (* xx *eta* diff)))
             *weights*
             x)))


(define (mat+ x y)
  (map (lambda (x1 y1)
         (map + x1 y1))
       x y))

(plot (list (points training-set
              #:size 0.5
              #:x-min (- lim) #:x-max lim
              #:y-min (- lim) #:y-max lim)
            (line (decision-boundary *weights*)))
      #:width 600 #:height 600
      #:title "Training set")

(plot (list (points test-set
              #:size 0.5
              #:x-min (- lim) #:x-max lim
              #:y-min (- lim) #:y-max lim)
            (line (decision-boundary *weights*)))
      #:width 600 #:height 600
      #:title "Test set")
