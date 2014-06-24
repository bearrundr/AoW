;; Perceptron convergence algorithm
;; assume two linearly-separable classes
;; then after training:
;; (lc w x) is > 0 for input vector x belonging to class C1
;; (lc w x) is <= 0 for input vector x belonging to class C2

;; Training algorithm:
;; if x is correctly classified, no change
;; Otherwise update the weight vector
;; w(n+1) = w(n) - Eta(n)*x(n)
;; page 91 from Haykin's "Neural netrowks and learning machines"
(require racket/base)
(require plot)
;; load because procedures use some functions from this file
(load "plotting.rkt") 
(require "distributions.rkt")

(plot-new-window? #t)

;; linear combiner
(define (linear-combiner inputs weights)
  (for/fold ([sum 0])
      ([i inputs]
       [w weights])
    (+ sum (* i w))))

;; A test
(linear-combiner '(1 2 3) '(0 0 1))
;; -> 3

;; This linear function computes the sign of the response
;; It is used as the perceptron's activation function
(define (sign x) (if (> x 0) 1 -1))

;; get the desired result from the dataset
(define (get-desired example)
  (last example))

;; get the input data from an example (line of a dataset)
(define (get-input example)
  (drop-right example 1))

(define (compute-response weights x)
  (sign (linear-combiner weights x)))

;; compute the error over the whole dataset given some weights
(define (compute-error weights dataset)
  (define errors 0)
  (for ([example dataset]
        [i (length dataset)])
    (define d (last example))
    (unless (= (compute-response weights
                                 (cons 1.0 (get-input example)))
               d)
      (set! errors (+ errors 1))))
  (/ errors 1.0 (length dataset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Training
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (train-perceptron dataset weights eta)
  (define (update-weights weights input gradient eta)
    (map
     (lambda (w x)
       (+ w (* x eta gradient)))
     weights input))
  (define (train-iterative weights eta dataset)
    (if (empty? dataset)
        weights
        (let* ([example (car dataset)]
               [x (cons 1.0 (get-input example))]
               [desired (get-desired example)]
               [response (compute-response weights x)]
               [gradient (- desired response)])
          (train-iterative
           (update-weights weights x
                           gradient eta)
           eta (cdr dataset)))))
  (train-iterative weights eta dataset))

;; Half moons distribution parameters
(define WIDTH    4)     ;; thickness of the half moon ring
(define DISTANCE 1)   ;; y-distance between the two rings
(define RADIUS   7)     ;; ring radius
(define N        10000) ;; size of the dataset
(define dataset (generate-half-moons WIDTH DISTANCE RADIUS N))

;; Perceptron parameters
(define (make-random-weights n (scale 1.0))
  (build-list (+ n) (lambda _ (* scale (- (random) 0.5)))))
(define M      3)
(define WEIGHTS (make-random-weights M))
(define ETA    0.3)   ;; learning rate

(set! WEIGHTS (train-perceptron dataset WEIGHTS ETA))
(printf "error: ~a~n" (compute-error WEIGHTS dataset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compute the decision boundary line
(define (decision-boundary weights)
  (lambda (x)
    (/ (+ (car weights)
          (* (cadr weights) x))
       (- (caddr weights)))))

;; Produce some plots
(plot-random-training WIDTH -1 RADIUS 5000 
                      train-perceptron M 0.3 1.0
                      100)

(plot-error-vs-nof-samples WIDTH -3 RADIUS N
                           train-perceptron M ETA 1.0
                           500 50)
