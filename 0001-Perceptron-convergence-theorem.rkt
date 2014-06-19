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

;; Sample from a half-moon distribution
;; generate-half-moons: real real real int
;; -> (list (list x0 y0 class0) (list x1 y1 class1) ...)
;; the half moon distribution comes from a ring whose lower
;; part has been cut off and shifted down to the right
;; points in the upper half moon belong to class A, and the
;; points in the lower half moon belong to clas B
;; x, y, and the corresponding class are saved in a list
;; the points are sampled randomly, each region has the same
;; probability to occur
(define (generate-half-moons width distance radius nof-samples)
  (for/list ([n nof-samples])
    (define d (+ radius (/ width -2.0)
                 (* width (random))))
    (if (zero? (random 2)) ;; approx. equal distributions
        (let ([angle (* pi (random))])
          (list 1.0
                (* d (cos angle))
                (* d (sin angle))
                -1))
        (let ([angle (* (- pi) (random))])
          (list 1.0
                (+ radius       (* d (cos angle)))
                (+ (- distance) (* d (sin angle)))
                1)))))

;; This linear function computes the sign of the response
;; It is used as the perceptron's activation function
(define (sign x) (if (> x 0) 1 -1))

;; Half moon parameters
(define WIDTH    4)     ;; thickness of the half moon ring
(define DISTANCE -4)     ;; y-distance between the two rings
(define RADIUS   7)     ;; ring radius
(define N        10000) ;; size of the dataset

;; Perceptron parameters
(define M      3)
(define WEIGHTS (build-list (+ M) (lambda _ (random))))
(define ETA    0.5)   ;; learning rate


(define dataset (generate-half-moons WIDTH DISTANCE RADIUS N))

;; get the desired result from the dataset
(define (get-desired example)
  (list-ref example 3))

;; get the input data from an example (line of a dataset)
(define (get-input example)
  (drop-right example 1))

;; compute the response of the perceptron
;; given its weights and input
;; the input is extended with a leading 1.0
(define (compute-response weights x)
  (sign (linear-combiner weights x)))

;; compute the error over the whole dataset given some weights
(define (compute-error weights dataset)
  (define errors 0)
  (for ([example dataset]
        [i (length dataset)])
    (define d (last example))
    (unless (= (compute-response weights (drop-right example 1))
               d)
      (set! errors (+ errors 1))))
  (/ errors 1.0 (length dataset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Training
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update-weights weights input gradient eta)
  (map
   (lambda (w x)
     (+ w (* x eta gradient)))
   weights input))

;; assume that each input has to be extended by 1.0
;; in order to account for the bias
(define (train-iterative weights eta dataset)
  (if (empty? dataset)
      weights
      (let* ([example (car dataset)]
             [x (get-input example)]
             [desired (get-desired example)]
             [response (compute-response weights x)]
             [gradient (- desired response)])
        (train-iterative
         (update-weights weights x gradient eta)
         eta (cdr dataset)))))

(set! WEIGHTS (train-iterative WEIGHTS ETA dataset))

;; compute the decision boundary line
(define (decision-boundary weights)
  (lambda (x)
    (/ (+ (car weights)
          (* (cadr weights) x))
       (- (caddr weights)))))

;; make sure the w/h plot ratio is the same
(define lim (max (+ (/ WIDTH 2.0) (* 2 RADIUS))
                 (+ DISTANCE (* 2 RADIUS))))

(define (get-clean-dataset dataset)
  (map (lambda (x)
         (drop-right (cdr x) 1))
       dataset))
       

(parameterize ([plot-width   600]
               [plot-height  600]
               [plot-x-label #f]
               [plot-y-label #f])
  ;; Color each region differently
  (define regionA (get-clean-dataset
                   (filter (lambda (x)
                             (= (get-desired x) 1)) dataset)))
  (define regionB (get-clean-dataset
                   (filter (lambda (x) (= (get-desired x) -1))
                           dataset)))
  (plot (list (points regionA #:size 0.5 #:color '(0 0 100))
              (points regionB #:size 0.5 #:color '(100 100 0))
              ;; Plot the decision boundary
              (function (decision-boundary WEIGHTS)))
        #:x-min (- lim) #:x-max lim
        #:y-min (- lim) #:y-max lim
        #:title "Dataset"))
