(define (plot-random-training
         ;; half moon parameters
         width distance radius n
         ;; training parameters
         training-fn weight-dimensions eta weight-scale
         ;; plot parameters
         nof-retrainings
         (figure-name "dataset")
         (file-name #f) (kind 'svg))
  (define dataset (generate-half-moons width distance radius n))
  ;; train over random weights several times
  (parameterize ([plot-width   600]
                 [plot-height  600]
                 [plot-x-label #f]
                 [plot-y-label #f])
    ;; make sure the w/h plot ratio is the same
    (define lim (max (+ (/ width 2.0) (* 2 radius))
                     (+ distance (* 2 radius))))
    ;; color each region differently
    (define regionA (map get-input
                         (filter (lambda (x)
                                   (= (get-desired x) 1))
                                 dataset)))
    (define regionB (map get-input
                         (filter (lambda (x) (= (get-desired x) -1))
                                 dataset)))
    (define plot-list
      (append
       (list (points regionA #:size 0.5 #:color
                     '(0 0 100))
             (points regionB #:size 0.5 #:color
                     '(100 100 0)))
       (for/list ([n nof-retrainings])
         (define w
           (train-perceptron
            dataset
            (make-random-weights
             weight-dimensions
             weight-scale)
            eta))
         (function (decision-boundary
                    w)
                   #:width 0.1))))
    (if file-name
        (plot-file plot-list
                   file-name kind                   
                   #:x-min (- lim) #:x-max lim
                   #:y-min (- lim) #:y-max lim
                   #:title figure-name)
        (plot plot-list
              #:x-min (- lim) #:x-max lim
              #:y-min (- lim) #:y-max lim
              #:title figure-name))))

(define (plot-error-vs-nof-samples
         ;; half moon parameters
         width distance radius n
         ;; training parameters
         training-fn weight-dimensions eta weight-scale
         ;; plot parameters
         step repetitions
         (figure-name "dataset")
         (file-name #f) (kind 'svg))
  (define dataset (generate-half-moons width distance radius n))
  (define graph
    (for/list ([n (range 0 n step)])
      (define error
        (/ 
         (for/fold ([sum 0.0])
             ([repetition (range 0 repetitions)])
           (define weights (make-random-weights
                            weight-dimensions
                            weight-scale))
           (+ sum (compute-error
                   (training-fn
                    (take dataset n)
                    weights eta)
                   dataset)))
         repetitions))
      (list n error)))
  (parameterize ([plot-width   600]
                 [plot-height  300]
                 [plot-x-label "n"]
                 [plot-y-label "error"])
    (if file-name
        (plot-file (lines graph
                          #:width 2
                          #:color (list 255 0 0))
                   #:title #f
                   file-name kind)
        (plot (lines graph
                     #:width 2
                     #:color (list 255 0 0))
              #:title #f))))
