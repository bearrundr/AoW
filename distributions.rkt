(module distributions racket
  (provide (all-defined-out))
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
            (list (* d (cos angle))
                  (* d (sin angle))
                  -1))
          (let ([angle (* (- pi) (random))])
            (list (+ radius       (* d (cos angle)))
                  (+ (- distance) (* d (sin angle)))
                  1)))))
  )
