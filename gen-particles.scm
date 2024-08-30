(declare (uses quasi-random))

(import (chicken process-context))
(import (chicken random))
(import (chicken io))
(import (chicken format))
(import srfi-4)
(import srfi-37)
(import medea)

(define (log10 x)
  (/ (log x)
     (log 10)))

(define (exponential-10 x)
  (expt 10 x))

(define (make-interval-random-transformation lower-range upper-range exponential?)
  (let ((higher-limit (if exponential? (log10 upper-range) upper-range))
        (lower-limit (if exponential? (log10 lower-range) lower-range))) 
    (lambda (random-number)
      (let* ((delta (- higher-limit lower-limit))
            (offset lower-limit)
            (uniformly-distributed-value (+ offset 
                                            (* delta 
                                               random-number))))
        (if exponential? 
            (exponential-10 uniformly-distributed-value) 
            uniformly-distributed-value)))))

(define (make-random-list list-length)
  (lambda ()  
    (define (do-make-random-list list-length) 
      (if (<= list-length 0)
        '()
        (cons (pseudo-random-real) (do-make-random-list (- list-length 1)))))
    (do-make-random-list list-length)))

(define (make-quasirandom-list generator-type list-length)
  (let ((quasirandom-generator (new-quasi-random-number-generator generator-type list-length))) 
   (lambda ()
     (f64vector->list (generate-value! quasirandom-generator)))))

(define (generate-particle-positions generate-randomish-list 
                                     coordinate-transformers 
                                     number-particles)
  (define (generate-particle-position)
    (map (lambda (transformer coordinate) 
           (transformer coordinate)) 
         coordinate-transformers 
         (generate-randomish-list)))
  (define (do-generate-particle-positions number-particles) 
    (if (<= number-particles 0)
        '()
        (cons (generate-particle-position)
              (do-generate-particle-positions (- number-particles 1)))))
  (do-generate-particle-positions number-particles))

(define (generate-initial-particles generator-type 
                                    particle-config 
                                    number-particles)
  (let* ((generate-randomish-list 
           (if (eq? 'random generator-type) 
               (make-random-list (length particle-config))
               (make-quasirandom-list generator-type (length particle-config))))
         (variable-transformers (map cdr particle-config))
         (variable-names (map car particle-config))
         (initial-particle-positions (generate-particle-positions generate-randomish-list 
                                                                  variable-transformers 
                                                                  number-particles))) 
    (define (label-position position)
      (map (lambda (coordinate variable-name) 
             (cons (string->symbol variable-name) coordinate)) 
           position 
           variable-names))
    (list->vector 
      (map label-position 
           initial-particle-positions))))

(define (output-particles particles)
  (write-json particles))

(define (alist-lookup key alist)
  (let ((looked-up (assoc key alist))) 
   (if looked-up (cdr looked-up) '())))

(define (truthy? value)
  (if (not (or (null? value) 
               (eq? value #f))) 
      #t 
      #f))

(define (parse-config config-json)
  (let ((parsed-objects (read-json config-json))) 
   (map (lambda (variable-config) 
          (let ((upper-variable-limit (alist-lookup 'upper variable-config))
                (lower-variable-limit (alist-lookup 'lower variable-config))
                (use-decade-exponential-distribution? (truthy? (alist-lookup 'decade variable-config)))) 
            (if (null? upper-variable-limit)
                (error (sprintf "error: missing upper limit in variable configuration: ~A" variable-config)))
            (if (null? lower-variable-limit)
                (error (sprintf "error: missing lower limit in variable configuration: ~A" variable-config)))
            (cons (alist-lookup 'name variable-config) 
                (make-interval-random-transformation lower-variable-limit
                                                     upper-variable-limit
                                                     use-decade-exponential-distribution?)))) 
        (vector->list parsed-objects))))


(define (main)
  (let* ((arguments (command-line-arguments))
         (number-guesses (string->number (car arguments)))
         (generator-type (string->symbol (cadr arguments)))
         (configuration-input (read-string)))
    (output-particles (generate-initial-particles generator-type 
                                                  (parse-config configuration-input) 
                                                  number-guesses))))

(main)
