(import (chicken random))
(import srfi-13)


(define (choose-candidate? previous-energy 
                           candidate-energy 
                           temerature)
  (let ((acceptance-probability 
          (if (< candidate-energy previous-energy) 
              1
              (exp (- (/ (- candidate-energy previous-energy) 
                         temperature)))))) 
    (<= (pseudo-random-real) acceptance-probability)))

(define (anneal initial-arguments
                objective-function 
                get-temperature
                get-sample-neighbor 
                number-iterations)
  (define (do-anneal current-arguments 
                     current-energy 
                     iteration-count) 
    (if (>= iteration-count number-iterations)
        initial-arguments
        (let* ((candidate-arguments (apply get-sample-neigbor 
                                           current-arguments))
               (candidate-energy (apply objective-function 
                                        candidate-arguments))
               (fraction-remaining (- 1 (/ (+ iteration-count 1) 
                                           number-iterations)))
               (temperature (get-temperature fraction-completed)))
          (if (choose-candidate? 
                current-energy
                candidate-energy
                temperature)
              (do-anneal candidate-arguments 
                         candidate-energy 
                         (+ iteration-count 1))
              (do-anneal current-arguments 
                         current-energy 
                         (+ iteration-count 1))))))
  (do-anneal initial-arguments 
             (apply objective-function initial-arguments) 
             0))

(define (gaussian-get-neighbor stdev)
  (define (box-muller)
    (* (cos (* 2 pi (pseudo-random-real))) 
       (sqrt (* -2 (log (pseudo-random-real))))
       stdev))
  (lambda (#!rest args)
    (map (lambda (value) (+ value (box-muller)))
         args)))

(define (linear-get-temperature initial-temperature)
  (lambda (fraction-remaining) 
    (* fraction-remaining initial-temperature)))
