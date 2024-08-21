(declare (unit pso))

(import srfi-1)
(import (chicken random))

(define pi (acos -1))

(define (square x) (expt x 2))

(define (sample-normal-distribution-1d mean standard-deviation)
  (+ mean
     (* (cos (* 2 pi (pseudo-random-real))) 
        (sqrt (* -2 (log (pseudo-random-real))))
        standard-deviation)))

(define-record particle-state position energy)

(define (select-optimum-energy-state state-1 state-2)
  (if (< (particle-state-energy (force state-1))
         (particle-state-energy (force state-2)))
      state-1
      state-2))

(define (vector-norm vect)
  (sqrt (fold (lambda (accumulated component)
                + accumulated (square component))
              0
              vect)))

(define (vector-add vector-1 vector-2)
  (map + vector-1 vector-2))

(define (vector-subtract vector-1 vector-2)
  (map - vector-1 vector-2))

(define (vector-scale scalar vect)
  (map (lambda (component) (* component scalar)) vect))

(define (get-best-state particle-states)
  (fold (lambda (best-state-so-far candidate-best-state)
          (select-optimum-energy-state candidate-best-state 
                                      best-state-so-far))
        (car particle-states)
        (cdr particle-states)))

(define (pool-map function lst pool-size)
  (define (do-pool-map completed running remaining)
    (if (null? running)
        completed
        (let* ((next-completed (force (car running)))
               (updated-completed (append completed 
                                         (list next-completed))))
          (if (null? remaining)
              (let ((updated-running (cdr running))) 
               (do-pool-map updated-completed 
                            updated-running
                            remaining))
              (let ((updated-running (append (cdr running) 
                                            (list (function (car remaining)))))
                    (updated-remaining (cdr remaining))) 
                (do-pool-map updated-completed 
                             updated-running
                             updated-remaining))))))
  (do-pool-map '() 
               (map function 
                    (if (> pool-size (length lst)) lst (take lst pool-size))) 
               (if (> pool-size (length lst)) '() (drop lst pool-size))))

(define (update-position particle-best-position global-best-position)
  (let ((mean 
          (vector-scale (/ 1 2) 
                        (vector-add particle-best-position global-best-position)))
        (standard-deviation 
          (vector-norm 
            (vector-subtract particle-best-position 
                             global-best-position))))
    (map (lambda (component) 
           (sample-normal-distribution-1d component standard-deviation))
         mean)))

(define (evaluate-positions particle-positions objective-function pool-size)
  (map (lambda (particle-position particle-energy)
         (make-particle-state particle-position particle-energy))
       particle-positions
       (pool-map (lambda (particle-position)
                   (apply objective-function particle-position))
                 particle-positions
                 pool-size)))

(define (update-states best-particle-states objective-function pool-size)
  (let* ((global-best-state (get-best-state best-particle-states))
         (updated-positions (map (lambda (particle-state)
                                   (update-position 
                                     (particle-state-position (force particle-state))
                                     (particle-state-position (force global-best-state))))
                                 best-particle-states)))
    (evaluate-positions updated-positions objective-function pool-size)))

(define (particle-swarm-optimize initial-positions objective-function max-iterations pool-size) 
  (define (particle-swarm-iteration best-states iteration-count)
    (if (> iteration-count max-iterations)
        (get-best-state best-states)
        (particle-swarm-iteration 
          (let ((candidate-states (update-states best-states objective-function pool-size)))
           (map (lambda (candidate-state best-state) 
                  (select-optimum-energy-state candidate-state 
                                               best-state)) 
                candidate-states 
                best-states))
          (+ iteration-count 1))))
  (particle-swarm-iteration 
    (evaluate-positions initial-positions objective-function pool-size) 
    0))
