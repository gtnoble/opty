(declare (uses pso))

(import (chicken process))
(import (chicken io))
(import (chicken process-context))
(import (chicken format))
(import srfi-13)
(import srfi-37)
(import medea)

(define (get-particles-header particles-config)
  (car particles-config))

(define (get-particle-positions particles-config)
  (cadr particles-config))

(define (make-particles-config header particle-positions)
  (list header particle-positions))

(define (read-particles-file filename)
  (let* ((input-file (open-input-file filename))
         (particles-config (vector->list (read-json input-file))))
    (close-input-port input-file)
    (let ((global-header '())) 
     (let ((particle-positions  
             (map (lambda (particle-config)
                    (assert (list? particle-config))
                    (let* ((particle-header (map car particle-config))
                           (particle-position (map cdr particle-config)))
                      (cond ((null? global-header) (set! global-header particle-header))
                            ((equal? particle-header global-header) #t)
                            (#t (error (sprintf 
                                         "error: particle header ~A is not the same as the global header ~A" 
                                         particle-headers 
                                         global-header))))
                      particle-position))
                  particles-config)))
       (assert (not (null? global-header)))
       (make-particles-config global-header particle-positions)))))

(define (write-particle position particles-header . port)
  (define (label-position position)
    (string-join 
      (map (lambda (coordinate variable-name) 
             (string-join (list (symbol->string variable-name) 
                                (number->string coordinate))
                          "=")) 
           position 
           particles-header)
      " "))
  (apply display (label-position position) port)
  (apply newline port))


(define (run-child command command-arguments particles-header 
                   #!optional (negate? #f))
  (assert (string? command))
  (assert (list? command-arguments))
  (assert (list? particles-header))
  (define (read-function-value port)
    (car (read-list port 
                    read-line 
                    1)))
  (define (function-call-child #!rest particle-position) 
    (assert (not (null? particle-position)))
    (let-values (((process-output-port process-input-port pid) 
                  (process command 
                           command-arguments)))

                (write-particle particle-position particles-header process-input-port)
                (close-output-port process-input-port)
                ;; Delay reading objective command output so we can run multiple objective commands in parallel
                (delay
                  (let ((result (string->number (read-function-value process-output-port))))
                   (close-input-port process-output-port)
                   ;; Negate the objective if you want to maximize rather than minimize
                   ((if negate? - +) result)))))
   function-call-child)

(define (display-error message)
  (let ((stderr (current-error-port)))
   (display message stderr)
   (newline stderr)))

(define (print-help #!rest args)
  (display-error "Optimizes an objective command using particle swarm optimization (PSO)")
  (display-error "")
  (display-error "Usage:")
  (display-error "  opty [-M | --maximize] [-n | --num-iterations] (-g <filename> | --initial-guesses=<filename> ) <objective-command>")
  (display-error "  opty [-h | --help]")
  (display-error "")
  (display-error "Options:")
  (display-error "  -h --help                                     Show this screen")
  (display-error "  -M --maximize                                 Maximize objective command, omit to minimize")
  (display-error "  -n --num-iterations                           Number of PSO iterations to run [default: 100]")
  (display-error "  -j --num-processes                            Maximum number of simultaneous objective processes [default: 100]")
  (display-error "  -g <filename>, --initial-guesses=<filename>   Initial guesses for optimum objective function arguments")
  (exit 1))

(define (main)
  (let* ((num-iterations 100)
         (maximize? #f)
         (pool-size 100)
         (command '())
         (command-arguments '())
         (particles-config '())
         (cli-options (list (option '(#\M "maximize") #f #f
                                    (lambda (operand . rest)
                                      (set! maximize? #t)))
                            (option '(#\n "num-iterations") #t #f
                                    (lambda (operand name value . rest)
                                      (set! num-iterations (string->number value))))
                            (option '(#\h "help") #f #f print-help)
                            (option '(#\j "num-processes") #t #f
                                    (lambda (operand name value . rest)
                                      (set! pool-size (string->number value))))
                            (option '(#\g "initial-guesses") #t #f
                                    (lambda (operand name filename . rest)
                                      (set! particles-config (read-particles-file filename)))))))
    (args-fold (command-line-arguments)
               cli-options
               (lambda (option option-name #!rest args) 
                 (display-error (sprintf "error: invalid option: ~A" option-name))
                 (print-help))
               (lambda (command-string #!rest args)
                 (set! command (append command 
                                       (list command-string))))
               '())
    (when (null? particles-config)
      (display-error "error: must supply particles configuration filename")
      (display-error "")
      (print-help))
    (when (null? command)
      (display-error "error: must supply objective command")
      (display-error "")
      (print-help))
    (let* ((particles-header (get-particles-header particles-config))
           (objective-function (run-child (car command) 
                                          (cdr command) 
                                          particles-header  
                                          (if maximize? #t #f)))
           (optimized-point 
             (force 
               (particle-swarm-optimize (get-particle-positions particles-config)
                                        objective-function
                                        num-iterations
                                        pool-size))))
      (write-particle (particle-state-position optimized-point) 
                           particles-header))))
(main)
