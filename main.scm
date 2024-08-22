(declare (uses pso))

(import (chicken process))
(import (chicken io))
(import (chicken process-context))
(import (chicken format))
(import srfi-13)
(import srfi-37)

(define (read-initial-positions-file filename)
  (let* ((input-file (open-input-file filename))
         (position-lines (read-list input-file read-line)))
    (close-input-port input-file)
    (map (lambda (line-strings)
           (map string->number 
                (string-tokenize line-strings)))
         position-lines)))

(define (run-child command #!optional (negate? #f))
  (define (function-call-child #!rest args) 
    (let-values (((process-output-port process-input-port pid) 
                  (process command (map number->string 
                                        (map exact->inexact args)))))
                ;; Delay reading objective command output so we can run multiple objective commands in parallel
                (delay
                  (let ((result (string->number (car (read-list process-output-port 
                                                                read-line 
                                                                1)))))
                    (close-input-port process-output-port)
                    (close-output-port process-input-port)
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
         (initial-positions '())
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
                                      (set! initial-positions (read-initial-positions-file filename)))))))
    (args-fold (command-line-arguments)
               cli-options
               (lambda (option option-name #!rest args) 
                 (display-error (sprintf "error: invalid option: ~A" option-name))
                 (print-help))
               (lambda (command-string #!rest args)
                 (set! command command-string))
               '())
    (when (null? initial-positions)
      (display-error "error: must supply initial-guesses filename")
      (display-error "")
      (print-help))
    (when (null? command)
      (display-error "error: must supply objective command")
      (display-error "")
      (print-help))
    (let* ((objective-function (run-child command (if maximize? #t #f)))
           (optimized-point 
             (force 
               (particle-swarm-optimize initial-positions
                                        ;; Negate the objective if you want to maximize rather than minimize
                                        objective-function
                                        num-iterations
                                        pool-size))))
      (print (string-join (map number->string 
                               (particle-state-position optimized-point)) 
                          " ")))))

(main)
