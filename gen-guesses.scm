(import (chicken process-context))
(import (chicken random))
(import srfi-13)
(import srfi-14)

(define (log10 x)
  (/ (log x)
     (log 10)))

(define (exponential-10 x)
  (expt 10 x))

(define (make-interval-random-generator lower-range upper-range exponential?)
  (let ((higher-limit (if exponential? (log10 upper-range) upper-range))
        (lower-limit (if exponential? (log10 lower-range) lower-range))) 
    (lambda ()
      (let* ((delta (- higher-limit lower-limit))
            (offset lower-limit)
            (uniformly-distributed-value (+ offset 
                                            (* delta 
                                               (pseudo-random-real)))))
        (if exponential? 
            (exponential-10 uniformly-distributed-value) 
            uniformly-distributed-value)))))

(define (parse-argument argument-string)
  (let* ((format-flag-parsed (string-tokenize argument-string 
                                              (char-set-complement (char-set #\_))))
         (format-flag (if (= (length format-flag-parsed) 2) 
                          (cadr format-flag-parsed) 
                          #f))
         (exponential? (if (equal? format-flag "d") #t #f))
         (range-parsed (string-tokenize (car format-flag-parsed) 
                                        (char-set-complement (char-set #\,))))
         (range (map exact->inexact
                     (map string->number 
                          range-parsed))))
    (let* ((lower-range (car range))
           (upper-range (cadr range)))
      (list lower-range upper-range exponential?))))

(define (generate-initial-guesses variable-generators number-guesses)
  (when (> number-guesses 0)
    (display (string-join (map (lambda (generator) (number->string (exact->inexact (generator)))) 
                               variable-generators)
                          " "))
    (newline)
    (generate-initial-guesses variable-generators (- number-guesses 1))))

(define (main)
  (let* ((arguments (command-line-arguments))
         (number-guesses (string->number (car arguments)))
         (field-arguments (cdr arguments))
         (variable-generators 
           (map (lambda (field-argument) 
                  (apply make-interval-random-generator (parse-argument field-argument)))
                field-arguments)))
    (generate-initial-guesses variable-generators number-guesses)))

(main)
