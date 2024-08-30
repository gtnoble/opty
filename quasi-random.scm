(declare (unit quasi-random))

(import (chicken foreign))
(import srfi-4)

(foreign-declare "#include <gsl/gsl_qrng.h>")

(define-foreign-type qrng-type c-pointer)
(define-foreign-type qrng u8vector)

(define-foreign-variable sobol-type qrng-type "gsl_qrng_sobol")
(define-foreign-variable halton-type qrng-type "gsl_qrng_halton")
(define-foreign-variable reverse-halton-type qrng-type "gsl_qrng_reversehalton")
(define-foreign-variable niederreiter-type qrng-type "gsl_qrng_niederreiter_2")

(define-record quasi-random-number-generator qrng state dimension)

(define get-qrng-state-size 
  (foreign-lambda* unsigned-long ((qrng-type qrng_type) (int qrng_dimension))
                   "C_return((unsigned long)(((gsl_qrng_type *) qrng_type)->state_size(qrng_dimension)));"))

(define max-dimension 
  (foreign-lambda* unsigned-int ((qrng-type qrng_type))
                   "C_return(((gsl_qrng_type *) qrng_type)->max_dimension);"))

(define (new-qrng type dimension)
    (assert (<= dimension (max-dimension type)))
    (let* ((qrng-state-size (get-qrng-state-size type dimension))
           (qrng-state (make-u8vector qrng-state-size 0 #t))
           (qrng (make-u8vector (foreign-type-size "gsl_qrng") 0 #t)))
      ((foreign-lambda* void 
                        ((qrng qrng) 
                         (qrng-type qrng_type) 
                         (int qrng_dimension) 
                         (unsigned-long qrng_state_size) 
                         (u8vector qrng_state))
                        "((gsl_qrng *) qrng)->type = (gsl_qrng_type *) qrng_type;
                         ((gsl_qrng *) qrng)->dimension = qrng_dimension;
                         ((gsl_qrng *) qrng)->state_size = qrng_state_size; 
                         ((gsl_qrng *) qrng)->state = qrng_state;
                         gsl_qrng_init((gsl_qrng *) qrng);"
                        )
       qrng type dimension qrng-state-size qrng-state)
      (make-quasi-random-number-generator qrng qrng-state dimension)))

(define (new-quasi-random-number-generator generator-type dimension) 
  (new-qrng (cond ((eq? generator-type 'sobol) sobol-type)
                  ((eq? generator-type 'halton) halton-type)
                  ((eq? generator-type 'niederreiter) niederreiter-type)
                  ((eq? generator-type 'reverse-halton) reverse-halton-type)
                  (#t (error "error: invalid quasirandom generator type"))) 
            dimension))

(define (generate-value! qrng)
  (let ((result (make-f64vector (quasi-random-number-generator-dimension qrng))))
   ((foreign-lambda* void ((qrng qrng) (f64vector result))
                     "C_return(gsl_qrng_get((gsl_qrng *) qrng, result));")
    (quasi-random-number-generator-qrng qrng) 
    result)
   result))
