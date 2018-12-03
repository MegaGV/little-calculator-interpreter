#lang scheme

(define (list-display list blank?)
  (cond ((not (null? list))
         (begin
           (display (car list))
           (cond (blank? (display " ")))
           (list-display (cdr list) blank?)))))

(define (error-proc . msg)
  (display "ERROR: ") (list-display msg #f) (newline))

(define (error-args . msg)
  (display "ARGS: ") (list-display msg #t) (newline))

(define (empty-function . x)
  (begin x))

; (+ 1 2)
(define (function-to-get-proc symbol)
  (cond ((eq? symbol '+) +)
        ((eq? symbol '-) -)
        ((eq? symbol '*) *)
        ((eq? symbol '/) /)
        (else (begin (error-proc "Unrecognize proc \"" symbol "\".") error-args))))

(define (function-to-get-sym expr)
  (function-to-get-proc expr))

(define (function-to-get-list expr)
  (apply (function-to-get-proc (car expr))
         (cdr expr)))

(define (function-to-get-num expr)
  expr)

(define (my-eval expr)
  (cond ((list? expr) (function-to-get-list expr))
        ((number? expr) (function-to-get-num expr))
        ((symbol? expr) (function-to-get-sym expr))))