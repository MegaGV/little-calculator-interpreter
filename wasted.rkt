#lang scheme
(define (function-to-get-list expr)
  (let ((operator (function-to-get-proc (car expr)))
        (args (cdr expr)))
    (cond ((check-args operator args)
           (apply operator args)))))

(define (function-to-get-list expr)
  (let ((operator (function-to-get-proc (car expr)))
        (args (cdr expr)))
    (cond ((check-args operator args)
           (apply operator args)))))

(define (check-args operator args)
  (cond ((eq? operator /)
         (if (= (cadr args) 0)
             (begin (error-proc " division by zero") (error-args args) #f)
             #t)))
  (if (variable? args)
      #t
      (begin (error-proc "wrong variable") #f)))

(define (variable? item)
  (cond((null? item) #t)
       ((number? (car item)) (variable? (cdr item)))
       (else #f)))