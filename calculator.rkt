#lang scheme
;定义非运算
(define (!= x y) (not (= x y)))

;eval主函数
(define (my-eval expr)
  (cond((null? expr) (error-proc "empty list."))
       ((number? expr) (function-to-get-num expr))
       ((symbol? expr) (function-to-get-sym expr))
       ((list? expr) (function-to-get-list expr))
       ))

;错误情况处理
;错误报告函数，参数为错误描述
(define (error-proc . msg)
  (display "ERROR: ") (list-display msg #f) (newline))

(define (error-args . msg)
  (display "ARGS: ") (list-display msg #t) (newline))

(define (empty-function . x)
  (begin x))

;辅助函数，分行输出表示列表的所有内容
(define (list-display list blank?)
  (cond ((not (null? list))
         (begin
           (display (car list))
           (cond (blank? (display " ")))
           (list-display (cdr list) blank?)))))

;当对象为单一数字时，直接返回数字本身
(define (function-to-get-num expr)
  expr)

;当对象为单一运算符时，先获取该运算符，再返回该运算符
(define (function-to-get-sym expr)
  (if (function-to-get-proc-proccheck expr) ;Check proc
      (function-to-get-proc expr)
      (error-proc "Error proc.")
      ))
  

;当对象为列表时，先获取运算符和参数，再分别处理
(define (function-to-get-list expr)
  (let ((proc-symbol (car expr)) (args (cdr expr)))
    (if (function-to-get-proc-proccheck proc-symbol)  ;Check proc
        (if ((function-to-get-proc-argcheck proc-symbol) args)  ; Check Args
            (apply (function-to-get-proc proc-symbol) args)
            (error-proc "Error args."))
        (error-proc "Error proc."))
    ))

;检查运算符集中是否有该运算符
;assoc用于字典查询，根据关键字(这里是运算符的符号)找到运算符对应的列表，没有则返回#f
(define (function-to-get-proc-proccheck symbol)
  (assoc symbol one-operator-set))

;
(define (function-to-get-proc-argcheck symbol)
  (get-operators-proc one-operator-set get-operators-argcheck-sub symbol))

;获取运算符函数（前置），参数为运算符的符号。通过get-operators-proc获取运算符
;此处传出的第二个参数为获取列表中第二个参数的函数
(define (function-to-get-proc symbol)
  (get-operators-proc one-operator-set get-operators-proc-sub symbol))

;获取运算符函数，参数为运算符集、辅助函数、运算符
;递归遍历整个运算符集，对目标运算符的列表调用辅助函数
(define (get-operators-proc operator-set sub-func operator)
  (cond ((eq? operator (caar operator-set)) (sub-func (car operator-set)))
        (else (get-operators-proc (cdr operator-set) sub-func operator))))

;辅助函数:获取列表中第二个元素。此处用于获取运算符本身
(define (get-operators-proc-sub operator-list)
  (cadr operator-list))
;辅助函数:获取列表中第三个元素。此处用于获取运算符的定义(lambda表达式)
(define (get-operators-argcheck-sub operator-list)
  (caddr operator-list))


;运算符集
;集合中第一个列表为运算符列表，然后每个元素又是一个单独的列表
;作为元素的列表的元素分别为符号、运算符本身、lambda表达式
(define one-operator-set
  (list (list '+
              +
              (lambda (args) (and (>= (length args) 0) #t)))  ; (+) means (+ 0 0)
        (list '-
              -
              (lambda (args) (and (>= (length args) 1) #t)))  ; (- 1) means (- 0 1)
        (list '*
              *
              (lambda (args) (and (>= (length args) 0) #t)))  ; (*) means (* 1 1)
        (list '/
              /
              (lambda (args) (not (or (< (length args) 1) (and (>= (length args) 2) (ormap zero? (cdr args)))))))  ; (/ 1) means (/ 1 1)
        (list 'chu
              (lambda (x) (display "This function is in chu with ") (display x) (display ".") (newline))
              (lambda (args) (and (= (length args) 1) #t)))
        ))
















