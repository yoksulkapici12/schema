(define (twoOperatorCalculator item)
  (if (null? item)
      0
      (if (null? (cdr item))
          (car item)
          (if (eqv? '+ (car (cdr item)))
              (+ (car item) (twoOperatorCalculator (cdr (cdr item))))
              (if (eqv? '- (car (cdr item)))
                  (+(- (car item) (car (cddr item)))  (twoOperatorCalculator (cdr(cddr item)))  ) 
                  (twoOperatorCalculator (cdr item)))))))


(define (fourOperatorCalculator item)
  (if (null? (cdr item))
      item
      (if (eqv? '* (cadr item))
          (fourOperatorCalculator (cons (* (car item) (caddr item)) (cdddr item)))
          (if (eqv? '/ (cadr item))
              (fourOperatorCalculator (cons (/ (car item) (caddr item)) (cdddr item)))
              (cons (car item) (fourOperatorCalculator (cdr item)))))))


(define (Nested item)
  (if (pair? item)
      (twoOperatorCalculator (fourOperatorCalculator (calculatorNested item)))
      item))

(define (calculatorNested item)
  (map Nested item))

(define (checkoperators item)
  (cond
    ((null? item) #f)
    ((number? item) #f)
    ((and (number? (car item)) (null? (cdr item))) #t)
    ((and (pair? (car item)) (null? (cdr item))) (checkoperators (car item)))
    ((and (number? (car item)) (memv (cadr item) '(+ - * /))) (checkoperators (cddr item)))
    ((and (pair? (car item)) (memv (cadr item) '(+ - * /)))
     (and (checkoperators (car item)) (checkoperators (cddr item))))
    (else #f)))


(define (calculator item)
  (if (checkOperators item)
      (twoOperatorCalculator (fourOperatorCalculator (calculatorNested item)))
      #f))
