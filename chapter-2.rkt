#lang racket

(define lat? 
  (lambda (a)
    (cond 
      ((null? l) #t)
      ((atom? (car l))(lat? (cdr? l)))
      (else #f))))

(lat? '())

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                  (member? a (cdr lat)))))))


;creates a closure that "remembers' 2 values
(define (consm x y)    (lambda (m) (m x y)))
;recieves a cons holding 2 values, returning the 0th value
(define (carm z)       (z (lambda (p q) p)))
;recieves a cons holding 2 values, returning the 1st value
(define (cdrm z)       (z (lambda (p q) q)))

(carm '(1 2))