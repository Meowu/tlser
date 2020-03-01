#lang racket

(define atom?
  (lambda (x)
   (and (not (pair? x)) (not (null? x)))))

(define eqan?
  (lambda (a1 a2)
    (cond
    ((and (number? a1) (number? a2)) (= a1 a2))
    ((or (number? a1) (number? a2)) #f)
    (else (eq? a1 a2)))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else
        (cond 
          ((equal? (car lat) a) (multirember a (cdr lat)))
          (else 
            (cons (car lat) (multirember a (cdr lat)))))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (a b)
    (cond
      ((and (atom? a) (atom? b)) (eqan? a b))
      ((or (atom? a) (atom? b)) #f)
     (else
      (eqlist? a b)))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? (car lat) a)
                  (member? a (cdr lat)))))))


(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) (quote()))
      ((test? a (car l)) (cdr l))
      (else
       (cons (car l) (rember-f test? a (cdr l)))))))

(define la (list "lemonade" (list "pop" "corn") "and" (list "cake")))
(rember-f equal? 5 '(6 2 5 3)) ;; '(6 2 3)
(rember-f equal? (list "pop" "corn") la)

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? a x))))

((eq?-c "2") "2") ;; #t

(define eq?-salad
  (eq?-c "salad"))

(eq?-salad "salad") ;; #t