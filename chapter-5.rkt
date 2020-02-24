#lang racket

(define atom?
  (lambda (x)
   (and (not (pair? x)) (not (null? x)))))

(atom? 1); #t
(atom? (quote())); #f

(define > 
  (lambda (n m)
    (cond
     ; we should evaluate n first, so the program works while n == m, which returns #f;
      ((zero? n) #f) 
      ((zero? m) #t)
      (else 
        (> (sub1 n) (sub1 m))))))

(> 10 5); #t
(> 2 4); #f

(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else 
        (< (sub1 n) (sub1 m))))))

(< 4 8) ; #t
(< 5 3) ; #f
(< 7 7) ; #f

(define =
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))

(define eqan?
  (lambda (a1 a2)
    (cond
    ((and (number? a1) (number? a2)) (= a1 a2))
    ((or (number? a1) (number? a2)) #f)
    (else (eq? a1 a2)))))

(define rember*
   (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((atom? (car lat))
       (cond
        ((eq? (car lat) a)
         (rember* a (cdr lat)))
        (else
         (cons (car lat) (rember* a (cdr lat))))))
      (else ; recursively invoke if lat.
       (cons (rember* a (car lat)) (rember* a (cdr lat)))))))

(rember* 1 '(1 2 3))

(rember* 1 (list (list 1 2 3) 4 5 6 1 (list 1 7 8)))
