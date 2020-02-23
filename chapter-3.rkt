#lang racket

;we lost the atom before the first a here, so we need cons.
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
                ((eq? (car lat) a) (cdr lat))
                (else (rember a 
                            (cdr lat))))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
                ((eq? (car lat) a) (cdr lat))
                (else (cons (car lat) 
                          (rember a 
                            (cdr lat)))))))))


(define firsts
  (lambda (lat)
  (cond
    ((null? lat) '())
    (else (cons (car (car lat)) 
                 (firsts (cdr lat)))))))

(define a (cons 1 (cons 2  '())))
(define b '(4 5 6))
(define c (list a b))
;(firsts c)
'(a b)
; (first '(a b))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) ((cons new quote())))
      (else 
        (cond
          ((eq? (car lat) old)
            (cons (car lat) 
                  (cons new (cdr lat))))
        (else (cons (car lat) 
                    (insertR new old (cdr lat)))))))))


(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else 
        (cond
          ((eq? (car lat) old) (cons new lat))
        (else (cons (car lat)
               (insertL new old (cdr lat)))))))))

(define subst
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote()))
      (else 
        (cond
          ((eq? (car lat) old) (cons new (cdr lat)))
          (else 
            (cons (car lat) (subst new old (cdr lat)))))))))


(define subst2
  (lambda (new o1 o2 lat)
    (cond 
      ((null? lat) (quote()))
      (else
        (cond
          ((or (eq? (car lat) o1) (eq? (car lat) o1)) (cons new (cdr lat)))
          (else 
            (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

(define a (cons 4 (cons 5 (cons 6 (cons 7 null)))))
(subst2 3 4 7 a) ; '(3 5 6 7)

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else
        (cond 
          ((eq? (car lat) a) (multirember a (cdr lat)))
          (else 
            (cons (car lat) (multirember a (cdr lat)))))))))

(define b '(2 3 3 4 5 6 7))
(multirember 3 b) ; '(2 4 5 6 7)

(define multiinsertR
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote()))
      (else 
        (cond
          ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
          (else 
            (cons (car lat) (multiinsertR  new old (cdr lat)))))))))

(define mir '(1 2 3 4 5 2 7))
(multiinsertR 3 2 mir) ; '(1 2 3 3 4 5 2 3 7)

(define multiinsertL
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote()))
      (else 
        (cond
          ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
          (else 
            (cons (car lat) (multiinsertL  new old (cdr lat)))))))))


(define multisubst
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote()))
      (else 
        (cond
          ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
          (else 
            (cons (car lat) (multisubst new old (cdr lat)))))))))
