#lang racket

(define atom?
  (lambda (x)
   (and (not (pair? x)) (not (null? x)))))

(atom? 1); #t
(atom? (quote())); #f

(define eqan?
  (lambda (a1 a2)
    (cond
    ((and (number? a1) (number? a2)) (= a1 a2))
    ((or (number? a1) (number? a2)) #f)
    (else (eq? a1 a2)))))

(define **
  (lambda (n m)
    (cond 
      ((zero? m) 1)
      (else 
        (x n (** n (sub1 m)))))))

(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else 
        (add n (x n (sub1 m)))))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define sub
  (lambda (n m)
    (cond 
      ((zero? m) n)
      (else
        (sub1 (sub n (sub1 m)))))))

(define add
  (lambda (n m)
    (cond 
      ((zero? m) n)
      (else 
        (add1 (add n (sub1 m)))))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
        (and (numbered? (car aexp))
             (number? (car (cdr (cdr aexp)))))))))
;; seems it only support three atom.
(display "test -> numbered?") (newline)
(numbered? '(5 + 4)) ;; #t
(numbered? 5) ;; #t
(numbered? '(1 + 3 x 4)) ;; #t
; (numbered? '(3 4 5)) ;; #f
; (numbered? '()) ;; #f


(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

;; Yes, because we used help functions to hide the representation.
;; rule, rule, rule
;; abstract, abstractm abstract
;; representation
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote +))
       (add (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote x))
       (x (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
      (else
       (** (value (1st-sub-exp nexp))
           (value (2nd-sub-exp nexp)))))))

(display "test -> value") (newline)
(value 5) ;; 5
(value '(+ 1 5)) ;; 6
(value '(x 3 4)) ;; 12
(value '(+ 1 2)) ;; 3

(define sero?
  (lambda (n)
    (null? n)))
(sero? '()) ;; #t

(define edd1
  (lambda (n)
    (cons (quote()) n)))

(edd1 '()) ;; '(())

(define zub1
  (lambda (n)
    (cdr n)))

(zub1 '('())) ;; '()

(define ＋
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else
       (edd1 (＋ n (zub1 m)))))))

(＋ (list '()) (list '()))