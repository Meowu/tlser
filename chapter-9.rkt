#lang racket

(define atom?
  (lambda (x)
   (and (not (pair? x)) (not (null? x)))))

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


(define addtup
  (lambda (tup)
    (cond 
      ((null? tup) 0)
      (else 
        (add (car tup) 
          (addtup (cdr tup)))))))


(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else 
        (add n (x n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) (quote()))
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else 
        (cons (add (car tup1) (car tup2))
          (tup+ (cdr tup1) (cdr tup2)))))))


(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define > 
  (lambda (n m)
    (cond
     ; we should evaluate n first, so the program works while n == m, which returns #f;
      ((zero? n) #f) 
      ((zero? m) #t)
      (else 
        (> (sub1 n) (sub1 m))))))


(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else 
        (< (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))


(define equal
  (lambda (n m)
    (cond 
      ((zero? n) (zero? m))
      ((zero? m) #f)
      (else 
        (equal (sub1 n) (sub1 m))))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 null))))

(define **
  (lambda (n m)
    (cond 
      ((zero? m) 1)
      (else 
        (x n (** n (sub1 m)))))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      (else
       (null? (cdr (cdr x)))))))


(define divide
  (lambda (n m)
    (cond 
      ((< n m) 0)
      (else
        (add1 (divide (sub n m) m))))))


(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else
        (pick (sub1 n) (cdr lat))))))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else
       (eq? sorn a)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define look-list '(6 2 4 "caviar" 5 7 3))
(looking "caviar" look-list)

(define externity
  (lambda (x)
    (externity x)))

(define shift
  (lambda (pair)
    (build (first (first pair))
       (build (second (first pair))
              (second pair)))))

(shift '((a b) c)) ;; 只是第一个是 pair，和第二部分组成一个新的 pair；
(shift '((a b) (c d)))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
    (else
     (add (length* (first pora))
          (length* (second pora)))))))

(length* '(a (b c)))
(length* '((a b) (c d)))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (add (x (weight* (first pora)) 2)
            (weight* (second pora)))))))

(weight* '((a b) c)) ;; (2 + 1) x 2 + 1
(weight* '(a (b c))) ;; (2 + (2 + 1))

