#lang racket

(define add1
  (lambda (n)
    (+ n 1)))

(add1 2) ; 3

(define sub1
  (lambda (n)
    (- n 1)))

(sub1 4); 3


(define sub
  (lambda (n m)
    (cond 
      ((zero? m) n)
      (else
        (sub1 (sub n (sub1 m)))))))

(sub 5 2); 3

(define add
  (lambda (n m)
    (cond 
      ((zero? m) n)
      (else 
        (add1 (add n (sub1 m)))))))

(add 5 2) ; 7

(define addtup
  (lambda (tup)
    (cond 
      ((null? tup) 0)
      (else 
        (add (car tup) 
          (addtup (cdr tup)))))))

(addtup '(1 2 3 4)) ; 10

(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else 
        (add n (x n (sub1 m)))))))

(x 5 6); 30

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) (quote()))
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else 
        (cons (add (car tup1) (car tup2))
          (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(1 2 3) '(4 5 6)); '(5 7 9)
(tup+ '(3 7) '(4 6 8 1)); '(7 13 8 1)

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

(= 4 5) ; #f
(= 6 3) ; #f
(= 8 8) ; #t

(define equal
  (lambda (n m)
    (cond 
      ((zero? n) (zero? m))
      ((zero? m) #f)
      (else 
        (equal (sub1 n) (sub1 m))))))

(equal 4 5) ; #f
(equal 6 3) ; #f
(equal 8 8) ; #t

(define **
  (lambda (n m)
    (cond 
      ((zero? m) 1)
      (else 
        (x n (** n (sub1 m)))))))

(** 2 3)

(define divide
  (lambda (n m)
    (cond 
      ((< n m) 0)
      (else
        (add1 (divide (sub n m) m))))))

(divide 4 2) ; 2
(divide 15 4) ; 3

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else
        (add1 (length (cdr lat)))))))

(length '(1 2 3 4)); 4

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else
        (pick (sub1 n) (cdr lat))))))
(pick 2 '(1 2 3)); 2

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else 
        (cons (car lat) 
          (rempick (sub1 n) (cdr lat)))))))
(rempick 3 (list 1 2 3)); '(1 2)
(rempick 2 (list "black" "green" "white" "red")); '("black" "white" "red")

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else 
        (cond
          ((number? (car lat)) (no-nums (cdr lat)))
          (else
            (cons (car lat) (no-nums (cdr lat)))))))))

(no-nums '(1 "233" "who" "cool")) ; '("233" "who" "cool")
(no-nums '("1" "2" 3 4 "5" "6")) ; '("1" "2" "5" "6")

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else
        (cond
          ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
          (else
            (all-nums (cdr lat))))))))

(all-nums '(1 2 "3" "4" 5 6)); '(1 2 5 6)

(define eqan?
  (lambda (a1 a2)
    (cond
    ((and (number? a1) (number? a2)) (= a1 a2))
    ((or (number? a1) (number? a2)) #f)
    (else (eq? a1 a2)))))

(eqan? 1 2); #f
(eqan? '1' 2); #f
(eqan? 2 2); #t
(eqan? 2 '()); #f

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else 
      (cond 
        ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
        (else (occur a (cdr lat))))))))

(occur 1 '(1 1 2 3 4)); 2
(occur "1" '("1" 2 "1" "1" 4)); 3

(define one?
  (lambda (n)
    (cond
      ((zero? n) #f)
      (else (zero? (sub1 n))))))

(define one?
  (lambda (n)
    (= n 1)))

(one? 1); #t
(one? 2); #f

(define rempick2
  (lambda (n lat)
    (cond 
      ((one? n) (cdr lat))
      (else
        (cons (car lat) (rempick2 sub1(n) (cdr lat)))))))

(rempick2 3 (list 1 2 3)); '(1 2)
(rempick2 2 (list "black" "green" "white" "red")); '("black" "white" "red")

