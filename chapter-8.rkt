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

(define operator
  (lambda (aexp)
    (car aexp)))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

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

(define rember-ff
  (lambda (test?)
    (lambda (x y)
      (cond
        ((null? y) (quote()))
        ((test? x (car y)) (cdr y))
        (else
         (cons (car y) ((rember-ff test?) x (cdr y))))))))

((rember-ff eq?) "tuna" (list "shrimp" "salad" "and" "tuna" "salad"))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))
        ((test? (car l) old) (cons new
                                   (cons old (cdr l))))
        (else
         (cons (car l) ((insertL-f test?) new old (cdr l))))))));

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))
        ((test? (car l) old) (cons old (cons new (cdr l))))
        (else
         (cons (car l) ((insertR-f test?) new old (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))
        ((eq? (car l) old) (seq new (car l) (cdr l)))
        (else
         (cons (car l) ((insert-g seq) new old (cdr l))))))))

(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define insertR
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x (quote +)) add)
      ((eq? x (quote x)) x)
      (else **))))

(atom-to-function (operator '(+ 5 3))) ;; func: add

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
      ((atom-to-function (operator nexp))
       (value (1st-sub-exp nexp))
       (value (2nd-sub-exp nexp)))))))


