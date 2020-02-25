#lang racket

(define atom?
  (lambda (x)
   (and (not (pair? x)) (not (null? x)))))

(atom? 1); #t
(atom? (quote())); #f

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


(define insertR*
 (lambda (new old lat)
  (cond
   ((null? lat) (quote()))
    ((atom? (car lat))
      (cond
       ((eq? (car lat) old)
        (cons (car lat) 
               (cons new (insertR* new old (cdr lat)))))
        (else 
         (cons (car lat) (insertR* new old (cdr lat))))))
      (else
       (cons (insertR* new old (car lat)) 
        (insertR* new old (cdr lat)))))))

(define l (list 
           (list "how" "much" 
            (cons "wood" null)) 
             "could"
              (list (list "a" '("wood") "chuck"))
              '('('("chuck")))
               (list "if" (list "a") (list (list "wood" "chuck")))
                "could" "chuck" "wood"))
(insertR* "roast" "chuck" l)


(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else
          (occur* a (cdr l)))))
      (else
       (add (occur* a (car l)) (occur* a (cdr l)))))))

(define ll '('("banana") '("split" '('('('("banana" "ice")))
                                    '("cream" '("banana"))
                                    "sherbet"))
                        '("banana")
                        '("bread")
                        '("banana" "brandy")))

(occur* "banana" ll)
(occur* "Apple" '("Apple" "Egg" "Bear" "Apple" "Banana"))
 
(define subst*
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((atom? (car lat))
       (cond
         ((eq? (car lat) old)
          (cons new (subst* new old (cdr lat))))
       (else
        (cons (car lat) (subst* new old (cdr lat))))))
     (else
      (cons (subst* new old (car lat)) (subst* new old (cdr lat)))))))


(define occurl '('("banana") '("split" '('('('("banana" "ice")))
                                         '("cream" '("banana"))
                                         "sherbet"))
                             '("banana")
                             '("brean")
                             '("banana" "brandy")))
(subst* "orange" "banana" occurl)

(define insertL*
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat))
       (cond
         ((eq? old (car lat))
          (cons new (cons old (insertL* new old (cdr lat)))))
         (else
          (cons old (insertL* new old (cdr lat))))))
      (else
       (cons (insertL* new old (car lat)) (insertL* new old (cdr lat)))))))

(define insertl '('("banana") '("split" '('('('("banana" "ice")))
                                         '("cream" '("banana"))
                                         "sherbet"))
                             '("banana")
                             '("brean")
                             '("banana" "brandy")))

(insertL* "apple" "banana" insertl)


