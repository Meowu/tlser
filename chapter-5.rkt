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
          (cons (car lat) (insertL* new old (cdr lat))))))
      (else
       (cons (insertL* new old (car lat)) (insertL* new old (cdr lat)))))))

(define insertl '('("banana") '("split" '('('('("banana" "ice")))
                                         '("cream" '("banana"))
                                         "sherbet"))
                             '("banana")
                             '("brean")
                             '("banana" "brandy")))

(insertL* "apple" "banana" insertl)

(define member*
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((atom? (car lat))
       (or (eq? (car lat) a)
           (member* a (cdr lat))))
      (else
       (or (member* a (car lat)) (member* a (cdr lat)))))))

(member* "brean" insertl)
(member* "brandy" insertl)
(member* "peach" insertl)
(member* "banana" insertl)

(define leftmost
  (lambda (l)
    (cond
      ;((null? ) #f)
      ((atom? (car l)) (car l))
     (else
      (leftmost (car l))))))

(leftmost insertl)
(leftmost '("banana" "pear"))
(leftmost (list '("apple") "orange")) ; we cannot use ' to create outer most list.
; (leftmost '()) ; error.

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? l1) (atom? l2)) (eqan? l1 l2))
      ((or (atom? l1) (atom? l2)) #f)
    (else
     (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(display "test -> eqlist") (newline)
(eqlist? '() '()) ;; #t
(eqlist? '() '("queen")) ;; #f
(eqlist? (list "strawberry" "ice" "cream")  (list "strawberry" "ice" "cream")) ;; #t
(eqlist? (list "strawberry" "cream" "ice")  (list "strawberry" "ice" "cream")) ;; #f
(eqlist? (list "banana" (list (list "split"))) (list (list "bananan") (list "split"))) ;; #f
(eqlist? (list "beef" (list (list "sausage")) (list "and" (list "soda")))
        (list "beef" (list (list "salami")) (list "and" (list "soda")))) ;; #f
(eqlist? (list "beef" (list (list "sausage")) (list "and" (list "soda")))
        (list "beef" (list (list "sausage")) (list "and" (list "soda")))) ;; #t

;; An S-expression is either an atom or a (possibly empty) list of S-expressions.

(define equal?
  (lambda (a b)
    (cond
      ((and (atom? a) (atom? b)) (eqan? a b))
      ((or (atom? a) (atom? b)) #f)
     (else
      (eqlist? a b)))))
(display "test -> equal?") (newline)
(equal? 1 2) ;; #f
(equal? 1 1) ;; #t
(equal? '() '()) ;; #t
(equal? '("a") '("a")) ;; #t
(equal? "a" '("a")) ;; #f

(define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist2? (cdr l1) (cdr l2)))))))

;; now it removes the first matching S-expression s in l, instead of the first matching atom a in lat.
(define rember
  (lambda (s l)
    (cond
      ((null? l) (quote()))
      (else
       (cond
         ((equal? (car l) s) (cdr l))
         (else
          (cons (car l) (rember s (car l)))))))))
