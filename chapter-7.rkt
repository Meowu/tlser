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

(define set?
  (lambda (s)
    (cond
      ((null? s) #t)
      ((member? (car s)
                (cdr s)) #f)
      (else
        (set? (cdr s))))))

(define lis  '("apple" 3 "pear" 4 9 "apple" 3 4))
(set? '("apple" "orange" "apple")) ;; #f
(set? '("a" "b" "c" "d")) ;; #t
(set? lis) ;; #f

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat))
       (makeset (cdr lat)))
    (else
     (cons (car lat) (makeset (cdr lat)))))))

(define fruits '("apple" "peach" "pear" "peach" "plum" "apple" "lemon" "peach"))
(makeset fruits)

(define makeset2
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cons (car lat) (makeset2 (multirember (car lat) (cdr lat))))))))

(makeset2 fruits)
(makeset2 lis)

(define subset?
  (lambda (s1 s2)
    (cond
      ((null? s1) #t)
      (else
       (and (member? (car s1) s2)
            (subset? (cdr s1) s2))))))

(define s1 '(5 "chicken" "wings"))
(define s2 '(5 "hamburgers" 2 "pieces" "fried" "chicken" "and" "light" "duckling" "wings"))
(subset? s1 s2) ;; #t

(define eqset?
  (lambda (s1 s2)
    (cond
      ((and (null? s1) (null? s2)) #t)
      ((or (null? s1) (null? s2)) #f)
      (else
        (and (equal? (car s1) (car s2)) (eqset? (cdr s1) (cdr s2)))))))
(eqset? s1 '(5 "chicken" "wings")) ;; #t

;; 相等的集合互为子集
(define eqset2?
  (lambda (s1 s2)
    (and (subset? s1 s2) (subset? s2 s1))))
(eqset2? s1 '(5 "chicken" "wings")) ;; #t

(define intersect?
  (lambda (s1 s2)
    (cond
      ((null? s1) #f)
      (else
       (or (member? (car s1) s2) (intersect? (cdr s1)))))))

(intersect? '(5) s1) ;; #t

(define intersect
  (lambda (s1 s2)
    (cond
      ((null? s1) (quote()))
      ((member? (car s1) s2)
       (cons (car s1) (intersect (cdr s1) s2)))
      (else
       (intersect (cdr s1) s2)))))

(intersect s1 s2) ;; '(5 "chicken" "wings")

(define union
  (lambda (s1 s2)
    (cond
      ((null? s1) s2)
      ((member? (car s1) s2)
        (union (cdr s1) s2))
       (else
        (cons (car s1) (union (cdr s1) s2))))))

(define s3 (list "beef" 8 5))
(union s1 s3); '("chicken" "wings" "beef" 8 5)

;; bottom-up...
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else
       (intersect (car l-set) (intersectall (cdr l-set)))))))
(define allset (list (list "a" "b" "c" 5) (list "c" "d" "a" "e") (list "e" "f" "g" "h" "a" "b")))
(intersectall allset)
(intersectall (cons (cons 5 null) (cons s1 (cons s2 null))));

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      (else
       (null? (cdr (cdr x)))))))
(a-pair? '());
(a-pair? '("a" "b" "c"));

(define first
  (lambda (p)
    (car p)))

(define firsts
  (lambda (lat)
  (cond
    ((null? lat) '())
    (else (cons (first (first lat)) 
                 (firsts (cdr lat)))))))

(define second
  (lambda (p)
    (car (cdr p))))

(define seconds
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cons (second (car lat))
             (seconds (cdr lat)))))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 null))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define rel (list (list 8 3) (list 4 2) (list 7 6) (list 6 2) (list 3 4)))
(fun? rel) ;; #t

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote()))
      (else
       (cons (revpair (car rel))
             (revrel (cdr rel)))))))

(revrel rel) ;; '((3 8) (2 4) (6 7) (2 6) (4 3))

(define fullfun?
  (lambda (rel)
    (set? (seconds rel))))
(fullfun? rel) ;; #f

(define one-to-one?
  (lambda (rel)
    (fun? (revrel rel))))

(define food (list (list "chocolate" "chip") (list "dough" "cookie")))
(one-to-one? food) ;; #t

