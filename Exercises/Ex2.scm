;2.1
(define (make-rat n d) 
  (define num-sign 
    (if (< (* n d) 0)
      -
      +))
  (let ((g (gcd n d)))
    (if (< (* n d) 0)
      (cons (num-sign (abs (/ n g))) (abs (/ d g))))))

;2.2

(define (average a b)
  (/ (+ a b) 2))

(define (make-point n d) (cons n d)) 
(define (x-point x) (car x)) 
(define (y-point x) (cdr x))

(define (make-segment start end) (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg)  (cdr seg))

(define (midpoint-segment seg)
  (let ((x (start-segment seg)) 
        (y (end-segment seg)))
    (make-point (average (x-point x) (x-point y))
                (average (y-point x) (y-point y)))))

(define (print-point p) 
  (newline)
  (display "(") 
  (display (x-point p)) 
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment-try seg)
  (print-point (midpoint-segment seg)))

;test
(midpoint-segment (cons (cons 1 2) (cons 2 3)))
    
;2.3
;1st implementation
(define (make-rectangle pseg1 pseg2) (cons pseg1 pseg2))
(define (seg1 rect) (car rect))
(define (seg2 rect) (cdr rect))

(define (distance x y)
  (+ (square (- (x-point x) (x-point y))) 
                   (square (- (y-point x) (y-point y)))))

(define (length rect)
    (let ((seg (seg1 rect)))
      (let ((x (start-segment seg))
            (y (end-segment seg)))
        (distance x y))))

(define (breadth rect)
  (let ((seg1 (seg1 rect))
        (seg2 (seg2 rect)))
    (let ((x (start-segment seg1))
          (y (start-segment seg2)))
      (distance x y))))

;2nd implementation

(define (make-rectangle-2 any-pt width height) 
  (make-point any-pt (make-point width height)))
(define (length rect) (car (cdr rect)))
(define (breadth rect) (cdr (cdr rect)))

;main

(define (perimeter rect)
    (+ (* 2 (length rect)) (* 2 (breadth rect))))

(define (area rect)
  (* (length rect) (breadth rect)))

;test
(area (make-rectangle (make-segment (cons 1 2) (cons 2 3)) 
                           (make-segment (cons 1 4) (cons 2 5))))

(area (make-rectangle-2 (make-point 1 2) 2 4))
    
;2.4

(define (cons x y) 
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

;Substution method
;(car (cons x y))
;(car (lambda (m) (m x y)))
;(lambda (lambda (p q) p) 
;(lambda (p q) p) x y)
;x

;2.5

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))
(define (car z)
  (pow z 2))
(define (cdr z)
  (pow z 3))

(define (pow num x)
  (define (iter a count)
    (if (= (remainder a x) 0)
      (iter (/ a x) (+ count 1))
      count))
    (iter num 0))

;test
;(car (cons 2 3))

;2.6

(define zero 
  (lambda (f) (lambda (x) x))) 
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;  substitution procedure
;  one
;  (add-1 zero)
;  (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;  (lambda (f) (lambda (x) (f (((lambda (x) x)) x))))
;  (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;  (lambda (f) (lambda (x) (f x)))

(define one
  (lambda (f) (lambda (x) (f x))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;2.7

(define (make-interval a b) (cons a b))
(define (lower-bound z) (min (cdr z) (car z)))
(define (upper-bound z) (max (cdr z) (car z)))

;2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

;2.9

(define (width x)
  (/ (- (upper-bound z) (lower-bound z)) 2))

; l1 u1 l2 u2 be 2 intervals
; width1 = u1 - l1/2 width2 = u2-l2/2
; sub-interval l1-l2 u1-u2
; width = u1-u2-l1+l2/2
; width = width1-width2
; similarly for addition
;
; multiplication
; (1,2) (5, 14) width1 = 3
; (2,4) (6,20) width2 = 4
; (2,8) (30,280) width = 9.7
;
; (1,2) (3,8) width1 = 3
; (2,4) (4,12) width2 = 4
; (2,8) (12,72) width = 6.4
;
; So not possible to write as function of width1 & width2

;2.10

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval 
          (min p1 p2 p3 p4)
          (max p1 p2 p3 p4))))

(define (div-interval x y) 
  (if (= (upper-bound y) (lower-bound y))
    (error "Span can't be zero")
    (mul-interval
      x
      (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y))))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))


;2.11 
;boring..

;2.12

(define (make-center-width c w) 
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c pt)
  (make-center-width c (* pt c 0.01)))
(define (percent i)
  (* 100 (/ (width i) (center i))))

;2.13

;interval1 (c1-w1,c1+w1)
;interval2 (c2-w2,c2+w2) 
;(considering all are positive)
;int1*int2 = ((c1*c2-c1*w2-w1*c2+w1*w2) , (c1*c2-c2*w2-w2*c2+w1*w2))
;= (c1*c2) * ((1-t1-t2+t1*t2) , (1+t1+t2+t1*t2))
;given tolerances are small t1*t2 = 0
;= (c1*c2) * ((1-t1-t2) * (1+t1+t2))

;so tolerance t is t1+t2

;2.14

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
        (div-interval
          one 
          (add-interval (div-interval one r1)
                        (div-interval one r2)))))

(define (print-interval r)
      (display (lower-bound r))
      (display " , ")
      (display (upper-bound r))
      (newline))

(print-interval (par2 (make-interval 1 2) (make-interval 1 3)))
(print-interval (par1 (make-interval 1 2) (make-interval 1 3)))

;2.15

;Yes par2 gives tighter bounds than par1 which can be verified using results in 2.14
;
;The reason for this is that in interval arthimetic when we use a variable more than
;once the uncertainity increases as the value can be anything in the interval by
;using it once we limit the uncertainity to mininum.


;2.16
;One way to go about this is to first reduce any equation to equation in which variables are not repeated but a universal algorithm for this is beyond my reach.


;2.17

(define (last-pair lst)
  (if (null? (cdr lst))
    (list (car lst))
    (last-pair (cdr lst))))

(last-pair (list 23 72 149 34))

;2.18

(define (reverse lst)
  (if (null? lst)
    ()
    (append (reverse (cdr lst)) (list (car lst)))))

(reverse (list 1 4 9 16 25))

;2.19

(define (cc amount coin-values) 
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0) 
        (else 
           (+ (cc amount
                  (except-first-denomination
                    coin-values))
              (cc (- amount
                     (first-denomination
                       coin-values))
                  coin-values)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (length items)
(define (length-iter a count)
  (if (null? a) count
            (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (no-more? lst)
   (= (length lst) 0))

(define (except-first-denomination kinds-of-coins)
  (cdr kinds-of-coins))
   
(define (first-denomination kinds-of-coins)
  (car kinds-of-coins))

(cc 100 us-coins)

;2.20

(define (square-list items) 
  (if (null? items)
    ()
    (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

(define (map proc items) 
  (if (null? items)
    ()
    (cons (proc (car items))
          (map proc (cdr items)))))


(square-list (list 1 2 3 4))

;2.21

(define (square-list items) 
  (define (iter things answer)
    (if (null? things) answer
      (iter (cdr things)
            (cons (square (car things))
                   answer ))))
  (iter items ()))

;This doesn't work the car is appended at the end so it gets reversed

(define (square-list items) 
  (define (iter things answer)
    (if (null? things) answer
      (iter (cdr things)
            (cons answer 
                  (square (car things))))))
  (iter items ()))

;In this case the order is correct but the output will not be a list

;2.23

(define (for-each x items)
  (if (null? items) 
    #f
    (if (= (car items) x)
        #t
        (for-each x (cdr items)))))

(for-each 322 (list 57 321 88))

;2.24

;2.25

(cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))

(car (car (list (list 7))))

(define big-tree
  (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(define (get7 tree f n)
  (if(= n 0)
    (f tree)
    (get7 (f tree) f (- n 1))))

(get7 big-tree (lambda (x) (car (cdr x)))  5)

;2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)

;2.27

(define x (list (list 1 (list 5 6)) (list 3 4)))

(define (deep-reverse tree) 
  (if (list? tree) 
    (reverse (map deep-reverse tree)) 
    tree)) 

(deep-reverse x)

;2.28

(define (fringe tree)
  (if (null? tree)
    ()
    (if (list? tree)
        (append (fringe (car tree)) (fringe (cdr tree)))
        (list tree))))

(fringe x)

;2.29

(define (make-mobile left right) 
  (list left right))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))

(define (make-branch length structure) 
  (list length structure))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))

(define (total-weight mobile)
  (let ((l (left-branch mobile))
        (r (right-branch mobile)))
    (define (get-weight branch)
      (if (null? branch)
        0
      (if (pair? branch)
        (+ (total-weight branch))
            branch)))
  (+ (get-weight l) (get-weight r))))


(total-weight (make-mobile (list 2 5) (list 2 5)))

;2.30

(define (square-tree tree) 
  (cond ((null? tree) ())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square-tree tree) 
  (map (lambda (sub-tree)
         (if (pair? sub-tree) 
           (square-tree sub-tree) 
           (square sub-tree)))
       tree))

;2.31

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map f sub-tree)
           (f sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))

;2.32

(define (subsets s) 
  (if (null? s)
    (list ())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (append (list (car s)) x)) rest)))))

(subsets (list 1 2 3))

;2.33

(define (accumulate op initial sequence) 
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (append seq1 seq2) (accumulate cons seq2 seq1))
(define (length sequence) (accumulate (lambda (x y) (+ y 1))  0 sequence))

;2.34

(define (horner-eval x coefficient-sequence) 
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;2.35

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (pair? x) 
                                     (count-leaves x) 
                                     1 )) 
                       t)))

(count-leaves (list 1 2 3))

;2.36

(define (accumulate-n op init seqs) 
  (if (null? (car seqs))
    ()
    (cons (accumulate op init (map (lambda (x) (car x)) seqs))
          (accumulate-n op init (map (lambda (x) (cdr x)) seqs) ))))


(define test-seq 
  (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(accumulate-n + 0 test-seq)

;2.37

(define (dot-product v w)
        (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v) 
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat) 
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n) 
  (let ((cols (transpose n)))
    (map (lambda (x) 
           (map lambda (y) 
                (dot-product x y) cols)) 
         m)))

(define test-mat
  (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(transpose test-mat)

;2.38

(define (fold-left op initial sequence) 
  (define (iter result rest)
    (if (null? rest) result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list () (list 1 2 3))
(fold-left list () (list 1 2 3))

;op should satisfy commutative propety 

;2.39

(define nil () )
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x) )) nil sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;2.40

(define (enumerate-interval low high) 
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
    (flatmap (lambda (i)
               (map (lambda (j)
                      (list i j))
                    (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))

(unique-pairs 10) 

(define (prime-sum-pairs n) 
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;2.41

(define (triples n s)
  (map (lambda (y) (make-triplet-sum y s))
       (filter (lambda (x) 
                 (sum-equals-neg-of-s? x s (enumerate-interval 1 n))) 
               (unique-pairs n))))

(define (sum-equals-neg-of-s? pair s n)
  (if (null? n)
    #f
    (if (= (+ (car pair) (cadr pair) (car n)) s)
      #t
      (sum-equals-neg-of-s? pair s (cdr n)))))

(define (make-triplet-sum pair s)
  (list (car pair) (cadr pair) (- s (+ (car pair) (cadr pair)))))

;TODO: very laborious solution, improve this...
(triples 10 5)

;2.42

(define (queens board-size) 
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board) (filter
                           (lambda (positions) (safe? k positions)) 
                           (flatmap
                             (lambda (rest-of-queens) 
                               (map (lambda (new-row) 
                                      (adjoin-position
                                        new-row k rest-of-queens))
                                    (enumerate-interval 1 board-size)))
                             (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
(cons new-row rest-of-queens))
(define empty-board ())

(define (safe? k positions)
  (define queen (car positions))
  (define (safe-iter up down positions)
    (if (null? positions)
      #t
      (let ((pos (car positions)))
      (if (or (= pos up)
              (= pos down)
              (= pos queen))
      #f
      (safe-iter (- up 1) (+ down 1) (cdr positions))))))
  (safe-iter (- queen 1) (+ queen 1) (cdr positions)))

(queens 6)

;2.43

(define (queens board-size) 
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board) (filter
                           (lambda (positions) (safe? k positions)) 
                           (flatmap
                             (lambda (new-row) 
                               (map (lambda (rest-of-queens) 
                                      (adjoin-position
                                        new-row k rest-of-queens))
                                    (queen-cols (- k 1))))
                               (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;Queen-cols will now be called for every item in enumerated interval of boardsize
;so the new time will be size^size * T

;2.44

(define (up-split painter n) 
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

;2.45

(define (split pos1 pos2)
  (lambda painter n)
  (if (= n 0)
    (let ((smaller ((split pos1 pos2) painter (- n 1))))
      (pos1 painter (pos2 smaller smaller)))))

(define right-split (split beside below)) 
(define up-split (split below beside))

;2.46

(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
 (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect v s)
 (make-vect (* (xcor-vect v1) s) (* (ycor-vect v1) s)))

;2.47

(define (make-frame origin edge1 edge2) 
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

(define (make-frame origin edge1 edge2) 
  (cons origin (cons edge1 edge2)))
;;same origin and edge1 procedures as above
(define (edge2-frame frame)
  (cddr frame))

;2.48

(define (make-segment v1 v2)
  (cons v1 v2))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

;2.49

(define (frame-coord-map frame) 
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (segments->painter segment-list) 
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame)
           (start-segment segment))
          ((frame-coord-map frame)
           (end-segment segment))))
      segment-list)))

;a
(define (outline frame)
(segments->painter (list (make-segment (make-vect 0 0) (make-vect 0 1))
                          (make-segment (make-vect 0 1) (make-vect 1 1))
                          (make-segment (make-vect 1 1) (make-vect 1 0))
                          (make-segment (make-vect 0 0) (make-vect 1 0))))
 frame)

;b
(define (cross frame)
(segments->painter (list (make-segment (make-vect 0 1) (make-vect 1 0))
                          (make-segment (make-vect 0 0) (make-vect 1 1))))
 frame)

;c
;similarly make midpoints

;d
;tedious and certainly boring...


;2.50

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate-counter180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate-counter270 painter)
(transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
;2.51

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below 
            (transform-painter
              painter1
              (make-vect 0.0 0.0)
              split-point
              (make-vect 1.0 0.0)))
          (paint-top
            (transform-painter
              painter2
              split-point
              (make-vect 1.0 0.5)
              (make-vect 0.0 1.0))))
      (lambda (frame) (paint-below frame) (paint-top frame)))))

(define (below painter1 painter2)
   (rotate-counter270 (beside (rotate90 painter1)  (rotate90 painter2) )))

;2.52

;b
(define (corner-split painter n) 
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1)))      
          (corner (corner-split painter (- n 1))))
        (beside (below up painter)
                (below corner right)))))
;c
(define (square-limit painter n)
    (let ((combine4 (square-of-four identity flip-horiz
                                    rotate90 rotate180)))
      (combine4 (corner-split painter n))))

;2.53

(define (memq item x) 
  (cond ((null? x) false)
        ((eq? item (car x)) x) 
        (else (memq item (cdr x)))))

;2.54

(define (equal? l1 l2)
  (cond ((and (not (pair? l1)) (not (pair?  l2))) 
         (eq?  l1 l2))  
        ((and (pair? l1)  (pair? l2)) 
         (and (equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2))))
        (else #f)))

(equal? '(this is a list) '(this is a list))

;2.55

;Interpreter must have interpreted as (car (quote (quote abracadabra)))
;this can be verified by seeing the result of (cadr ''abracadabra)

;2.56

(define (base exp)
  (cadr exp))

(define (make-exponentiation base exponent)
  (cond ((=number? base 0) 1)
        ((=number? base 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

(define (exponent exp)
  (caddr exp))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (deriv exp var) 
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0)) 
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
          ((exponentiation? exp) (make-product 
                                 (make-product 
                                   (exponent exp) 
                                   (make-exponentiation (base exp) (- (exponent n) 1)))
                                 (deriv (base exp) var)))

          (else
          (error "unknown expression type: DERIV" exp))))

;2.57

(define (augend s)
  (define (iter s)
    (if (null? s)
      0
      (+ (addend s) (augend s))))
  (iter (cadr s)))

(define (multiplicand p) 
  (define (iter p)
    (if (null? p)
      1
      (* (multiplier p) (multiplicand p))))
  (iter (cadr p)))

;2.58
;a

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))
(define (augend s) (caddr s))

(define (product? x)
    (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

;2.59
(define (element-of-set? x set) 
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
  (define (set1-not-in-set2 set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
                          (set1-not-in-set2 (cdr set1) set2))
                (else (cons (car set1) (set1-not-in-set2 (cdr set1) set2)))))
   (set1-not-in-set2 set1 set2))

;2.60

(define (element-of-set? x set) 
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) 
    (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cons set1 set2))

;Performance of adjoin and union operations would be better but intersection and
;element-of-set? procedures would be inefficient expecially if the size grows 

;2.61

(define (adjoin-set x set)
  (define (insert first-part set)
  (cond ((null? set) x)
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (insert (cons first-part (car set)) (cdr set)))))
  (insert '() set))

;2.62

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2)) (cons 
                                   (car set1) 
                                   (union-set (cdr set1) set2)))
        ((> (car set1) (car set2)) (cons 
                                   (car set2) 
                                   (union-set set1 (cdr set2))))
        (else (cons (car set1) (union-set (cdr set1) (cdr set2))))))

;;Binary Trees
;2.63

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
    (list entry left right))

(define (tree->list-1 tree)
    (if (null? tree)
            '()
            (append (tree->list-1 (left-branch tree))
                    (cons (entry tree)
                          (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
          (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                          (cons (entry tree)
                                (copy-to-list (right-branch tree)
                                              result-list)))))
    (copy-to-list tree '()))

;Both produces same result
;
;Complexity of 1st version is O(nlogn) 
;T(n) = 2*T(n/2) + O(n) 
;append procedure has to traverse the left list which takes O(n)
;of 2nd version is O(n)
;T(n)  = 2*T(n/2) + O(1) here we just cons the entry constant time

;2.64

(define (list->tree elements)
    (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
    (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(list->tree (list 1 3 5 7 9 11))

;complexity O(n)

;2.65

(define (union-set-tree tree1 tree2)
  (let ((union-list (union-set (tree->list-2 tree1) (tree->list-2 tree2))))
    (list->tree union-list)))

(define (intersection-set-tree set1 set2)
    (let ((intersection-list (intersection-set (tree->list set1) (tree->list set2))))
      (list->tree intersection-list)))

;2.66

(define (lookup given-key set-of-records)
    (if (null? set-of-records)
            false
              (let ((k (key (entry (set-of-records)))))
                (cond ((= given-key k) (entry (set-of-records))
                      ((< given-key k)
                       (lookup given-key (left-branch set-of-records)))
                      (else
                        (lookup given-key (right-branch set-of-records))))))))

;2.67

(define (make-leaf symbol weight)
    (list 'leaf symbol weight))
(define (leaf? object)
    (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
    (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
    (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
    (define (decode-1 bits current-branch)
          (if (null? bits)
            '()
            (let ((next-branch
                    (choose-branch (car bits) current-branch)))
              (if (leaf? next-branch)
                (cons (symbol-leaf next-branch)
                      (decode-1 (cdr bits) tree))
                (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

(define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad-bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                      (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
    (if (null? pairs)
            '()
            (let ((pair (car pairs)))
              (adjoin-set (make-leaf (car pair)
                                     (cadr pair))
                          (make-leaf-set (cdr pairs))))))
(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                      (make-leaf 'B 2)
                      (make-code-tree (make-leaf 'D 1)
                                      (make-leaf 'C 1)))))
(define sample-message '( 0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

;2.68
    
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (has-symbol? symbol symbol-list)
    (not (equal? #f (memq symbol symbol-list))))
  (if (has-symbol? symbol (symbols tree))
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (if (has-symbol? symbol (symbols left))
          (if (leaf? left)
            '(0)
            (cons 0 (encode-symbol symbol left)))
          (if (leaf? right)
            '(1)
            (cons 1 (encode-symbol symbol right)))))
      (error "Symbol not found")))

(encode '(a d a b b c a) sample-tree)

;2.69

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                      (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
    (if (null? pairs)
            '()
            (let ((pair (car pairs)))
              (adjoin-set (make-leaf (car pair)
                                     (cadr pair))
                          (make-leaf-set (cdr pairs))))))

(car (make-leaf-set '((A 4) (B 2) (C 5) (D 1))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge tree)
  (if (null? (cdr tree))
    (car tree)
    (successive-merge 
      (adjoin-set (make-code-tree (car tree) (cadr tree)) 
                     (cddr tree)))))

(generate-huffman-tree '((A 4) (B 2) (C 5) (D 1)))






