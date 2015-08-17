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
  (if (= (car items) x)
    #t
    (for-each x (cdr items))))

(for-each 321 (list 57 321 88))






