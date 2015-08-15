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
                   (/ 1.0 (lower-bound y)))))


    
