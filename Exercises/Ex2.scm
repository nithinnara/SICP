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
    

 
