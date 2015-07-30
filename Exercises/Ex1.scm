#1.11
(define (f n) 
  (if (< n 3) 
    n
    (+ (f (- n 1)) 
       (* 2 (f (- n 2))) 
       (* 3 (f (- n 3))))))

(define (f n)
    (define (f-iter a b c count) 
        (if (< count 3)
        a
        (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
    (if (< n 3)
        n
        (f-iter 2 1 0 n)))

#1.12

(define (pascal row col)
  (cond ((= col 1) 1) 
        ((= row col) 1) 
        (else (+ (pascal (- row 1) (- col 1)) 
                 (pascal (- row 1) col))))) 

#1.13

#1.14
;; Its a linear recursive process so theta(n) in time and theta(n) in space

#1.15
;; a. 5
;; b. order = O(log a) , f(a) = (round (\ (log (* a 10) (log 3)))) + 1

#1.16

(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))

(define (fast-expt b n)
    (define (fast-expt-iter b result count)
      (if (= count 0) result
      (if (even? count)
        (fast-expt-iter (square b) result (/ count 2))
        (fast-expt-iter b (* b result) (- count 1)))))
    (fast-expt-iter b 1 n))

#1.17

(define (double a)
  (+ a a))

(define (halve a)
   (/ a 2))

(define (fast-* a b)
    (cond ((= b 0) 0)
      ((even? b) (double (fast-* a (halve b))))
      (else (+ a (fast-* a (- b 1))))))

#1.18

(define (fast-* a b)
  (define (fast-iter-* a result count)
    (if (= count 0) result
    (if (even? count)
    (fast-iter-* (double a) result (halve count))
    (fast-iter-* a (+ a result) (- count 1)))))
  (fast-iter-* a 0 b))

#1.19

;; q' = (+ (* 2 (* p q)) (* q q))
;; p' = (+ (* p p) (* q q))

(define (fib n) (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count) 
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a 
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 (* p q)) (* q q))
                   (/ count 2))) 
        (else (fib-iter (+ (* b q) (* a q) (* a p)) 
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
