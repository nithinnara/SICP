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



