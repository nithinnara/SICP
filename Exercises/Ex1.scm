;1.11
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

;1.12

(define (pascal row col)
  (cond ((= col 1) 1) 
        ((= row col) 1) 
        (else (+ (pascal (- row 1) (- col 1)) 
                 (pascal (- row 1) col))))) 

;1.13

;1.14
;; theta(n^k) in time and theta(n) in space

;1.15
;; a. 5
;; b. order = O(log a) , f(a) = (round (\ (log (* a 10) (log 3)))) + 1

;1.16

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

;1.17

(define (double a)
  (+ a a))

(define (halve a)
   (/ a 2))

(define (fast-* a b)
    (cond ((= b 0) 0)
      ((even? b) (double (fast-* a (halve b))))
      (else (+ a (fast-* a (- b 1))))))

;1.18

(define (fast-* a b)
  (define (fast-iter-* a result count)
    (if (= count 0) result
    (if (even? count)
    (fast-iter-* (double a) result (halve count))
    (fast-iter-* a (+ a result) (- count 1)))))
  (fast-iter-* a 0 b))

;1.19

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

;1.20
;; Applicative order 4 (straight forward)
;; Normal order 18

;1.21

(define (smallest-divisor n) 
  (find-divisor n 2))

(define (find-divisor n test-divisor) 
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1))))) 

(define (divides? a b) (= (remainder b a) 0))

;1.22

(define (timed-prime-test n) 
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time) 
  (if (prime? n)
    (report-prime (- (runtime) start-time)))) 
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes start end time count)
(if (or (= count 0) (> start end) (= start end))
  (report-prime time)
  (if (even? start)
    (search-for-primes (+ start 1) end (+ time (- (runtime) time)) count)
    (if (prime? start)
      (search-for-primes (+ start 2) end (+ time (- (runtime) time)) (- count 1)) 
      (search-for-primes (+ start 2) end (+ time (- (runtime) time)) count)))))

(search-for-primes 1000 10000 (runtime) 3)
(search-for-primes 100000001 1000000000 (runtime) 3)

;1.23

(define (next a)
  (if (= a 2)
    3
    (+ a 1)))

(define (find-divisor n test-divisor) 
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(search-for-primes 100000001 1000000000 (runtime) 3)

;1.24

(define (expmod base exp m) 
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
           (square (expmod base (/ exp 2) m))
           m)) 
        (else
          (remainder 
            (* base (expmod base (- exp 1) m)) 
            m))))
(define (expmod base exp m) (remainder (fast-expt base exp) m))

(define (fermat-test n) 
  (define (try-it a)
    (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times) 
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1))) 
        (else false)))

;1.27

(define (is-prime? n)
  (define (loop n upperlimit)
  (cond ((= upperlimit 0) true)
        ((fermat-test-complete-iter (- upperlimit 1) n) (loop n (- upperlimit 1) ))
        (else false)))
  (loop n n))

(define (fermat-test-complete-iter a n)
    (= (expmod a n n) a))

;(is-prime? 561)
;1105, 1729, 2465, 2821, and 6601 are other Carmicheal numbers

;1.28

(define (expmod-miller base exp m) 
  
  (define (nontrivial-squareroot? num m)
    (if (and (not (or 
          (= num 1) (= num (- m 1))))
      (= (remainder (square num) m) 1))
    0
    m))
    
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
           (square (nontrivial-squareroot? (expmod-miller base (/ exp 2) m) m))
           m)) 
        (else
          (remainder 
            (* base (expmod-miller base (- exp 1) m)) 
            m))))

(define (miller-test a n) 
    (= (expmod-miller a (- n 1) n) a))

(define (miller-fast-prime? n upperlimit) 
  (cond ((= upperlimit 0) true)
        ((miller-test (- upperlimit 1) n) (miller-fast-prime? n (- upperlimit 1) ))
        (else false)))

;(miller-fast-prime? 561 561)

;1.29

(define (sum term a next b) 
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (simpson-integral f a b n)
  (define geth
     (/ (- b a) n))
  
  (define (next x)
    (+ x 1))
  
  (define (factor k)
    (f (+ a (* k geth))))
  
  (define (term k)
  (* (cond ((= k 0) 1)
        ((= k n) 1)
        ((even? k) 2)
        (else 4))
  (factor k)))
    
  (* (/ geth 3) (sum term 0 next n)))

(define (cube x)
  (* x x x))

(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000)

;1.30

(define (sum term a next b) 
  (define (iter a result)             
    (if (> a b)
      result
      (iter (next a) (+ result (term a))))) 
  (iter a 0))

;1.31

(define (product term a next b)
  (define (iter a result)
  (if (> a b)
    result
    (iter (next a) (* result (term a)))))
  (iter a 1))

(define (inc x)
  (+ x 1))

(define (identity x)
x)

(define (factorial n)
  (product identity 1 inc n))

(define (compute-pi n)
 (define (term k)
    (if (even? k)
      (/ (+ k 2) (+ k 1))
      (/ (+ k 1) (+ k 2))))

  (* 4 (product-rec term 1 inc n) ))

(compute-pi 10)

(define (product-rec term a next b) 
  (if (> a b)
    1
    (* (term a)
       (product-rec term (next a) next b))))

;1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
  (if (> a b)
    null-value
    (iter (next a) (combiner (term a) result))))
  (iter a null-value))

; (accumulate + 0 term a next b) sum
; (accumulate * 1 term a next b) product

;1.33

(define (filtered-accumulate combiner null-value term a next b filterVal)
  (if (> a b)
    null-value
    (if (filterVal a)
      (combiner (term a) 
                (filtered-accumulate combiner null-value term (next a) next b filterVal))
      (combiner null-value (filtered-accumulate combiner null-value term (next a) next b filterVal))
                )))

(define (sum-of-squares-prime a b)
  (filtered-accumulate + 0 square a inc b is-prime?))

(define (gcd a b) 
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (product-rel-primes a b)
  (define (is-rel-prime? x)
    (= 1 (gcd x b)))
  (filtered-accumulate * 1 identity a inc b is-rel-prime?))

;1.34

; (f f) -> (f 2) -> (2 2) the interpreter can't resolve 2 as it is not a func

;1.35

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) 
    (< (abs (- v1 v2))
       tolerance)) 
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define golden-ratio 
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

;1.36

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
  (let ((next (f guess)))
    (newline)
    (display next)
    (if (close-enough? guess next)
      next
      (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(fixed-point (lambda (x) (/ (log 1000) (log x)))
             10)
(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
             10)
;Using average dumping almost halves the no of steps

;1.37

(define (cont-frac n d k)
    (if (< k 1)
      0
      (/ (n k) (+ (d k) (cont-frac n d  (- k 1))))))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 11)

(define (cont-frac-iter n d k)
  (define (iter k res)
      (if (= 0 k)
        res
        (iter (- k 1) (/ (n k) (+ (d k) res)))))
  (iter k 0))

; k= 11 gives accurate value till 4 decimals

;1.38

(define (d k)
  (if (< k 3) 
    k
    (if (= (remainder (- k 2) 3) 0) 
      (* 2 (+ 1 (/ (- k 2) 3)))
      1)))

(cont-frac (lambda (i) 1.0) d 100)

;1.39

(define (tan-cf x k)
    (define (n i)
          (if (= 1 i)
                    x
                    (- (square x))))
      (define (d i)
            (- (* 2 i) 1))
      (cont-frac-iter n d k))

;1.40

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess) (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))  

;1.41

;21

;1.42

(define (compose f g x)
  (f (g x)))

;1.43

(define (repeated f n)
  (define (iter count)
    (if (= count 1)
      f
      (lambda (x) (f ((iter (- count 1)) x)))))
  (iter n))

((repeated square 2) 5)

;1.44

(define (smooth f)
  (lambda (x) (/ 3 (+ (f x) (f (- x dx)) (f (+ x dx))))))

((repeated (smooth square) 2) 2)

;1.45

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point-of-transform g transform guess) 
  (fixed-point (transform g) guess))

(define (nth-root x n) (fixed-point-of-transform
                   (lambda (y) (/ x pow(y (- n 1)))) (repeated average-damp n) 1.0)
;assuming pow as a primitive type y^n-1

;1.46

(define (iterative-improve guess-good? improve-guess)
    (lambda (guess) (if(guess-good? guess)
                      guess
                      (guess-good? (improve-guess guess)))))

(define (sqrt x) 
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess) (average guess (/ x guess)))
  (((iterative-improve good-enough? improve-guess) 1.0)))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)) 
  (define (good-enough? 
            (lambda (x) (close-enough? x (f x)))))
   ((iterative-improve good-enough? f) first-guess))

;***************** End of Chapter 1 *******************





























