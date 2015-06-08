;;; http://www.luschny.de/math/factorial/binarysplitfact.html

(define (bit-length n)
  "Returns the number of bits necessary to represent an integer in binary,
   excluding the sign and leading zeros."
  (string-length (number->string n 2)))

(define (count-set-bits n)
  ""
  (let loop ((count 0)
             (n n))
    (if (zero? n) count
        (loop (+ count 1) (logand n (- n 1))))))

(define (partial-product start stop)
  "Product of integers, start and stop should both be odd, with start <= stop."
  ((lambda (i)
     (cond ((< stop start) 1)
           ((even? i)
            (let loop ((n (* start stop))
                       (r (- (* 4 i) 8))
                       (f 1))
              (if (< r 0) f
                  (loop (+ n r) (- r 8) (* f n)))))
           (else (* stop (partial-product start (- stop 2))))))
   (+ 1 (/ (- stop start) 2))))

(define (inner-number n i)
  ""
  (logior (+ (floor (/ n (integer-expt 2 i))) 1) 1))

(define (binsplit-factorial n)
  "Factorial of nonnegative integer n, via binary split."
  (let loop ((i (bit-length n))
             (inner 1) (outer 1)
             (a 1) (b 1))
    (if (< i -1) (* outer (integer-expt 2 (- n (count-set-bits n))))
        (let ((inner (* inner (partial-product a (- b 2)))))
          (loop (- i 1)
                inner
                (* outer inner)
                (inner-number n (+ i 1))
                (inner-number n i))))))
