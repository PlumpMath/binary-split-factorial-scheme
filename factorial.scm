(define (bit-length n)
  ""
  (string-length (number->string n 2)))

(define (count-set-bits n)
  ""
  (let loop ((count 0)
             (n n))
    (if (zero? n) count
        (loop (+ count 1) (logand n (- n 1))))))

(define (partial-product start stop)
  ""
  ((lambda (i)
     (if (even? i)
         (let loop ((n (* start stop))
                    (r (- (* 4 i) 8))
                    (f 1))
           (if (< r 0) f
               (loop (+ n r) (- r 8) (* f n))))
         (* stop (partial-product start (- stop 2)))))
   (+ 1 (/ (- stop start) 2))))

(define (inner-number n i)
  ""
  (logior (+ (floor (/ n (integer-expt 2 i))) 1) 1))

(define (binsplit-factorial n)
  ""
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
