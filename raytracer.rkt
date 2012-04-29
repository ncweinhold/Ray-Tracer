#lang racket

(define (square x)
  (* x x))

(define (mag x y z)
  (sqrt (+ (square x)
           (square y)
           (square z))))

(define (unit-vector x y z)
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))

;;;; The code in ANSI Common Lisp makes use of the minusp predicate.
;;;; Unable to find a built-in equivalent in Racket, so making a
;;;; helper function to do the same job.
(define (minus? x)
  (< x 0))

(struct point (x y z))

(define (distance p1 p2)
  (mag (- (point-x p1) (point-x p2))
       (- (point-y p1) (point-y p2))
       (- (point-z p1) (point-z p2))))

(define (minroot a b c)
  (if (zero? a)
      (/ (- c) b)
      (let ((disc (- (square b) (* 4 a c))))
        (unless (minus? disc)
          (let ((discrt (sqrt disc)))
            (min (/ (+ (- b) discrt) (* 2 a))
                 (/ (- (- b) discrt) (* 2 a))))))))

(struct surface (color))

(define world (make-vector 3))
(define *eye* (point 0 0 200))

(define (tracer pathname [res 1])
  (with-output-to-file pathname
    (lambda () 
      (printf "P2 ~A ~A 255\n" (* res 100) (* res 100))
      (let ((inc (/ res)))
        (do ([y -50 (+ y inc)])
          [(< (- 50 y) inc)]
          (do ([x -50 (+ x inc)])
            [(< (- 50 x) inc)]
            (printf "~A\n" (color-at x y))))))))

(define (color-at x y)
  (define-values (xr yr zr)
    (unit-vector (- x (point-x *eye*))
                 (- y (point-y *eye*))
                 (- 0 (point-z *eye*))))
  (round (* (sendray *eye* xr yr zr) 255)))

(define (sendray pt xr yr zr)
  (define-values (s int) (first-hit pt xr yr zr))
  (if (surface? s)
      (* (lambert s int xr yr zr) (surface-color s))
      0))

(define (first-hit pt xr yr zr)
  (let ((surface '())
        (hit '())
        (dist '()))
    (for-each (lambda (s)
                (let ((h (intersect s pt xr yr zr)))
                  (unless (null? h)
                    (let ((d (distance h pt)))
                      (when (or (null? dist) (< d dist))
                        (set!-values (surface hit dist) (values s h d)))))))
              (vector->list world))
    (values surface hit)))

(define (lambert s int xr yr zr)
  (define-values (xn yn zn) (normal s int))
  (max 0 (+ (* xr xn) (* yr yn) (* zr zn))))

(define (intersect s pt xr yr zr)
  (cond
    ((sphere? s) (sphere-intersect s pt xr yr zr))))

(define (sphere-intersect s pt xr yr zr)
  (let* ((c (sphere-center s))
         (n (minroot (+ (square xr) (square yr) (square zr))
                     (* 2 (+ (* (- (point-x pt) (point-x c)) xr)
                             (* (- (point-y pt) (point-y c)) yr)
                             (* (- (point-z pt) (point-z c)) zr)))
                     (+ (square (- (point-x pt) (point-x c)))
                        (square (- (point-y pt) (point-y c)))
                        (square (- (point-z pt) (point-z c)))
                        (- (square (sphere-radius s)))))))
    (if (not (void? n))
        (point (+ (point-x pt) (* n xr))
               (+ (point-y pt) (* n yr))
               (+ (point-z pt) (* n zr)))
        null)))

(define (normal s pt)
  (cond
    ((sphere? s) (sphere-normal s pt))))

(define (sphere-normal s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (point-x c) (point-x pt))
                 (- (point-y c) (point-y pt))
                 (- (point-z c) (point-z pt)))))

(struct sphere surface (radius center))

(define (defsphere x y z r col pos)
  (let ((s (sphere col r (point x y z))))
    (vector-set! world pos s)))

(define (ray-test [res 2])
  (defsphere 0 -300 -1200 200 0.8 0)
  (defsphere -80 -150 -1200 200 0.7 1)
  (defsphere 70 -100 -1200 200 0.9 2)
  (tracer "racket-spheres.pgm" res))