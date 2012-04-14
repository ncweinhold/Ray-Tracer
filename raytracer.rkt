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

(define *world* '())
(define *eye* (point 0 0 200))

(define (tracer pathname)
  'TODO)

(define (color-at x y)
  'TODO)

(define (sendray pt xr yr zr)
  'TODO)

(define (first-hit pt xr yr zr)
  'TODO)

(define (lambert s int xr yr zr)
  'TODO)