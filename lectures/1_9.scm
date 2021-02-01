(load "1_8.scm")

(define ((Gamma-bar f-bar) local)
  ((f-bar (osculating-path local)) (time local)))

(define (F->C F)
  (define (C local)
    (let ((n (vector-length local)))
      (define (f-bar q-prime)
        (define q
          (compose F (Gamma q-prime)))
        (Gamma q n))
      ((Gamma-bar f-bar) local)))
  C)

(define (Dt F)
  (define (DtF state)
    (let ((n (vector-length state)))
      (define (DF-on-path q)
        (D (compose F (Gamma q (- n 1)))))
      ((Gamma-bar DF-on-path) state)))
  DtF)

(define (Euler-Lagrange-operator L)
  (- (Dt ((partial 2) L)) ((partial 1) L)))
