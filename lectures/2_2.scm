(load "1_9.scm")

(define (((M-of-q->omega-of-t M-of-q) q) t)
  (define M-on-path (compose M-of-q q))
  (define (omega-cross t)
    (* ((D M-on-path) t)
       (transpose (M-on-path t))))
  (antisymmetric->column-matrix (omega-cross t)))

(define (((M-of-q->omega-body-of-t M-of-q) q) t)
  (* (transpose (M-of-q (q t)))
     (((M-of-q->omega-of-t M-of-q) q) t)))

(define (M->omega M-of-q)
  (Gamma-bar
    (M-of-q->omega-of-t M-of-q)))

(define (M->omega-body M-of-q)
  (Gamma-bar
    (M-of-q->omega-body-of-t M-of-q)))
