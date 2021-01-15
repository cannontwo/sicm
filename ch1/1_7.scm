(load "1_6.scm")

(define (Lagrangian->acceleration L)
  (let ((P ((partial 2) L)) (F ((partial 1) L)))
    (solve-linear-left
      ((partial 2) P)
      (- F
         (+ ((partial 0) P)
            (* ((partial 1) P) velocity))))))

(define (Lagrangian->state-derivative L)
  (let ((acceleration (Lagrangian->acceleration L)))
    (lambda (state)
      (up 1
          (velocity state)
          (acceleration state)))))

(define (harmonic-state-derivative m k)
  (Lagrangian->state-derivative (L-harmonic m k)))

(define ((Lagrange-equations-first-order L) q v)
  (let ((state-path (qv->state-path q v)))
    (- (D state-path)
       (compose (Lagrangian->state-derivative L)
                state-path))))

(define ((qv->state-path q v) t)
  (up t (q t) (v t)))

(define ((periodic-drive amplitude frequency phase) t)
  (* amplitude (cos (+ (* frequency t) phase))))

(define (L-periodically-driven-pendulum m l g A omega)
  (let ((ys (periodic-drive A omega 0)))
    (L-pend m l g ys)))

(define (pend-state-derivative m l g A omega)
  (Lagrangian->state-derivative
    (L-periodically-driven-pendulum m l g A omega)))

(define ((monitor-theta win) state)
  (let ((theta ((principal-value :pi) (coordinate state))))
    (plot-point win (time state) theta)))
