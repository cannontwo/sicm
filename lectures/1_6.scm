(load "1_5.scm")

(define ((L-central-rectangular m U) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (U (sqrt (square q))))))

(define ((L-central-polar m U) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((r (ref q 0)) (phi (ref q 1))
          (rdot (ref qdot 0)) (phidot (ref qdot 1)))
      (- (* 1/2 m
            (+ (square rdot)
               (square (* r phidot))))
         (U r)))))

(define ((F->C F) local)
  (up (time local)
      (F local)
      (+ (((partial 0) F) local)
         (* (((partial 1) F) local)
            (velocity local)))))

(define (p->r local)
  (let ((polar-tuple (coordinate local)))
    (let ((r (ref polar-tuple 0))
          (phi (ref polar-tuple 1)))
      (let ((x (* r (cos phi)))
            (y (* r (sin phi))))
        (up x y)))))

(define ((L-free-rectangular m) local)
  (let ((vx (ref (velocities local) 0))
        (vy (ref (velocities local) 1)))
    (* 1/2 m (+ (square vx) (square vy)))))

(define (L-free-polar m)
  (compose (L-free-rectangular m) (F->C p->r)))

(define ((rotating-F Omega) local)
  (let ((t (time local))
        (r (ref (coordinates local) 0))
        (theta (ref (coordinates local) 1)))
    (up r (+ theta (* Omega t)))))

(define (L-rotating-polar m Omega)
  (compose (L-free-polar m) (F->C (rotating-F Omega))))

(define (L-rotating-rectangular m Omega)
  (compose (L-rotating-polar m Omega) (F->C r->p)))

(define ((T-pend m l g ys) local)
  (let ((t (time local))
        (theta (coordinate local))
        (thetadot (velocity local)))
    (let ((vys (D ys)))
      (* 1/2 m
         (+ (square (* l thetadot))
            (square (vys t))
            (* 2 l (vys t) thetadot (sin theta)))))))

(define ((V-pend m l g ys) local)
  (let ((t (time local))
        (theta (coordinate local)))
    (* m g (- (ys t) (* l (cos theta))))))

(define L-pend (- T-pend V-pend))

