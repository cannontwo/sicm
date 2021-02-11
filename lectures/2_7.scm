(load "2_2.scm")

(define ((T-body A B C) omega-body)
  (* 1/2
     (+ (* A (square (ref omega-body 0)))
        (* B (square (ref omega-body 1)))
        (* C (square (ref omega-body 2))))))

(define ((L-body A B C) omega-body)
  (down (* A (ref omega-body 0))
        (* B (ref omega-body 1))
        (* C (ref omega-body 2))))

(define (Rz-matrix angle)
  (matrix-by-rows
    (list (cos angle) (- (sin angle)) 0)
    (list (sin angle) (cos angle) 0)
    (list 0 0 1)))

(define (Rx-matrix angle)
  (matrix-by-rows
    (list 1 0 0)
    (list 0 (cos angle) (- (sin angle)))
    (list 0 (sin angle) (cos angle))))

(define (Euler->M angles)
  (let ((theta (ref angles 0))
        (phi (ref angles 1))
        (psi (ref angles 2)))
    (* (Rz-matrix phi)
       (Rx-matrix theta)
       (Rz-matrix psi))))

(define (Euler-state->omega-body local)
  (let ((q (coordinate local)) (qdot (velocity local)))
    (let ((theta (ref q 0))
          (psi (ref q 2))
          (thetadot (ref qdot 0))
          (phidot (ref qdot 1))
          (psidot (ref qdot 2)))
      (let ((omega-a (+ (* thetadot (cos psi))
                        (* phidot (sin theta) (sin psi))))
            (omega-b (+ (* -1 thetadot (sin psi))
                        (* phidot (sin theta) (cos psi))))
            (omega-c (+ (* phidot (cos theta)) psidot)))
        (up omega-a omega-b omega-c)))))

(define ((T-body-Euler A B C) local)
  ((T-body A B C)
   (Euler-state->omega-body local)))

(define ((L-body-Euler A B C) local)
  ((L-body A B C)
   (Euler-state->omega-body local)))

(define ((L-space-Euler A B C) local)
  (let ((angles (coordinate local)))
    (* ((L-body-Euler A B C) local)
       (transpose (Euler->M angles)))))
