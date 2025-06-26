(import numpy :as np)

(defn draw-line [canvas start-x start-y end-x end-y width]
  (let [dx (- end-x start-x)
        dy (- end-y start-y)
        l2 (+ (* dx dx) (* dy dy)) ;length squared
        half-width (int (/ width 2.0))
        thresh-sq (* half-width half-width)
        min-x (max 0 (- (min start-x end-x) half-width))
        max-x (min 149 (+ (max start-x end-x) half-width))
        min-y (max 0 (- (min start-y end-y) half-width))
        max-y (min 149 (+ (max start-y end-y) half-width))]
    (for [x (range min-x (+ max-x 1))]
      (for [y (range min-y (+ max-y 1))]
        (let [dist-sq (if (= 0 l2) ;point case
                         (let [dx (- x start-x)
                               dy (- y start-y)]
                           (+ (* dx dx) (* dy dy)))
                         (let [
			       vx (- x start-x) ;vector from start
			       vy (- y start-y) ;vector from start
			       ;; Projection parameter
			       t (max 0 (min 1 (/ (+ (* vx dx) (* vy dy)) l2)))
                               proj-x (+ start-x (* t dx))
                               proj-y (+ start-y (* t dy))
                               dist-x (- x proj-x)
                               dist-y (- y proj-y)]
                           (+ (* dist-x dist-x) (* dist-y dist-y))))]
          (when (<= dist-sq thresh-sq)
            (setv (get canvas (tuple [y x])) 0))))))
  canvas)
