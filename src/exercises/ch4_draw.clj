(def size (Math/pow 2 8))
(def frame (java.awt.Frame.))
(.setVisible frame true)
(.setSize frame (java.awt.Dimension. size size))

(def gfx (.getGraphics frame))

(defn clear [g] (.clearRect g 0 0 (.width (.size frame)) (.height (.size frame))))

(defn f-values [f xs ys]
  (for [x (range xs) y (range ys)]
    [x y (rem (f x y) 256)]))

(defn draw-values [f xs ys]
  (clear gfx)
  (doseq [[x y v] (f-values f xs ys)]
    (.setColor gfx (java.awt.Color. v v v))
    (.fillRect gfx x y 1 1)))


(draw-values * size size)

;(.dispose frame)