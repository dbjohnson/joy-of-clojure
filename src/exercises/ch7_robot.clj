(ns exercises.ch7-robot
  (:require [dbj.pq :refer :all]))


(def headings
  {:north {:x 0,  :y -1  :sprite "▲", :left :west,  :right :east},
   :east  {:x 1,  :y 0,  :sprite "▶", :left :north, :right :south},
   :south {:x 0,  :y 1,  :sprite "▼", :left :east,  :right :west},
   :west  {:x -1, :y 0,  :sprite "◀", :left :south, :right :north}})


(defn bot
  ([x y] (bot x y :north))
  ([x y heading]
    {:coords   {:x x, :y y, :heading heading}
     :forward (fn [] (bot (+ x (:x (heading headings)))
                       (+ y (:y (heading headings)))
                       heading))
     :turn    (fn [direction] (bot x y (direction (heading headings))))}))


(defn- rand-binary-map
  "Creates a square binary map size n.  Optional "
  [n & rest]
  (let [density (or (first rest) 5)]
    (vec (for [i (range n)]
           (vec (for [j (range n)]
                  (if (zero? (rand-int density)) 1 0)))))))


(defn prn-world
  "Print world, including goal and robot state"
  [w]
  (let [state (:coords (:bot w))
        m (assoc-in (:map w)
            [(:y state) (:x state)]
            (:sprite (headings (:heading state))))
        m (assoc-in m [(:y (:goal w)) (:x (:goal w))] "x")]
    (doseq [r m]
      (doseq [c r]
        (print (str (or ({0 " ", 1 "▢"} c) c) " ")))
      (println))))


(defn- rand-open-pos
  "Selects a random open position on the map.  For initializing random
   locations for the goal and robot"
  [m]
  (loop []
    (let [x (rand-int (count m))
          y (rand-int (count m))]
      (if (not= 0 (get-in m [y x]))
        (recur)
        {:x x :y y}))))


(defn- rand-robot
  "Put that robot somewhere random"
  [m]
  (let [heading (rand-nth (keys headings))
        pos (rand-open-pos m)]
    (bot (:x pos) (:y pos) heading)))


(defn create-world
  "Create a world of size n, including randomized goal
   and initial robot location"
  [n]
  (let [m (rand-binary-map n)]
    {:map m
     :bot (rand-robot m)
     :goal (rand-open-pos m)
     :size n}))


(defn- city-block-dist
  "Calculate city block distance between two points.  Serves as the
   heuristic function h(n) for A*"
  [p1 p2]
  (+
    (Math/abs (- (:x p1) (:x p2)))
    (Math/abs (- (:y p1) (:y p2)))))



(defn- path-cost
  "Calculate total estimated path cost, including heuristic to goal"
  [p g]
  (+ (count p) (city-block-dist (:coords (last p)) g)))


(defn- neighbors
  "Return valid neighbors for pathent state.
   Does not return blocked positions."
  [w]
  (let [b (:bot w)
        n (:size w)]
    (filter
      (fn [bb]
        (let [x (:x (:coords bb))
              y (:y (:coords bb))]
          (and
            (every? #(< -1 % n) [x y])
            (= 0 (get-in (:map w) [y x])))))
      [((:forward b))
       ((:turn b) :right)
       ((:turn b) :left)])))


(defn- prn-path
  "Draws path over the map before sending on to prn-world"
  [p w]
  (loop [m (:map w) s p]
    (if (seq s)
      (let [step (:coords (first s))
            mm   (assoc-in
                   m
                   [(:y step) (:x step)]
                   (:sprite (headings (:heading step))))]
        (recur mm (next s)))
      (prn-world (assoc w :map m)))))


(defn- path->node
  "Create a priority queue node for the path"
  [p w]
  (pqpush p (path-cost p (:goal w)) nil))


(defn Astar
  "Find the optimal path between robot and goal"
  [w]
  (loop [pqpaths (path->node [(:bot w)] w)
         best   nil
         cnt    0]
    (if (pqpeek pqpaths)
      (let [path   (pqpeek pqpaths)
            ; TODO: this 'last' is probably very expensive
            pos    (:coords (last path))]

        (if (and (= (:x (:goal w)) (:x pos))
                 (= (:y (:goal w)) (:y pos)))
          (prn-path path w)
          (let [pqrest  (pqpop pqpaths)
                ; only accept steps that result in states we haven't been
                ; in before
                visited (set (for [bot path] (:coords bot)))
                steps   (filter
                          (fn [step] (nil? (visited (:coords step))))
                          (neighbors (assoc w :bot
                                       (bot (:x pos)
                                            (:y pos)
                                            (:heading pos)))))]
            (if (seq steps)
              (let [npaths  (map #(conj path %) steps)
                    qnew    (reduce pqpush
                              (map #(path->node % w) npaths))]
                (recur (pqpush qnew pqrest) best (inc cnt)))
                (recur pqrest best (inc cnt))))))

      (do
        (prn "No solution found!
              (could be legitimate, i.e., goal or bot boxed in)")
        (prn (:map w) (:coords (:bot w)))))))

; Let 'er rip on a random map
(def w (create-world 12))()
(prn-world w)
; Print the world definition in case the result is interesting and we want to
; repeat it.
; TODO: figure out how to set the random seed
(prn
  (:coords (:bot w))
  (:goal w)
  (:map w))
(Astar w)
