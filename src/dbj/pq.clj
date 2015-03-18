(ns dbj.pq
  (:require [dbj.utils :refer :all])
  (:require clojure.test))


;;;; Implements priority queue via binary tree

; TODO: figure out how to avoid these declares
(declare pqstats)
(declare rand-pq)

(defn pqpush
  "Insert node into priority queue according to its associated score."
  ; test that the tree is ~close to balanced
  {:test (fn [] (let [n     10000
                      pq    (rand-pq n)
                      stats (pqstats pq)]
                  (assert (= n (:total stats)))
                  (assert (every? #(< (* 0.45 n) %)
                            [(:left stats) (:right stats)]))))}
  ([item score pq] (pqpush {:L nil, :score score, :item item, :R nil} pq))
  ([nd pq]
    (let [score (:score nd)]
      (cond (nil? pq)
              nd

            (nil? nd)
              pq

            (< (:score nd) (:score pq))
              ; if the node's score is less than the pq's head,
              ; swap so that the pq's score is less - just to reduce
              ; number of test conditions below
              (pqpush pq nd)

            (or ; left is empty, right is set
                (and (nil? (:L pq))
                     (:R pq))
                ; new value is less than current left
                (and (:L pq)
                     (<= (:score nd) (:score (:L pq))))
                ; flip a coin when both left and right are empty
                (and (every? nil? [(:L pq) (:R pq)])
                     (zero? (rand-int 2))))
              {:L     (pqpush (:L pq) nd)
               :score (:score pq)
               :item  (:item pq)
               :R     (:R pq)}

            :else
              {:L     (:L pq)
               :score (:score pq)
               :item  (:item pq)
               :R     (pqpush (:R pq) nd)}))))


(defn pqpeek [pq]
  (:item pq))


(defn pqpop
  "Pops the head node and returns the combined left/right children"
  [pq]
  (pqpush (:R pq) (:L pq)))


(defn pqseq
  "Returns a lazy-seq from given priority queue"
  {:test (fn [] (let [n 10000]
                  (assert (apply <= (take n (pqseq (rand-pq n)))))))}
  [pq]
  (lazy-seq
    (loop [pq pq]
      (when pq
        (cons (pqpeek pq) (pqseq (pqpop pq)))))))

(defn rand-pq
  "Returns a randomly generated priority queue - for testing/exploration"
  [n]
  (reduce pqpush
    (map (fn [x] {:L nil, :score x, :item x, :R nil})
      (take n (rand-seq)))))


(defn pqstats
  "Basic tree statistics.  Would be nice to include max/avg depth"
  [pq]
  (loop [q     [pq]
         total 0
         left  0
         right 0]
    (if (seq q)
      (let [n    (first q)
            rest (next q)]
        (recur
          (into rest
            (filter identity [(:L n) (:R n)]))
          (inc total)
          (if (:L n) (inc left) left)
          (if (:R n) (inc right) right)))
      {:total total, :left left, :right right})))

(clojure.test/run-tests)
