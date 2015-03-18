(ns exercises.ch6-binary_tree
  (:require [dbj.utils :refer :all]))

(defn insert [tr val]
  (cond
    (nil? tr)       {:l nil, :v val, :r nil}
    (< val (:v tr)) {:l (insert (:l tr) val)
                     :v (:v tr)
                     :r (:r tr)}
    :else           {:l (:l tr)
                     :v (:v tr)
                     :r (insert (:r tr) val)}))

(defn trseq [tr]
  (when tr
    (concat (trseq (:l tr)) [(:v tr)] (trseq (:r tr)))))

(def tree nil)
(doseq [i (range 10)]
  (def tree (insert tree (rand-int 100))))

(prn-code tree)
(prn-code (trseq tree))
