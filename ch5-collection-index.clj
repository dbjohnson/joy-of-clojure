(load-file "dbj.clj")
(refer 'dbj)

(defn index [coll]
  (cond
    (map? coll) (seq coll)
    (set? coll) (map vector coll coll)
    :else (map vector (iterate inc 0) coll)))

(defn pos [pred coll]
  (for [[idx val] (index coll) :when (pred val)] idx))

(prn-code (index #{4 5 6}))
(prn-code (pos #(< 4 %) [4 5 6]))
