(ns dbj.utils)

(defmacro prn-code [form]
  (prn form)
  (prn '=> (eval form)))


(defn rand-seq
  "Returns lazy sequence drawn from (rand-int n)"
  ([] (rand-seq 1000))
  ([n] (lazy-seq (repeatedly #(rand-int n)))))
