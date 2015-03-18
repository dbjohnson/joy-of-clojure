(load-file "/Users/bryan/dropbox/code/joy-of-clojure/dbj.clj")
(refer 'dbj)

;; book example for generating a random seqeuce
(defn nom [n] (take n (repeatedly #(rand-int n))))

;; we can do better than that
; NOTE: moved into dbj.clj
; (defn rand-seq
;   "Returns lazy sequence drawn from (rand-int n)"
;   ([] (rand-seq 1000))
;   ([n] (lazy-seq (repeatedly #(rand-int n)))))


(defn sort-parts
  "Lazy, tail-recursive, incremental quicksort. Works against
  and creates partitions based on the pivot, defined as 'work'."
  [work]
  (lazy-seq
    (loop [[part & parts] work]
      (if-let [[pivot & xs] (seq part)]
        ;; FIXME: in this implementation from the book,
        ;;        the sequence xs is being passed over twice
        ; (let [smaller? #(< % pivot)]
        ;   (recur (list*
        ;   (filter smaller? xs)
        ;   pivot
        ;   (remove smaller? xs)
        ;   parts)))

        ; Ah, that's better (or is it? need to investigate implementation details)
        (let [pivoted (group-by (partial < pivot) xs)
              smaller (get pivoted false)
              larger  (get pivoted true)]
          (recur (list*
           smaller
           pivot
           larger
           parts)))
        (when-let [[x & parts] parts]
          (cons x (sort-parts parts)))))))

(defn qsort
  {:test (fn [] (assert (apply <= (qsort (take 1000 (rand-seq))))))}
  [xvals]
  (sort-parts (list xvals)))

(prn-code (qsort (take 100 (rand-seq))))
(clojure.test/run-tests)
