(ns dbj)

(defmacro prn-code [form]
  (prn form)
  (prn '=> (eval form)))