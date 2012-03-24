(ns my_little_clojurer.chapter1)

(defn atom?
  [x]
  (not (coll? x)))
