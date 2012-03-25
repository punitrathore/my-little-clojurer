(ns my-little-clojurer.chapter1)

(defn atom?
  [x]
  (not (coll? x)))
