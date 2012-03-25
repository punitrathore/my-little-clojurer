(ns my-little-clojurer.chapter2
  (:use (my-little-clojurer chapter1)))

(defn lat? [l]
  (cond (empty? l)
        true

        (atom? (first l))
        (recur (rest l))

        :else
        false))

(defn member? [element list]
  (cond (empty? list)
        false

        :else
        (or (= (first list) element) (recur element (rest list)))))
