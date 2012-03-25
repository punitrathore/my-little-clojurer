(ns my-little-clojurer.chapter6
  (:use (my-little-clojurer chapter1)
        (my-little-clojurer chapter2)
        (my-little-clojurer chapter3)
        (my-little-clojurer chapter4)))

(defn numbered? [aexp]
  (cond (atom? aexp)
        (number? aexp)

        :else
        (and (numbered? (first aexp)) (numbered? (first (rest (rest aexp)))))))

(defn value [nexp]
  (cond (atom? nexp)
        nexp

        (= (first (rest nexp)) '+)
        (+ (value (first nexp))
           (value (first (rest (rest nexp)))))

        (= (first (rest nexp)) '*)
        (* (value (first nexp))
           (value (first (rest (rest nexp)))))

        :else
        (power (value (first nexp))
               (value (first (rest (rest nexp)))))))

(defn value-prefix [nexp]
  (cond (atom? nexp)
        nexp

        (= (first nexp) '+)
        (+ (value-prefix (first (rest nexp)))
           (value-prefix (first (rest (rest nexp)))))

        (= (first nexp) '*)
        (* (value-prefix (first (rest nexp)))
           (value-prefix (first (rest (rest nexp)))))

        :else
        (power (value-prefix (first (rest nexp)))
               (value-prefix (first (rest (rest nexp)))))))

(defn first-sub-exp-for-prefix [aexp]
  (first (rest aexp)))

(defn second-sub-exp-for-prefix [aexp]
  (first-sub-exp-for-prefix (rest aexp)))

(defn operator-for-prefix [aexp]
  (first aexp))

(defn value-prefix2 [nexp]
  (cond (atom? nexp)
        nexp

        (= (operator-for-prefix nexp) '+)
        (+ (value-prefix2 (first-sub-exp-for-prefix nexp))
           (value-prefix2 ))

        (= (operator-for-prefix nexp) '*)
        (* (value-prefix2 (first-sub-exp-for-prefix nexp))
           (value-prefix2 (second-sub-exp-for-prefix nexp)))

        :else
        (power (value-prefix2 (first-sub-exp-for-prefix nexp))
               (value-prefix2 (second-sub-exp-for-prefix nexp)))))

(defn first-sub-exp-for-infix [aexp]
  (first aexp))

(defn second-sub-exp-for-infix [aexp]
  (second-sub-exp-for-prefix aexp))

(defn operator-for-infix [aexp]
  (first (rest aexp)))

(defn value2 [nexp]
  (cond (atom? nexp)
        nexp

        (= (operator-for-infix nexp) '+)
        (+ (value2 (first-sub-exp-for-infix nexp))
           (value2 (second-sub-exp-for-infix nexp)))

        (= (operator-for-infix nexp) '*)
        (* (value2 (first-sub-exp-for-infix nexp))
           (value2 (second-sub-exp-for-infix nexp)))

        :else
        (power (value2 (first-sub-exp-for-infix nexp))
               (value2 (second-sub-exp-for-infix nexp)))))



(defn sero? [n]
  (empty? n))

(defn edd1 [n]
  (cons '() n))

(defn zub1 [n]
  (rest n))

(defn blus [n m]
  (cond (sero? m)
        n

        :else
        (recur (edd1 n) (zub1 m))))
