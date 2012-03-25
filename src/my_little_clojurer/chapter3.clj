(ns my-little-clojurer.chapter3
  (:use (my-little-clojurer chapter1)
        (my-little-clojurer chapter2)))


(defn rember [a lat]
  (cond (empty? lat)
        '()

        (= a (first lat))
        (rest lat)

        :else
        (cons (first lat) (rember a (rest lat)))))

(defn firsts [list]
  (cond (empty? list)
        '()

        :else
        (cons (first (first list)) (firsts (rest list)))))

(defn insertR [new old lat]
  (cond (empty? lat)
        '()

        (= (first lat) old)
        (cons (first lat)
              (cons new (rest lat)))

        :else
        (cons (first lat) (insertR new old (rest lat)))))

(defn insertL [new old lat]
  (cond (empty? lat)
        '()

        (= (first lat) old)
        (cons new lat)

        :else
        (cons (first lat) (insertL new old (rest lat)))))

(defn subst [new old lat]
  (cond (empty? lat)
        '()

        (= (first lat) old)
        (cons new (rest lat))

        :else
        (cons (first lat) (subst new old (rest lat)))))

(defn subst2 [new o1 o2 lat]
  (cond (empty? lat)
        '()

        (or (= (first lat) o1) (= (first lat) o2))
        (cons new (rest lat))

        :else
        (cons (first lat) (subst2 new o1 o2 (rest lat)))))

(defn multirember [a lat]
  (cond (empty? lat)
        '()

        (= (first lat) a)
        (recur a (rest lat))

        :else
        (cons (first lat) (multirember a (rest lat)))))


(defn multiinsertR [new old lat]
  (cond (empty? lat)
        '()

        (= (first lat) old)
        (cons old (cons new (multiinsertR new old (rest lat))))

        :else
        (cons (first lat) (multiinsertR new old (rest lat)))))

(defn multiinsertL [new old lat]
  (cond (empty? lat)
        '()

        (= (first lat) old)
        (cons new (cons old (multiinsertL new old (rest lat))))

        :else
        (cons (first lat) (multiinsertL new old (rest lat)))))

(defn multisubst [new old lat]
  (cond (empty? lat)
        '()

        (= (first lat) old)
        (cons new (multisubst new old (rest lat)))

        :else
        (cons (first lat) (multisubst new old (rest lat)))))

