(ns my_little_clojurer.chapter4
  (:use (my_little_clojurer chapter1)
        (my_little_clojurer chapter2)))

(defn add1 [n]
  (+ 1 n))

(defn sub1 [n]
  (- n 1))

(defn plus [n m]
  (cond (zero? m)
        n

        :else
        (recur (add1 n) (sub1 m))))

(defn minus [n m]
  (cond (zero? m)
        n

        :else
        (recur (sub1 n) (sub1 m))))

(defn addtup [tup]
  (cond (empty? tup)
        0

        :else
        (plus (first tup) (addtup (rest tup)))))

(defn multiply [n m]
  (cond (zero? m)
        0
        
        :else
        (plus n (multiply n (sub1 m)))))

(defn tup+ [tup1 tup2]
  (cond (empty? tup1)
        tup2

        (empty? tup2)
        tup1
        
        :else
        (cons (plus (first tup1) (first tup2))
              (tup+ (rest tup1) (rest tup2)))))

(defn my-> [n m]
  (cond (= n m)
        false
        
        (zero? n)
        false

        (zero? m)
        true

        :else
        (recur (sub1 n) (sub1 m))))

(defn my-< [n m]
  (cond (= n m)
        false
        
        (zero? n)
        true

        (zero? m)
        false

        :else
        (recur (sub1 n) (sub1 m))))

(defn power [n m]
  (cond (zero? m)
        1

        (zero? n)
        0

        :else
        (multiply n (power n (sub1 m)))))

(defn division [n m]
  (cond (my-< n m)
        0

        :else
        (add1 (division (- n m) m))))

(defn length [lat]
  (cond (empty? lat)
        0

        :else
        (add1 (length (rest lat)))))

(defn pick [n lat]
  (cond (zero? (sub1 n))
        (first lat)

        :else
        (recur (sub1 n) (rest lat))))

(defn rempick [n lat]
  (cond (zero? (sub1 n))
        (rest lat)

        :else
        (cons (first lat)
              (rempick (sub1 n) (rest lat)))))

(defn no-nums [lat]
  (cond (empty? lat)
        '()

        (number? (first lat))
        (no-nums (rest lat))

        :else
        (cons (first lat) (no-nums (rest lat)))))

(defn all-nums [lat]
  (cond (empty? lat)
        '()

        (number? (first lat))
        (cons (first lat) (all-nums (rest lat)))

        :else
        (all-nums (rest lat))))

(defn occur [a lat]
  (cond (empty? lat)
        0

        (= a (first lat))
        (add1 (occur a (rest lat)))

        :else
        (occur a (rest lat))))


(defn one? [n]
  (= 1 n))


(defn rempick2 [n lat]
  (cond (one? n)
        (rest lat)

        :else
        (cons (first lat) (rempick2 (sub1 n) (rest lat)))))

