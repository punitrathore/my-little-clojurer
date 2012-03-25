(ns my-little-clojurer.chapter5
  (:use (my-little-clojurer chapter1)
        (my-little-clojurer chapter2)
        (my-little-clojurer chapter3)
        (my-little-clojurer chapter4)))


(defn rember* [a l]
  (cond (empty? l)
        '()

        (atom? (first l))
        (cond (= a (first l))
              (rember* a (rest l))

              :else
              (cons (first l) (rember* a (rest l))))

        :else
        (cons (rember* a (first l)) (rember* a (rest l)))))


(defn insertR* [new old l]
  (cond (empty? l)
        '()

        (atom? (first l))
        (cond (= old (first l))
              (cons old (cons new (insertR* new old (rest l))))

              :else
              (cons (first l) (insertR* new old (rest l))))

        :else
        (cons (insertR* new old (first l)) (insertR* new old (rest l)))))

(defn occur* [a l]
  (cond (empty? l)
        0

        (atom? (first l))
        (cond (= a (first l))
              (add1 (occur* a (rest l)))
              
              :else
              (occur* a (rest l)))
        
        :else
        (plus (occur* a (first l))
              (occur* a (rest l)))))

(defn subst* [new old l]
  (cond (empty? l)
        '()

        (atom? (first l))
        (cond (= (first l) old)
              (cons new (subst* new old(rest l)))

              :else
              (cons (first l) (subst* new old (rest l))))

        :else
        (cons (subst* new old (first l)) (subst* new old (rest l)))))

(defn insertL* [new old l]
  (cond (empty? l)
        '()

        (atom? (first l))
        (cond (= (first l) old)
              (cons new (cons old (insertL* new old (rest l))))

              :else
              (cons (first l) (insertL* new old (rest l))))

        :else
        (cons (insertL* new old (first l))
              (insertL* new old (rest l)))))

(defn member* [a l]
  (cond (empty? l)
        false

        (atom? (first l))
        (cond (= (first l) a)
              true

              :else
              (member* a (rest l)))

        :else
        (or (member* a (first l))
            (member* a (rest l)))))

(defn leftmost [l]
  (cond (atom? (first l))
        (first l)

        :else
        (recur (first l))))

(defn eqlist? [l1 l2]
  (cond (and (empty? l1) (empty? l2))
        true

        (or (empty? l1) (empty? l2))
        false

        (and (atom? (first l1)) (atom? (first l2)))
        (and (= (first l1) (first l2))
             (eqlist? (rest l1) (rest l2)))

        (or (atom? (first l1)) (atom? (first l2)))
        false

        :else
        (and (eqlist? (first l1) (first l2))
            (eqlist? (rest l1) (rest l2)))))

;; forward declaration cause equal is written using eqlist2? and vice
;; versa. This is brillant! Defining function which depend on each other
(declare equal eqlist2?)

(defn equal? [s1 s2]
  (cond (and (atom? s1) (atom? s2))
        (= s1 s2)

        (or (atom? s1) (atom? s2))
        false

        :else
        (eqlist2? s1 s2)))

(defn eqlist2? [l1 l2]
  (cond (and (empty? l1) (empty? l2))
        true

        (or (empty? l1) (empty? l2))
        false

        :else
        (and (equal? (first l1) (first l2))
             (eqlist2? (rest l1) (rest l2)))))

(defn rember2 [a l]
  (cond (empty? l)
        '()

        (equal? (first l) a)
        (rest l)

        :else
        (cons (first l) (rember2 a (rest l)))))
