(ns my_little_clojurer.chapter5
  (:use (my_little_clojurer chapter1)
        (my_little_clojurer chapter2)
        (my_little_clojurer chapter3)
        (my_little_clojurer chapter4)))


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
        (cond (= old (first l))
              (cons new (cons old (insertL* new old (rest l))))

              :else
              (cons (first l) (insertL* new old (rest l))))

        :else
        (cons (insertL* new old (first l))
              (insertL* new old (rest l)))))
