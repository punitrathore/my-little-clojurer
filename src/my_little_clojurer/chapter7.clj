(ns my-little-clojurer.chapter7
  (:use (my-little-clojurer chapter1)
        (my-little-clojurer chapter2)
        (my-little-clojurer chapter3)
        (my-little-clojurer chapter4)
        (my-little-clojurer chapter5)
        (my-little-clojurer chapter6)))

(defn my-set? [lat]
  (cond (empty? lat)
        true

        (member? (first lat) (rest lat))
        false

        :else
        (my-set? (rest lat))))

(defn make-set [lat]
  (cond (empty? lat)
        '()

        (member? (first lat) (rest lat))
        (make-set (rest lat))
        
        :else
        (cons (first lat)
              (make-set (rest lat)))))

(defn make-set2 [lat]
  (cond (empty? lat)
        '()

        :else
        (cons (first lat)
              (make-set2
               (multirember (first lat)
                            (rest lat))))))

(defn subset? [set1 set2]
  (cond (empty? set1)
        true

        :else
        (and (member? (first set1) set2)
             (subset? (rest set1) set2))))

(defn eqset? [set1 set2]
  (and (subset? set1 set2)) (subset? set2 set1))

(defn intersect? [set1 set2]
  (cond (empty? set1)
        false

        :else
        (or (member? (first set1) set2)
            (recur (rest set1) set2))))

(defn intersect [set1 set2]
  (cond (empty? set1)
        '()

        (member? (first set1) set2)
        (cons (first set1)
              (intersect (rest set1) set2))

        :else
        (intersect (rest set1) set2)))

(defn union [set1 set2]
  (cond (empty? set1)
        set2

        (member? (first set1) set2)
        (union (rest set1) set2)

        :else
        (cons (first set1)
              (union (rest set1) set2))))

(defn difference [set1 set2]
  (cond (empty? set1)
        '()

        (member? (first set1) set2)
        (union (rest set1) set2)

        :else
        (cons (first set1)
              (union (rest set1) set2))))

(defn intersectall [l-set]
  (cond (empty? (rest l-set))
        (first l-set)

        :else
        (intersect (first l-set)
                   (intersectall (rest l-set)))))

(defn pair? [x]
  (cond (atom? x)
        false

        (empty? x)
        false

        (empty? (rest x))
        false

        (empty? (rest (rest x)))
        true

        :else
        false))

(defn build [s1 s2]
  (cons s1 (cons s2 '())))

(defn third [l]
  (first (rest (rest l))))

(defn fun? [rel]  
  (my-set? (firsts rel)))

(defn revrel [rel]
  (cond (empty? rel)
        '()

        :else
        (cons (build (second (first rel)) (first (first rel)))
              (revrel (rest rel)))))

(defn revpair [pair]
  (build (second pair) (first pair)))

(defn revrel2 [rel]
  (cond (empty? rel)
        '()

        :else
        (cons (revpair (first rel))
              (revrel2 (rest rel)))))

(defn seconds [l]
  (cond (empty? l)
        '()

        :else
        (cons (second (first l)) (seconds (rest l)))))

(defn fullfun? [fun]
  (fun? (seconds fun)))

(defn fullfun2? [fun]
  (fun? (revrel2 fun)))

