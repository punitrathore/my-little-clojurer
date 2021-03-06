(ns my-little-clojurer.chapter8
  (:use (my-little-clojurer chapter1)
        (my-little-clojurer chapter2)
        (my-little-clojurer chapter3)
        (my-little-clojurer chapter4)
        (my-little-clojurer chapter5)
        (my-little-clojurer chapter6)
        (my-little-clojurer chapter7)))

(defn rember-f [test?]
  (fn [a l]
    (cond (empty? l)
          '()
          
          (test? (first l) a)
          (rest l)

          :else
          (cons (first l) ((rember-f test?) a (rest l))))))

(def rember-=?
     (rember-f =))


(defn insertL-f [test?]
  (fn [new old lat]
    (cond (empty? lat)
          '()

          (test? (first lat) old)
          (cons new (rest lat))

          :else
          (cons (first lat) ((insertL-f test?) new old (rest lat))))))

(defn insertR-f [test?]
  (fn [new old lat]
    (cond (empty? lat)
          '()

          (test? (first lat) old)
          (cons old (cons new (rest lat)))

          :else
          (cons (first lat) ((insertR-f test?) new old (rest lat))))))

(defn insert-g2 [where new old lat]
  (cond (empty? lat)
        '()

        (= (first lat) old)
        (if (= where :left)
          (cons new lat)
          (cons (first lat)
                (cons new (rest lat))))

        :else
        (cons (first lat) (insert-g2 where new old (rest lat)))))

(defn seqL [new old l]
  (cons new (cons old l)))

(defn seqR [new old l]
  (cons old (cons new l)))

(defn insert-g [seq]
  (fn [new old l]
    (cond (empty? l)
          '()

          (= (first l) old)
          (seq new old (rest l))

          :else
          (cons (first l) ((insert-g seq) new old (rest l))))))

(def insertL-seq
  (insert-g seqL))

(def insertR-seq
  (insert-g seqR))

(defn seqS [new old l]
  (cons new l))

(def subst-seq
   (insert-g seqS))

(defn seqRem [new old l]
   (l))

(def rember-seq
  (insert-g seqRem))


(defn atom-to-function [x]
  (cond (= x '+)
        plus

        (= x '*)
        multiply

        :else
        power))


(defn value-fn [nexp]
  (cond (atom? nexp)
        nexp

        :else
        ((atom-to-function
          (operator-for-infix nexp))
         (value-fn (first-sub-exp-for-infix nexp))
         (value-fn (second-sub-exp-for-infix nexp)))))


(defn multirember-f [test?]
  (fn [a lat]
    (cond (empty? lat)
          '()

          (test? (first lat) a)
          ((multirember-f test?) a (rest lat))

          :else
          (cons (first lat)
                ((multirember-f test?) a (rest lat))))))

(def multirember-=
     (multirember-f =))

(defn multiremberT [test?]
  (fn [lat]
    (cond (empty? lat)
          '()

          (test? (first lat))
          ((multiremberT test?) (rest lat))

          :else
          (cons (first lat)
                ((multiremberT test?) (rest lat))))))

(defn multirember1 [lat]
  ((multiremberT (fn [x] (= 1 x))) lat))


(defn multirember&co [a lat col]
  (cond (empty? lat)
        (col '() '())

        (= (first lat) a)
        (multirember&co a (rest lat)
                        (fn [newlat seen]
                          (col newlat
                                (cons (first lat) seen))))

        :else
        (multirember&co a (rest lat)
                        (fn [newlat seen]
                          (col (cons (first lat) newlat)
                                seen)))))

(defn a-friend? [x y]
  (empty? y))

(defn multiinsertLR [new oldL oldR lat]
  (cond (empty? lat)
        '()

        (= (first lat) oldL)
        (cons new (cons oldL (multiinsertLR new oldL oldR (rest lat))))

        (= (first lat) oldR)
        (cons oldR (cons new (multiinsertLR new oldL oldR (rest lat))))
              
        :else
        (cons (first lat) (multiinsertLR new oldL oldR (rest lat)))))


(defn multiinsertLR&co [new oldL oldR lat col]
  (cond (empty? lat)
        (col '() 0 0)

        (= (first lat) oldL)
        (multiinsertLR&co new oldL oldR (rest lat)
                          (fn [newlat L R]
                            (col (cons new (cons oldL) newlat) (add1 L) R)))

        (= (first lat) oldR)
        (multiinsertLR&co new oldL oldR (rest lat)
                          (fn [newlat L R]
                            (col (cons oldR (cons new newlat)) L (add1 R))))

        :else
        (multirember&co new oldL oldR (rest lat)
                        (fn [newlat L R]
                          (col (cons (first lat) newlat) L R)))))

(defn evens-only* [l]
  (cond (empty? l)
        '()

        (atom? (first l))
        (cond (even? (first l))
              (cons (first l) (evens-only* (rest l)))

              :else
              (evens-only* (rest l)))

        :else
        (cons (evens-only* (first l)) (evens-only* (rest l)))))

(defn evens-only*&co [l col]
  (cond (empty? l)
        (col '() 1 0)

        (atom? (first l))
        (cond (even? (first l))
              (evens-only*&co (rest l)
                              (fn [newlat prod sum]
                                (col (cons (first l) newlat) (* prod (first l)) sum)))

              :else
              (evens-only*&co (rest l)
                              (fn [newlat prod sum]
                                (col newlat prod (+ sum (first l))))))

        :else
        (evens-only*&co (first l)
                        (fn [al ap as]
                          (evens-only*&co (rest l)
                                          (fn [dl dp ds]
                                            (col (cons (first al) dl)
                                                 (* ap dp)
                                                 (+ as ds))))))))

(defn the-last-friend [newl product sum]
  (cons sum
        (cons product
              newl)))
