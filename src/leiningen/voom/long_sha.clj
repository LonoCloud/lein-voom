(ns leiningen.voom.long-sha
  (:require [clojure.data.fressian :as fress]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn ^:private ^long nibbles [^long num]
  (-> num (bit-shift-right 60) (bit-and 0xf)))

(defn ^long mask [^long num]
  (bit-shift-right
   0xfffffffffffffff
   (* 4 (- 15 (nibbles num)))))

(deftype LongSha [^long num]
  Object
  (toString [this]
    (apply str (map #(nth "0123456789abcdef"
                          (-> num
                              (bit-shift-right (bit-shift-left % 0x2))
                              (bit-and 0xf)))
                    (range (nibbles num)))))
  (hashCode [_]
    (bit-and num 0xffff))
  (equals [this o]
    (and (instance? LongSha o)
         (let [onum (.num ^LongSha o)]
           (or (== num onum) ;; perfect match
               (let [tmask (mask num)
                     omask (mask onum)]
                 (if (== tmask omask)
                   false ;; nums didn't match but mask did
                   (let [m (bit-and (mask num) (mask onum))]
                     ;; match up through minimum prefix?
                     (== (bit-and m num) (bit-and m onum))))))))))

(defn mk
  "Create a new LongSha from a hex string"
  [^String s]
  (let [s-len (min 15 (count s))]
    (assert (<= 4 s-len))
    (loop [acc 0, i (dec s-len)]
      (if (neg? i)
        (LongSha. (-> s-len (bit-shift-left 60) (bit-or acc)))
        (recur (-> acc
                   (bit-shift-left 4)
                   (bit-or
                    (case (nth s i)
                      \0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9
                      \a 10 \b 11 \c 12 \d 13 \e 14 \f 15
                      \A 10 \B 11 \C 12 \D 13 \E 14 \F 15)))
               (dec i))))))

;; This is probably not necessary -- should get reset automatically
;; when leaving the namespace:
(set! *unchecked-math* false)


(def write-handlers
  (-> (assoc fress/clojure-write-handlers
        LongSha
        {"sha"
         (reify org.fressian.handlers.WriteHandler
           (write [this w sha]
             (.writeTag w "sha" 1)
             (.writeInt w (.num ^LongSha sha))))})
      fress/associative-lookup
      fress/inheritance-lookup))

(def read-handlers
  (-> (assoc fress/clojure-read-handlers
        "sha"
        (reify org.fressian.handlers.ReadHandler
          (read [_ rdr tag component-count]
            (LongSha. (.readInt rdr)))))
      fress/associative-lookup))
