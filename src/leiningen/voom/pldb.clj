(ns leiningen.voom.pldb
  (:require [clojure.core.reducers :as red]
            [clojure.core.logic.pldb :as pldb
             :refer [rel-key rel-indexes]]))

(defn ^:dynamic *missing-rel*
  [rels rel-name]
  (throw (ex-info (str "reldata contains rel not provided to 'from-reldata': "
                       rel-name)
                  {:rel-name rel-name
                   :provided-rels (map rel-key rels)})))

(defn get-column [db rel col]
  (let [rel-name (rel-key rel)]
    (if-let [index (get-in db [rel-name col])]
      (keys index)
      (map #(nth % col) (get-in db [rel-name ::pldb/unindexed])))))

(defn get-facts
  [db rel]
  (pldb/facts-for [db] (rel-key rel)))

(defn to-reldata
  "Returns reldata for a core.logic pldb db. The reldata is a seq of
  pairs that can be passed to from-reldata to reconstruct a database.
  The reldata is smaller than the database because it has no indexes."
  [db]
  (for [[rel-name indexes] db]
    (if-let [min-index (and (< 1 (count indexes))
                            (apply min (filter number? (keys indexes))))]
      [rel-name (mapcat seq (vals (get indexes min-index)))]
      [rel-name (::pldb/unindexed indexes)])))

(defn from-reldata
  "Returns a core.logic pldb for the given reldata (such as generated
  by 'to-reldata'. Also requires a colection of rels that includes every
  rel in the reldata. This is because reldata doesn't specify what is
  indexed, so the live rels are used for that."
  [rels reldata]
  (let [rel-index-i (into {} (for [rel rels]
                               [(rel-key rel)
                                (keep-indexed #(when %2 %1)
                                              (rel-indexes rel))]))]
    (reduce
     (fn [db [rel-name unindexed]]
       (let [unindexed (into #{} (red/map vec unindexed))]
         (assoc db
           rel-name
           (into {::pldb/unindexed unindexed}
                 (for [i (or (rel-index-i rel-name)
                             (*missing-rel* rels rel-name))]
                   [i (persistent!
                       (reduce (fn [index tuple]
                                 (let [key (nth tuple i)]
                                   (assoc! index key ((fnil conj #{})
                                                      (get index key)
                                                      tuple))))
                               (transient {})
                               unindexed))])))))
     pldb/empty-db
     reldata)))
