(ns leiningen.voom.pldb
  (:require [clojure.core.reducers :as red]
            [clojure.core.logic.pldb :as pldb]))

(defn ^:dynamic *missing-rel*
  [rels rel-name]
  (throw (ex-info (str "reldata contains rel not provided to 'from-reldata': "
                       rel-name)
                  {:rel-name rel-name
                   :provided-rels (map #(:rel-name (meta %)) rels)})))

(defn to-reldata
  "Returns reldata for a core.logic pldb db. The reldata is a seq of
  pairs that can be passed to from-reldata to reconstruct a database.
  The reldata is smaller than the database because it has no indexes."
  [db]
  (for [[rel-name indexes] db]
    [rel-name (::pldb/unindexed indexes)]))

(defn from-reldata
  "Returns a core.logic pldb for the given reldata (such as generated
  by 'to-reldata'. Also requires a colection of rels that includes every
  rel in the reldata. This is because reldata doesn't specify what is
  indexed, so the live rels are used for that."
  [rels reldata]
  (let [rel-index-i (into {} (for [rel rels]
                               [(:rel-name (meta rel))
                                (keep-indexed #(when %2 %1)
                                              (:indexes (meta rel)))]))]
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
