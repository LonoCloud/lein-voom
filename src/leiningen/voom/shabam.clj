(ns leiningen.voom.shabam
  (:import [java.util Arrays]))

;; ===== ancestry sha-bitmap (shabam) =====

(def ^Long bits-per-byte 8)
(def ^Long bytes-per-long 8)
(def ^Long bits-per-long (* bits-per-byte bytes-per-long))

(defn ^Long bm-longs [bits]
  (long (Math/ceil (/ bits bits-per-long))))

(defn ^"[J" bm-new []
  (long-array 1))

(defn ^"[J" bm-set
  [^"[J" bm idx]
  (let [size (max (count bm) (bm-longs (inc idx)))
        nbm (Arrays/copyOf bm ^Long size)
        w (quot idx bits-per-long)
        o (mod idx bits-per-long)
        m (bit-set 0 o)
        v (aget nbm w)
        nv (bit-or v ^Long m)]
    (aset nbm w nv)
    nbm))

(defn ^Long bm-get
  [^"[J" bm idx]
  (when (<= (bm-longs (inc idx)) (count bm))
    (let [w (quot idx bits-per-long)
          o (mod idx bits-per-long)
          m (bit-set 0 o)
          v (aget bm w)
          mv (bit-and v m)]
      (not (zero? mv)))))

(defn ^"[J" bm-or
  [& bms]
  (if (empty? bms)
    (bm-new)
    (let [size (apply max (map count bms))
          nbm (Arrays/copyOf ^"[J" (first bms) ^Long size)]
      (doseq [bm (rest bms)
              [i v] (map-indexed list bm)
              :let [mv (bit-or v (aget nbm i))]]
        (aset nbm i mv))
      nbm)))

(defn shabam-new []
  {:sha->idx {} :bitmaps []})

(defn shabam-contains? [shabam sha]
  (-> shabam :sha->idx (contains? sha)))

(defn shabam-add [shabam sha & parents]
  (if (shabam-contains? shabam sha)
    shabam
    (let [{:keys [sha->idx bitmaps]} shabam
          nid (count sha->idx)
          sha->idx (assoc sha->idx sha nid)
          pidxs (map sha->idx parents)
          nbm (if (empty? pidxs)
                (bm-new)
                (apply bm-or (map bitmaps pidxs)))
          nbm (reduce bm-set nbm pidxs)
          bitmaps (conj bitmaps nbm)]
      {:sha->idx sha->idx
       :bitmaps bitmaps})))

(defn sha-ancestors [{:keys [sha->idx bitmaps]} child ancs]
  (if-let [cidx (sha->idx child)]
    (filter #(when-let [sha (sha->idx %)]
               (bm-get (get bitmaps cidx)
                       sha))
            ancs)))

(defn sha-successors [{:keys [sha->idx bitmaps]} parent succ]
  (if-let [pidx (sha->idx parent)]
    (filter #(when-let [sha (sha->idx %)]
               (bm-get (get bitmaps sha)
                       pidx))
            succ)))
