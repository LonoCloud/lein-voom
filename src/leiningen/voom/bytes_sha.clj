(ns leiningen.voom.bytes-sha
  (:import [java.util Arrays]))

(defprotocol Sha
  (get-byte-array [_])
  (get-hex-string [_]))

;; TODO: sub-sha match
;; (= (str->sha "abcde") (str->sha "abcd"))
(deftype BytesSha [^bytes bytes]
  Sha
  (get-byte-array [_] bytes)
  (get-hex-string [_] (.toString (java.math.BigInteger. bytes) 16))

  Object
  (toString [this] (get-hex-string this))
  (hashCode [_]
    (bit-or (aget bytes 1) (bit-shift-left (aget bytes 0) 8)))
  (equals [this o]
    (and (instance? BytesSha o) (Arrays/equals bytes ^bytes (get-byte-array o)))))
