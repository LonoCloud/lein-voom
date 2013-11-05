(ns leiningen.git-version
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as s]
            [leiningen.core.main :as lmain])
  (:import [java.util Date]))

(def timestamp-fmt "yyyyMMddhhmmss")

(defn formatted-timestamp
  [fmt t]
  (.format (doto (java.text.SimpleDateFormat. fmt java.util.Locale/US)
             (.setTimeZone (java.util.SimpleTimeZone. 0 "GMT")))
           t))

(defn get-git-version
  [path]
  (let [{:keys [out exit err]} (sh "git" "log" "-1" "--pretty=%h,%cd" path)
        ;; Throw exception if error?
        [sha, datestr] (-> out s/trim (s/split #"," 2))
        ctime (Date. datestr)]
    {:ctime ctime :sha sha}))

(defn format-git-ver
  [gver fmt]
  (let [{:keys [ctime sha]} gver]
    (str "-" (formatted-timestamp fmt ctime) "-" sha)))

(defn git-version
  [project & args]
  (let [[kstrs sargs] (split-with #(.startsWith % ":") args)
        kargs (map #(keyword (.substring % 1)) kstrs)
        gver (-> project :root get-git-version)
        qual (format-git-ver gver timestamp-fmt)
        upfn #(s/replace % #"-SNAPSHOT" qual)
        nproj (update-in project [:version] upfn)
        nmeta (update-in (meta project) [:without-profiles :version] upfn)
        nnproj (with-meta nproj nmeta)]
    ;; TODO throw exception if dirty
    ;; TODO throw exception if upstream doesn't contain this commit
    ;; args :insanely-allow-dirty-working-copy :no-upstream
    (lmain/resolve-and-apply nnproj sargs)))
