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
  [path & [long-sha]]
  (let [shafmt (if long-sha "%H" "%h")
        fmt (str "--pretty=" shafmt ",%cd")
        {:keys [out exit err]} (sh "git" "log" "-1" fmt path)
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
        long-sha (some #{:long-sha} kargs)
        gver (-> project :root (get-git-version long-sha))
        qual (format-git-ver gver timestamp-fmt)
        upfn #(str (s/replace % #"-SNAPSHOT" "") qual)
        nproj (update-in project [:version] upfn)
        nmeta (update-in (meta project) [:without-profiles :version] upfn)
        nnproj (with-meta nproj nmeta)]
    ;; TODO throw exception if dirty
    ;; TODO throw exception if upstream doesn't contain this commit
    ;; args :insanely-allow-dirty-working-copy :no-upstream :print :long-sha
    (if (some #{:print} kargs)
      (println (upfn (:version project)))
      (lmain/resolve-and-apply nnproj sargs))))
