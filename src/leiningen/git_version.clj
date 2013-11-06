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

(defn ver-parse
  "Parses jar-path-like-string or versioned-artifact-string to find ctime and sha.
   Can handle cases in the range of:
     foo-1.2.3-20120219223112-abc123f
     /path/to/foo-1.2.3-20120219223112-abc123f19ea8d29b13.jar"
  [ver-str]
  (let [[_ ctime sha] (re-matches #".*-([0-9]{14})-([a-f0-9]{5,40})(?:\.jar)?$" ver-str)]
    (when (and ctime sha)
      {:ctime ctime :sha sha})))

(defn dirty-wc?
  [path]
  ;; TODO Is it right to ignore untracked files?
  (let [{:keys [out err exit]} (sh "git" "status" "--short" "--untracked-files=no" path)]
    (not (empty? out))))

(defn git-version
  "Usage:
    lein git-version [flags] [lein command ...]
      Runs lein command with a project version augmented with git
      version of the most recent change of this project directory.
      Flags include:
        :insanely-allow-dirty-working-copy - by default git-version
          refuses to handle a dirty working copy
        :no-upstream - by default git-version wants to see the current
          version reachable via an upstream repo
        :long-sha - uses a full length sha instead of the default
          short form
    lein git-version [:long-sha] :print
    lein git-version :parse <version-str>"
  [project & args]
  (let [[kstrs sargs] (split-with #(.startsWith % ":") args)
        kargset (set (map #(keyword (.substring % 1)) kstrs))
        long-sha (kargset :long-sha)
        gver (-> project :root (get-git-version long-sha))
        qual (format-git-ver gver timestamp-fmt)
        upfn #(str (s/replace % #"-SNAPSHOT" "") qual)
        nproj (update-in project [:version] upfn)
        nmeta (update-in (meta project) [:without-profiles :version] upfn)
        nnproj (with-meta nproj nmeta)]
    ;; TODO throw exception if upstream doesn't contain this commit :no-upstream
    (cond
     (:print kargset) (println (upfn (:version project)))
     (:parse kargset) (prn (ver-parse (first sargs)))
     :else (if (and (dirty-wc? (:root project))
                    (not (:insanely-allow-dirty-working-copy kargset)))
             (lmain/abort "Refusing to continue with dirty working copy. (Hint: Run 'git status')")
             (lmain/resolve-and-apply nnproj sargs)))))
