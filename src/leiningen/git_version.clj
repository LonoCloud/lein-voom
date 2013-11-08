(ns leiningen.git-version
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [leiningen.core.project :as project]
            [leiningen.core.main :as lmain]
            [org.satta.glob :refer [glob]])
  (:import [java.util Date]
           [java.io File]
           [java.util.logging Logger Handler Level]
           [org.sonatype.aether.transfer ArtifactNotFoundException]))

(set! *warn-on-reflection* true)

;; === git sha-based versions ===

(def timestamp-fmt "yyyyMMdd_hhmmss")

(defn formatted-timestamp
  [^String fmt t]
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
        ctime (Date. ^String datestr)]
    {:ctime ctime :sha sha}))

(defn format-git-ver
  [gver fmt]
  (let [{:keys [ctime sha]} gver]
    (str "-" (formatted-timestamp fmt ctime) "-g" sha)))

(defn ver-parse
  "Parses jar-path-like-string or artifact-version-string to find ctime and sha.
   Can handle cases in the range of:
     1.2.3-20120219223112-abc123f
     1.2.3-20120219_223112-gabc123f
     foo-1.2.3-20120219223112-gabc123f
     foo-1.2.3-20120219_223112-abc123f
     /path/to/foo-1.2.3-20120219_223112-gabc123f19ea8d29b13.jar"
  [ver-str]
  (let [[_ ctime sha] (re-matches #".*-?([0-9]{8}_?[0-9]{6})-g?([a-f0-9]{4,40})(?:\.jar)?$" ver-str)]
    (when (and ctime sha)
      {:ctime (s/replace ctime #"_" "") :sha sha})))

(defn dirty-wc?
  [path]
  (let [{:keys [out err exit]} (sh "git" "status" "--short" path)]
    (not (empty? out))))


;; === manage REPOS_HOME directory ===

(def repos-home (or (System/getenv "REPOS_HOME")
                    (str (System/getProperty "user.home")
                         (str "/.voom-repos"))))

(defn with-log-level [level f]
  (let [handlers (doall (.getHandlers (Logger/getLogger "")))
        old-levels (doall (map #(.getLevel ^Handler %) handlers))
        _ (doseq [h handlers] (.setLevel ^Handler h level))
        result (f)]
    (dorun (map #(.setLevel ^Handler %1 %2) handlers old-levels))
    result))

(defn git
  [^File d & subcmd]
  ;; We won't handle bare repos or displaced worktrees
  (let [gitdir (.getPath d)
        worktree (.getParent d)]
    (apply sh "git" (str "--git-dir=" gitdir) (str "--work-tree=" worktree) subcmd)))

(defn find-project-files
  [^File d]
  (let [{:keys [out err exit]} (git d "ls-files" "project.clj" "**/project.clj")
        projects (when-not (empty? out)
                   (s/split-lines out))]
    (map #(str (.getParent d) "/" %) projects)))

(defn contains-sha? [d sha]
  (prn "contains-sha?" d sha)
  (->>
   (git d "rev-parse" "--verify" "--quiet" sha)
   :exit
   zero?))

(defn locate-sha
  [dirs sha]
  (seq (for [d dirs
             :when (contains-sha? d sha)]
         d)))

(defn fetch-all
  [dirs]
  (doseq [^File d dirs]
    (println "Fetching:" (.getPath d))
    (git d "fetch")))

(defn find-project
  [pgroup pname candidate]
  (prn "find-project" pgroup pname candidate)
  (for [p (find-project-files candidate)
        :let [{:keys [group name] :as prj} (project/read p)]
        :when (and (= group pgroup)
                   (= name pname))]
    prj))

(defn find-matching-projects
  "[project {groupId name ctime sha}] [project coordinate-string]"
  [repos-dir pspec]
  (prn "find-matching-projects" repos-dir pspec)
  (let [{:keys [sha artifactId groupId]} pspec
        dirglob (str (s/replace repos-dir #"/$" "") "/*/.git")
        dirs (glob dirglob)
        _ (prn "dirs" dirglob dirs)
        sha-candidates (locate-sha dirs sha)
        sha-candidates (or sha-candidates
                           (do (fetch-all dirs)
                               (locate-sha dirs sha)))]
    (prn "sha-candidates" sha-candidates)
    (mapcat (fn [c]
              (git c "checkout" sha)
              (find-project groupId artifactId c))
            sha-candidates)))

(defn install-versioned-artifact
  [proot]
  (let [r (sh "lein" "git-version" "install" :dir proot)]
    (prn "r" r)))

(defn missing-artifacts-from-exception
  "Returns a sequence of artifacts indicated as missing anywhere in
  any ArtifactNotFoundException that appears in the cause chain of e"
  [e]
  (for [^Exception cause (iterate #(.getCause ^Exception %) e)
        :while cause
        :when (instance? ArtifactNotFoundException cause)]
    (let [art (.getArtifact ^ArtifactNotFoundException cause)]
      (select-keys (bean art) [:groupId :artifactId :version]))))


;; === build-deps ===

(defn resolve-artifact
  "Build and install given artifact using git. Return value is
  undefined. Throws an exception with detailed message if artifact
  cannot be resolved."
  [old-exception {:keys [artifactId version] :as art}]

  (if-let [vmap (ver-parse version)]
    (let [prjs (find-matching-projects repos-home (merge vmap art))]
      (when (empty? prjs)
        (throw (ex-info (str "No project found for " artifactId " " version
                             " (Hint: might need to clone a new repo into "
                             repos-home ")")
                        {:artifact art :vmap vmap} old-exception)))
      (when (< 1 (count prjs))
        (println "WARNING: multiple projects match" artifactId ":" )
        (doseq [prj prjs]
          (println "->" (:root prj))))
      (doseq [prj prjs]
        (install-versioned-artifact (:root prj))))
    (throw (ex-info (str "Not parseable as git-version: " version) {:artifact art} old-exception))))

(def null-writer
  "Like /dev/null, but for Java!"
  (proxy [java.io.Writer] []
    (write ([_]) ([_ _ _]))
    (flush [])
    (close [])))

(defn try-once-resolve-voom-version [project]
  (try
    (with-log-level Level/OFF
      #(binding [*err* null-writer]
         (leiningen.core.classpath/resolve-dependencies :dependencies project)))
    :ok
    (catch Exception e
      ;; lein resolve-dependencies wraps a
      ;; DependencyResolutionException in an ex-info, so if we want
      ;; the real cause of failure we have to dig for it:
      (if-let [arts (seq (missing-artifacts-from-exception e))]
        (doseq [art arts]
          (resolve-artifact e art))
        (throw e)))))

(defn build-deps
  "Resolves project dependencies like 'lein deps', but also uses REPOS_HOME"
  [project & args]
  (try
    (loop []
      (when-not (= :ok (try-once-resolve-voom-version project))
        (recur)))
    (catch clojure.lang.ExceptionInfo e
      (println "Failed to resolve dependency:" (.getMessage e))
      (pprint {:exception-data (ex-data e)}))))


;; === lein entrypoint ===

(defn nope [& args]
  (println "That voom sub-comand is not yet implemented."))

;; TODO: Consider revamping these entry points. Separate top-level
;; lein commands?  Separate lein plugins?
(def sub-commands
  {"build-deps" build-deps
   "freshen" nope
   "task-add" nope
   "new-task" nope})

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
  (let [[kstrs sargs] (split-with #(.startsWith ^String % ":") args)
        kargset (set (map #(keyword (subs % 1)) kstrs))
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
     :else (if-let [f (get sub-commands (first sargs))]
             (apply f nnproj (rest sargs))
             (if (and (dirty-wc? (:root project))
                      (not (:insanely-allow-dirty-working-copy kargset)))
               (lmain/abort "Refusing to continue with dirty working copy. (Hint: Run 'git status')")
               (lmain/resolve-and-apply nnproj sargs))))))
