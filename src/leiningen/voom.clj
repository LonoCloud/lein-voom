(ns leiningen.voom
  (:require [clojure.java.shell :as shell]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint print-table]]
            [clojure.java.io :as io]
            [leiningen.core.project :as project]
            [leiningen.core.main :as lmain]
            [org.satta.glob :refer [glob]]
            [robert.hooke :as hooke])
  (:import [java.util Date]
           [java.io File]
           [java.util.logging Logger Handler Level]
           [org.sonatype.aether.transfer ArtifactNotFoundException]))

(set! *warn-on-reflection* true)

(defn sh
  [& cmdline]
  (apply shell/sh (map #(if (= File (class %))
                          (.getPath ^File %)
                          %)
                       cmdline)))

(defn git
  [{:keys [^File gitdir ok-statuses]
    :or {ok-statuses #{0}}} & subcmd]
  ;; We won't handle bare repos or displaced worktrees
  (let [dir-args (if (nil? gitdir)
                   []
                   [(str "--git-dir=" (.getPath gitdir))
                    (str "--work-tree=" (.getParent gitdir))])
        all-args (concat dir-args subcmd)
        ;; _ (prn :calling (doall (cons 'git all-args)))
        {:keys [exit] :as rtn} (apply sh "git" all-args)]
    (when-not (ok-statuses exit)
      (throw (ex-info "git error" (assoc rtn :git all-args))))
    (assoc rtn :lines (when (not= "\n" (:out rtn))
                        (re-seq #"(?m)^.*$" (:out rtn))))))

;; === git sha-based versions ===

(def timestamp-fmt "yyyyMMdd_hhmmss")

(defn formatted-timestamp
  [^String fmt t]
  (.format (doto (java.text.SimpleDateFormat. fmt java.util.Locale/US)
             (.setTimeZone (java.util.SimpleTimeZone. 0 "GMT")))
           t))

(defn get-voom-version
  [path & {:keys [long-sha? gitdir]}]
  (let [shafmt (if long-sha? "%H" "%h")
        fmt (str "--pretty=" shafmt ",%cd")
        {:keys [out]} (git {:gitdir gitdir} "log" "-1" fmt path)
        [sha, datestr] (-> out s/trim (s/split #"," 2))
        ctime (Date. ^String datestr)]
    {:ctime ctime :sha sha}))

(defn format-voom-ver
  [gver]
  (let [{:keys [ver ctime sha]} gver]
    (assert ver   (str "format-voom-ver requires :ver " (pr-str gver)))
    (assert ctime (str "format-voom-ver requires :ctime " (pr-str gver)))
    (assert sha   (str "format-voom-ver requires :sha " (pr-str gver)))
    (str (s/replace ver #"-SNAPSHOT" "")
         "-" (formatted-timestamp timestamp-fmt ctime) "-g" sha)))

(defn update-proj-version
  [project long-sha?]
  (let [gver (-> project :root (get-voom-version :long-sha? long-sha?))
        upfn #(format-voom-ver (assoc gver :ver %))
        nproj (update-in project [:version] upfn)
        nmeta (update-in (meta project) [:without-profiles :version] upfn)
        nnproj (with-meta nproj nmeta)]
    nnproj))

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
  (let [{:keys [out err exit]} (git {} "status" "--short" path)]
    (not (empty? out))))


;; === manage REPOS_HOME directory ===

(def task-dir "/.voom")

(def repos-home (or (System/getenv "REPOS_HOME")
                    (str (System/getProperty "user.home")
                         (str "/.voom-repos"))))

(defn remotes
  "For a given .git directory, returns a map like:
  {:origin {:push  \"git@github.com:abrooks/lein-voom.git\",
            :fetch \"git@github.com:abrooks/lein-voom.git\"}}"
  [gitdir]
  (reduce
   (fn [m line]
     (let [[_ remote url direction] (re-matches #"(.*)\t(.*)\s+\(([^)]*)\)$" line)]
       (assoc-in m [(keyword remote) (keyword direction)] url)))
   {}
   (:lines (git {:gitdir gitdir} "remote" "-v"))))

(defn with-log-level [level f]
  (let [handlers (doall (.getHandlers (Logger/getLogger "")))
        old-levels (doall (map #(.getLevel ^Handler %) handlers))
        _ (doseq [h handlers] (.setLevel ^Handler h level))
        result (f)]
    (dorun (map #(.setLevel ^Handler %1 %2) handlers old-levels))
    result))

(defn find-project-files
  [^File d]
  (let [{:keys [lines]} (git {:gitdir d} "ls-files" "project.clj" "**/project.clj")]
    (map #(str (.getParent d) "/" %) lines)))

(defn contains-sha? [d sha]
  (->>
   (git {:gitdir d :ok-statuses #{0 1}} "rev-parse" "--verify" "--quiet" sha)
   :exit
   zero?))

(defn locate-sha
  [dirs sha]
  (seq (for [d dirs
             :when (contains-sha? d sha)]
         d)))

(defn all-repos-dirs []
  (glob (str (s/replace repos-home #"/$" "") "/*/.git")))

(defn fetch-all
  [dirs]
  (doseq [^File d dirs]
    (print (str "Fetching: " (.getPath d) "\n"))
    (flush)
    (git {:gitdir d} "fetch")))

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
  ;; TODO: find correct sha and project.clj more efficiently and with
  ;; less ambiguity. (only consider projects changed at the given sha)
  (let [{:keys [sha artifactId groupId]} pspec
        dirs (all-repos-dirs)
        sha-candidates (locate-sha dirs sha)
        sha-candidates (or sha-candidates
                           (do (fetch-all dirs)
                               (locate-sha dirs sha)))]
    (prn "sha-candidates" sha-candidates)
    (mapcat (fn [c]
              (git {:gitdir c} "checkout" sha)
              (find-project groupId artifactId c))
            sha-candidates)))

(defn install-versioned-artifact
  [proot]
  (let [install-cmd ["lein" "voom" "install" :dir proot]
        _ (apply println "install-versioned-artifact:" install-cmd)
        rtn (sh "lein" "voom" "install" :dir proot)]
    (when-not (zero? (:exit rtn))
      (throw (ex-info "lein voom install error" (assoc rtn :cmd install-cmd))))
    rtn))

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
    (throw (ex-info (str "Not parseable as voom-version: " version) {:artifact art} old-exception))))

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


;; === freshen ===

(defn fetch-checkout-all
  "This is slower than necessary for most real use cases."
  [dirs]
  (doseq [^File d dirs]
    (println "Checking out latest:" (.getPath d))
    (git {:gitdir d} "fetch")
    (git {:gitdir d} "checkout" "origin/HEAD")))

(defn read-project [gitdir sha prj-path]
  (let [tmp-file (File/createTempFile ".project-" ".clj")
        _ (spit tmp-file
                (:out (git {:gitdir gitdir} "show" (str sha ":" prj-path))))
        prj (try (assoc (project/read (str tmp-file))
                   :root (or (.getParent (io/file prj-path)) ""))
                 (catch Throwable t
                   (throw (ex-info "Error reading project file"
                                   {:project-file prj-path
                                    :git-sha sha
                                    :git-dir gitdir}
                                   t))))]
    (.delete tmp-file)
    prj))


(defn patch-fn
  [f default-val]
  (fn [filename & args]
    (if (re-find #"\.lein|project[^/]*\.clj$" filename)
      (apply f filename args)
      default-val)))

;; Shhh...
(.setDynamic #'slurp)
(.setDynamic #'load-file)
(defn robust-read-project
  [gitdir sha prj-path]
  (try
    ;; Hack to work around our crazy project.clj files
    (binding [slurp (patch-fn slurp "{}")
              load-file (patch-fn load-file {})]
      (read-project gitdir sha prj-path))
    (catch Exception e
      ;; 128 means git complained about
      ;; something. Probably a non-existant
      ;; project.clj at this sha.
      (when-not (= 128 (:exit (ex-data e)))
        (println "Ignoring error:" (pr-str e)))
      nil)))

(defn origin-branches
  [gitdir]
  (->> (git {:gitdir gitdir} "rev-parse"
            "--symbolic-full-name" "--remotes=origin/")
       :lines
       (map #(re-find #"[^/]+$" %))
       distinct))

(defn report-progress
  "Eagerly consume xs, but return a lazy seq that reports progress
  every half second as the returned seq is consumed."
  [msg xs]
  (concat
   (let [last-report (clojure.lang.Box. 0)
         c (count xs)
         digits (inc (long (quot (Math/log c) (Math/log 10))))]
     (map-indexed
      (fn [i x]
        (let [now (System/currentTimeMillis)]
          (when (or (= (inc i) c) (< 500 (- now (.val last-report))))
            (set! (.val last-report) now)
            (printf (str "\r%s %" digits "d/%d ...") msg (inc i) c)
            (flush)))
        x)
      xs))
   (lazy-seq (println "done"))))

(defn parse-sha-refs
  [s]
  (let [[sha datestr parents refstr] (vec (.split #"," s 4))
        refs (when refstr
               (when-let [[_ x] (re-find #"\((.*)\)" refstr)]
                 (mapv #(s/replace % #"^tag: " "") (.split #",\s+" x))))]
    {:sha sha, :ctime (Date. ^String datestr), :parents parents, :refs refs}))

(defn project-change-shas
  [gitdir & opts]
  (->> (apply git {:gitdir gitdir} "log" "--pretty=format:%H,%cd,%p,%d"
              "--name-status" "-m" opts)
       :lines
       (keep #(if-let [[_ op path] (re-matches #"(.)\t(.*)" %)]
                (when (re-find #"(^|/)project\.clj$" path)
                  {:op op :path path})
                (when (seq %)
                  (parse-sha-refs %))))
       (#(concat % [{:sha "end sigil"}]))
       (reductions (fn [[partial complete] entry]
                     (if (:sha entry)
                       [entry partial]
                       [(update-in partial [:ops] (fnil conj []) entry) nil]))
                   [nil nil])
       (keep second)
       (filter :ops)))

(defn tag-repo-projects
  [gitdir]
  (let [branches (origin-branches gitdir)
        proj-shas (apply project-change-shas gitdir
                         "--not" "--tags=voom-branch--*"
                         (map #(str #_double-negative--> "^origin/" %) branches))]

    ;; add missing voom-- tags
    (doseq [:when (seq proj-shas)
            {:keys [sha refs parents ops]} (report-progress gitdir proj-shas)
            {:keys [op path]} ops]
      (when-let [p (and (not= "D" op)
                        (robust-read-project gitdir sha path))]
        (let [tag (s/join "--" (-> ["voom"
                                    (str (:group p) "%" (:name p))
                                    (:version p)
                                    (s/replace (:root p) #"/" "%")
                                    (subs sha 0 7)]
                                   (cond-> (empty? parents)
                                     (conj "no-parent"))))]
          (git {:gitdir gitdir} "tag" "-f" tag sha))))

    ;; TODO: clean up abandoned voom-- and voom-branch-- tags
    ;; Update all voom-branch-- tags
    (doseq [branch branches]
      (let [tag (str "voom-branch--" branch)]
        (git {:gitdir gitdir} "tag" "-f" tag (str "origin/" branch))))))

(defn tag-all-repos
  [& {:keys [fetch]}]
  (->> (all-repos-dirs)
       (map #(future
               (when fetch
                 (fetch-all [%]))
               (tag-repo-projects %)))
       doall
       (map deref)
       dorun))

(defn clear-voom-tags
  [gitdir]
  (let [tags (->> (git {:gitdir gitdir} "tag" "--list" "voom-*")
                  :lines
                  (remove empty?))]
    (when (seq tags)
      (apply git {:gitdir gitdir} "tag" "--delete" tags)
      nil)))

(defn retag
  "Clear and retag all repos in .voom-repos"
  [project & args]
  (doseq [gitdir (all-repos-dirs)]
    (clear-voom-tags gitdir)
    (tag-repo-projects gitdir)))

(defn parse-tag
  [tag]
  (->
   (zipmap [:prefix :proj :ver :path :sha :no-parent] (s/split tag #"--"))
   (update-in [:path] (fnil #(s/replace % #"%" "/") :NOT_FOUND))
   (update-in [:proj] (fnil #(s/replace % #"%" "/") :NOT_FOUND))))

(defn newest-voom-ver-by-spec
  [proj-name ver-spec {:keys [repo branch path]}]

  (for [gitdir (all-repos-dirs)
        :when (or (nil? repo) (= repo (-> (remotes gitdir) :origin :fetch)))
        :let [ptn (s/join "--" ["voom"
                                (str (namespace proj-name) "%" (name proj-name))
                                (str ver-spec "*")])
              tags (:lines (git {:gitdir gitdir} "tag" "--list" ptn))
              ;;_ (prn :tags tags)
              tspecs (if (= tags [""])
                       []
                       (map parse-tag tags))
              paths (set (map :path tspecs))]
        found-path paths
        :when (or (= found-path path) (nil? path))
        found-branch (origin-branches gitdir)
        :when (or (= found-branch branch) (nil? branch))
        :let [tags-with-parents (remove #(.endsWith ^String % "--no-parent") tags)
              neg-tags (map #(str "^" % "^") tags-with-parents)
              commits
              , (map
                 parse-sha-refs
                 (:lines (apply git {:gitdir gitdir} "log"
                                "--pretty=format:%H,%cd,%p,%d" "--reverse"
                                (concat neg-tags [(str "origin/" found-branch) "--" found-path]))))]
        :when (seq commits)]
    (let [refs (-> commits first :refs)
          ;; _ (pprint {:gitdir gitdir :path found-path :branch found-branch :commits commits})
          reflist (filter #(and
                            (= (str proj-name) (:proj %))
                            (= found-path (:path %))) (map parse-tag refs))]
      (some (fn [[current next-commit]]
              (when (or (= :end next-commit)
                        (some #(= found-path (:path (parse-tag %)))
                              (:refs next-commit)))
                {:sha (:sha current)
                 :ctime (:ctime current)
                 :ver (-> reflist first :ver)
                 :path found-path
                 :proj proj-name
                 :gitdir gitdir
                 :branch found-branch}))
            (partition 2 1 (concat commits [:end]))))))

(defn print-repo-infos
  [repo-infos]
  (->> repo-infos
       (map (fn [info]
              (-> info
                  (dissoc :gitdir)
                  (assoc :repo (-> (remotes (:gitdir info)) :origin :fetch))
                  (update-in [:sha] #(subs (str % "--------") 0 7)))))
       (sort-by :ctime)
       (print-table [:repo :proj :ver :branch :path :ctime :sha]))
  (newline))

(defn fresh-version [[prj ver :as dep]]
  (let [voom-meta (:voom (meta dep))
        ver-spec (or (:version voom-meta)
                     (re-find #"^[^.]+." ver))
        groups (->> (newest-voom-ver-by-spec prj ver-spec voom-meta)
                    (map #(assoc % :voom-ver (format-voom-ver
                                              (update-in % [:sha] subs 0 7))))
                    (group-by :voom-ver))]
    (case (count groups)
     0 (do (println "No matching version found for" prj (pr-str ver-spec))
           dep)
     1 (assoc dep 1 (key (first groups)))
     (do (print "\nMultiple bump resolutions for:"
                prj (pr-str ver-spec) (pr-str voom-meta))
         (print-repo-infos (map #(first (val %)) groups))
         dep))))

(defn rewrite-project-file [input-str replacement-map]
  (reduce (fn [^String text [[prj old-ver :as old-dep] [_ new-ver]]]
            (let [short-prj (if (= (name prj) (namespace prj))
                              (name prj)
                              (str prj))
                  pattern (re-pattern (str "\\Q" short-prj "" "\\E(\\s+)\\Q\"" old-ver "\"\\E"))
                  matches (re-seq pattern input-str)]
              (when (empty? matches)
                (throw (ex-info (str "No match found for " [prj old-ver])
                                {:pattern pattern :dep old-dep})))
              (when (second matches)
                (throw (ex-info (str "More than one match found for " [prj old-ver])
                                {:pattern pattern :dep old-dep})))
              (s/replace text pattern (str short-prj "$1" \" new-ver \"))))
          input-str
          replacement-map))

(defn freshen [project & args]
  (tag-all-repos :fetch true)
  (let [prj-file-name (str (:root project) "/project.clj")
        old-deps (:dependencies project)
        desired-new-deps (doall (map #(fresh-version %) old-deps))]
    (doseq [[[prj old-ver] [_ new-ver]] (map list old-deps desired-new-deps)]
      (println (format "%-40s" prj)
               (str old-ver
                    (when (not= old-ver new-ver)
                      (str " -> " new-ver)))))
    (if (= old-deps desired-new-deps)
      (println "No versions bumped.")
      (let [replacement-map (into {} (map #(when (not= %1 %2) [%1 %2])
                                          old-deps desired-new-deps))
            tmp-file (File/createTempFile
                      ".project-" ".clj" (File. ^String (:root project)))]

        (spit tmp-file
              (rewrite-project-file (slurp prj-file-name) replacement-map))

        (if (= desired-new-deps (:dependencies (project/read (str tmp-file))))
          (.renameTo tmp-file (File. prj-file-name))
          (throw (ex-info (str "Freshen mis-fire. See "
                               tmp-file " for attempted change.")
                          {:old-deps old-deps
                           :replacement-map replacement-map
                           :desired-new-deps desired-new-deps
                           :tmp-file-name (str tmp-file)})))))))

(defn resolve-short-proj
  [dep]
  (let [proj-set
        , (into #{}
                (flatten
                 (for [g (all-repos-dirs)]
                   (map #(s/replace (second (s/split % #"--")) #"%" "/")
                        (:lines (git {:gitdir g} "tag" "--list" "voom--*"))))))]
    (for [proj proj-set
          :when (.contains ^String proj dep) ]
      proj)))

(defn adj-path
  ([f path] (adj-path false f path))
  ([dir-flag f path]
     (->
      ;; TODO Should this be made an isDirectory check?
      (if (and = dir-flag (.isFile ^File f))
        (.getParentFile ^File f)
        ^File f)
      (.getCanonicalPath)
      ^String (str "/" path)
      (File.))))

(defn find-box
  "Locates voom-box root starting from current working directory."
  [& args]
  (loop [path (File. (System/getProperty "user.dir"))]
    (let [ppath (.getCanonicalPath path)
          pfile (File. (str ppath "/.voom-box"))]
      (when-not (= "/" ppath)
        (if (.exists pfile)
          path
          (recur (.getParentFile path)))))))

(defn box-repo-add
  [{:keys [gitdir branch sha proj path]}]
  (if-let [bdir (find-box)]
    (let [pname (-> (str proj)
                    (s/replace #"/" "--"))
          pdir (adj-path bdir pname)
          checkout (adj-path bdir (str ".voom-box/" pname))]
      (println "Found box at:" bdir pdir)
      (if (.exists ^File checkout)
        (git {:gitdir (adj-path checkout "/.git")}
             "fetch")
        (git {} "clone" gitdir
             "--refer" gitdir
             checkout))
      (git {:gitdir (adj-path checkout "/.git")}
           "checkout" sha) ; must detach head for update...
      (git {:gitdir (adj-path checkout "/.git")}
           "branch" "-f" branch sha)
      (git {:gitdir (adj-path checkout "/.git")}
           "checkout" branch)
      (sh "rm" "-f" pdir)
      ;; (SYMLINK WILL NEED TO BE UPDATED FOR EACH CHECKOUT)
      (sh "ln" "-s" (adj-path checkout path) pdir))
    (println "Can't find box")))

(defn box-add
  [proj & deps]
  (tag-all-repos)
  (doseq [dep deps]
    (let [full-projs (resolve-short-proj (name dep))
          full-projs (map symbol full-projs)
          repo-infos (mapcat #(newest-voom-ver-by-spec % "" {}) full-projs)]
      (if (< 1 (count repo-infos))
        (do
          (print "Multiple projects / locations match" (str \" dep \"\:))
          (print-repo-infos repo-infos))
        (box-repo-add (first repo-infos))))))

;; === lein entrypoint ===

(defn nope [& args]
  (println "That voom sub-comand is not yet implemented."))

;; TODO: Consider revamping these entry points. Separate top-level
;; lein commands?  Separate lein plugins?
(def sub-commands
  {"build-deps" build-deps
   "freshen" freshen
   "retag" retag
   "new-task" nope})

(defn ^:no-project-needed voom
  "Usage:
    lein voom [flags] [lein command ...]
      Runs lein command with a project version augmented with git
      version of the most recent change of this project directory.
      Flags include:
        :insanely-allow-dirty-working-copy - by default voom
          refuses to handle a dirty working copy
        :no-upstream - by default voom wants to see the current
          version reachable via an upstream repo
        :long-sha - uses a full length sha instead of the default
          short form
    lein voom [:long-sha] :print
    lein voom :parse <version-str>"
  [project & args]
  (let [[kstrs sargs] (split-with #(.startsWith ^String % ":") args)
        kargset (set (map #(keyword (subs % 1)) kstrs))
        long-sha (kargset :long-sha)
        new-project (delay (update-proj-version project long-sha))]
    ;; TODO throw exception if upstream doesn't contain this commit :no-upstream
    (cond
     (:print kargset) (println (:version @new-project))
     (:parse kargset) (prn (ver-parse (first sargs)))
     (:find-box kargset) (prn (find-box))
     (:box-add kargset) (apply box-add project sargs)
     :else (if-let [f (get sub-commands (first sargs))]
             (apply f @new-project (rest sargs))
             (if (and (dirty-wc? (:root @new-project))
                      (not (:insanely-allow-dirty-working-copy kargset)))
               (lmain/abort "Refusing to continue with dirty working copy. (Hint: Run 'git status')")
               (lmain/resolve-and-apply @new-project sargs))))))
