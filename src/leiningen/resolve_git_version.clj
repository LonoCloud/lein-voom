(ns leiningen.resolve-git-version
  (:require [cemerick.pomegranate.aether :as aether]
            [clojure.java.shell :refer [sh]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as s]
            [leiningen.core.project :as project]
            [leiningen.git-version :as gv]
            [org.satta.glob :refer [glob]])
  (:import (org.sonatype.aether.transfer ArtifactNotFoundException)
           (java.util.logging Logger Handler Level)))

(set! *warn-on-reflection* true)

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
  [d & subcmd]
  ;; We won't handle bare repos or displaced worktrees
  (let [gitdir (.getPath d)
        worktree (.getParent d)]
    (apply sh "git" (str "--git-dir=" gitdir) (str "--work-tree=" worktree) subcmd)))

(defn find-project-files
  [d]
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
  (doseq [d dirs]
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

(defn resolve-artifact
  "Build and install given artifact using git. Return value is
  undefined. Throws an exception with detailed message if artifact
  cannot be resolved."
  [old-exception {:keys [artifactId version] :as art}]

  (if-let [vmap (gv/ver-parse version)]
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

(defn resolve-voom-version
  "Resolves project dependencies like 'lein deps', but also uses TOOL_REPOS_DIR"
  [project & args]
  (try
    (loop []
      (when-not (= :ok (try-once-resolve-voom-version project))
        (recur)))
    (catch clojure.lang.ExceptionInfo e
      (println "Failed to resolve dependency:" (.getMessage e))
      (pprint {:exception-data (ex-data e)}))))