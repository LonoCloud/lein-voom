(ns leiningen.resolve-git-version
  (:require [cemerick.pomegranate.aether :as aether]
            [clojure.pprint :refer [pprint]]
            [leiningen.git-version :as gv])
  (:import (org.sonatype.aether.transfer ArtifactNotFoundException)
           (java.util.logging Logger Handler Level)))

(set! *warn-on-reflection* true)

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
  [old-exception {:keys [version] :as art}]

  (if-let [{:keys [ctime sha]} (gv/ver-parse version)]
    (do
      (println "TODO: Build and install git version for artifact" (pr-str art))
      (throw (ex-info "TODO" {:ctime ctime :sha sha} old-exception)))
    (throw (ex-info (str "Not parseable as git-version: " version) {:artifact art} old-exception))))

(defn with-log-level [level f]
  (let [handlers (doall (.getHandlers (Logger/getLogger "")))
        old-levels (doall (map #(.getLevel ^Handler %) handlers))
        _ (doseq [h handlers] (.setLevel ^Handler h level))
        result (f)]
    (dorun (map #(.setLevel ^Handler %1 %2) handlers old-levels))
    result))

(def null-writer
  (proxy [java.io.Writer] []
    (write ([_]) ([_ _ _]))
    (flush [])
    (close [])))

(defn try-once-resolve-git-version [project]
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

(defn resolve-git-version
  "Resolves project dependencies like 'lein deps', but also uses TOOL_REPOS_DIR"
  [project & args]

  (try
    (loop []
      (when-not (= :ok (try-once-resolve-git-version project))
        (recur)))
    (catch clojure.lang.ExceptionInfo e
      (println "Failed to resolve dependency:" (.getMessage e))
      (pprint {:exception-data (ex-data e)}))))
