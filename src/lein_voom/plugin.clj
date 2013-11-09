(ns lein-voom.plugin
  (:require [leiningen.core.classpath :as lcp]
            [leiningen.voom :as voom]
            [robert.hooke :as hooke])
  (:import [java.io File]))

(set! *warn-on-reflection* true)

;; == Hooke hooks ==

(defn checkout-deps-paths
  "Checkout dependencies are used to place source for a dependency
  project directly on the classpath rather than having to install the
  dependency and restart the dependent project."
  [orig-fn project]
  (let [f  (-> (:root project) (str "/.." voom/task-dir) File.)]
    (if (.isDirectory f)
      (for [[d & _] (keys (lcp/get-dependencies :dependencies project))
            :let [dep-path (str (:root project) "/../" (name d))]
            :when (-> dep-path File. .isDirectory)]
        dep-path)
      (orig-fn project))))


(defn hooks []
  (hooke/add-hook #'lcp/checkout-deps-paths
                  #'checkout-deps-paths))

(comment
  ;; Note for future travelers:
  (prn "deps0" (:dependencies project)) ; top level deps
  (prn "deps1" (lcp/get-dependencies :dependencies project)) ; transitive deps, flat
  (prn "deps2" (lcp/dependency-hierarchy :dependencies project)) ; transitive deps, nested
  (prn "deps3" (lcp/resolve-dependencies :dependencies project)) ; transitive deps, jars
  )