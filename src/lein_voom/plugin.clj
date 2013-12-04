(ns lein-voom.plugin
  (:require [leiningen.core.classpath :as lcp]
            [leiningen.voom :as voom]
            [robert.hooke :as hooke])
  (:import [java.io File]))

(set! *warn-on-reflection* true)

;; == Hooke hooks ==

;; Priate defn- from leinignen.core.classpath
(defn- checkout-dep-paths [project dep-project]
  ;; can't mapcat here since :checkout-deps-shares points to vectors and strings
  (flatten (map #(% dep-project) (:checkout-deps-shares project))))

(defn checkout-deps-paths
  "Checkout dependencies are used to place source for a dependency
  project directly on the classpath rather than having to install the
  dependency and restart the dependent project."
  [orig-fn project]
  (->
   (when-let [f  (voom/find-box)]
     (apply concat
            (for [[d & _] (keys (lcp/get-dependencies :dependencies project))
                  :let [dep-name (str (or (namespace d) (name d)) "--" (name d))
                        proj-path (voom/adj-path f dep-name "project.clj")]
                  :when (.isFile proj-path)
                  ;; The below is from lcp/read-dependency-project:
                  ;;   TODO: core.project and core.classpath currently rely upon each other *uk*
                  :let [_ (require 'leiningen.core.project)
                        dep-proj
                        , (try ((resolve 'leiningen.core.project/read) (.getAbsolutePath proj-path) [:default])
                               (catch Exception e
                                 (throw (Exception. (format "Problem loading %s" project) e))))]
                  :when dep-proj]
              (checkout-dep-paths project dep-proj))))
   (concat (orig-fn project))))


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
