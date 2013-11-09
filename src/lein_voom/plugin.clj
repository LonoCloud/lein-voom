(ns lein-voom.plugin
  (:require [leiningen.core.classpath :as lcp]
            [robert.hooke :as hooke]))

(set! *warn-on-reflection* true)

;; == Hooke hooks ==

(defn checkout-deps-paths
  "Checkout dependencies are used to place source for a dependency
  project directly on the classpath rather than having to install the
  dependency and restart the dependent project."
  [_ project]
  ;; TODO: Optimize classpath searches by filtering for only currenly
  ;; existing directories?
  (for [[d & _] (keys (lcp/get-dependencies :dependencies project))]
       (str (:root project) "/../" (name d))))


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