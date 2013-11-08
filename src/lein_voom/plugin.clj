(ns lein-voom.plugin
  (:require [leiningen.core.classpath :as lcp]
            [robert.hooke :as hooke]))

(set! *warn-on-reflection* true)

;; == Hooke hooks ==

(defn checkout-deps-paths
  "Checkout dependencies are used to place source for a dependency
  project directly on the classpath rather than having to install the
  dependency and restart the dependent project."
  [project & args]
  (prn "Calling hook" args)
  [])


(defn hooks []
  (prn "Activating voom hook")
  (hooke/add-hook #'lcp/checkout-deps-paths
                  #'checkout-deps-paths))

(prn "loading hooks namespace")