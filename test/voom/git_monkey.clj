(ns voom.git-monkey
  (:require [leiningen.voom :as voom]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defn check-sh-rtn
  ([rtn] (check-sh-rtn rtn #{0}))
  ([rtn ok]
     (when-not (ok (:exit rtn))
       (throw (ex-info (:err rtn) rtn)))
     rtn))

(defn sh [& args]
  (check-sh-rtn (apply sh/sh args)))

(defn git-commit [root-path repo]
  (sh/with-sh-dir (str root-path "/" repo)
    (sh "git" "add" ".")
    (sh "git" "commit" "-av" "--author=git-monkey <git@monkey>"
        "-m" (str/join " " (repeatedly (+ 5 (rand-int 10))
                                       #(rand-nth '[git git! monkey monkey!
                                                    giiiit elbow pants]))))))

(defn affirm-repo [root-path repo]
  (let [git-dir (str root-path "/" repo)]
    (sh "mkdir" "-p" git-dir)
    (sh "git" "init" :dir git-dir)
    (sh "git" "config" "user.name" "git-monkey" :dir git-dir)
    (sh "git" "config" "user.email" "git@monkey" :dir git-dir)))

(defn ns-for-proj [proj-name]
  (symbol (str (namespace proj-name) "." (name proj-name))))

(defn main-for-proj [proj-name]
  (symbol (str (ns-for-proj proj-name)) "main"))

(defn affirm-proj [root-path {:keys [repo branch path proj-name
                                     version dependencies]}]
  (let [git-dir (str root-path "/" repo)
        src-dir (str git-dir "/" path "/src/" (namespace proj-name))]
    (sh/sh "git" "checkout" branch :dir git-dir)
    (sh "mkdir" "-p" src-dir)
    (spit (str git-dir "/" path "/project.clj")
          (binding [*print-meta* true]
            (prn-str `(~'defproject ~proj-name ~version
                        :dependencies ~(conj dependencies
                                             '[org.clojure/clojure "1.5.1"])
                        :main ~(main-for-proj proj-name)))))
    (spit (str src-dir "/" (name proj-name) ".clj")
          (str/join "\n"
                    (map #(with-out-str (pprint %))
                         `[(~'ns ~(ns-for-proj proj-name)
                             ~@(when (seq dependencies)
                                 `[(:require ~@(map ns-for-proj
                                                    (map first dependencies)))]))
                           (~'defn ~'main []
                             ~@(for [[proj] dependencies]
                                 (list (main-for-proj proj)))
                             (~'prn '~(ns-for-proj proj-name) ~version))])))))


(defn clone-to-voom-repos
  [root-path & repos]
  (doseq [repo repos]
    (sh/with-sh-dir root-path
      (sh "cp" "-a" repo (str repo "-upstream"))
      (sh "git" "clone" (str repo "-upstream") (str voom/voom-repos "/" repo)))))

(defn crank [root-path]
  (doto root-path
    (affirm-repo "crank")
    (affirm-proj {:repo "crank" :branch "master" :path "modules/foo"
                  :proj-name 'top/foo :version "1.0.0"})
    (git-commit "crank")

    (affirm-proj {:repo "crank" :branch "master" :path "modules/foo"
                  :proj-name 'top/foo :version "1.2.0"})
    (git-commit "crank")

    (affirm-proj {:repo "crank" :branch "master" :path "modules/bar"
                  :proj-name 'top/bar :version "1.0.0"
                  :dependencies '[^{:voom {:version "1.0"}} [top/foo "1.0.0"]]})
    (git-commit "crank")
    (clone-to-voom-repos "crank")))
