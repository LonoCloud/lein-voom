(defproject lein-voom "0.1.0-SNAPSHOT"
  :description "Tool for generating artifacts versioned on the most recent git commit sha and commit time."
  :url "https://github.com/LonoCloud/lein-voom"
  :scm {:name "git"
        :url "https://github.com/LonoCloud/lein-voom"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[aysylu/loom "0.5.0"]
                 [clj-glob "1.0.0"]
                 [lonocloud/synthread "1.0.5"]
                 [org.clojure/data.codec "0.1.0"]
                 [org.clojure/data.fressian "0.2.0"]
                 [org.clojure/core.logic "0.8.5"]
                 [org.clojure/test.check "0.6.1"]]
  :signing {:gpg-key "A4D5C342"}
  :repositories [["sonatype-snapshot" "http://oss.sonatype.org/content/repositories/snapshots"]]
  :eval-in-leiningen true
  :profiles {:leiningen-core-2.5.1 {:dependencies [[leiningen-core "2.5.1"]]}
             :clojure-1.5 {:dependencies [[org.clojure/clojure "1.5.0"]]}
             :clojure-1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}})
