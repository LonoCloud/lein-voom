(defproject lein-voom "0.1.0-SNAPSHOT"
  :description "Tool for generating artifacts versioned on the most recent git commit sha and commit time."
  :url "https://github.com/LonoCloud/lein-voom"
  :scm {:name "git"
        :url "https://github.com/LonoCloud/lein-voom"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[clj-glob "1.0.0"]
                 [com.datomic/datomic-free "0.9.4324"
                  :exclusions [org.slf4j/slf4j-nop org.slf4j/slf4j-log4j12]]]
  :signing {:gpg-key "A4D5C342"}
  :eval-in-leiningen true)
