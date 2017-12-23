#!/bin/bash

set -e

#export PATH=$PATH:$(readlink -f .)

IGNORE="target/\npom.xml"

git config --global user.name "Voom Test"
git config --global user.email "none@yo.biz"

mkdir /test
cd test

################################################################################
# Create dependency project
################################################################################

mkdir dep1
cd dep1

cat <<EOF > project.clj
(defproject dep1 "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0"]])
EOF

mkdir -p src/dep1/
echo "(ns dep1.core)" > src/dep1/core.clj

echo -e "$IGNORE" > .gitignore

git init
git add .
git commit -m "initial commit"

ver=$(lein voom wrap install |sed 's/\.jar//' |sed 's#.*/##' |sed 's/-/ /')

read dep1 dep1_ver <<<"$ver"
echo "Installed: ${dep1} ${dep1_ver}"

cd -

################################################################################
# Create project depending on dep1
################################################################################

mkdir proj
cd proj

cat <<EOF > project.clj
(defproject proj "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 ^{:voom {:repo "/test/dep1"}}
                 [${dep1} "${dep1_ver}"]])
EOF

mkdir -p src/proj/
echo "(ns proj.core)" > src/proj/core.clj

echo -e "$IGNORE" > .gitignore

git init
git add .
git commit -m "initial commit"

ver=$(lein voom wrap install |sed 's/\.jar//' |sed 's#.*/##' |sed 's/-/ /')

read proj proj_ver <<<"$ver"
echo "Installed: ${proj} ${proj_ver}"

cd -

################################################################################
# Add new version to dep
################################################################################

cd dep1
echo "(def foo 1)" >> src/dep1/core.clj
git commit -a  -m "New version"
cd -

################################################################################
# Test freshen
################################################################################

cd proj
lein deps
lein voom freshen
lein voom build-deps
cd -



