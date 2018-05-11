#!/bin/bash

set -e

#export PATH=$PATH:$(readlink -f .)

IGNORE="target/\npom.xml"

git config --global user.name "Voom Test"
git config --global user.email "none@yo.biz"

mkdir /test
cd test

################################################################################
# Create dependency project and bad project in same repo
################################################################################

mkdir repo1
cd repo1

### badproj ###

# Must come first (alphabetically?) due to laziness
mkdir badproj
cat <<EOF > badproj/project.clj
#=(DOES_NOT_EXIST)
(defproject badproj "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0"]])
EOF

mkdir -p badproj/src/badproj/
echo "(ns badproj.core)" > badproj/src/badproj/core.clj

echo -e "$IGNORE" > badproj/.gitignore

### proj1 ###

mkdir proj1
cat <<EOF > proj1/project.clj
(defproject proj1 "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0"]])
EOF

mkdir -p proj1/src/proj1/
echo "(ns proj1.core)" > proj1/src/proj1/core.clj

echo -e "$IGNORE" > proj1/.gitignore

git init
git add .
git commit -m "initial commit"

# NOTE: We're only *building* the jar, not installing it so the build-deps triggers
# the scan of the other projects and hits the bad project first.
ver=$(cd proj1 && lein voom wrap jar |sed 's/\.jar//' |sed 's#.*/##' |sed 's/-/ /')

read proj1 proj1_ver <<<"$ver"
echo "Installed: ${proj1} ${proj1_ver}"

cd -

################################################################################
# Create project depending on proj1
################################################################################

mkdir repo2
cd repo2

cat <<EOF > project.clj
(defproject proj2 "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 ^{:voom {:repo "/test/repo1"}}
                 [${proj1} "${proj1_ver}"]])
EOF

mkdir -p src/proj2/
echo "(ns proj2.core)" > src/proj2/core.clj

lein voom build-deps

echo "Successfully built proj2"

exit 0

echo -e "$IGNORE" > .gitignore

git init
git add .
git commit -m "initial commit"

ver=$(lein voom wrap install |sed 's/\.jar//' |sed 's#.*/##' |sed 's/-/ /')

read proj2 proj2_ver <<<"$ver"
echo "Installed: ${proj2} ${proj2_ver}"

cd -

################################################################################
# Test freshen
################################################################################

cd proj2
lein deps
lein voom freshen
lein voom build-deps
cd -



