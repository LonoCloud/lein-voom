#!/bin/bash

set -e

LEIN_VOOM_VERSION=${LEIN_VOOM_VERSION:-0.1.0-20171223_174652-g4b97ccc}
# lein-voom currently generates a stacktrace for lein 2.8.1
#LEIN_TEST_VERSIONS=${LEIN_TEST_VERSIONS:-2.6.1 2.7.1 2.8.1}
LEIN_TEST_VERSIONS=${LEIN_TEST_VERSIONS:-2.6.1 2.7.1}
LEIN_TESTS=${LEIN_TESTS:-scripts/lcp_test.sh}

for ver in $LEIN_TEST_VERSIONS; do
    docker build -t lein-test:${ver} --build-arg LEIN_VER=${ver} --build-arg VOOM_VER=${LEIN_VOOM_VERSION} .
    for test in $LEIN_TESTS; do
        docker run -i --rm \
                --name lein-test \
                -v $(readlink -f scripts/):/scripts \
                -w / \
                lein-test:${ver} \
                "${test}"
    done
done
