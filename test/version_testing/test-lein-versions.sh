#!/bin/bash

set -e

LEIN_VOOM_VERSION=${LEIN_VOOM_VERSION:-0.1.0-20171223_184737-g8fc284c}
# lein-voom currently generates a stacktrace for lein 2.8.1
#LEIN_TEST_VERSIONS=${LEIN_TEST_VERSIONS:-2.6.1 2.7.1 2.8.1}
LEIN_TEST_VERSIONS=${LEIN_TEST_VERSIONS:-2.6.1 2.7.1}
LEIN_TESTS=${LEIN_TESTS:-$(echo scripts/*)}

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
