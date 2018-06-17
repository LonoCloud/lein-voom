#!/bin/bash

set -e

LEIN_VOOM_VERSION=${LEIN_VOOM_VERSION:-0.1.0-20180511_185307-g3823723}
LEIN_TEST_VERSIONS=${LEIN_TEST_VERSIONS:-2.6.1 2.7.1 2.8.1}
LEIN_TESTS=${LEIN_TESTS:-$(echo scripts/*)}

for ver in $LEIN_TEST_VERSIONS; do
    # Attempt to copy in the appropriate pre-fetched or locally built lein-voom
    cp -a ~/.m2/repository/lein-voom/lein-voom/. artifacts/lein-voom/lein-voom/. || true
    docker build -t lein-test:${ver} --build-arg LEIN_VER=${ver} --build-arg VOOM_VER=${LEIN_VOOM_VERSION} .
    for test in $LEIN_TESTS; do
        docker run -i --rm \
                --name lein-test \
                -v $(readlink -f scripts/):/scripts:ro \
                -w / \
                lein-test:${ver} \
                "${test}"
        echo ">>> Test '${test}' of lein-voom ${LEIN_VOOM_VERSION} against lein ${ver} ::SUCCESS::"
    done
done
