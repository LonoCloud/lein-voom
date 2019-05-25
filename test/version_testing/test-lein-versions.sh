#!/usr/bin/env bash

set -e

LEIN_VOOM_VERSION=${LEIN_VOOM_VERSION:-0.1.0-20180617_140646-g0ba7ec8}
LEIN_TEST_VERSIONS=${LEIN_TEST_VERSIONS:-2.6.1 2.7.1 2.8.1}
LEIN_TESTS=${LEIN_TESTS:-$(echo scripts/*)}

for ver in $LEIN_TEST_VERSIONS; do
    # Attempt to copy in the appropriate pre-fetched or locally built lein-voom
    cp -a ~/.m2/repository/lein-voom/lein-voom/. artifacts/lein-voom/lein-voom/. || true
    docker build -t lein-test:${ver} --build-arg LEIN_VER=${ver} --build-arg VOOM_VER=${LEIN_VOOM_VERSION} .
    for test in $LEIN_TESTS; do
        if docker run -i --rm \
                  --name lein-test \
                  -v $(readlink -f scripts/):/scripts:ro \
                  -w / \
                  lein-test:${ver} \
                  "${test}"; then
           echo ">>> Test '${test}' of lein-voom ${LEIN_VOOM_VERSION} against lein ${ver} ::SUCCESS::"
        else
           echo ">>> Test '${test}' of lein-voom ${LEIN_VOOM_VERSION} against lein ${ver} ::FAILURE::"
        fi
    done
done
