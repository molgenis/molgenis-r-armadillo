#!/usr/bin/env bash

export PACKAGE=`grep Package DESCRIPTION | head -n1 | cut -d':' -f2 | xargs`
export TAG=`grep Version DESCRIPTION | head -n1 | cut -d':' -f2 | xargs`

echo "Testing ${PACKAGE}:${TAG}"

Rscript -e 'devtools::check(remote=TRUE, force_suggests = TRUE, error_on="error")'
Rscript -e 'library(covr);codecov()'
R CMD build .
Rscript -e "devtools::check_built(path = './${PACKAGE}_${TAG}.tar.gz', remote=TRUE, force_suggests = TRUE)"
Rscript -e "pkgdown::build_site()"
