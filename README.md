MolgenisArmadillo
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Build
Status](https://api.travis-ci.com/molgenis/molgenis-r-armadillo.svg?branch=master)](https://api.travis-ci.com/molgenis/molgenis-r-armadillo.svg?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/MolgenisArmadillo)](https://CRAN.R-project.org/package=MolgenisArmadillo)
[![codecov](https://codecov.io/gh/molgenis/molgenis-r-armadillo/branch/master/graph/badge.svg?token=ITPMERAWYI)](https://app.codecov.io/gh/molgenis/molgenis-r-armadillo)
<!-- badges: end -->

Client to share data in a [MOLGENIS Armadillo DataSHIELD
Service](https://github.com/molgenis/molgenis-service-armadillo/).

## Purpose

This library can be used by data managers to share datasets on a
MOLGENIS Armadillo server. Researchers can then analyse these datasets
and datasets shared on other servers using DataSHIELD. Researchers will
only be able to access aggregate information and cannot see individual
rows.

## Overview

The datasets are stored in shared folders on a MinIO file store. The
MOLGENIS Armadillo server has access to the file store and can load the
data sets into a shielded RServe environment so that researchers can
call DataSHIELD analysis methods on the data.

## Usage

Login to the service.

``` r
library('MolgenisArmadillo')
armadillo.login("https://armadillo-url-example.org")
```

Now you can create a project and upload tables to the project to share
them for analysis.

``` r
library(datasets)
armadillo.create_project("project")
armadillo.upload_table("project", "folder", iris)
```

Listing the tables.

``` r
armadillo.list_tables("project")
```

Removing the data from the storage. First you need to remove the content
of a project before you can throw away the project.

``` r
armadillo.delete_table("project", "folder", "iris")
armadillo.delete_project("project")
```

## Documentation

For more in depth documentation please check the
[howto](https://molgenis.github.io/molgenis-r-armadillo/articles/MolgenisArmadillo.html).

## Armadillo 2

The newest version (2.x) of MolgenisArmadillo will be only compatible
with Armadillo version 3. If you still use Armadillo 2, you should use
the 1.1.13 version of MolgenisArmadillo. You can install this specific
version using the following commands:

For windows:

``` r
packageurl <- "https://cran.rstudio.com/bin/windows/contrib/4.2/MolgenisArmadillo_1.1.3.zip"
install.packages(packageurl, repos=NULL, type="source")
```

For Mac:

``` r
packageurl <- "https://cran.rstudio.com/bin/macosx/contrib/4.2/MolgenisArmadillo_1.1.3.tgz"
install.packages(packageurl, repos=NULL, type="source")
```

For Linux:

``` r
packageurl <- "https://cran.rstudio.com/src/contrib/MolgenisArmadillo_1.1.3.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
```

## For developers

- To build documentation, do `devtools::document()`
- To run all unit tests, do `devtools::test()`
- While writing code (or tests), you can use `devtools::load_all()` to
  quickly “install” the package.
- To run a single test file, open it in Rstudio and do
  `devtools::test_active_file()`
- To run the linter, do `devtools::lint()`. Tip: run it often :)
- To create new README from Rmd: run `devtools::build_readme()`
