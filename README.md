MolgenisArmadillo
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Build
Status](https://travis-ci.org/molgenis/molgenis-r-armadillo.svg?branch=master)](https://travis-ci.org/molgenis/molgenis-r-armadillo)
[![CRAN
status](https://www.r-pkg.org/badges/version/MolgenisArmadillo)](https://CRAN.R-project.org/package=MolgenisArmadillo)
[![codecov](https://codecov.io/gh/molgenis/molgenis-r-armadillo/branch/master/graph/badge.svg)](https://codecov.io/gh/molgenis/molgenis-r-armadillo)
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
armadillo.login("https://armadillo.dev.molgenis.org",
      "https://armadillo-minio.dev.molgenis.org")
```

Now you can create a project and upload tables to the project to share
them for analysis.

``` r
library(datasets)
armadillo.create_project("project")
#> Created project 'project'
armadillo.upload_table("project", "folder", iris)
#> Compressing table...
#> Uploaded table folder/iris
```

Listing the tables.

``` r
armadillo.list_tables("project")
#> [1] "folder/iris"
```

Removing the data from the storage. First you need to remove the content
of a project before you can throw away the project.

``` r
armadillo.delete_table("project", "folder", "iris")
#> Deleted table 'folder/iris'.
armadillo.delete_project("project")
#> Deleted project 'project'
```

## Documentation

For more in depth documentation please check the
[howto](https://molgenis.github.io/molgenis-r-armadillo/articles/MolgenisArmadillo.html).
