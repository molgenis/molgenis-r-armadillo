[![Build Status](https://jenkins.dev.molgenis.org/buildStatus/icon?job=molgenis%2Fmolgenis-r-armadillo%2Fmaster)](https://jenkins.dev.molgenis.org/job/molgenis/job/molgenis-r-armadillo/job/master/)
[![codecov](https://codecov.io/gh/molgenis/molgenis-r-armadillo/branch/master/graph/badge.svg)](https://codecov.io/gh/molgenis/molgenis-r-armadillo)


Client to use the Armadillo service. You can manage your data in the Armadillo service using this client.

## Purpose
This library can be used by data managers to share datasets on a
MOLGENIS Armadillo server.
Researchers can then analyse these datasets and datasets shared on other servers
using DataSHIELD.

## Overview
The datasets are stored in shared folders on a MinIO file store. The MOLGENIS
Armadillo server has access to the file store and can load the data sets into
a shielded RServe environment so that researchers can call DataSHIELD analysis
methods on the data.

## Usage
Login to the service.
```R
armadillo.login("https://armadillo.dev.molgenis.org",
      "https://armadillo-minio.dev.molgenis.org")
```

Now you can create a project and upload datasets to it to share them.
```R
library(datasets)
armadillo.create_project("project")
armadillo.upload_table("project", "folder", iris)
```

Looking at the data with yb listing the tables.
```R
armadillo.list_tables("project")
```

Removing the data from the storage. First you need to remove the content of a project before you can throw away the project.
```R
armadillo.delete_table("project", "folder", "iris")
armadillo.delete_project("project")
```

## Documentation
For more indepth documentation please check the [howto](https://molgenis.github.io/molgenis-r-armadillo/articles/MolgenisArmadillo.html).