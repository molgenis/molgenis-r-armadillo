## Test environments
* local R installation, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel)

## R CMD check results

> checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Fleur Kelpin <f.kelpin@umcg.nl>’
  
  New submission
  
  Found the following (possibly) invalid URLs:
    URL: https://armadillo-minio.test.molgenis.org/
      From: inst/doc/MolgenisArmadillo.html
            inst/doc/creating_data_subsets.html
      Status: 400
      Message: Bad Request

0 errors ✓ | 1 warning x | 1 note x

The vignettes point out that you can also manage the data using the UI, instead
of using this package. The URL is that of our MinIO test server, used to
generate the vignette.

The 400 status only occurs when the User-Agent header is missing. Otherwise the
user is redirected to the login screen
```
> curl -H "User-Agent: Mozilla/5.0" https://armadillo-minio.test.molgenis.org/
<a href="/minio/">Temporary Redirect</a>.
```

* This is a new release.
