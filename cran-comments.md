## Test environments
* local R installation, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel)

## R CMD check results

> checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Fleur Kelpin <f.kelpin@umcg.nl>’
  
  New submission

0 errors ✓ | 0 warnings ✓ | 1 note x

* This is a new release.

Processed comments on 0.2.4:
* Added @return annotations where missing to generate \value tags in Rd
* Removed armadillo.set_credentials example
* Removed zzz.R which set default value for option, it is not needed
* Changed load_object behavior, it no longer writes to parent frame but now returns the object it has read.