## 2.1.8 Fix documentation
* Update github.io using CI

## 2.1.3 Bugfix
* Fix #79 load_table function doesn't show error messages

## 2.1.2 Subset function and bugfixes
* Add subset function
* Fix unclear error message when table name is not specified 
* Fix load_table and assign functions for arrow 3 update

## 2.0.0 Make compatible with armadillo 3
* Armadillo 3 compatibility
* Discontinue Armadillo 2 compatibility (breaking)
* Use new Armadillo APIs instead of MinIO
* Add documentation for using older versions of armadillo to README

## 1.1.3 Edit vignets
* Remove packages from vignettes

## 1.1.2 Add documentation
* Added resourcer documentation

## 1.1.1 Add install package and change maintainer
* Add install package
* Add whitelist package
* Make Mariska maintainer

## 1.0.4 Fix codecov badge links
* Fix codecov badge links 

## 1.0.3 Fix travis badge links
* Fix travis badge links 

## 1.0.2 Change maintainer
* Make Sido maintainer

## 1.0.1 Fixed r-patched-solaris-x86 bugs
Remove test dependency on arrow C library which caused Package Check ERRORs on
r-patched-solaris-x86 and r-oldrel-macos-x86_64.
I got an email asking to correct this.

## Test environments
* local R installation, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel)

## R CMD check results
❯ checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Fleur Kelpin <f.kelpin@umcg.nl>’
  
  Days since last update: 4

0 errors ✔ | 0 warnings ✔ | 1 note ✖
