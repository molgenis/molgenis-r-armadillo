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
