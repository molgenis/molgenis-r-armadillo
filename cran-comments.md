Remove test dependency on arrow C library which caused Package Check ERRORs on
r-patched-solaris-x86 and r-oldrel-macos-x86_64.

## Test environments
* local R installation, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel)

## R CMD check results

0 errors ✓ | 0 warnings ✓ | 0 notes ✓