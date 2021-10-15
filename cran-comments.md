## Resubmission
Thank you for the fast review and useful comments.

* Angle brackets added to URL and BugReports links in DESCRIPTION
* The software name 'TUFLOW FV' has been enclosed in single quotes in DESCRIPTION title and description.
* I can't find the issue with writing to the user directory I'm sorry. All examples are in 'dontrun{}' blocks and there are no vignettes or tests. The example for TFVInitCons() now creates an output path using tempdir(), but this should not run. One print() statement has been removed. 

>Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the
package directory and getwd()). This is not allowed by CRAN policies. In
your examples/vignettes/tests you can write to tempdir().

## Test environments
* local R installation, R 4.0.0
* win-builder release 4.1.1 (2021-08-10)
* win-builder devel (2021-10-07 r81018)
* rhub Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* rhub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* rhub Fedora Linux, R-devel, clang, gfortran

## R CMD check results

> checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Matt Gibbs <gibbs.ms@gmail.com>'
  New submission

0 errors √ | 0 warnings √ | 1 note x
