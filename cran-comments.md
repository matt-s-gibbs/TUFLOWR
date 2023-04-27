# Test environments
* local R installation, R 4.1.3
* win-builder release and devel
* Windows Server 2022, R-devel, 64 bit
* rhub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* rhub Fedora Linux, R-devel, clang, gfortran

# R CMD check results

0 errors √ | 0 warnings √ | 1 note x

## win-builder NOTE

> checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Matt Gibbs <gibbs.ms@gmail.com>'
  New submission
  
  
## Rhub Windows Server 2022, R-devel, 64 bit NOTE

> checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
    
Apparently this is a [bug in Rhub](https://github.com/r-hub/rhub/issues/503)
