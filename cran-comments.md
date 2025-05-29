# Test environments
- Github actions:
 - Ubuntu latest, release, devel, oldrel-1
 - windows latest, release
 - macOS latest, release
- devtools::check_win_release()
- local R windows installation, R 4.4.1

# Github actions

All environments:

0 errors √ | 0 warnings √ | 0 note √

# check_win_release()
0 errors √ | 1 warnings √ | 1 note √

NOTE
Maintainer: 'Matt Gibbs <gibbs.ms@gmail.com>'

WARNING
Found the following (possibly) invalid URLs:
  URL: <https://github.com/matt-s-gibbs/TUFLOWR>
    From: man/TUFLOWR.Rd
    Message: Invalid URI scheme
    
This is a valid link, inserted by roxygen2

# Local windows
0 errors ✔ | 1 warning ✖ | 1 note ✖

WARNING
  checkRd: (7) TUFLOWR.Rd:15: Invalid URL: <https://github.com/matt-s-gibbs/TUFLOWR>
  
This is a valid link, inserted by roxygen2

NOTE
  unable to verify current time
