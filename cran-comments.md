# v0.1.1

* This is a re-submission after reviews from Benjamin Altmann [CRAN]

>   Please rather use the Authors@R field and declare Maintainer, Authors
    and Contributors with their appropriate roles with person() calls.

Added detailed author description with roles ( 'cre', 'aut', 'cph')

> If there are references describing the methods in your package, please
  add these in the description field of your DESCRIPTION file
  
Added references in [DESCRIPTION](DESCRIPTION)

> You write information messages to the console that cannot be easily
  suppressed
  
Removed `print()/cat()` function calls.


# v0.1.0

* This is a new release.

# Test environments

* local Ubuntu-22.04_LTS install, R 4.2.1
* winbuilder
* rhub check_for_cran with build argument `--compact-vignettes=gs+qpdf`
* GitHub Action [`R CMD CHECK`](.github/workflows/R-CMD-check.yaml) on platforms (macOS-latest, windows-latest, ubuntu-latest)


# R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

None
