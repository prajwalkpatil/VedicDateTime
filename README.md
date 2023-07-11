# VedicDateTime<a href='https://www.neerajbokde.in/viggnette/2022-09-05-VedicDateTime'><img src="vignettes/icon.png" alt="alt text" align="right" width="200"/>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/VedicDateTime)](https://cran.r-project.org/package=VedicDateTime)
[![codecov](https://codecov.io/gh/saradindusengupta/VedicDateTime/branch/main/graph/badge.svg?token=788ELH8KF6)](https://codecov.io/gh/saradindusengupta/VedicDateTime)
[![R-CMD-check](https://github.com/saradindusengupta/VedicDateTime/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/saradindusengupta/VedicDateTime/actions/workflows/R-CMD-check.yaml)
[![lintr](https://github.com/saradindusengupta/VedicDateTime/actions/workflows/lintr.yml/badge.svg?branch=main&event=workflow_dispatch)](https://github.com/saradindusengupta/VedicDateTime/actions/workflows/lintr.yml)
<!-- badges: start -->

An R Package to work with Vedic Calendar System.

@maintainer: [@neerajdhanraj](https://github.com/neerajdhanraj)

## Getting Started

To get `tithi` or lunar day given a Gregorian calendar date

```r
library("VedicDateTime")
place <- c(15.34, 75.13, +5.5)
tithi(gregorian_to_jd(08,09,2022), place)
```

To get `vaara` or day of the week given a Gregorian calendar date

```r
library("VedicDateTime")
vaara(gregorian_to_jd(08,09,2022))
```

To get `masa` or lunar month given a Gregorian calendar date

```r
library("VedicDateTime")
place <- c(15.34, 75.13, +5.5)
masa(gregorian_to_jd(08,09,2022), place)
```

To get Julian day from a given Gregorian calendar date

```r
library("VedicDateTime")
gregorian_to_jd(08,09,2022) # dd, mm, yyyy
```

## Install

To install the development version from GitHub

```r
devtools::install_github("saradindusengupta/VedicDateTime")
devtools::install_github("prajwalkpatil/VedicDateTime")
```

## Build

To build from source, clone the repository from Github

```r
devtools::build(args = c('--compact-vignettes=both'))
devtools::install()
```
## R CMD CHECK

To check for CRAN submission

### `rhub`
```r
rhub::check_for_cran(check_args = c('--compact-vignettes=both'))
```
### Local - `R CMD CHECK`
```r
devtools::check(build_args = c('--compact-vignettes=both'))
```

### Submit to CRAN
```r
devtools::release(args = c('--compact-vignettes=both'))
```


## Test

To run the test cases for development

```r
devtools::test()
```
## Test Coverage

To generate the test coverage report cases for development

```r
covr::package_coverage("VedicDateTime")
```

## CRAN

Check [cran-comments.md](cran-comments.md) to verify latest updates

## Documentation

- [`man/`](man/)
- [`doc/VedicDateTime.pdf`](doc/VedicDateTime.pdf)
