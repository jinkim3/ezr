
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ezr

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/jinkim3/ezr.svg?branch=master)](https://travis-ci.com/jinkim3/ezr)
[![R build
status](https://github.com/jinkim3/ezr/workflows/R-CMD-check/badge.svg)](https://github.com/jinkim3/ezr/actions)
[![Total
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/ezr?color=blue)](https://cran.r-project.org/package=ezr)
[![CodeFactor](https://www.codefactor.io/repository/github/jinkim3/ezr/badge)](https://www.codefactor.io/repository/github/jinkim3/ezr)
<!-- badges: end -->

Point and click with your mouse to conduct basic analyses and create
graphics quickly and easily.

This package will run a “Shiny App” on your local machine, which allows
you to analyze data without the need to type a line of code.

You do not need to know anything about R to use this tool.

Examples of things you can do include (but are not limited to):

  - tabulating descriptive statistics for a variable
  - creating a frequency table for a variable
  - creating histograms by experimental group
  - creating a scatter plot
  - calculating a correlation between two variables

## Installation

You can install the released version of ezr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ezr")
```

You can also install the development version from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("jinkim3/ezr")
```

## Example

Here are some examples of using this package.

``` r
library(ezr)

# Start the Shiny server on a local machine
start_ezr(data = mtcars)
```
