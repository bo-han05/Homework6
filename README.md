
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Homework6

<!-- badges: start -->

<!-- badges: end -->

The goal of Homework6 is to implement a sparse numeric vector class and
additional methods for arithmetic operators, mean, norm, and
standardization.

## Installation

You can install the development version of Homework6 from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("bo-han05/Homework6")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Homework6)
#> 
#> Attaching package: 'Homework6'
#> The following object is masked from 'package:base':
#> 
#>     norm
x <- as(c(0, 1, 0, 9), "sparse_numeric")

mean(x)
#> [1] 2.5
norm(x)
#> [1] 9.055385
standardize(x)
#> Length = 4 
#>        value pos
#> 1 -0.5735393   1
#> 2 -0.3441236   2
#> 3 -0.5735393   3
#> 4  1.4912023   4
```
