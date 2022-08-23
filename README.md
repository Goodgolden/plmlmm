
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plmlmm

<!-- badges: start -->
<!-- badges: end -->

The goal of plmlmm is to …

## Installation

You can install the development version of plmlmm from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Goodgolden/plmlmm")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(plmlmm)
#> Loading required package: brokenstick
#> Loading required package: broom.mixed
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: forcats
#> Loading required package: gamlss
#> Loading required package: splines
#> Loading required package: gamlss.data
#> 
#> Attaching package: 'gamlss.data'
#> The following object is masked from 'package:datasets':
#> 
#>     sleep
#> Loading required package: gamlss.dist
#> Loading required package: MASS
#> 
#> Attaching package: 'MASS'
#> The following object is masked from 'package:dplyr':
#> 
#>     select
#> Loading required package: nlme
#> 
#> Attaching package: 'nlme'
#> The following object is masked from 'package:dplyr':
#> 
#>     collapse
#> Loading required package: parallel
#>  **********   GAMLSS Version 5.4-3  **********
#> For more on GAMLSS look at https://www.gamlss.com/
#> Type gamlssNews() to see new features/changes/bug fixes.
#> Loading required package: here
#> here() starts at /Users/goodgolden5/Desktop/project/plmlmm
#> Loading required package: janitor
#> 
#> Attaching package: 'janitor'
#> The following objects are masked from 'package:stats':
#> 
#>     chisq.test, fisher.test
#> Loading required package: JMbayes
#> Loading required package: survival
#> Loading required package: doParallel
#> Loading required package: foreach
#> Loading required package: iterators
#> Loading required package: rstan
#> Loading required package: StanHeaders
#> Loading required package: ggplot2
#> rstan (Version 2.21.5, GitRev: 2e1f913d3ca3)
#> For execution on a local, multicore CPU with excess RAM we recommend calling
#> options(mc.cores = parallel::detectCores()).
#> To avoid recompilation of unchanged Stan programs, we recommend calling
#> rstan_options(auto_write = TRUE)
#> 
#> Attaching package: 'JMbayes'
#> The following object is masked from 'package:gamlss.data':
#> 
#>     aids
#> Loading required package: lme4
#> Loading required package: Matrix
#> 
#> Attaching package: 'lme4'
#> The following object is masked from 'package:gamlss':
#> 
#>     refit
#> The following object is masked from 'package:nlme':
#> 
#>     lmList
#> Loading required package: matrixcalc
#> Loading required package: shiny
#> Loading required package: tibble
#> Loading required package: tidyr
#> 
#> Attaching package: 'tidyr'
#> The following objects are masked from 'package:Matrix':
#> 
#>     expand, pack, unpack
#> The following object is masked from 'package:rstan':
#> 
#>     extract
#> Loading required package: tidyverse
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
#> ✔ readr   2.1.2     ✔ stringr 1.4.0
#> ✔ purrr   0.3.4     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ purrr::accumulate() masks foreach::accumulate()
#> ✖ nlme::collapse()    masks dplyr::collapse()
#> ✖ tidyr::expand()     masks Matrix::expand()
#> ✖ tidyr::extract()    masks rstan::extract()
#> ✖ dplyr::filter()     masks stats::filter()
#> ✖ dplyr::lag()        masks stats::lag()
#> ✖ tidyr::pack()       masks Matrix::pack()
#> ✖ MASS::select()      masks dplyr::select()
#> ✖ tidyr::unpack()     masks Matrix::unpack()
#> ✖ purrr::when()       masks foreach::when()
#> 
#>  Welcome to my package; this is a package developed for Randy Jin's MS thesis
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
