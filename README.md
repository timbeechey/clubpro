
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clubpro

An R package for *cl*assification *u*sing *b*inary *p*rocrustes
*ro*tation based on Grice (2011).

## Installation

Install the development version of `clubpro` from
[GitHub](https://github.com/timbeechey/clubpro) with:

``` r
# install.packages("remotes")
remotes::install_github("timbeechey/clubpro")
```

## Citation

To cite `clubpro` in your work you can use the output of:

``` r
citation(package = "clubpro")
```

## Background

`clubpro` is an implementation of methods described in [Grice
(2011)](https://psycnet.apa.org/record/2011-14580-000).

## using `clubpro`

``` r
library(clubpro)
library(lattice)
```

Simulate some count data.

``` r
set.seed(123)

n <- 300

dat <- data.frame(x = rep(c("A", "B", "C"), each = n),
                  y = c(rpois(n, lambda = 5),
                        rpois(n, lambda = 25),
                        rpois(n, lambda = 15)))

dat$x <- factor(dat$x)

histogram( ~ y | x, data = dat, type = "count", layout = c(1,3))
```

![](man/figures/README-simulate_data-1.png)<!-- -->

Run the model.

``` r
mod <- with(dat, club(y, x))
```

Print a summary of the model output (note: only the first 50 lines of
output are shown here).

``` r
summary(mod)
#> ********** Classification Results **********
#> Observations: 900 
#> Missing observations: 0 
#> Target groups: 3 
#> Correctly classified observations: 798 
#> Incorrectly classified observations: 102 
#> Ambiguously classified observations: 0 
#> PCC: 88.67 
#> Median classification strength index: 1 
#> 
#> ********** Randomisation Test **********
#> Random reorderings: 1000 
#> Minimum random PCC: 37 
#> Maximum random PCC: 46.33 
#> Chance-value: 0 
#> 
#>     individual observation target prediction  accuracy  csi
#> 1            1           4      A          A   correct 1.00
#> 2            2           7      A          A   correct 0.97
#> 3            3           4      A          A   correct 1.00
#> 4            4           8      A          A   correct 0.99
#> 5            5           9      A          A   correct 0.73
#> 6            6           2      A          A   correct 1.00
#> 7            7           5      A          A   correct 1.00
#> 8            8           8      A          A   correct 0.99
#> 9            9           5      A          A   correct 1.00
#> 10          10           5      A          A   correct 1.00
#> 11          11           9      A          A   correct 0.73
#> 12          12           5      A          A   correct 1.00
#> 13          13           6      A          A   correct 1.00
#> 14          14           5      A          A   correct 1.00
#> 15          15           2      A          A   correct 1.00
#> 16          16           8      A          A   correct 0.99
#> 17          17           3      A          A   correct 1.00
#> 18          18           2      A          A   correct 1.00
#> 19          19           4      A          A   correct 1.00
#> 20          20           9      A          A   correct 0.73
#> 21          21           8      A          A   correct 0.99
#> 22          22           6      A          A   correct 1.00
#> 23          23           6      A          A   correct 1.00
#> 24          24          11      A          C incorrect 1.00
#> 25          25           6      A          A   correct 1.00
#> 26          26           6      A          A   correct 1.00
#> 27          27           5      A          A   correct 1.00
#> 28          28           5      A          A   correct 1.00
#> 29          29           4      A          A   correct 1.00
#> 30          30           3      A          A   correct 1.00
#> 31          31           9      A          A   correct 0.73
#> 32          32           8      A          A   correct 0.99
#> 33          33           6      A          A   correct 1.00
...
```

Plot the classification results.

``` r
plot(mod)
```

![](man/figures/README-plot-1.png)<!-- -->

# References

Grice, J. W. (2011). *Observation oriented modeling: Analysis of cause
in the behavioral sciences*. Academic Press.
