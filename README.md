
# openmpt <img src="man/figures/logo.png" align="right" height="139" copyright="cc-sa" alt="logo"  />

<!-- badges: start -->

[![R-CMD-check](https://github.com/pepijn-devries/gghourglass/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pepijn-devries/gghourglass/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

TODO

## Installation

Install latest developmental version from R-Universe:

``` r
install.packages("gghourglass", repos = c('https://pepijn-devries.r-universe.dev', 'https://cloud.r-project.org'))
```

## Example

``` r
library(ggplot2)
library(gghourglass)

data(bats)

monitoring <- attr(bats, "monitoring")

ggplot(subset(bats, format(RECDATETIME, "%Y") == "2018"),
      aes(x = RECDATETIME, col = SPECDESCSCI)) +
  annotate_daylight(monitoring$longitude[1], monitoring$latitude[1]) +
  annotate_daylight(monitoring$longitude[1], monitoring$latitude[1], c("dusk", "dawn")) +
  geom_hourglass() +
  labs(x = "Date", y = "Time of day", col = "Species")
```

![](man/figures/README-example-1.png)<!-- -->

TODO
