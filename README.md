
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sgtourism

<!-- badges: start -->

[![SpiGesXML status
badge](https://swissstatsr.r-universe.dev/badges/SpiGesXML)](https://swissstatsr.r-universe.dev/SpiGesXML)
[![R-CMD-check](https://github.com/statistikSG/sgtourism/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/statistikSG/sgtourism/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The **sgtourism** R package contains the R code to recreate the
[official St.Gallen Tourism
Dashboard](https://ffssg.shinyapps.io/sgtourismus/).

## Installation

You can install sgtourism like so:

``` r
install.packages("sgtourism", repos = "https://swissstatsr.r-universe.dev" )
```

## Launch the app

``` r
library(sgtourism)

run_app() # launch the app locally
```

The R Shiny app has be packaged using the
[Golem](https://github.com/ThinkR-open/golem) R package.
