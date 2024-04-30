
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sgtourism

<!-- badges: start -->

[![swissstatsr
badge](https://swissstatsr.r-universe.dev/badges/:name)](https://swissstatsr.r-universe.dev/)
[![sgtourism status
badge](https://swissstatsr.r-universe.dev/badges/sgtourism)](https://swissstatsr.r-universe.dev/sgtourism)
[![R-CMD-check](https://github.com/statistikSG/sgtourism/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/statistikSG/sgtourism/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The **sgtourism** R package contains the code used to produce the
[official St.Gallen Tourismus
Dashboard](https://ffssg.shinyapps.io/sgtourismus/).

## Installation

You can install the package like so:

``` r
install.packages("sgtourism", repos = "https://swissstatsr.r-universe.dev")
```

## How to update the Shiny app

The app is built using the [golem](https://thinkr-open.github.io/golem/)
R package.

To update the data, you should first replace the CSV file in the
“data-raw” folder.

Then you should run the related R script in the “data-raw” folder, which
will create a local RDA file in the “data” folder.

Once it is done, you can open the “app.R” file and push the updated app.

## Dockerfile with renv

``` r
# If you want to deploy via a generic Dockerfile
golem::add_dockerfile_with_renv(output_dir = "deploy")
```
