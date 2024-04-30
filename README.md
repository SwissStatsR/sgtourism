
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sgtourism

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The sgtourism contains the R code for the R Shiny Tourismus Dashboard.

## Installation

You can install the development version like so:

``` r
install.packages("sgtourism", repos = "https://swissstatsr.r-universe.dev")
```

## How to update the Shiny app

The app is built using the Golem R package.

To update the data, you should first replace the CSV file in the
“data-raw” folder.

Then you should run the R script “data_prep.R”, which will create a
local RDA file.

Once it is done, you can open the “app.R” file and push the updated app
on Posit Connect.

## Dockerfile with renv

``` r
# If you want to deploy via a generic Dockerfile
golem::add_dockerfile_with_renv(output_dir = "deploy")
```
