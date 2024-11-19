
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sgtourism <img src="man/figures/logo.png" align="right" height="138" />

<!-- badges: start -->

[![swissstatsr
badge](https://swissstatsr.r-universe.dev/badges/:name)](https://swissstatsr.r-universe.dev/)
[![sgtourism status
badge](https://swissstatsr.r-universe.dev/badges/sgtourism)](https://swissstatsr.r-universe.dev/sgtourism)
[![R-CMD-check](https://github.com/statistikSG/sgtourism/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/statistikSG/sgtourism/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The **sgtourism** R package contains the code to produce the [official
St.Gallen tourism dashboard](https://ffssg.shinyapps.io/sgtourismus/).

### Launch the app

Install the R package with:

``` r
install.packages("sgtourism", repos = "https://swissstatsr.r-universe.dev")
# remotes::install_github("statistikSG/sgtourism")
```

Then launch the app locally:

``` r
# Run the application
sgtourism::run_app()
```

### How to reuse the app

What if you want to use a different version of **sgtourism** for your
own project? We recommend to clone the GitHub repository locally first.

As your own environment may differ from ours, a stratagy could be to
take advantage of the “renv.lock.prod” file to set up the R package
dependencies:

``` r
# install.packages("renv")
# renv::activate() # run first
renv::restore(lockfile = "deploy/renv.lock.prod")
```

Learn more about **renv**
[here](https://rstudio.github.io/renv/articles/renv.html).

Another strategy could be to install all R packages dependencies of
**sgtourism**:

``` r
devtools::install()
```

### How to use your own data

This R shiny application is transformed as an R package using the
[golem](https://thinkr-open.github.io/golem/) framework.

To update the data, you should first replace our CSV files with your own
files in the `/data-raw` folder.

Then you should run the related R script in the “data-raw” folder to
replace the internal RDA file datasets in the `/data` folder.

Once it is done, you can go to `/dev/03_deploy.R` for testing and
deploying your application.

### Contribute

Any contribution is strongly appreciated. Feel free to report a bug, ask
any question or make a pull request for any remaining
[issue](https://github.com/statistikSG/sgtourism/issues).
