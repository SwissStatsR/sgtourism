% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_boxes.R
\name{get_box_data_markt}
\alias{get_box_data_markt}
\title{Get data for Markt box tables}
\usage{
get_box_data_markt(data, months_selected, input)
}
\arguments{
\item{data}{data.frame produced by `filter_data()`.}

\item{months_selected}{reactive vector months_selected()}

\item{input}{input}
}
\value{
a data.frame to be used with `create_box_*()` functions.
}
\description{
Data wrangling to prepare data for the boxes tables related to the "markt"
input, i.e. "input$markt".
}
\keyword{internal}
