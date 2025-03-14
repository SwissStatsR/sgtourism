% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_helpers.R
\name{get_number_color}
\alias{get_number_color}
\title{Get color relative to number}
\usage{
get_number_color(
  number,
  color_positive = "#009fe3",
  color_negative = "#8a2432",
  color_neutral = "#000000"
)
}
\arguments{
\item{number}{A number}

\item{color_positive}{Color of the positive number}

\item{color_negative}{Color of the negative number}

\item{color_neutral}{Color of the neutral number}
}
\value{
An color hex code.
}
\description{
Logic to return an hex color code. Returns black if number is 0 or is not a number.
}
\keyword{internal}
