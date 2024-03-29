% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_indicator.R
\name{find_indicator}
\alias{find_indicator}
\title{Find available indicator code names in the Wittgenstein Centre Human Capital Data Explorer}
\usage{
find_indicator(x)
}
\arguments{
\item{x}{Character string on key word or name related to indicator of potential interest.}
}
\value{
A subset of the \code{wic_indicators} data frame with one or more of the \code{indicator}, \code{description} or \code{definition} columns matching the keyword given to \code{x}. Use the result in the \code{indicator} column to input to the \code{get_wcde} function for downloading data.
}
\description{
Find available indicator code names in the Wittgenstein Centre Human Capital Data Explorer
}
\examples{
find_indicator("education")
find_indicator("migr")
find_indicator("fert")
}
