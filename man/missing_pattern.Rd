% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missing_pattern.R
\name{missing_pattern}
\alias{missing_pattern}
\title{Characterise missing data for \code{finalfit} models}
\usage{
missing_pattern(.data, dependent = NULL, explanatory = NULL)
}
\arguments{
\item{.data}{Dataframe. Missing values must be coded \code{NA}.}

\item{dependent}{Character vector usually of length 1, name of depdendent
variable.}

\item{explanatory}{Character vector of any length: name(s) of explanatory
variables.
to a missing data pattern (1=observed, 0=missing). Rows and columns are
sorted in increasing amounts of missing information. The last column and
row contain row and column counts, respectively.}
}
\description{
Using \code{finalfit} conventions, produces a missing data matrix using
\code{\link[mice]{md.pattern}}.
}
\examples{
library(finalfit)
library(dplyr)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"

colon_s \%>\%
	missing_pattern(dependent, explanatory)

}