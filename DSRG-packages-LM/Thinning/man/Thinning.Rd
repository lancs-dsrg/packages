% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Thinning.R
\name{Thinning}
\alias{Thinning}
\title{Thinning - Generating arrivals from a NHPP}
\usage{
Thinning(f, x, N, max_rate, Tend)
}
\arguments{
\item{f}{The intensity function we want to be thinned}

\item{x}{A sequence of points between 0 and Tend to thin}

\item{N}{The max number of points to return}

\item{max_rate}{The maximum rate within the interval [0, Tend)}

\item{Tend}{The end time}
}
\value{
Event times from a NHPP with intensity function f
}
\description{
Thinning - Generating arrivals from a NHPP
}
