% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataPipeline.R
\name{robust}
\alias{robust}
\title{Convert single-argument function f to a robust version. The robust version of
the function will be run over and over on the same input until successful.
Each individual run is not allowed to take longer than 60s (or a different
specified timeout).}
\usage{
robust(f, timeout = 60)
}
\arguments{
\item{f}{The function to make robust (must accept one argument).}

\item{timeout}{The number of seconds each individual run is allowed to use.}
}
\value{
A robust version of \code{f}.
}
\description{
Convert single-argument function f to a robust version. The robust version of
the function will be run over and over on the same input until successful.
Each individual run is not allowed to take longer than 60s (or a different
specified timeout).
}
