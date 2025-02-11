% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataPipeline.R
\name{importNOAAM}
\alias{importNOAAM}
\title{ImportNOAA is a function exposed in "worldmet" library. Gets data from weather stations.}
\usage{
importNOAAM(
  code = "037720-99999",
  year = 2014,
  hourly = TRUE,
  precip = FALSE,
  PWC = FALSE,
  parallel = TRUE,
  quiet = FALSE,
  path = NA
)
}
\arguments{
\item{code}{Station codes to get data from.}

\item{year}{Years to get data for.}

\item{hourly}{If TRUE, aggregate hourly.}

\item{precip}{If TRUE, include precipitation.}

\item{PWC}{If TRUE, include text description.}

\item{parallel}{If TRUE, use multiple cores.}

\item{quiet}{If TRUE, do not produce output.}

\item{path}{If provided, store results in a file.}
}
\value{
The codes of the closest weather stations in a list format.
}
\description{
ImportNOAA is a function exposed in "worldmet" library. Gets data from weather stations.
}
