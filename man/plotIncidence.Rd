% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plotIncidence}
\alias{plotIncidence}
\title{Plot incidence results}
\usage{
plotIncidence(
  result,
  x = "incidence_start_date",
  y = "incidence_100000_pys",
  ylim = c(0, NA),
  ribbon = FALSE,
  facet = NULL,
  colour = NULL,
  options = list()
)
}
\arguments{
\item{result}{Incidence results}

\item{x}{Variable to plot on x axis}

\item{y}{Variable to plot on y axis. Options are: "incidence_100000_pys",
"outcome_count", "denominator_count", "person_days"}

\item{ylim}{Limits for the Y axis}

\item{ribbon}{If TRUE, the plot will join points using a ribbon}

\item{facet}{Variables to use for facets}

\item{colour}{Variables to use for colours}

\item{options}{A list of optional plot options. See optionsPlot() for the
default parameters.}
}
\value{
A ggplot with the incidence results plotted
}
\description{
Plot incidence results
}
\examples{
\donttest{
cdm <- mockIncidencePrevalence(sampleSize = 1000)
cdm <- generateDenominatorCohortSet(
  cdm = cdm, name = "denominator",
  cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
)
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome"
)
plotIncidence(inc)
}
}
