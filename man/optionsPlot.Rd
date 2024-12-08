% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{optionsPlot}
\alias{optionsPlot}
\title{List of parameter options for IncidencePrevalence plot functions.}
\usage{
optionsPlot()
}
\value{
List of available parameters and their default values.
}
\description{
Options are:

\strong{hideConfidenceInterval:} Logical. Whether to show confidence intervals

\strong{line:} Logical. Whether to plot a line using \code{geom_line}

\strong{point:} Logical. Whether to plot points using \code{geom_point}

\strong{facetNcols} -> Numeric. Number of facet columns

\strong{facetScales} -> Character. Should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
}
\examples{
\donttest{
optionsPlot()
}
}
