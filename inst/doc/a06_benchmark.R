## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(IncidencePrevalence)
library(visOmopResults)
library(dplyr)
library(ggplot2)

cdm <- mockIncidencePrevalence(
  sampleSize = 100,
  earliestObservationStartDate = as.Date("2010-01-01"),
  latestObservationStartDate = as.Date("2010-01-01"),
  minDaysToObservationEnd = 364,
  maxDaysToObservationEnd = 364,
  outPre = 0.1
)

timings <- benchmarkIncidencePrevalence(cdm)
timings |>
  glimpse()

## -----------------------------------------------------------------------------
visOmopTable(timings,
  hide = c(
    "variable_name", "variable_level",
    "strata_name", "strata_level"
  ),
  groupColumn = "task"
)

## -----------------------------------------------------------------------------
test_db <- IncidencePrevalenceBenchmarkResults
test_db |>
  glimpse()

## -----------------------------------------------------------------------------
visOmopTable(bind(timings, test_db),
  hide = c(
    "variable_name", "variable_level",
    "strata_name", "strata_level"
  ),
  groupColumn = "task"
)

