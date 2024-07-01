## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  eval = Sys.getenv("$RUNNER_OS") != "macOS"
)

## ----message= FALSE, warning=FALSE--------------------------------------------
library(CDMConnector)
library(IncidencePrevalence)
library(dplyr)
library(tidyr)
library(ggplot2)

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm <- mockIncidencePrevalenceRef(
  sampleSize = 50000,
  outPre = 0.2
)

## ----eval=FALSE---------------------------------------------------------------
#  cdm$outcome %>%
#    glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  cohortDateRange = c(as.Date("2008-01-01"), as.Date("2012-01-01")),
  ageGroup = list(c(18, 65)),
  sex = c("Male", "Female", "Both"),
  daysPriorObservation = 365
)

## -----------------------------------------------------------------------------
cdm$denominator %>%
  glimpse()

## -----------------------------------------------------------------------------
settings(cdm$denominator)

## -----------------------------------------------------------------------------
cohortCount(cdm$denominator)

## ----message= FALSE, warning=FALSE--------------------------------------------
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "quarters",
  minCellCount = 0
)

prev %>%
  glimpse()

## ----message= FALSE, warning=FALSE, echo=FALSE--------------------------------
plotPrevalence(prev, 
               facet = "denominator_sex", 
               colour = "denominator_sex")

## ----message= FALSE, warning=FALSE--------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = c("Years"),
  outcomeWashout = 180
)

inc %>%
  glimpse()

## ----message= FALSE, warning=FALSE, echo=FALSE--------------------------------
plotIncidence(inc, facet = "denominator_sex", colour = "denominator_sex")

