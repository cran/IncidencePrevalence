## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  eval = Sys.getenv("$RUNNER_OS") != "macOS"
)

## ----message= FALSE, warning=FALSE, echo=FALSE--------------------------------
library(here)
library(DBI)
library(dbplyr)
library(dplyr)
library(tibble)
library(tidyr)
library(duckdb)
library(knitr)
library(IncidencePrevalence)

## ----setup--------------------------------------------------------------------
cdm <- mockIncidencePrevalenceRef(
  sampleSize = 10000,
  outPre = 0.5
)

cdm <- generateDenominatorCohortSet(
  cdm = cdm, name = "denominator",
  cohortDateRange = c(as.Date("2008-01-01"), as.Date("2012-01-01")),
  sex = c("Male", "Female")
)

inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years"
)

## ----defplot------------------------------------------------------------------
plotIncidence(inc)

## ----facetplot----------------------------------------------------------------
plotIncidence(inc, facet = "denominator_sex")

## ----linesplot----------------------------------------------------------------
plotIncidence(inc, facet = "denominator_sex", ribbon = TRUE)

## ----noconfplot---------------------------------------------------------------
plotIncidence(inc, facet = "denominator_sex", ribbon = TRUE, 
              options = list('hideConfidenceInterval' = TRUE))

## ----stackedplot--------------------------------------------------------------
plotIncidence(inc, facet = "denominator_sex", ribbon = TRUE, 
              options = list('hideConfidenceInterval' = TRUE,
                             'facetNcols' = 1, 
                             'facetScales' = "free"))

## ----message= FALSE, warning=FALSE--------------------------------------------
CDMConnector::listTables(attr(attr(cdm, "cdm_source"), "dbcon"))
CDMConnector::dropTable(cdm = cdm, name = starts_with("denominator"))
CDMConnector::dropTable(cdm = cdm, name = starts_with("inc_participants_"))
CDMConnector::listTables(attr(attr(cdm, "cdm_source"), "dbcon"))

