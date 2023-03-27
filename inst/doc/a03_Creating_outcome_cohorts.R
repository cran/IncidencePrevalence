## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = Sys.getenv("$RUNNER_OS") != "macOS"
)

## ---- message= FALSE, warning=FALSE, echo=FALSE-------------------------------
library(here)
library(knitr)

## ----eval=FALSE---------------------------------------------------------------
#  outcome_cohorts <- CDMConnector::readCohortSet(here::here("outcome_cohorts"))
#  cdm <- CDMConnector::generateCohortSet(cdm = cdm,
#                                         cohortSet = outcome_cohorts,
#                                         name = outcome_table)

