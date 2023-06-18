## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  eval = Sys.getenv("$RUNNER_OS") != "macOS"
)

## ---- message= FALSE, warning=FALSE-------------------------------------------
library(CDMConnector)
library(IncidencePrevalence)
library(dplyr)
library(tidyr)
library(ggplot2)

## ----eval=FALSE---------------------------------------------------------------
#  con <- DBI::dbConnect(RPostgres::Postgres(),
#    dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
#    host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
#    user = Sys.getenv("CDM5_POSTGRESQL_USER"),
#    password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD")
#  )
#  cdm <- CDMConnector::cdm_from_con(con,
#    cdm_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
#  )

## ---- message= FALSE, warning=FALSE-------------------------------------------
cdm <- mockIncidencePrevalenceRef(
  sampleSize = 50000,
  outPre = 0.5
)

## ----eval=FALSE---------------------------------------------------------------
#  outcome_cohorts <- CDMConnector::readCohortSet(here::here("outcome_cohorts"))
#  cdm <- CDMConnector::generateCohortSet(
#    cdm = cdm,
#    cohortSet = outcome_cohorts,
#    name = outcome_table
#  )

## ---- message= FALSE, warning=FALSE-------------------------------------------
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  cohortDateRange = c(as.Date("2008-01-01"), as.Date("2012-01-01")),
  ageGroup = list(c(18, 65)),
  sex = c("Male", "Female", "Both"),
  daysPriorHistory = 365
)

## -----------------------------------------------------------------------------
cdm$denominator %>%
  glimpse()

## -----------------------------------------------------------------------------
cohortSet(cdm$denominator)

## -----------------------------------------------------------------------------
cohortCount(cdm$denominator)

## ---- message= FALSE, warning=FALSE-------------------------------------------
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "quarters",
  minCellCount = 0
)

prev %>%
  glimpse()

## ---- message= FALSE, warning=FALSE, echo=FALSE-------------------------------
prev %>%
  ggplot(aes(
    x = prevalence_start_date, y = prevalence,
    ymin = prevalence_95CI_lower,
    ymax = prevalence_95CI_upper,
    colour = denominator_sex
  )) +
  geom_point(position = position_dodge(50)) +
  geom_errorbar(width = 0, position = position_dodge(50)) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.3)
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

## ---- message= FALSE, warning=FALSE-------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = c("Years"),
  outcomeWashout = 180
)

inc %>%
  glimpse()

## ---- message= FALSE, warning=FALSE, echo=FALSE-------------------------------
inc %>%
  ggplot(aes(
    x = incidence_start_date, y = incidence_100000_pys,
    ymin = incidence_100000_pys_95CI_lower,
    ymax = incidence_100000_pys_95CI_upper,
    colour = denominator_sex
  )) +
  geom_point(position = position_dodge(50)) +
  geom_errorbar(width = 0, position = position_dodge(50)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  theme(legend.title = element_blank())

## ---- eval=FALSE--------------------------------------------------------------
#  exportIncidencePrevalenceResults(
#    resultList = list(
#      "incidence" = inc,
#      "prevalence" = prev
#    ),
#    zipName = "example_results",
#    outputFolder = here::here()
#  )

