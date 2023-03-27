## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  eval = Sys.getenv("$RUNNER_OS") != "macOS"
)

## ---- message= FALSE, warning=FALSE, echo=FALSE-------------------------------
library(here)
library(ggplot2)
library(DBI)
library(dbplyr)
library(dplyr)
library(tibble)
library(tidyr)
library(duckdb)
library(knitr)
library(IncidencePrevalence)

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/inc_no_rep_no_washout.png"))

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/inc_no_rep_washout_all.png"))

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/inc_no_rep_some_washout.png"))

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/inc_rep_some_washout.png"))

## ----setup--------------------------------------------------------------------
library(IncidencePrevalence)
library(dplyr)
library(tidyr)
library(ggplot2)

cdm <- mockIncidencePrevalenceRef(
  sampleSize = 50000,
  outPre = 0.5
)

cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2008-01-01"),
  endDate = as.Date("2012-01-01"),
  ageGroup = list(c(0, 150)),
  sex = "Both",
  daysPriorHistory = 0, 
  tablePrefix = "example_inc"
)

cdm$denominator %>%
  glimpse()

## ---- message= FALSE, warning=FALSE-------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  outcomeWashout = 0,
  repeatedEvents = FALSE,
  tablePrefix = "example_inc"
)

inc %>%
  glimpse()

inc %>%
  ggplot(aes(x = incidence_start_date, y = incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper)) +
  geom_point() +
  geom_errorbar(width = 0) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

## ---- message= FALSE, warning=FALSE-------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  tablePrefix = "example_inc"
)

inc %>%
  glimpse()

inc %>%
  ggplot(aes(x = incidence_start_date, y = incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper)) +
  geom_point() +
  geom_errorbar(width = 0) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

## ---- message= FALSE, warning=FALSE-------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  outcomeWashout = 180,
  repeatedEvents = FALSE,
  tablePrefix = "example_inc"
)

inc %>%
  glimpse()

inc %>%
  ggplot(aes(x = incidence_start_date, y = incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper)) +
  geom_point() +
  geom_errorbar(width = 0) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

## ---- message= FALSE, warning=FALSE-------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  outcomeWashout = 180,
  repeatedEvents = TRUE,
  tablePrefix = "example_inc"
)

inc %>%
  glimpse()

inc %>%
  ggplot(aes(x = incidence_start_date, y = incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper)) +
  geom_point() +
  geom_errorbar(width = 0) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

## ---- message=TRUE, warning=FALSE---------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = c("weeks"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = 180,
  repeatedEvents = TRUE,
  minCellCount = 0,
  tablePrefix = "example_inc"
)

## ---- message= FALSE, warning=FALSE-------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = c("Years"),
  outcomeWashout = c(0, 180),
  repeatedEvents = TRUE,
  tablePrefix = "example_study", # returnParticipants can only be TRUE if tablePrefix is not null
  returnParticipants = TRUE
)
incidenceSet(inc)
incidenceAttrition(inc)

## ---- message= FALSE, warning=FALSE-------------------------------------------
participants(inc, analysisId = 1) %>% 
  glimpse()

## ---- message= FALSE, warning=FALSE-------------------------------------------
CDMConnector::listTables(attr(cdm, "dbcon"))
CDMConnector::dropTable(cdm = cdm, name = starts_with("example"))
CDMConnector::listTables(attr(cdm, "dbcon"))

