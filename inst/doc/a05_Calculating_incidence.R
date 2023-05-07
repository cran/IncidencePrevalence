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

cdm <- mockIncidencePrevalenceRef(
  sampleSize = 50000,
  outPre = 0.5
)

cdm <- generateDenominatorCohortSet(
  cdm = cdm, name = "denominator",
  cohortDateRange = c(as.Date("2008-01-01"), as.Date("2012-01-01")),
  ageGroup = list(c(0, 150)),
  sex = "Both",
  daysPriorHistory = 0, 
  temporary =  FALSE,
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
  temporary = FALSE
)

inc %>%
  glimpse()

plotIncidence(inc)

## ---- message= FALSE, warning=FALSE-------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  temporary = FALSE
)

inc %>%
  glimpse()

plotIncidence(inc)


## ---- message= FALSE, warning=FALSE-------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  outcomeWashout = 180,
  repeatedEvents = FALSE,
  temporary = FALSE
)

inc %>%
  glimpse()

plotIncidence(inc)

## ---- message= FALSE, warning=FALSE-------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  outcomeWashout = 180,
  repeatedEvents = TRUE,
  temporary = FALSE
)

inc %>%
  glimpse()

plotIncidence(inc)


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
  temporary = FALSE
)

## ---- message= FALSE, warning=FALSE-------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = c("Years"),
  outcomeWashout = c(0, 180),
  repeatedEvents = TRUE,
  temporary = FALSE,
  returnParticipants = TRUE
)
incidenceAttrition(inc)

## ---- message= FALSE, warning=FALSE-------------------------------------------
participants(inc, analysisId = 1) %>% 
  glimpse()

## ---- message= FALSE, warning=FALSE-------------------------------------------
CDMConnector::listTables(attr(cdm, "dbcon"))
CDMConnector::dropTable(cdm = cdm, name = starts_with("denominator"))
CDMConnector::dropTable(cdm = cdm, name = starts_with("inc_participants"))
CDMConnector::listTables(attr(cdm, "dbcon"))

