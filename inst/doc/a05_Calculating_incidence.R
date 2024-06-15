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
  sampleSize = 1000,
  outPre = 0.5
)

cdm <- generateDenominatorCohortSet(
  cdm = cdm, name = "denominator",
  cohortDateRange = c(as.Date("2008-01-01"), as.Date("2012-01-01")),
  ageGroup = list(c(0, 150)),
  sex = "Both",
  daysPriorObservation = 0
)

cdm$denominator %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  outcomeWashout = 0,
  repeatedEvents = FALSE
)

inc %>%
  glimpse()

plotIncidence(inc)

## ----message= FALSE, warning=FALSE--------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  outcomeWashout = Inf,
  repeatedEvents = FALSE
)

inc %>%
  glimpse()

plotIncidence(inc)

## ----message= FALSE, warning=FALSE--------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  outcomeWashout = 180,
  repeatedEvents = FALSE
)

inc %>%
  glimpse()

plotIncidence(inc)

## ----message= FALSE, warning=FALSE--------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  outcomeWashout = 180,
  repeatedEvents = TRUE
)

inc %>%
  glimpse()

plotIncidence(inc)

## -----------------------------------------------------------------------------
cdm <- generateDenominatorCohortSet(
  cdm = cdm, name = "denominator_age_sex",
  cohortDateRange = c(as.Date("2008-01-01"), as.Date("2012-01-01")),
  ageGroup = list(c(0, 39),
                  c(41, 65),
                  c(66, 150)),
  sex = "Both",
  daysPriorObservation = 0
)
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_age_sex",
  outcomeTable = "outcome",
  interval = "years",
  outcomeWashout = 180,
  repeatedEvents = TRUE
)

plotIncidence(inc, facet = "denominator_age_group")

## -----------------------------------------------------------------------------
cdm$denominator <- cdm$denominator %>% 
  mutate(group = if_else(as.numeric(subject_id)  < 500, "first", "second")) 

inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  strata = list("group"),
  outcomeWashout = 180,
  repeatedEvents = TRUE
)

plotIncidence(inc, 
               facet = "strata_level")

cdm$denominator <- cdm$denominator %>% 
  mutate(group_1 = if_else(as.numeric(subject_id)  < 1500, "first", "second"))  %>% 
  mutate(group_2 = if_else(cohort_start_date  < as.Date("2010-01-01"), 
                           "pre", "post"))

inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  strata = list(c("group_1"), # for just group_1
                c("group_2"), # for just group_2
                c("group_1", "group_2")),  # for group_1 and group_2  outcomeWashout = 180,
  repeatedEvents = TRUE
)

plotIncidence(inc, 
               facet = "strata_level")

## ----message=TRUE, warning=FALSE----------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = c("weeks"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = 180,
  repeatedEvents = TRUE,
  minCellCount = 0
)

## ----message= FALSE, warning=FALSE--------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = c("Years"),
  outcomeWashout = c(0, 180),
  repeatedEvents = TRUE,
  returnParticipants = TRUE
)
attrition(inc)

## ----message= FALSE, warning=FALSE--------------------------------------------
participants(inc, analysisId = 1) %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
CDMConnector::listTables(attr(attr(cdm, "cdm_source"), "dbcon"))
CDMConnector::dropTable(cdm = cdm, name = starts_with("denominator"))
CDMConnector::dropTable(cdm = cdm, name = starts_with("inc_participants_"))
CDMConnector::listTables(attr(attr(cdm, "cdm_source"), "dbcon"))

