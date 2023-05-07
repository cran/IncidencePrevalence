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
library(knitr)
library(IncidencePrevalence)

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/point_prev.png"))

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/period_prev.png"))

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
  temporary = FALSE
)

cdm$denominator %>%
  glimpse()

## ---- message= FALSE, warning=FALSE-------------------------------------------
prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Years",
  minCellCount = 0, 
  temporary = FALSE
)

prev %>%
  glimpse()

plotPrevalence(prev, ylim =  c(0, NA))

## ---- message= FALSE, warning=FALSE-------------------------------------------
prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Months",
  minCellCount = 0, 
  temporary = FALSE
)

prev %>%
  glimpse()

plotPrevalence(prev, ylim =  c(0, NA))

## ---- message= FALSE, warning=FALSE-------------------------------------------
prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Years",
  timePoint = "middle",
  minCellCount = 0, 
  temporary = FALSE
)

prev %>%
  glimpse()

plotPrevalence(prev, ylim =  c(0, NA))

## ---- message= FALSE, warning=FALSE-------------------------------------------
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Years",
  minCellCount = 0, 
  temporary = FALSE
)

prev %>%
  glimpse()

plotPrevalence(prev, ylim =  c(0.1, 0.3))

## ---- message= FALSE, warning=FALSE-------------------------------------------
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Months",
  minCellCount = 0, 
  temporary = FALSE
)

prev %>%
  glimpse()

plotPrevalence(prev, ylim =  c(0, NA))


## ---- message= FALSE, warning=FALSE-------------------------------------------
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Months",
  fullContribution = FALSE,
  minCellCount = 0, 
  temporary = FALSE
)

prev %>%
  glimpse()

plotPrevalence(prev,
               ylim = c(0,0.07))

## ---- message= FALSE, warning=FALSE-------------------------------------------
prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Years",
  outcomeLookbackDays = c(0, 30),
  minCellCount = 0, 
  temporary = FALSE
)

prev %>%
  glimpse()

plotPrevalence(prev,
               colour = "analysis_outcome_lookback_days", 
               colour_name = "Outcome lookback days",
               ylim = c(0,NA))

## ---- message= FALSE, warning=FALSE-------------------------------------------
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Years",
  fullContribution = c(TRUE, FALSE),
  minCellCount = 0, 
  temporary = FALSE, # returnParticipants can only be TRUE with temporary = FALSE
  returnParticipants = TRUE
)
prevalenceAttrition(prev)

## ---- message= FALSE, warning=FALSE-------------------------------------------
participants(prev, analysisId = 1) %>% 
  glimpse()

## ---- message= FALSE, warning=FALSE-------------------------------------------
CDMConnector::listTables(attr(cdm, "dbcon"), schema = attr(cdm, "write_schema"))
# drop tables created when instantiating denominator cohorts
CDMConnector::dropTable(cdm = cdm, 
              name = dplyr::starts_with(paste0(attr(cdm, "write_prefix"), 
                            "denominator"))) 
# drop table with study participants when returnParticipants = TRUE
CDMConnector::dropTable(cdm = cdm, 
              name = paste0(attr(cdm, "write_prefix"), 
                            "period_prev_participants1")) 


