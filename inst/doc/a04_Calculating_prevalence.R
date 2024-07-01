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

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/point_prev.png"))

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/period_prev.png"))

## ----message=FALSE, warning=FALSE---------------------------------------------
library(IncidencePrevalence)
library(dplyr)
library(tidyr)

cdm <- mockIncidencePrevalenceRef(
  sampleSize = 20000,
  earliestObservationStartDate = as.Date("1960-01-01"), 
  minOutcomeDays = 365,
  outPre = 0.3
)

cdm <- generateDenominatorCohortSet(
  cdm = cdm, name = "denominator",
  cohortDateRange = c(as.Date("1990-01-01"), as.Date("2009-12-31")),
  ageGroup = list(c(0, 150)),
  sex = "Both",
  daysPriorObservation = 0
)

cdm$denominator %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Years",
  minCellCount = 0
)

prev %>%
  glimpse()

plotPrevalence(prev, ylim = c(0, NA))

## ----message= FALSE, warning=FALSE--------------------------------------------
prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Months",
  minCellCount = 0
)

prev %>%
  glimpse()

plotPrevalence(prev, ylim = c(0, NA))

## ----message= FALSE, warning=FALSE--------------------------------------------
prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Years",
  timePoint = "middle",
  minCellCount = 0
)

prev %>%
  glimpse()

plotPrevalence(prev, ylim = c(0, NA))

## ----message= FALSE, warning=FALSE--------------------------------------------
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Years",
  minCellCount = 0
)

prev %>%
  glimpse()

plotPrevalence(prev)

## ----message= FALSE, warning=FALSE--------------------------------------------
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Months",
  minCellCount = 0
)

prev %>%
  glimpse()

plotPrevalence(prev, ylim = c(0, NA))

## ----message= FALSE, warning=FALSE--------------------------------------------
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Months",
  fullContribution = FALSE,
  minCellCount = 0
)

prev %>%
  glimpse()

plotPrevalence(prev)

## -----------------------------------------------------------------------------
cdm <- generateDenominatorCohortSet(
  cdm = cdm, name = "denominator_age_sex",
  cohortDateRange = c(as.Date("1990-01-01"), as.Date("2009-12-31")),
  ageGroup = list(c(0, 39),
                  c(41, 65),
                  c(66, 150)),
  sex = "Both",
  daysPriorObservation = 0
)
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator_age_sex",
  outcomeTable = "outcome",
  minCellCount = 0
)

plotPrevalence(prev, facet = "denominator_age_group")

## -----------------------------------------------------------------------------
cdm$denominator <- cdm$denominator %>% 
  mutate(group = if_else(as.numeric(subject_id)  < 500, "first", "second")) 

prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  strata = "group",
  minCellCount = 0
)

plotPrevalence(prev, 
               facet = "strata_level")

## -----------------------------------------------------------------------------
cdm$denominator <- cdm$denominator %>% 
  mutate(group_1 = if_else(as.numeric(subject_id)  < 1500, "first", "second"),
         group_2 = if_else(as.numeric(subject_id)  < 1000, "one", "two"))

prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  strata = list(c("group_1"), # for just group_1
                c("group_2"), # for just group_2
                c("group_1", "group_2")),  # for group_1 and group_2
  minCellCount = 0
)

plotPrevalence(prev, 
               facet = "strata_level")

## ----message= FALSE, warning=FALSE--------------------------------------------
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Years",
  fullContribution = c(TRUE, FALSE),
  minCellCount = 0,
  returnParticipants = TRUE
)
attrition(prev)

## ----message= FALSE, warning=FALSE--------------------------------------------
participants(prev, analysisId = 1) %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
# drop tables created when instantiating denominator cohorts
CDMConnector::dropTable(
  cdm = cdm,
  name = dplyr::starts_with("denominator")
)
# drop table with study participants when returnParticipants = TRUE
CDMConnector::dropTable(
  cdm = cdm,
  name = "period_prev_participants_1"
)
CDMConnector::cdm_disconnect(cdm)


