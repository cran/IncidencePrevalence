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
  daysPriorHistory = 0
)

cdm$denominator %>%
  glimpse()

## ---- message= FALSE, warning=FALSE-------------------------------------------
prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Years",
  minCellCount = 0
)

prev %>%
  glimpse()

prev %>%
  ggplot(aes(x = prevalence_start_date, y = prevalence,
             ymin = prevalence_95CI_lower,
             ymax = prevalence_95CI_upper)) +
  geom_point() +
  geom_errorbar(width = 0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  theme_minimal()

## ---- message= FALSE, warning=FALSE-------------------------------------------
prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Months",
  minCellCount = 0
)

prev %>%
  glimpse()

prev %>%
  ggplot(aes(x = prevalence_start_date, y = prevalence,
             ymin = prevalence_95CI_lower,
             ymax = prevalence_95CI_upper)) +
  geom_point() +
  geom_errorbar(width = 0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  theme_minimal()

## ---- message= FALSE, warning=FALSE-------------------------------------------
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

prev %>%
  ggplot(aes(x = prevalence_start_date, y = prevalence,
             ymin = prevalence_95CI_lower,
             ymax = prevalence_95CI_upper)) +
  geom_point() +
  geom_errorbar(width = 0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) + 
  theme_minimal()

## ---- message= FALSE, warning=FALSE-------------------------------------------
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Years",
  minCellCount = 0
)

prev %>%
  glimpse()

prev %>%
  ggplot(aes(x = prevalence_start_date, y = prevalence,
             ymin = prevalence_95CI_lower,
             ymax = prevalence_95CI_upper)) +
  geom_point() +
  geom_errorbar(width = 0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  theme_minimal()

## ---- message= FALSE, warning=FALSE-------------------------------------------
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Months",
  minCellCount = 0
)

prev %>%
  glimpse()

prev %>%
  ggplot(aes(x = prevalence_start_date, y = prevalence,
             ymin = prevalence_95CI_lower,
             ymax = prevalence_95CI_upper)) +
  geom_point() +
  geom_errorbar(width = 0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  theme_minimal()

## ---- message= FALSE, warning=FALSE-------------------------------------------
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

prev %>%
  ggplot(aes(x = prevalence_start_date, y = prevalence,
             ymin = prevalence_95CI_lower,
             ymax = prevalence_95CI_upper)) +
  geom_point() +
  geom_errorbar(width = 0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  theme_minimal()

## ---- message= FALSE, warning=FALSE-------------------------------------------
prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Years",
  outcomeLookbackDays = c(0, 30),
  minCellCount = 0
)

prev %>%
  glimpse()

prev %>%
  left_join(prevalenceSet(prev)) %>%
  mutate(analysis_outcome_lookback_days = as.character(analysis_outcome_lookback_days)) %>%
  ggplot(aes(x = prevalence_start_date, y = prevalence,
             ymin = prevalence_95CI_lower,
             ymax = prevalence_95CI_upper,
             colour = analysis_outcome_lookback_days)) +
  geom_point() +
  geom_errorbar(width = 0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  theme_minimal()

## ---- message= FALSE, warning=FALSE-------------------------------------------
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Years",
  fullContribution = c(TRUE, FALSE),
  minCellCount = 0, 
  tablePrefix = "example_study", # returnParticipants can only be TRUE if tablePrefix is not null
  returnParticipants = TRUE
)
prevalenceSet(prev)
prevalenceAttrition(prev)

## ---- message= FALSE, warning=FALSE-------------------------------------------
participants(prev, analysisId = 1) %>% 
  glimpse()

