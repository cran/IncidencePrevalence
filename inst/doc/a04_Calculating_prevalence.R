## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, 
  warning = FALSE, 
  message = FALSE,
  comment = "#>",
  fig.width = 8.5,
  fig.height = 6,
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
library(ggplot2)
library(patchwork)

cdm <- mockIncidencePrevalence(
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
  interval = "Years"
)

prev %>%
  glimpse()

plotPrevalence(prev)

## ----message= FALSE, warning=FALSE--------------------------------------------
outcome_plot <- plotPrevalencePopulation(result = prev, y = "outcome_count") + xlab("") +
  theme(axis.text.x = element_blank()) +
  ggtitle("a) Number of outcomes by year")
denominator_plot <- plotPrevalencePopulation(result = prev) +
  ggtitle("b) Number of people in denominator population by year")
outcome_plot / denominator_plot

## ----message= FALSE, warning=FALSE--------------------------------------------
prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Months"
)

prev %>%
  glimpse()

plotPrevalence(prev)

## ----message= FALSE, warning=FALSE--------------------------------------------
prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Years",
  timePoint = "middle"
)

prev %>%
  glimpse()

plotPrevalence(prev, line = FALSE)

## ----message= FALSE, warning=FALSE--------------------------------------------
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Years"
)

prev %>%
  glimpse()

plotPrevalence(prev)

## ----message= FALSE, warning=FALSE--------------------------------------------
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Months"
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
  fullContribution = FALSE
)

prev %>%
  glimpse()

plotPrevalence(prev)

## -----------------------------------------------------------------------------
cdm <- generateDenominatorCohortSet(
  cdm = cdm, name = "denominator_age_sex",
  cohortDateRange = c(as.Date("1990-01-01"), as.Date("2009-12-31")),
  ageGroup = list(
    c(0, 39),
    c(41, 65),
    c(66, 150)
  ),
  sex = "Both",
  daysPriorObservation = 0
)
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator_age_sex",
  outcomeTable = "outcome"
)

plotPrevalence(prev) +
  facet_wrap(vars(denominator_age_group), ncol = 1)

## ----message= FALSE, warning=FALSE--------------------------------------------
denominator_plot <- plotPrevalencePopulation(result = prev)

denominator_plot +
  facet_wrap(vars(denominator_age_group), ncol = 1)

## -----------------------------------------------------------------------------
cdm$denominator <- cdm$denominator %>%
  mutate(group = if_else(as.numeric(subject_id) < 500, "first", "second"))

prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  strata = "group"
)

plotPrevalence(prev,
  colour = "group"
) + 
  facet_wrap(vars(group), ncol = 1)

## ----fig.width=13-------------------------------------------------------------
cdm$denominator <- cdm$denominator %>%
  mutate(
    group_1 = if_else(as.numeric(subject_id) < 1500, "first", "second"),
    group_2 = if_else(as.numeric(subject_id) < 1000, "one", "two")
  )

prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  strata = list(
    c("group_1"), # for just group_1
    c("group_2"), # for just group_2
    c("group_1", "group_2")
  ) # for group_1 and group_2
)

plotPrevalence(prev) + 
  facet_wrap(vars(group_1, group_2), ncol = 2)

## ----message= FALSE, warning=FALSE--------------------------------------------
prev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "Years",
  fullContribution = TRUE
)
tablePrevalenceAttrition(prev)

