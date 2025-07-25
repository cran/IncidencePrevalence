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
knitr::include_graphics(here("vignettes/inc_no_rep_no_washout.png"))

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/inc_no_rep_washout_all.png"))

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/inc_no_rep_some_washout.png"))

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/inc_rep_some_washout.png"))

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/inc_rep_some_washout_censor.png"))

## ----setup--------------------------------------------------------------------
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
outcome_plot <- plotIncidencePopulation(result = inc, y = "outcome_count") +
  xlab("") +
  theme(axis.text.x = element_blank()) +
  ggtitle("a) Number of outcomes by year")
denominator_plot <- plotIncidencePopulation(result = inc) +
  ggtitle("b) Number of people in denominator population by year")
pys_plot <- plotIncidencePopulation(result = inc, y = "person_years") +
  ggtitle("c) Person-years contributed by year")

outcome_plot / denominator_plot / pys_plot

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

## ----message= FALSE, warning=FALSE--------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  censorTable = "censor",
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
  cohortDateRange = c(as.Date("1990-01-01"), as.Date("2009-12-31")),
  ageGroup = list(
    c(0, 39),
    c(41, 65),
    c(66, 150)
  ),
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

plotIncidence(inc) + 
  facet_wrap(vars(denominator_age_group), ncol = 1)

## ----message= FALSE, warning=FALSE--------------------------------------------
pys_plot <- plotIncidencePopulation(result = inc, y = "person_years")

pys_plot +
  facet_wrap(vars(denominator_age_group), ncol = 1)

## ----fig.width=13-------------------------------------------------------------
cdm$denominator <- cdm$denominator %>%
  mutate(group = if_else(as.numeric(subject_id) < 3000, "first", "second"))

inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  strata = list("group"),
  outcomeWashout = 180,
  repeatedEvents = TRUE
)

plotIncidence(inc,
  colour = "group"
) + 
  facet_wrap(vars(group), ncol = 1)

cdm$denominator <- cdm$denominator %>%
  mutate(
    group_1 = if_else(as.numeric(subject_id) < 3000, "first", "second"),
    group_2 = if_else(as.numeric(subject_id) < 2000, "one", "two")
  )

inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  strata = list(
    c("group_1"), # for just group_1
    c("group_2"), # for just group_2
    c("group_1", "group_2")
  ), # for group_1 and group_2
  outcomeWashout = 180,
  repeatedEvents = TRUE
)

plotIncidence(inc,
  colour = c("group_1", "group_2")
) +
  facet_wrap(vars(group_1, group_2), ncol = 2) +
  theme(legend.position = "top")

## ----message=TRUE, warning=FALSE----------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = c("weeks"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = 180,
  repeatedEvents = TRUE
)

## ----message= FALSE, warning=FALSE--------------------------------------------
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = c("Years"),
  outcomeWashout = 180,
  repeatedEvents = TRUE
)
tableIncidenceAttrition(inc, style = "darwin")

