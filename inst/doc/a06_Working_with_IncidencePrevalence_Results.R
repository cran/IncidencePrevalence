## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE, 
  message = FALSE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  eval = Sys.getenv("$RUNNER_OS") != "macOS"
)

## -----------------------------------------------------------------------------
library(dplyr)
library(IncidencePrevalence)

cdm <- mockIncidencePrevalence(
  sampleSize = 10000,
  outPre = 0.3,
  minOutcomeDays = 365,
  maxOutcomeDays = 3650
)

cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  cohortDateRange = as.Date(c("2008-01-01", "2018-01-01")),
  ageGroup = list(
    c(0, 64),
    c(65, 100)
  ),
  sex = c("Male", "Female", "Both"),
  daysPriorObservation = 180
)

inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  repeatedEvents = TRUE,
  outcomeWashout = 180,
  completeDatabaseIntervals = TRUE
)

prev_point <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  timePoint = "start"
)

## -----------------------------------------------------------------------------
inherits(inc, "summarised_result")

inherits(prev_point, "summarised_result")

## -----------------------------------------------------------------------------
omopgenerics::resultColumns("summarised_result")

## -----------------------------------------------------------------------------
inc |> 
  glimpse()

prev_point |> 
  glimpse()

## -----------------------------------------------------------------------------
settings(inc) |> 
  glimpse()

settings(prev_point) |> 
  glimpse()

## -----------------------------------------------------------------------------
results <- bind(inc, prev_point) |> 
  glimpse()

results |> 
  glimpse()

settings(results) |> 
  glimpse()

## -----------------------------------------------------------------------------
dir <- file.path(tempdir(), "my_study_results")
dir.create(dir)

exportSummarisedResult(results,
                       minCellCount = 5,
                       fileName = "incidence_prevalence_results.csv",
                       path = dir)

## -----------------------------------------------------------------------------
list.files(dir)

## -----------------------------------------------------------------------------
res_imported <- importSummarisedResult(path = dir)

## ----message=TRUE, warning=TRUE-----------------------------------------------
omopgenerics::isResultSuppressed(results)

## ----message=TRUE, warning=TRUE-----------------------------------------------
omopgenerics::isResultSuppressed(res_imported)

## -----------------------------------------------------------------------------
asIncidenceResult(inc) |> glimpse()

## -----------------------------------------------------------------------------
asPrevalenceResult(prev_point) |> glimpse()

## -----------------------------------------------------------------------------
library(ggplot2)
asIncidenceResult(inc) |> 
  ggplot(aes(x = incidence_start_date, 
             y = incidence_100000_pys)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  theme_bw() +
  facet_wrap(vars(denominator_age_group, denominator_sex)) + 
  ggtitle("Smoothed incidence rates over time") +
  xlab("Date") +
  ylab("Incidence per 100,000 person-years")

