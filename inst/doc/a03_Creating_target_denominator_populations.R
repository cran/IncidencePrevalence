## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=FALSE, message=FALSE--------------------------------------
library(IncidencePrevalence)
library(CDMConnector)
library(IncidencePrevalence)
library(dplyr)
library(tidyr)
library(ggplot2)

## ----message=FALSE, warning=FALSE---------------------------------------------
personTable <- tibble(
  person_id = c("1", "2", "3", "4", "5"),
  gender_concept_id = c(rep("8507", 2), rep("8532", 3)),
  year_of_birth = 2000,
  month_of_birth = 06,
  day_of_birth = 01
)
observationPeriodTable <- tibble(
  observation_period_id = "1",
  person_id = c("1", "2", "3", "4", "5"),
  observation_period_start_date = c(
    as.Date("2010-12-19"),
    as.Date("2005-04-01"),
    as.Date("2009-04-10"),
    as.Date("2010-08-20"),
    as.Date("2010-01-01")
  ),
  observation_period_end_date = c(
    as.Date("2011-06-19"),
    as.Date("2005-11-29"),
    as.Date("2016-01-02"),
    as.Date("2011-12-11"),
    as.Date("2015-06-01")
  )
)

acute_asthma <- tibble(
  cohort_definition_id = rep("1", 5),
  subject_id = c("3", "3", "5", "5", "2"),
  cohort_start_date = c(
    as.Date("2011-01-01"),
    as.Date("2015-06-01"),
    as.Date("2014-10-01"),
    as.Date("2010-06-01"),
    as.Date("2005-08-20")
  ),
  cohort_end_date = c(
    as.Date("2013-01-01"),
    as.Date("2015-12-31"),
    as.Date("2015-04-01"),
    as.Date("2010-06-01"),
    as.Date("2005-09-20")
  )
)

# mock database
cdm <- mockIncidencePrevalence(
  personTable = personTable,
  observationPeriodTable = observationPeriodTable,
  targetCohortTable = acute_asthma
)

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator"
)
cdm$denominator

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm$denominator %>%
  filter(subject_id %in% c("1", "2", "3", "4", "5")) %>%
  collect() %>%
  pivot_longer(cols = c(
    "cohort_start_date",
    "cohort_end_date"
  )) %>%
  ggplot() +
  geom_point(aes(x = value, y = subject_id)) +
  geom_line(aes(x = value, y = subject_id)) +
  theme_minimal() +
  xlab("Year")

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm <- generateTargetDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_acute_asthma",
  targetCohortTable = "target"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm$denominator_acute_asthma %>%
  collect() %>%
  mutate(row = row_number()) %>%
  pivot_longer(cols = c(
    "cohort_start_date",
    "cohort_end_date"
  )) %>%
  ggplot(aes(group = row)) +
  geom_point(aes(x = value, y = subject_id)) +
  geom_line(aes(x = value, y = subject_id)) +
  theme_minimal() +
  xlab("Year")

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm$target |> 
  PatientProfiles::addDemographics(indexDate = "cohort_start_date")

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm <- generateTargetDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_acute_asthma_incident",
  ageGroup = list(c(11, 15)),
  sex = "Female",
  daysPriorObservation = 0,
  targetCohortTable = "target",
  requirementsAtEntry = TRUE
)

cdm$denominator_acute_asthma_incident %>%
  collect() %>%
  mutate(row = row_number()) %>%
  pivot_longer(cols = c(
    "cohort_start_date",
    "cohort_end_date"
  )) %>%
  ggplot(aes(group = row)) +
  geom_point(aes(x = value, y = subject_id)) +
  geom_line(aes(x = value, y = subject_id)) +
  theme_minimal() +
  xlab("Year")

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm <- generateTargetDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_acute_asthma_prevalent",
  ageGroup = list(c(11, 15)),
  sex = "Female",
  daysPriorObservation = 0,
  targetCohortTable = "target",
  requirementsAtEntry = FALSE
)

cdm$denominator_acute_asthma_prevalent %>%
  collect() %>%
  mutate(row = row_number()) %>%
  pivot_longer(cols = c(
    "cohort_start_date",
    "cohort_end_date"
  )) %>%
  ggplot(aes(group = row)) +
  geom_point(aes(x = value, y = subject_id)) +
  geom_line(aes(x = value, y = subject_id)) +
  theme_minimal() +
  xlab("Year")

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm <- generateTargetDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_acute_asthma_2",
  ageGroup = list(c(11, 15)),
  sex = "Female",
  daysPriorObservation = 0,
  targetCohortTable = "target",
  timeAtRisk = c(0, 30)
)

cdm$denominator_acute_asthma_2 %>%
  collect() %>%
  mutate(row = row_number()) %>%
  pivot_longer(cols = c(
    "cohort_start_date",
    "cohort_end_date"
  )) %>%
  ggplot(aes(group = row)) +
  geom_point(aes(x = value, y = subject_id)) +
  geom_line(aes(x = value, y = subject_id)) +
  theme_minimal() +
  xlab("Year")

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm <- generateTargetDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_acute_asthma_3",
  ageGroup = list(c(11, 15)),
  sex = "Female",
  daysPriorObservation = 0,
  targetCohortTable = "target",
  timeAtRisk = list(c(0, 30), c(31, 60))
)

cdm$denominator_acute_asthma_3 %>%
  collect() %>%
  dplyr::left_join(
    attr(cdm$denominator_acute_asthma_3, "cohort_set") %>%
      dplyr::select(c(
        "cohort_definition_id",
        "time_at_risk"
      )),
    by = "cohort_definition_id",
    copy = TRUE
  ) %>%
  mutate(row = row_number()) %>%
  pivot_longer(cols = c(
    "cohort_start_date",
    "cohort_end_date"
  )) %>%
  ggplot(aes(group = row, colour = time_at_risk)) +
  geom_point(aes(x = value, y = subject_id)) +
  geom_line(aes(x = value, y = subject_id)) +
  theme_minimal() +
  xlab("Year")

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm$target_2 <- cdm$target |>
  dplyr::mutate(dif = cohort_end_date - cohort_start_date) |>
  dplyr::mutate(cohort_end_date = dplyr::if_else(
    dif > 30,
    clock::add_days(cohort_start_date, 30),
    cohort_end_date
  )) |>
  dplyr::select(-"dif") |>
  dplyr::compute(temporary = FALSE, name = "target_2")

cdm <- generateTargetDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_acute_asthma_4",
  ageGroup = list(c(11, 15)),
  sex = "Female",
  daysPriorObservation = 0,
  targetCohortTable = "target_2",
  timeAtRisk = c(0, 90)
)

cdm$denominator_acute_asthma_4 %>%
  collect() %>%
  mutate(row = row_number()) %>%
  pivot_longer(cols = c(
    "cohort_start_date",
    "cohort_end_date"
  )) %>%
  ggplot(aes(group = row)) +
  geom_point(aes(x = value, y = subject_id)) +
  geom_line(aes(x = value, y = subject_id)) +
  theme_minimal() +
  xlab("Year")

