## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=FALSE, message=FALSE--------------------------------------
library(IncidencePrevalence)
library(CDMConnector)
library(IncidencePrevalence)
library(dbplyr)
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
    as.Date("2012-01-01"),
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
cdm <- mockIncidencePrevalenceRef(
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
cdm <- generateTargetDenominatorCohortSet(
  cdm = cdm, 
  name = "denominator_acute_asthma_2",
  ageGroup = list(c(11, 15)),
  sex = "Female",
  daysPriorObservation = 0,
  targetCohortTable = "target"
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

