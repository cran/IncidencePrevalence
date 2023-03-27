## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  eval = Sys.getenv("$RUNNER_OS") != "macOS"
)

## ---- message= FALSE, warning=FALSE, echo=FALSE-------------------------------
library(CDMConnector)
library(IncidencePrevalence)
library(dbplyr)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)

## ----echo=FALSE, message=FALSE, out.width="80%"-------------------------------
library(knitr)
library(here)
knitr::include_graphics(here("vignettes/dpop1.png"))

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/dpop2.png"))

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/dpop3.png"))

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/dpop4.png"))

## ---- message=FALSE, warning=FALSE, results='hide'----------------------------
library(IncidencePrevalence)
library(ggplot2)
library(tidyr)

cdm <- mockIncidencePrevalenceRef(sampleSize = 500)

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = NULL,
  endDate = NULL,
  ageGroup = list(c(0, 150)),
  sex = "Both",
  daysPriorHistory = 0
)
cdm$denominator

cdm$denominator %>%
  filter(subject_id %in% c("1", "2", "3", "4", "5"))

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
cdm$denominator %>%
  collect() %>%
  filter(subject_id %in% c("1", "2", "3", "4", "5")) %>%
  pivot_longer(cols = c(
    "cohort_start_date",
    "cohort_end_date"
  )) %>%
  ggplot() +
  geom_point(aes(x = value, y = subject_id)) +
  geom_line(aes(x = value, y = subject_id)) +
  theme_minimal() +
  xlab("Year")

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator %>%
  collect() %>%
  ggplot() +
  theme_minimal() +
  geom_histogram(aes(cohort_start_date),
    colour = "black", fill = "grey"
  )

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator %>%
  collect() %>%
  ggplot() +
  theme_minimal() +
  geom_histogram(aes(cohort_end_date),
    colour = "black", fill = "grey"
  )

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2008-01-01"),
  endDate = as.Date("2010-01-01"),
  ageGroup = list(c(0, 150)),
  sex = "Both",
  daysPriorHistory = 0
)
cdm$denominator

cohortCount(cdm$denominator)

cdm$denominator %>%
  filter(subject_id %in% c("1", "2", "3", "4", "5"))

## ---- message=FALSE, warning=FALSE--------------------------------------------
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

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator %>%
  collect() %>%
  ggplot() +
  theme_minimal() +
  geom_histogram(aes(cohort_start_date),
    colour = "black", fill = "grey"
  )

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator %>%
  collect() %>%
  ggplot() +
  theme_minimal() +
  geom_histogram(aes(cohort_end_date),
    colour = "black", fill = "grey"
  )

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2008-01-01"),
  endDate = as.Date("2010-01-01"),
  ageGroup = list(c(0, 150)),
  sex = "Both",
  daysPriorHistory = 365
)
cdm$denominator

cohortCount(cdm$denominator)

cdm$denominator %>%
  filter(subject_id %in% c("1", "2", "3", "4", "5"))

## ---- message=FALSE, warning=FALSE--------------------------------------------
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

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator %>%
  collect() %>%
  ggplot() +
  theme_minimal() +
  geom_histogram(aes(cohort_start_date),
    colour = "black", fill = "grey"
  )

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator %>%
  collect() %>%
  ggplot() +
  theme_minimal() +
  geom_histogram(aes(cohort_end_date),
    colour = "black", fill = "grey"
  )

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2008-01-01"),
  endDate = as.Date("2010-01-01"),
  ageGroup = list(c(18, 65)),
  sex = "Female",
  daysPriorHistory = 365
)
cdm$denominator %>%
  glimpse()

cohortCount(cdm$denominator)

cdm$denominator %>%
  filter(subject_id %in% c("1", "2", "3", "4", "5"))

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator %>%
  collect() %>%
  ggplot() +
  theme_minimal() +
  geom_histogram(aes(cohort_start_date),
    colour = "black", fill = "grey"
  )

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator %>%
  collect() %>%
  ggplot() +
  theme_minimal() +
  geom_histogram(aes(cohort_end_date),
    colour = "black", fill = "grey"
  )

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2008-01-01"),
  endDate = as.Date("2010-01-01"),
  ageGroup = list(
    c(0, 40),
    c(41, 100)
  ),
  sex = "Both",
  daysPriorHistory = 0
)
dpop <- cdm$denominator %>%
  collect() %>%
  left_join(cohortSet(cdm$denominator))

dpop %>%
  glimpse()

dpop %>%
  group_by(cohort_definition_id, age_group) %>%
  tally()

dpop %>%
  filter(subject_id %in% c("1", "3", "57", "353", "393", "496")) %>%
  collect() %>%
  pivot_longer(cols = c(
    "cohort_start_date",
    "cohort_end_date"
  )) %>%
  mutate(cohort_definition_id = as.character(cohort_definition_id)) %>%
  ggplot(aes(x = subject_id, y = value, colour = cohort_definition_id)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  theme_minimal() +
  theme(legend.position = "top") +
  ylab("Year") +
  coord_flip()

## ---- message=FALSE, warning=FALSE, fig.height=8------------------------------
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2008-01-01"),
  endDate = as.Date("2010-01-01"),
  ageGroup = list(
    c(0, 40),
    c(41, 100)
  ),
  sex = c("Male", "Female", "Both"),
  daysPriorHistory = 0
)
dpop <- cdm$denominator %>%
  collect() %>%
  left_join(cohortSet(cdm$denominator))

dpop %>% glimpse()

dpop %>%
  group_by(cohort_definition_id, age_group, sex) %>%
  tally()

dpop %>%
  filter(subject_id %in% c("1", "3", "57", "353", "393", "496")) %>%
  pivot_longer(cols = c(
    "cohort_start_date",
    "cohort_end_date"
  )) %>%
  mutate(cohort_definition_id = as.character(cohort_definition_id)) %>%
  ggplot(aes(x = subject_id, y = value, colour = cohort_definition_id)) +
  facet_grid(sex ~ ., space = "free_y") +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(legend.position = "top") +
  ylab("Year") +
  coord_flip()

## ---- message=FALSE, warning=FALSE, fig.height=10-----------------------------
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2008-01-01"),
  endDate = as.Date("2010-01-01"),
  ageGroup = list(
    c(0, 40),
    c(41, 100)
  ),
  sex = c("Male", "Female", "Both"),
  daysPriorHistory = c(0, 365)
)
dpop <- cdm$denominator %>%
  collect() %>%
  left_join(cohortSet(cdm$denominator))

dpop %>% glimpse()

dpop %>%
  group_by(cohort_definition_id, age_group, sex, days_prior_history) %>%
  tally()

dpop %>%
  filter(subject_id %in% c("1", "3", "57", "353", "393", "496")) %>%
  pivot_longer(cols = c(
    "cohort_start_date",
    "cohort_end_date"
  )) %>%
  mutate(cohort_definition_id = as.character(cohort_definition_id)) %>%
  ggplot(aes(x = subject_id, y = value, colour = cohort_definition_id)) +
  facet_grid(sex + days_prior_history ~ ., space = "free_y") +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(legend.position = "top") +
  ylab("Year") +
  coord_flip()

## ---- message=TRUE, warning=FALSE---------------------------------------------
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2008-01-01"),
  endDate = as.Date("2010-01-01"),
  ageGroup = list(
    c(0, 18),
    c(19, 100)
  ),
  sex = c("Male", "Female"),
  daysPriorHistory = c(0, 365)
)

head(cdm$denominator, 8)

## ---- message=TRUE, warning=FALSE---------------------------------------------
cohortSet(cdm$denominator)

## ---- message=TRUE, warning=FALSE---------------------------------------------
cohortCount(cdm$denominator)

## ---- message=TRUE, warning=FALSE---------------------------------------------
cohortAttrition(cdm$denominator)

## ---- message=FALSE, warning=FALSE--------------------------------------------
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

## ---- message=FALSE, warning=FALSE, results='hide'----------------------------
conditionX <- tibble(
  cohort_definition_id = c(rep("1", 3), rep("2", 3), rep("3", 5)),
  subject_id = c("1", "2", "4", "3", "5", "2", "3", "3", "5", "5", "2"),
  cohort_start_date = c(
    as.Date("2010-12-19"),
    as.Date("2005-04-01"),
    as.Date("2010-08-20"),
    as.Date("2012-01-01"),
    as.Date("2010-06-01"),
    as.Date("2005-08-20"),
    as.Date("2012-01-01"),
    as.Date("2015-06-01"),
    as.Date("2014-10-01"),
    as.Date("2010-06-01"),
    as.Date("2005-08-20")
  ),
  cohort_end_date = c(
    as.Date("2011-06-19"),
    as.Date("2005-11-29"),
    as.Date("2011-12-11"),
    as.Date("2013-01-01"),
    as.Date("2012-03-01"),
    as.Date("2005-11-29"),
    as.Date("2013-01-01"),
    as.Date("2015-12-31"),
    as.Date("2015-04-01"),
    as.Date("2010-06-01"),
    as.Date("2005-08-20")
  )
)

# mock database
cdm <- mockIncidencePrevalenceRef(
  personTable = personTable,
  observationPeriodTable = observationPeriodTable,
  strataTable = conditionX
)

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm
)
cdm$denominator

## ---- message=FALSE, warning=FALSE--------------------------------------------
observationPeriodTable

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  strataTable = "strata",
  strataCohortId = 1,
  strataCohortName = "strata_cohort_1"
)
cdm$denominator

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(dplyr)
observationPeriodTable %>%
  filter(person_id %in% c("1", "2", "4"))

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  strataTable = "strata",
  strataCohortId = 2,
  strataCohortName = "strata_cohort_2"
)
cdm$denominator

## ---- message=FALSE, warning=FALSE--------------------------------------------
conditionX %>%
  filter(cohort_definition_id == 2) %>%
  filter(subject_id %in% c("2", "3", "5"))

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  strataTable = "strata",
  strataCohortId = 3,
  strataCohortName = "strata_cohort_2"
)
cdm$denominator

## ---- message=FALSE, warning=FALSE--------------------------------------------
conditionX %>%
  filter(cohort_definition_id == 3) %>%
  filter(subject_id %in% c("2", "3", "5"))

## ---- message=FALSE, warning=FALSE--------------------------------------------
cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  strataTable = "strata",
  strataCohortId = 3,
  startDate = as.Date("2014-01-01"),
  endDate = as.Date("2016-01-01"),
  ageGroup = list(c(0, 150)),
  sex = "Female",
  daysPriorHistory = 0
)
cdm$denominator

## ---- message=FALSE, warning=FALSE--------------------------------------------
cohortSet(cdm$denominator)

