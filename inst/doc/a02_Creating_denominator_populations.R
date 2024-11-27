## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  eval = Sys.getenv("$RUNNER_OS") != "macOS"
)

## ----echo=FALSE, message=FALSE, out.width="80%", warning=FALSE----------------
library(knitr)
library(here)
knitr::include_graphics(here("vignettes/dpop1.png"))

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/dpop2.png"))

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/dpop3.png"))

## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics(here("vignettes/dpop4.png"))

## ----message=FALSE, warning=FALSE---------------------------------------------
library(CDMConnector)
library(IncidencePrevalence)
library(ggplot2)
library(tidyr)
library(dplyr)

## ----message=TRUE-------------------------------------------------------------
cdm <- mockIncidencePrevalence(sampleSize = 500)

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm <- generateDenominatorCohortSet(
  cdm = cdm, 
  name = "denominator",
  cohortDateRange = as.Date(c(NA,NA)),
  ageGroup = list(c(0, 150)),
  sex = "Both",
  daysPriorObservation = 0
)
cdm$denominator

cdm$denominator %>%
  filter(subject_id %in% c("1", "2", "3", "4", "5"))

## ----message=FALSE, warning=FALSE, echo=FALSE---------------------------------
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

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm$denominator %>%
  collect() %>%
  ggplot() +
  theme_minimal() +
  geom_histogram(aes(cohort_start_date),
    colour = "black", fill = "grey"
  )

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm$denominator %>%
  collect() %>%
  ggplot() +
  theme_minimal() +
  geom_histogram(aes(cohort_end_date),
    colour = "black", fill = "grey"
  )

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator", 
  cohortDateRange = c(as.Date("1990-01-01"), as.Date("2009-12-31")),
  ageGroup = list(c(0, 150)),
  sex = "Both",
  daysPriorObservation = 0
)
cdm$denominator

cohortCount(cdm$denominator)

cdm$denominator %>%
  filter(subject_id %in% c("1", "2", "3", "4", "5"))

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm$denominator %>%
  collect() %>%
  ggplot() +
  theme_minimal() +
  geom_histogram(aes(cohort_start_date),
    colour = "black", fill = "grey"
  )

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm$denominator %>%
  collect() %>%
  ggplot() +
  theme_minimal() +
  geom_histogram(aes(cohort_end_date),
    colour = "black", fill = "grey"
  )

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  cohortDateRange = c(as.Date("1990-01-01"), as.Date("2009-12-31")),
  ageGroup = list(c(0, 150)),
  sex = "Both",
  daysPriorObservation = 365
)
cdm$denominator

cohortCount(cdm$denominator)

cdm$denominator %>%
  filter(subject_id %in% c("1", "2", "3", "4", "5"))

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  cohortDateRange = c(as.Date("1990-01-01"), as.Date("2009-12-31")),
  ageGroup = list(c(18, 65)),
  sex = "Female",
  daysPriorObservation = 365
)
cdm$denominator %>%
  glimpse()

cohortCount(cdm$denominator)

cdm$denominator %>%
  filter(subject_id %in% c("1", "2", "3", "4", "5"))

## ----message=FALSE, warning=FALSE---------------------------------------------
cdm <- mockIncidencePrevalence(sampleSize = 500, 
                                  earliestObservationStartDate = as.Date("2000-01-01"),
                                  latestObservationStartDate =  as.Date("2005-01-01"),
                                  minDaysToObservationEnd = 10000, 
                                  maxDaysToObservationEnd = NULL,
                                  earliestDateOfBirth = as.Date("1960-01-01"), 
                                  latestDateOfBirth = as.Date("1980-01-01"))

cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  ageGroup = list(
    c(0, 40),
    c(41, 100)
  ),
  sex = "Both",
  daysPriorObservation = 0
)
cdm$denominator %>%
  filter(subject_id %in% !!as.character(seq(1:30))) %>%
  collect() %>%
  left_join(settings(cdm$denominator),
            by = "cohort_definition_id") %>%
  pivot_longer(cols = c(
    "cohort_start_date",
    "cohort_end_date"
  )) %>%
  mutate(subject_id = factor(as.numeric(subject_id))) %>%
  ggplot(aes(x = subject_id, y = value, colour = age_group)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  theme_minimal() +
  theme(legend.position = "top", 
        legend.title = element_blank()) +
  ylab("Year") +
  coord_flip()

## ----message=FALSE, warning=FALSE, fig.height=8-------------------------------
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  ageGroup = list(
    c(0, 40),
    c(41, 100)
  ),
  sex = c("Male", "Female", "Both"),
  daysPriorObservation = 0
)
cdm$denominator %>%
  filter(subject_id %in% !!as.character(seq(1:15))) %>%
  collect() %>%
  left_join(settings(cdm$denominator)) %>%
  pivot_longer(cols = c(
    "cohort_start_date",
    "cohort_end_date"
  )) %>%
  mutate(subject_id = factor(as.numeric(subject_id))) %>%
  ggplot(aes(x = subject_id, y = value, colour = age_group)) +
    facet_grid(sex ~ ., space = "free_y") +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(legend.position = "top", 
        legend.title = element_blank()) +
  ylab("Year") +
  coord_flip()

## ----message=FALSE, warning=FALSE, fig.height=10------------------------------
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  ageGroup = list(
    c(0, 40),
    c(41, 100)
  ),
  sex = c("Male", "Female", "Both"),
  daysPriorObservation = c(0, 365)
)
cdm$denominator %>%
  filter(subject_id %in% !!as.character(seq(1:8))) %>%
  collect() %>%
  left_join(cohortSet(cdm$denominator)) %>%
  pivot_longer(cols = c(
    "cohort_start_date",
    "cohort_end_date"
  )) %>%
  mutate(subject_id = factor(as.numeric(subject_id))) %>%
  ggplot(aes(x = subject_id, y = value, colour = age_group, 
             linetype = sex, shape = sex
             )) +
  facet_grid(sex + days_prior_observation ~ ., space = "free", 
             scales = "free") +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(legend.position = "top") +
  ylab("Year") +
  coord_flip()

## ----message=FALSE, warning=FALSE, fig.height=10------------------------------
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  ageGroup = list(
    c(0, 40),
    c(41, 100)
  ),
  sex = c("Male", "Female", "Both"),
  daysPriorObservation = c(0, 365),
  requirementInteractions = FALSE
)

cdm$denominator %>%
  filter(subject_id %in% !!as.character(seq(1:8))) %>%
  collect() %>%
  left_join(cohortSet(cdm$denominator)) %>%
  pivot_longer(cols = c(
    "cohort_start_date",
    "cohort_end_date"
  )) %>%
  mutate(subject_id = factor(as.numeric(subject_id))) %>%
  ggplot(aes(x = subject_id, y = value, colour = age_group, 
             linetype = sex, shape = sex
             )) +
  facet_grid(sex + days_prior_observation ~ ., space = "free", 
             scales = "free") +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(legend.position = "top") +
  ylab("Year") +
  coord_flip()

## ----message=TRUE, warning=FALSE, message=FALSE-------------------------------
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  cohortDateRange = c(as.Date("1990-01-01"), as.Date("2009-12-31")),
  ageGroup = list(
    c(0, 18),
    c(19, 100)
  ),
  sex = c("Male", "Female"),
  daysPriorObservation = c(0, 365)
)

head(cdm$denominator, 8)

## ----message=TRUE, warning=FALSE----------------------------------------------
settings(cdm$denominator) %>% 
  glimpse()

## ----message=TRUE, warning=FALSE----------------------------------------------
cohortCount(cdm$denominator) %>% 
  glimpse()

## ----message=TRUE, warning=FALSE----------------------------------------------
attrition(cdm$denominator) %>% 
  glimpse()

