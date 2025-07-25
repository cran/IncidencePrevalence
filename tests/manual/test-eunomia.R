test_that("eunomia test - some empty cohorts", {
  db <- DBI::dbConnect(duckdb::duckdb(),
    dbdir = CDMConnector::eunomia_dir()
  )
  cdm <- CDMConnector::cdmFromCon(
    con = db,
    cdmSchema = "main",
    writeSchema = "main"
  )

  # outcome cohorts
  acetaminophen_codes <- dplyr::tibble("concept_id" = c(1127433))
  diclofenac_codes <- dplyr::tibble("concept_id" = c(1124300))
  cdm$drug_exposure <- cdm$drug_exposure |>
    PatientProfiles::addInObservation("drug_exposure_start_date") |>
    dplyr::filter(in_observation == 1)

  # celecoxib
  cdm$acetaminophen <- cdm$drug_exposure %>%
    dplyr::inner_join(
      acetaminophen_codes %>%
        dplyr::select(concept_id),
      by = c("drug_concept_id" = "concept_id"),
      copy = TRUE
    ) %>%
    dplyr::select(
      "subject_id" = "person_id",
      "cohort_start_date" = "drug_exposure_start_date",
      "cohort_end_date" = "drug_exposure_start_date"
    ) %>%
    dplyr::mutate(cohort_definition_id = 1L) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date"
    ) %>%
    dplyr::compute()

  # diclofenac
  cdm$diclofenac <- cdm$drug_exposure %>%
    dplyr::inner_join(
      diclofenac_codes %>%
        dplyr::select(concept_id),
      by = c("drug_concept_id" = "concept_id"),
      copy = TRUE
    ) %>%
    dplyr::select(
      "subject_id" = "person_id",
      "cohort_start_date" = "drug_exposure_start_date",
      "cohort_end_date" = "drug_exposure_start_date"
    ) %>%
    dplyr::mutate(cohort_definition_id = 2L) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date"
    ) %>%
    dplyr::compute()

  cdm$outcome_cohort <- dplyr::union_all(
    cdm$acetaminophen,
    cdm$diclofenac
  ) %>%
    dplyr::compute(name = "outcome_cohort", temporary = FALSE)
  cdm$outcome_cohort <- CDMConnector::newGeneratedCohortSet(cdm$outcome_cohort)

  # denominator
  cdm <- generateDenominatorCohortSet(cdm,
    name = "denominator",
    cohortDateRange = c(
      as.Date("2000-01-01"),
      as.Date("2020-01-01")
    ),
    sex = c("Male", "Female")
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome_cohort",
    interval = "years"
  )
  expect_true(nrow(inc) > 0)

  prev <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome_cohort",
    interval = "years"
  )
  expect_true(nrow(prev) > 0)

  plotIncidence(inc,
    facet = c("outcome_cohort_name", "denominator_sex"),
    colour = "denominator_sex"
  )

  plotPrevalence(prev,
    facet = c("outcome_cohort_name", "denominator_sex"),
    colour = "denominator_sex"
  )
})

test_that("eunomia test - strata", {
  # Update  to your database details as appropriate here
  db <- DBI::dbConnect(duckdb::duckdb(),
    dbdir = CDMConnector::eunomia_dir()
  )
  cdm <- CDMConnector::cdmFromCon(
    con = db,
    cdmSchema = "main",
    writeSchema = "main"
  )

  asthma_cohort1 <- Capr::cohort(entry = Capr::entry(
    Capr::conditionOccurrence(Capr::cs(Capr::descendants(317009),
      name = "asthma"
    )),
    primaryCriteriaLimit = "First"
  ))
  asthma_cohort2 <- Capr::cohort(entry = Capr::entry(
    Capr::conditionOccurrence(
      Capr::cs(Capr::descendants(317009),
        name = "asthma"
      ),
      Capr::age(Capr::gte(18))
    ),
    primaryCriteriaLimit = "First"
  ))


  path1 <- file.path(tempdir(), "outcome")
  dir.create(path1)
  Capr::writeCohort(asthma_cohort1, file.path(path1, "asthma_cohort1.json"))

  path2 <- file.path(tempdir(), "strata")
  dir.create(path2)
  Capr::writeCohort(asthma_cohort2, file.path(path2, "asthma_cohort2.json"))



  outcome_cohort_set <- CDMConnector::readCohortSet(path = path1)
  cdm <- CDMConnector::generateCohortSet(
    cdm,
    outcome_cohort_set,
    name = "outcome",
    computeAttrition = TRUE,
    overwrite = TRUE
  )

  strata_cohort_set <- CDMConnector::readCohortSet(path = path2)
  cdm <- CDMConnector::generateCohortSet(
    cdm,
    strata_cohort_set,
    name = "strata",
    computeAttrition = TRUE,
    overwrite = TRUE
  )


  cdm$dpop <- generateDenominatorCohortSet(cdm,
    name = "dpop",
    targetCohortTable = "strata"
  )


  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "dpop",
    outcomeTable = "asthma",
    targetCohortTable = "target",
    interval = "years"
  )
  expect_true(nrow(inc) > 0)

  prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "dpop",
    outcomeTable = "asthma",
    interval = "years"
  )
  expect_true(nrow(prev) > 0)
})

test_that("eunomia test - participants", {
  db <- DBI::dbConnect(duckdb::duckdb(),
    dbdir = CDMConnector::eunomia_dir()
  )
  cdm <- CDMConnector::cdmFromCon(
    con = db,
    cdmSchema = "main",
    writeSchema = "main"
  )
  cdm$dpop <- generateDenominatorCohortSet(cdm)
  asthma_cohort <- Capr::cohort(entry = Capr::entry(
    Capr::condition(Capr::cs(Capr::descendants(317009))),
    primaryCriteriaLimit = "First"
  ))

  path <- file.path(tempdir(), "asthma_cohorts_participants")
  dir.create(path)

  Capr::writeCohort(asthma_cohort, file.path(path, "asthma_cohort.json"))

  asthma_cohort_set <- CDMConnector::readCohortSet(path = path)

  cdm <- CDMConnector::generateCohortSet(
    cdm,
    asthma_cohort_set,
    name = "asthma",
    computeAttrition = TRUE,
    overwrite = TRUE
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "dpop",
    outcomeTable = "asthma",
    interval = "years",
    returnParticipants = TRUE,
    tablePrefix = "test_inc"
  )
  expect_true(nrow(inc) > 0)
  participants(inc, 1)


  prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "dpop",
    outcomeTable = "asthma",
    interval = "years"
  )
  expect_true(nrow(prev) > 0)
})
