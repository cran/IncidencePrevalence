# Copyright 2024 DARWIN EU®
#
# This file is part of IncidencePrevalence
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

getPrevalence <- function(cdm,
                          denominatorTable,
                          denominatorCohortId,
                          outcomeTable,
                          outcomeCohortId,
                          type,
                          interval,
                          completeDatabaseIntervals,
                          timePoint,
                          fullContribution,
                          tablePrefix,
                          returnParticipants,
                          analysisId,
                          strata,
                          includeOverallStrata) {

  # keeping outcome of interest
  # of people in the denominator of interest
  studyPop <- cdm[[denominatorTable]] %>%
    dplyr::mutate(cohort_start_date = as.Date(.data$cohort_start_date),
                  cohort_end_date = as.Date(.data$cohort_end_date)) %>%
    dplyr::filter(.data$cohort_definition_id ==
                    .env$denominatorCohortId) %>%
    dplyr::select(-"cohort_definition_id") %>%
    dplyr::left_join(
      cdm[[outcomeTable]] %>%
        dplyr::filter(.data$cohort_definition_id == .env$outcomeCohortId) %>%
        dplyr::rename("outcome_start_date" = "cohort_start_date",
                      "outcome_end_date" = "cohort_end_date") %>%
        dplyr::select(
          "subject_id", "outcome_start_date",
          "outcome_end_date"
        ),
      by = "subject_id"
    )

  if(isTRUE(returnParticipants)){
    # continue on the database side so we can keep participants
    studyPop <- studyPop %>%
      dplyr::compute(
        name = paste0(tablePrefix, "_prev_working_1"),
        temporary = FALSE,
        overwrite = TRUE
      )
  } else {
    # otherwise collect already for
    studyPop <- studyPop %>%
      dplyr::collect()
  }

  attrition <- recordAttrition(
    table = studyPop,
    id = "subject_id",
    reasonId = 11,
    reason = "Starting analysis population"
  )

  startEnd <- studyPop %>%
    dplyr::summarise(
      min = min(.data$cohort_start_date, na.rm = TRUE),
      max = max(.data$cohort_end_date, na.rm = TRUE)
    ) %>%
    dplyr::collect()

  # get studyDays as a function of inputs
  studyDays <- getStudyDays(
    startDate = as.Date(startEnd$min),
    endDate =  as.Date(startEnd$max),
    timeInterval = interval,
    completeDatabaseIntervals = completeDatabaseIntervals,
    type = type,
    timePoint = timePoint
  )

  if (nrow(studyDays) == 0) {
    # if no study days we´ll return an empty tibble
    pr <- dplyr::tibble()

    attrition <- recordAttrition(
      table = dplyr::tibble(subject_id = integer()),
      id = "subject_id",
      reasonId = 12,
      reason = "Not observed during the complete database interval",
      existingAttrition = attrition
    )
    attrition <- recordAttrition(
      table = dplyr::tibble(subject_id = integer()),
      id = "subject_id",
      reasonId = 13,
      reason = "Do not satisfy full contribution requirement for any time interval",
      existingAttrition = attrition
    )
  } else {
    # drop for complete database intervals requirement
    minStartDate <- min(studyDays$start_time)
    maxStartDate <- max(studyDays$end_time)

    minStartDateChar <- as.character(minStartDate)
    maxStartDateChar <- as.character(maxStartDate)

    studyPop <- studyPop %>%
      dplyr::filter(
        .data$cohort_end_date >= as.Date(.env$minStartDateChar),
        .data$cohort_start_date <= as.Date(.env$maxStartDateChar)
      )

    attrition <- recordAttrition(
      table = studyPop,
      id = "subject_id",
      reasonId = 12,
      reason = "Not observed during the complete database interval",
      existingAttrition = attrition
    )

    # drop people who never fulfill contribution requirement
    if (fullContribution == TRUE) {
      if(isTRUE(returnParticipants)){
        checkExpression <- glue::glue("(.data$cohort_end_date >= local(studyDays$end_time[{seq_along(studyDays$end_time)}]) &
           .data$cohort_start_date <= local(studyDays$start_time[{seq_along(studyDays$start_time)}]))") %>%
          paste0(collapse = "||") %>%
          rlang::parse_expr()
      } else {
        checkExpression <- glue::glue("(.data$cohort_end_date >= studyDays$end_time[{seq_along(studyDays$end_time)}]) &
           (.data$cohort_start_date <= studyDays$start_time[{seq_along(studyDays$start_time)}])") %>%
          paste0(collapse = "|") %>%
          rlang::parse_expr()
      }

      studyPop <- studyPop %>%
        dplyr::mutate(
          has_full_contribution = dplyr::if_else(!!checkExpression,
                                                 1L,
                                                 0L
          )
        ) %>%
        dplyr::filter(.data$has_full_contribution >= 1) %>%
        dplyr::select(-"has_full_contribution")

      attrition <- recordAttrition(
        table = studyPop,
        id = "subject_id",
        reasonId = 13,
        reason = "Do not satisfy full contribution requirement for an interval",
        existingAttrition = attrition
      )
    } else if (fullContribution == FALSE) { # otherwise nobody excluded
      attrition <- recordAttrition(
        table = studyPop,
        id = "subject_id",
        reasonId = 14,
        reason = "Do not satisfy full contribution requirement for an interval",
        existingAttrition = attrition
      )
    }

    if (returnParticipants == TRUE) {
      # if using permanent tables (that get overwritten)
      # we need to keep a permanent one for a given analysis
      # so that we can refer back to it (e.g when using participants() function)
      studyPopParticipants <- studyPop %>%
        dplyr::select(!"outcome_end_date") %>%
        dplyr::rename(
          !!paste0(
            "cohort_start_date",
            "_analysis_",
            analysisId
          ) := "cohort_start_date",
          !!paste0(
            "cohort_end_date",
            "_analysis_",
            analysisId
          ) := "cohort_end_date",
          !!paste0(
            "outcome_start_date",
            "_analysis_",
            analysisId
          ) := "outcome_start_date"
        ) %>%
        dplyr::compute(
          name = paste0(
            tablePrefix,
            "_analysis_",
            analysisId
          ),
          temporary = FALSE,
          overwrite = TRUE
        )

      studyPop <- studyPop %>%
        dplyr::collect()

      CDMConnector::dropTable(
        cdm = cdm,
        name = dplyr::starts_with(paste0(tablePrefix, "_prev_working_"))
      )

    }

    # fetch prevalence
    # looping through each time interval
    pr <- vector(mode = "list", length = length(studyDays$time))

    for (i in seq_along(studyDays$time)) {
      workingStart <- studyDays$start_time[i]
      workingEnd <- studyDays$end_time[i]

      if (fullContribution == TRUE) {
        # require presence for all of period
        # drop people with end_date not after workingEnd
        # and start_date not before workingStart
        workingPop <- studyPop %>%
          dplyr::filter(.data$cohort_end_date >= .env$workingEnd,
                        .data$cohort_start_date <= .env$workingStart)
      } else {
        # otherwise include people if they can contribute a day
        # drop people with end_date prior to workingStart
        # and start_date after workingEnd
        workingPop <- studyPop %>%
          dplyr::filter(.data$cohort_end_date >= .env$workingStart,
                        .data$cohort_start_date <= .env$workingEnd)
      }

      workingPop <- workingPop %>%
        dplyr::mutate(
          # individuals start date for this period
          # which could be start of the period or later
          cohort_start_date =
            dplyr::if_else(.data$cohort_start_date <= .env$workingStart,
                           .env$workingStart,
                           as.Date(.data$cohort_start_date)
            ),
          # individuals end date for this period
          # end of the period or earlier
          cohort_end_date =
            dplyr::if_else(.data$cohort_end_date >= .env$workingEnd,
                           .env$workingEnd,
                           as.Date(.data$cohort_end_date)
            )
        )

      if(length(strata) == 0 || includeOverallStrata == TRUE){
        # include ongoing in current time of interest
        result <- workingPop %>%
          dplyr::summarise(
            n_persons = dplyr::n_distinct(.data$subject_id),
            n_cases = dplyr::n_distinct(.data$subject_id[
              !is.na(.data$outcome_start_date) &
                .data$outcome_start_date <= .data$cohort_end_date &
                .data$outcome_end_date >= .data$cohort_start_date
            ])
          )

        pr[[paste0(i)]] <- dplyr::tibble(
          n_population = result$n_persons,
          n_cases = result$n_cases
        )
      } else {
        pr[[paste0(i)]] <- dplyr::tibble()
      }

      if(length(strata)>=1){
        pr[[paste0(i)]] <- pr[[paste0(i)]] %>%
          dplyr::mutate(strata_name = "Overall",
                        strata_level = "Overall")
        for(j in seq_along(strata)){
          pr[[paste0(i)]] <- dplyr::bind_rows(pr[[paste0(i)]],
                                              getStratifiedPrevalenceResult(workingPop,
                                                                            workingStrata = strata[[j]]
                                              ) %>%
                                                dplyr::rename("n_population" = "n_persons"))
        }}
      pr[[paste0(i)]] <- dplyr::tibble(cbind(pr[[paste0(i)]], studyDays[i, ]))

    }

    pr <- dplyr::bind_rows(pr) %>%
      dplyr::mutate(prevalence = .data$n_cases / .data$n_population) %>%
      dplyr::select(dplyr::any_of(c(
        "n_cases", "n_population",
        "prevalence", "start_time", "end_time",
        "strata_name", "strata_level"
      ))) %>%
      dplyr::rename("prevalence_start_date" = "start_time") %>%
      dplyr::rename("prevalence_end_date" = "end_time")
  }


  results <- list()
  results[["pr"]] <- pr
  results[["attrition"]] <- attrition
  if (returnParticipants == TRUE) {
    if(nrow(pr) > 0){
      results[["person_table"]] <- studyPopParticipants
    }
  }

  return(results)
}

getStratifiedPrevalenceResult <- function(workingPop, workingStrata){

  # include ongoing in current time of interest
  result <- workingPop %>%
    dplyr::group_by(dplyr::pick(.env$workingStrata)) %>%
    dplyr::summarise(
      n_persons = dplyr::n_distinct(.data$subject_id),
      n_cases = dplyr::n_distinct(.data$subject_id[
        !is.na(.data$outcome_start_date) &
          .data$outcome_start_date <= .data$cohort_end_date &
          .data$outcome_end_date >= .data$cohort_start_date
      ])
    ) %>%
    dplyr::ungroup()


  result <- result %>%
    tidyr::unite("strata_level",
                 c(dplyr::all_of(.env$workingStrata)),
                 remove = FALSE, sep = " and ") %>%
    dplyr::mutate(strata_name = !!paste0(workingStrata, collapse = " and ")) %>%
    dplyr::relocate("strata_level", .after = "strata_name") %>%
    dplyr::select(!dplyr::any_of(workingStrata))

  return(result)

}
