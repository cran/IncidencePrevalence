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

#'  Participants contributing to an analysis
#'
#' @param result Result object
#' @param analysisId ID of a specific analysis to return participants for
#'
#' @return References to tables with the study participants contributing to
#' a given analysis
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 200)
#' cdm <- generateDenominatorCohortSet(cdm, name = "denominator")
#' incidence <- estimateIncidence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome",
#'   interval = "overall"
#' )
#' participants(result = incidence, analysisId = 1)
#' }
participants <- function(result, analysisId) {
  UseMethod("participants")
}

#' @export
participants.IncidencePrevalenceResult <- function(result,
                                                   analysisId) {

  lifecycle::deprecate_warn(
    when = "0.8.0",
    what = "IncidencePrevalence::participants()",
    details = "participants will be removed in the next release",
    always = TRUE
  )

  checkmate::assertIntegerish(analysisId)

  if (!is.null(attr(result, "participants"))) {
    included <- attr(result, "participants") %>%
      dplyr::select(
        "subject_id",
        paste0(
          "cohort_start_date",
          "_analysis_",
          analysisId
        ),
        paste0(
          "cohort_end_date",
          "_analysis_",
          analysisId
        ),
        paste0(
          "outcome_start_date",
          "_analysis_",
          analysisId
        )
      ) %>%
      dplyr::rename(
        "cohort_start_date" = paste0(
          "cohort_start_date",
          "_analysis_",
          analysisId
        ),
        "cohort_end_date" = paste0(
          "cohort_end_date",
          "_analysis_",
          analysisId
        ),
        "outcome_start_date" = paste0(
          "outcome_start_date",
          "_analysis_",
          analysisId
        )
      )

    included <- included %>%
      dplyr::filter(!is.na(.data$cohort_start_date)) %>%
      dplyr::mutate(cohort_start_date = as.Date(.data$cohort_start_date),
                    cohort_end_date  = as.Date(.data$cohort_end_date),
                    outcome_start_date = as.Date(.data$outcome_start_date))
  } else {
    included <- NULL
  }

  return(included)
}
