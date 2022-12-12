#' reset_survey
#'
#' Resets a survey to its original state by deleting the existing survey and
#' re-importing it.
#'
#' @param iSurveyID integer, Id of the survey that will be reset
#' @param sImportData string, Path to the file. If not set, the previous survey
#' is re-imported in its previous structure
#' @param sNewSurveyName String, if not set the previous name is used
#' @param verbose boolean, Giving out logging info
#'
#' @export
#'

reset_survey <- function(iSurveyID, sImportData = NULL, sNewSurveyName = NULL, verbose = FALSE) {

  if (is.null(sImportData)) {

    export_survey_structure(iSurveyID, filename = "tmp.lss", verbose = verbose)
    sImportData <- "tmp.lss"
  }

  if (!file.exists(sImportData))
    stop(glue::glue(
      "Could not find the structure file {sImportData} in this location"
    ), call. = F)

  delete_survey(iSurveyID, verbose = verbose)

  import_survey_structure(sImportData, sNewSurveyName = sNewSurveyName, DestSurveyID = iSurveyID, verbose = verbose)

  # cleanup
  if (sImportData == "tmp.lss")
    silence <- file.remove("tmp.lss")

}
