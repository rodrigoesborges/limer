#' reset_survey
#'
#' Resets or copy a survey to its original state by deleting the existing
#' survey and re-importing it.
#'
#' @param iSurveyID integer, Id of the survey that will be reset
#' @param sImportData string, Path to the file. If not set, the previous survey
#' is re-imported in its previous structure
#' @param sNewSurveyName String, if not set the previous name is used (optional)
#' @param DestSurveyID string, Id of the copied survey (optional)
#' @param verbose boolean, Giving out logging info
#'
#' @export
#'

reset_survey <- function(iSurveyID, sImportData = NULL, sNewSurveyName = NULL, DestSurveyID = NULL, verbose = FALSE) {

  if (is.null(sImportData)) {

    export_survey_structure(iSurveyID, filename = "tmp.lss", verbose = verbose)
    sImportData <- "tmp.lss"
  }

  if (!file.exists(sImportData))
    stop(glue::glue(
      "Could not find the structure file {sImportData} in this location"
    ), call. = F)



  if (is.null(DestSurveyID)) {
    DestSurveyID <- iSurveyID
    delete_survey(iSurveyID, verbose = verbose)
  } else {
    if (verbose)
      message(glue::glue("copy the survey to the new ID {DestSurveyID}"))
  }

  import_survey_structure(sImportData, sNewSurveyName = sNewSurveyName, DestSurveyID = DestSurveyID, verbose = verbose)

  # cleanup
  if (sImportData == "tmp.lss")
    silence <- file.remove("tmp.lss")

}
