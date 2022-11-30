#' Export LSS File from a LimeSurvey survey
#'
#' This function exports a survey structure (LSS).
#'
#' @param iSurveyID \dots
#' @param filename \dots
#' @param verbose boolean, Giving out logging info
#' @export
#' @examples \dontrun{
#' export_survey_structure(12345)
#' }
#' @note This function requires that the patch has been implemented in the
#' remotecontrol_handle.php file of your Limesurvey installation.
#' application/helpers/remotecontrol/remotecontrol_handle.php
#' system.file("patch.php", package="limer")
#' See @references
#' @references https://bugs.limesurvey.org/view.php?id=17747

export_survey_structure <- function(iSurveyID, filename = NULL, verbose = FALSE) {

  x <- call_limer("export_survey_structure",
                                   params = list("iSurveyID_org" = iSurveyID))
  if (is.null(x))
    stop(glue::glue("Could not export survey, server response from {getOption('lime_api')} is NULL.\nIs the function 'export_survey_structure()' installed in the remote_control API?"), call. = FALSE)

  if (length(x) > 1)
    stop(x$error, call. = F)

  survey_data_raw <- rawToChar(base64enc::base64decode(x))

  if (is.null(filename)) {
    filename <- glue::glue("limesurvey_survey_{iSurveyID}.lss")
  } else {
    if (!grepl("lss$", filename))
      filename <- glue::glue("{filename}.lss")
  }



    writeLines(survey_data_raw, filename)
  if (verbose)
    message(filename, " saved!")

}
