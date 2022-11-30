

#' delete_survey
#'
#' Deletes a survey and all its data
#'
#' @param iSurveyID integer, Id of the survey to be deleted
#' @param verbose boolean, Giving out logging info
#' @export
#'
#' @references https://api.limesurvey.org/classes/Survey.html#method_deleteSurvey

delete_survey <- function(iSurveyID, verbose) {
  iSurveyID <- as.numeric(iSurveyID) %>% suppressWarnings()
  if (is.na(iSurveyID))
    stop("No valid iSurveyID passed. iSurveyID must be a six-digit number!",
         call. = F)
  msg <- call_limer("delete_survey",
                    params = list("iSurveyID" = iSurveyID))

  if (msg == "No permission") {
    stop(
      glue::glue(
        "Either the survey with the ID \u00b4{iSurveyID}\u00b4 does not exist or the permission to delete it is missing!"
      ),
      call. = F
    )
  } else if (msg == "OK") {
    if (verbose)
      message("Survey with id \u00b4",
            iSurveyID,
            "\u00b4 successfully deleted!")
  } else {
    if (verbose)
      message(msg)
  }


}
