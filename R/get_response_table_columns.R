
#' get_response_table_columns
#'
#' Returns the column names of the response table
#'
#' @param iSurveyID integer, Id of the survey
#' @param verbose boolean, Giving out logging info
#'
#' @return String vector
#' @export
#' @note This function requires that the patch has been implemented in the
#' remotecontrol_handle.php file of your Limesurvey installation.
#' application/helpers/remotecontrol/remotecontrol_handle.php
#' system.file("patch.php", package="limer")

get_response_table_columns <- function(iSurveyID, verbose = FALSE){

  iSurveyID <- as.numeric(iSurveyID) %>% suppressWarnings()

  if (is.na(iSurveyID))
    stop("No valid iSurveyID passed. iSurveyID must be a six-digit number!",
         call. = F)

  res <- call_limer("get_response_table_columns",
                    params = list("iSurveyID" = iSurveyID)
  )



  return(res %>% unlist())
}

