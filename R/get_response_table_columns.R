
#' get_response_table_columns
#'
#' Returns the column names of the response table
#'
#' @param iSurveyID integer, Id of the survey
#' @param verbose boolean, Giving out logging info
#' @param sHeadingType character, "full" returns the complete text of the
#' questions, "complete" returns the code. Default = "full"
#'
#' @return String vector
#' @export
#' @note This function requires that the patch has been implemented in the
#' remotecontrol_handle.php file of your Limesurvey installation.
#' application/helpers/remotecontrol/remotecontrol_handle.php
#' system.file("patch.php", package="limer")

get_response_table_columns <- function(iSurveyID, verbose = FALSE,  sHeadingType = "full"){

  iSurveyID <- as.numeric(iSurveyID) %>% suppressWarnings()

  if (is.na(iSurveyID))
    stop("No valid iSurveyID passed. iSurveyID must be a six-digit number!",
         call. = F)

  res <- call_limer("get_responses",
                    params = list("iSurveyID" = iSurveyID)
  )

  if (any(res %>% unlist() == "No Data, survey table does not exist (propably yet not activated)."))
    stop(res %>% unlist(), call. = F)


  return(colnames(res))
}

