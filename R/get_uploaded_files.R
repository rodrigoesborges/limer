#' Obtain all uploaded files for all responses
#'
#' This function downloads all uploaded files to all responses from a LimeSurvey survey.
#' @param iSurveyID \dots
#' @param iStart \dots
#' @param iLimit \dots
#' @param bUnused \dots
#' @param aAttributes \dots
#' @export
#' @references https://api.limesurvey.org/classes/remotecontrol_handle.html#method_get_uploaded_files
#' @examples \dontrun{
#' get_uploaded_files(12345)
#' get_uploaded_files(12345, iStart=1, iLimit=10, bUnused=FALSE, aAttributes=FALSE)
#' }

get_uploaded_files <- function(iSurveyID,zzdownloadpath){
  # Put all the function's arguments in a list to then be passed to call_limer()
  params <- as.list(environment())
  params <- params[-length(params)]
  results <- call_limer(method = "get_uploaded_files", params = params)
  writeBin(results,zzdownloadpath)
}
