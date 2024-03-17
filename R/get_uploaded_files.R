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

get_uploaded_files <- function(iSurveyID,responseId,sToken="",downloadpath="."){
  # Put all the function's arguments in a list to then be passed to call_limer()
  params <- as.list(environment())
  params <- params[-length(params)]

  if((is.null(responseId)))  {
    stop("Please define response id to downlad uploaded files from")
  }
  results <- call_limer(method = "get_uploaded_files", params = params)
  if(!(is.null(results$error)))  {
    stop("Error in request")
  }

  updfile <- results[[1]]$content

  updfile <- base64enc::base64decode(updfile)

  filename <- paste0(iSurveyID,"-",responseId,".",results[[1]]$meta$ext)

  writeBin(updfile,paste0(downloadpath,"/",filename))
  print(paste("Downloaded uploaded files from Survey wih ID",iSurveyID,"and response ID",responseId,"to folder",
              downloadpath, "withe name", filename))

}
