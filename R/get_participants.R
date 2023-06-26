#' Export list of participants from a LimeSurvey survey
#'
#' This function exports and downloads the list of participants from a LimeSurvey survey.
#' @param iSurveyID \dots
#' @param iStart \dots
#' @param iLimit \dots
#' @param bUnused \dots
#' @param aAttributes \dots
#' @export
#' @references https://api.limesurvey.org/classes/remotecontrol_handle.html#method_list_participants
#' @examples \dontrun{
#' get_participants(12345, iStart=1, iLimit=10, bUnused=FALSE,
#'                                    aAttributes=c('attribute_1','attribute_2'))
#' get_participants(12345, iStart=1, iLimit=10, bUnused=FALSE, aAttributes=FALSE)
#' }

get_participants <- function(iSurveyID, iStart = 1, iLimit, bUnused, aAttributes){
  # Put all the function's arguments in a list to then be passed to call_limer()
  params <- as.list(environment())
  if (is_missing(iLimit)) {
    iLimit <- limer::get_participants(iSurveyID, iStart = 1, iLimit = 1000000) %>%
      dplyr::pull(tid) %>% max() %>% as.numeric()
    params$iLimit <-  iLimit
  }
  results <- call_limer(method = "list_participants", params = params)
  if (!inherits(results, "data.frame"))
    warning(results, call. = FALSE)


  return(results)
}
