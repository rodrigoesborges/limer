#' get_survey_list
#'
#' If user is admin he can get surveys of every user (parameter sUsername) or
#' all surveys (sUsername=null).
#'
#' @param sUsername string, username
#' @param names boolean, Shall be returned only the names of the surveys.
#' @param sid boolean, Shall be given back only the Ids of the surveys.
#'
#' @return dataframe, survey titles (names), or survey ids (sid)
#' @export
#' @references https://api.limesurvey.org/classes/remotecontrol_handle.html#method_list_surveys
get_survey_list <- function(sUsername = NULL, names = FALSE, sid = TRUE){

  res <- call_limer("list_surveys",
                    params = list("sUsername" = sUsername)
  )

  data <- data.table::rbindlist(res) %>% suppressWarnings() %>% as.data.frame()
  if (names)
    return(data$surveyls_title)

  if (sid)
    return(data$sid)

  return(data)
}

