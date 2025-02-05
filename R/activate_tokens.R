

#' activate_tokens
#'
#' Initialise the survey participant table of a survey where new participant
#' tokens may be later added.
#'
#' @param iSurveyID integer, ID of the Survey where a survey participants table
#' will be created for
#' @param aAttributeFields list, An array of integer describing any additional
#' attribute fields
#'
#' @return status
#' @export

activate_tokens <- function(iSurveyID, aAttributeFields = NULL) {
  params <- list("iSurveyID" = iSurveyID, "aAttributeFields" = aAttributeFields)

  resp <- tryCatch({
    call_limer(method = "activate_tokens", params = params)
  }, error = function(e) {
      stop(gsub("^.*: [0-9]+|The SQL.*","",  e[["message"]]) %>% trimws(), call. = F)
  })
  return(resp %>% unlist())
}


#' @rdname activate_tokens
#' @export
create_participants_table <- activate_tokens

