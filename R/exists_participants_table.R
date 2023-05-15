#' exists_participants_table
#'
#' checks if a participant table already exists
#'
#' @param iSurveyID integer, ID of the Survey
#'
#' @return boolean
#' @export
#'

exists_participants_table <- function(iSurveyID) {
  params <- list("iSurveyID" = iSurveyID,"bUnused" = TRUE, "iLimit" = 1, "iStart" = 1 )
  resp <- call_limer(method = "list_participants", params = params)
  if (!"status" %in% names(resp) || resp$status == "No survey participants found.") {
    TRUE
  } else {
    FALSE
  }


  #
  # tryCatch({
  #
  #   params <- list("iSurveyID" = iSurveyID,"bUnused" = TRUE, "iLimit" = 1, "iStart" = 1 )
  #   df1 <- call_limer(method = "list_participants", params = params)
  #   # dfs <- lapply(df, data.frame, stringsAsFactors = FALSE)
  #   # data <- dplyr::bind_rows(dfs)
  #
  #   # if (length(data) == 1)
  #   #   stop(data[1,1], call. = F)
  #
  #   rsp <- list_participants(iSurveyID, iStart = 1, iLimit = 1)
  #   if (inherits(rsp, "data.frame"))
  #     TRUE
  # }, error = function(e) {
  #   if (e[["message"]] == "Error: No survey participants table" ) {
  #     FALSE
  #   } else {
  #     message(sprintf("Error in %s: %s", deparse(e[["call"]]), e[["message"]]))
  #   }
  # })
}