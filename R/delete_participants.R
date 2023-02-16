


#' delete_participants
#'
#' delete participants from a survey by id
#'
#' @param iSurveyID integer, ID of the Survey to delete participants
#' @param aTokenIDs integer, Vector with the ID in the participant table
#' @param chunksize integer, size of chunks for handling php memory problems
#' @param max_id integer, up to which maximum id should the participants be
#' deleted? only necessary if aTokenIDs is NULL
#' @export
#'

delete_participants <-
  function(iSurveyID,
           aTokenIDs = NULL,
           max_id = 1000000,
           chunksize = 500) {
    options(scipen = 999)

    cat("Delete all participants up to the maximum id number `", max_id,"`. Proceed?[Y/N]");
    a <- readline() %>% tolower()
    if (a != "y")
      return("end without deleting participants")


    aTokenIDs <- NULL
    if (is.null(aTokenIDs)) {
      aTokenIDs <- as.list(1:max_id)
    } else {
      aTokenIDs <- as.list(aTokenIDs)
    }

    names(aTokenIDs) <- rep("aTokenIDs", length(aTokenIDs))

    n <- length(aTokenIDs)/chunksize
    limit <- 0
    for (i in 1:n) {
    params <-
      list("iSurveyID" = iSurveyID, "aTokenIDs" = as.list(aTokenIDs[limit:(limit + chunksize)]))

    resp <-
      call_limer(method = "delete_participants", params = params)

    limit <- limit + chunksize + 1
    cat("\r",round((i/n)*100, digits = 2), "%")
    utils::flush.console()

    }
    cat("\r")
    utils::flush.console()
  }
