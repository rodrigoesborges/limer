
#' add_participants
#'
#' Adds participants to a survey
#'
#' @param iSurveyID integer, ID of the Survey to insert responses
#' @param data dataframe with the columns firstname, lastname, email and token
#' @param bCreateToken boolean Should tokens be created
#' @param chunksize integer, size of chunks for handling php memory problems
#'
#' @return API Response
#' @export
#'

add_participants <- function(iSurveyID, data, bCreateToken = F, chunksize = 200){
  if (!all(colnames(data) %in% c("token", "firstname", "lastname", "email")) | length(data) != 4 )
    stop("data must be a dataframe with the columns firstname, lastname, email and token", call. = F)

  data <- stats::setNames(split(data, seq(nrow(data))), rownames(data))
  data <- lapply(data, FUN = function(x) unlist(x) %>% as.list() )

    # for php memory problems split iLimit in chunks
  if (length(data) > chunksize) {
    n <- length(data)/chunksize
    limit <- 1
    for (i in 1:n) {

      params <- list("iSurveyID" = iSurveyID, "aParticipantData" = data[limit:(limit + chunksize)], "bCreateToken" = bCreateToken)
      resp <- call_limer(method = "add_participants", params = params)
      resp <- data.table::rbindlist(resp, fill = T) %>% suppressWarnings()
      if ("errors" %in% colnames(resp))
        warning("there were errors when adding participants among others. ", resp$errors[[1]], call. = F)
      # set count
      limit <- limit + chunksize + 1
      cat("\r",round((i/n)*100, digits = 2), "%")
      utils::flush.console()
    }

  }


  cat("\r")
  utils::flush.console()

}
