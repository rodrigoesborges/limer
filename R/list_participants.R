#' list_participants
#'
#' Retrieves the list of participants of a survey
#'
#' @param iSurveyID integer, ID of the Survey to insert responses
#' @param bUnused boolean If you want unused tokens, set true
#' @param iLimit integer, Number of participants to return
#' @param iStart integer, Start id of the token list
#' @param tid boolean, return data with tid
#' @param chunksize integer, size of chunks for handling php memory problems
#' @param ... ellipsis parameters for call_limer
#' @return dataframe
#' @export
#'

list_participants <- function(iSurveyID, bUnused = TRUE, iStart = 1, iLimit = 100, tid = FALSE , chunksize = 5000, ...){

  # for php memory problems split iLimit in chunks
  if (iLimit > chunksize) {
    n <- iLimit/chunksize
    iLimit <- chunksize
      for (i in 1:n) {


        params <- list("iSurveyID" = iSurveyID,"bUnused" = bUnused, "iLimit" = iLimit, "iStart" = iStart )
        df <- call_limer(method = "list_participants", params = params, ...)
        dfs <- lapply(df, data.frame, stringsAsFactors = FALSE)
        data <- dplyr::bind_rows(dfs)
        if (data[1,1] == "No survey participants found.")
          stop("No survey participants found.", call. = F)

        # set count
        iStart <- iStart + chunksize + 1
        cat("\r",round((i/n)*100, digits = 2), "%")
        utils::flush.console()
      }

  } else{

    params <- list("iSurveyID" = iSurveyID,"bUnused" = bUnused, "iLimit" = iLimit, "iStart" = iStart )
    data <- call_limer(method = "list_participants", params = params, ...)

    if (length(data) == 1)
      stop(data[1,1], call. = F)

    }

  if (!tid)
    data <- data %>% dplyr::select(-tid)

  colnames(data) <- gsub("participant_info.","",colnames(data))

  cat("\r")
  utils::flush.console()
  return(data)
}

