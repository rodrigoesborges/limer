#' Make a call to the LimeSurvey API
#'
#' This function makes a generic call to the LimeSurvey API. See \url{https://manual.limesurvey.org/RemoteControl_2_API} for API documentation.
#' @param method API function to call. Full lis Defaults to value set in \code{options()}.
#' @param params Optional named list of parameters to pass to the function.
#' @param ssl_verifypeer boolean \code{httr::config()} parameter. Default is
#' FALSE.
#' @param \dots Other arguments passed to \code{\link[httr]{POST}}.
#' @return Results from the API (sometimes plain text, sometimes base64-encoded text).
#' @import httr
#' @export
#' @examples \dontrun{
#' call_limer(method = "list_surveys")
#' call_limer(method = "get_summary",
#'            params = list(iSurveyID = 238481,
#'                          sStatname = "completed_responses"))
#' }
#' @export

call_limer <-
  function(method,
           params = list(),
           ssl_verifypeer = FALSE,
           ...) {
    if (!is.list(params)) {
      stop("params must be a list.")
    }

    if (!exists("session_key", envir = session_cache)) {
      stop("You need to get a session key first. Run get_session_key().")
    }

    key.list <- list(sSessionKey = session_cache$session_key)
    params.full <- c(key.list, params)

    body.json <- list(method = method,
                      # This seems to not matter, but the API call breaks without it,
                      # so just pass nothing. ¯\_(ツ)_/¯
                      id = " ",
                      params = params.full)

    r <- httr::POST(
      getOption('lime_api'),
      httr::content_type_json(),
      body = jsonlite::toJSON(body.json, auto_unbox = TRUE, force = T),
      httr::config(ssl_verifypeer = ssl_verifypeer),
      ...
    )

    response <- jsonlite::parse_json(httr::content(r, as = 'text', encoding = "utf-8"), simplifyVector = T)$result

    if (is.null(response)) {
      err_msg <- jsonlite::parse_json(httr::content(r, as = 'text', encoding = 'utf-8'))$error
      err_msg <- ifelse(is.null(err_msg), glue::glue("{method} is a unknown function to remotecontrol"), err_msg)
      stop(err_msg, call. = F)

    } else {
      if (any(sapply(response, function(x) class(x)) == "data.frame")) {
        # detect all dataframe columns
        dataframe_columns <- sapply(response, function(x) is.data.frame(x))

        # get names of dataframe columns
        dataframe_column_names <- names(response)[dataframe_columns]

        response <- tidyr::unnest(response, cols = c(dataframe_column_names))

      }

      return(response)
    }



    # return(jsonlite::parse_json(httr::content(r, as = 'text', encoding = "utf-8"))$result)   # incorporated fix by petrbouchal

  }
