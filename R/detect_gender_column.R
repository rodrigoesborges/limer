
#' detect_gender_column
#'
#' This helper function corrects an incorrectly to boolean converted column
#' "gender" back to character.
#'
#' @param data dataframe
#'
#' @return dataframe
#'

detect_gender_column <- function(data) {

  for (i in 1:ncol(data)) {
    if (length(setdiff(data[ , i] %>% unique(), c("F","M",NA,""))) == 0) {
      data[ , i] <- as.character(data[ , i])
    }
  }
  return(data)
}