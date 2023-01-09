

copy_survey_to <- function(iSurveyID, DestSurveyID = NULL){

  if (iSurveyID == DestSurveyID)
    stop("to replace existing survey please use reset_survey()", call. = F)


  iSurveyID <- as.numeric(iSurveyID) %>% suppressWarnings()

  if (is.na(iSurveyID))
    stop("No valid iSurveyID passed. iSurveyID must be a six-digit number!",
         call. = F)

  res <- call_limer("copy_survey_to",
                    params = list("iSurveyID" = iSurveyID,
                                  "iDesiredSurveyId" = DestSurveyID))


}

# options(lime_username = "SWRData")
# options(lime_password = "R3p0rt3r2017")
# options(lime_api = 'https://217.66.32.71/medienanfrage/index.php/admin/remotecontrol')
# get_session_key()
# # debugonce(call_limer)
#
# copy_survey_to("419449", "123456")

