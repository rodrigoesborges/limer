% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reset_survey.R
\name{reset_survey}
\alias{reset_survey}
\title{reset_survey}
\usage{
reset_survey(
  iSurveyID,
  sImportData = NULL,
  sNewSurveyName = NULL,
  DestSurveyID = NULL,
  verbose = FALSE
)
}
\arguments{
\item{iSurveyID}{integer, Id of the survey that will be reset}

\item{sImportData}{string, Path to the file. If not set, the previous survey
is re-imported in its previous structure}

\item{sNewSurveyName}{String, if not set the previous name is used (optional)}

\item{DestSurveyID}{string, Id of the copied survey (optional)}

\item{verbose}{boolean, Giving out logging info}
}
\description{
Resets or copy a survey to its original state by deleting the existing
survey and re-importing it.
}
