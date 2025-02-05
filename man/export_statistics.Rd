% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_statistics.R
\name{export_statistics}
\alias{export_statistics}
\title{export_statistics}
\usage{
export_statistics(
  iSurveyID,
  sLanguage = "de",
  graph = FALSE,
  docType = "pdf",
  filename = NULL,
  verbose = TRUE,
  groupIDs = NULL
)
}
\arguments{
\item{iSurveyID}{numeric ID of the Survey}

\item{sLanguage}{string language of the survey to use (default from Survey),
default "de"}

\item{graph}{boolean, Create graph option. Default FALSE}

\item{docType}{string, Type of documents the exported statistics should be
(pdf|xls|html)}

\item{filename}{string, path/name of the generateted File}

\item{verbose}{boolean, Giving out logging info}

\item{groupIDs}{integer, Group ID of question}
}
\value{
file
}
\description{
Export survey statistics
}
\note{
the function application/helpers/common_helper.php must be modified to make the PDF export work.
https://bugs.limesurvey.org/view.php?id=18049
https://raw.githubusercontent.com/LimeSurvey/LimeSurvey/a34c39ecf400599f25806db2e239053bc29af4ac/application/helpers/common_helper.php
}
\references{
https://api.limesurvey.org/classes/remotecontrol_handle.html#method_export_statistics
}
