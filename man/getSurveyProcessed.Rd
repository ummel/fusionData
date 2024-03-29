% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSurveyProcessed.R
\name{getSurveyProcessed}
\alias{getSurveyProcessed}
\title{Download processed survey microdata}
\usage{
getSurveyProcessed(survey)
}
\arguments{
\item{survey}{Character. Survey identifier, possibly including vintage and respondent type. See Details.}
}
\value{
\code{\link[googledrive]{drive_download}} prints messages to console indicating which files were downloaded.
}
\description{
Since processed survey microdata files are generally too large to upload to Github or attach to the fusionData package, all of the processed microdata (.fst) files are stored on Google Drive. This function downloads requested survey microdata and places it in the appropriate local sub-directory of /fusionData/survey-processed. The full file path (e.g. intermediate directories) are created, if necessary, but no existing data on disk is altered or deleted.
}
\details{
\code{survey} must include a unique survey identifier. If just the identifier is provided (e.g. \code{survey = "RECS"}), then all available processed microdata across vintages and respondents is downloaded. Alternatively, can specify survey and vintage (e.g. \code{survey = "RECS_2015"}) or include additional respondent code (e.g. \code{survey = "RECS_2015_H"}) to limit download to more restrictive subsets.

The special argument \code{survey = "all"} will download ALL available processed microdata across surveys.
}
\examples{
getSurveyProcessed(survey = "RECS_2015_H")

}
