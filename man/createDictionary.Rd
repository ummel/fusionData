% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createDictionary.R
\name{createDictionary}
\alias{createDictionary}
\title{Generate data dictionary from survey microdata}
\usage{
createDictionary(data, survey, vintage, respondent)
}
\arguments{
\item{data}{Data frame. Survey microdata with variable descriptions stored in columns via \code{\link[labelled]{var_label}}.}

\item{survey}{Character. Unique survey identifier (e.g. "RECS").}

\item{vintage}{Character. Survey vintage (e.g. 2015).}

\item{respondent}{Character. Respondent type; either "Household" or "Person" or a string identifiable as such.}
}
\value{
Returns a tibble with standard "dictionary" information based on the provided microdata.
}
\description{
Produces a data dictionary with standard structure that can be saved alongside processed microdata. Resulting dictionary can be compiled with other survey dictionaries via \link{compileDictionary}. This function is typically called at the end of a .R script that generates processed survey microdata.
}
