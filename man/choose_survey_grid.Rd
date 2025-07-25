% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stitch-survey.R
\name{choose_survey_grid}
\alias{choose_survey_grid}
\title{Choose the survey grid for matching survey abbreviations. Data for all but the
IPHC FISS come from \code{gfdata}.}
\usage{
choose_survey_grid(.survey_abbrev)
}
\arguments{
\item{.survey_abbrev}{A vector containing at least one of the following surveys:
"SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI", "HBLL INS N" "HBLL INS S",
"HBLL OUT N", "HBLL OUT S", "MSSM WCVI", and "IPHC FISS".}
}
\value{
A survey grid of active blocks with associated overwater area
Synoptic surveys: returns grid fr
MSSM WCVI: returns grid covering locations sampled between 2009 to 2022
and has a cell area of 9 km2. It also includes only management areas 124 and 125
IPHC FISS: returns grid of locations covering stations sampled in 2017
and has a cell area of 1 km2
}
\description{
Choose the survey grid for matching survey abbreviations. Data for all but the
IPHC FISS come from \code{gfdata}.
}
