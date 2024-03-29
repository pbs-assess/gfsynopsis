% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cpue.R
\name{fit_cpue_indices}
\alias{fit_cpue_indices}
\title{Fit commercial catch per unit effort standardization models}
\usage{
fit_cpue_indices(
  dat,
  species = "pacific cod",
  areas = c("3[CD]+|5[ABCDE]+", "5[CDE]+", "5[AB]+", "3[CD]+"),
  center = TRUE,
  cache = here::here("report", "cpue-cache"),
  save_model = FALSE,
  arith_cpue_comparison = TRUE,
  parallel = FALSE,
  year_range = c(1996, lubridate::year(Sys.Date()) - 1)
)
}
\arguments{
\item{dat}{A data frame from \code{\link[gfdata:get_data]{gfdata::get_cpue_index()}}.}

\item{species}{The species common name to fit.}

\item{areas}{A vector of regular expressions representing the statistical
areas to fit.}

\item{center}{Logical for whether or not the index should be centered on its
geometric mean.}

\item{cache}{A folder in which to cache the model output if desired.}

\item{save_model}{Logical for whether the model should be cached. Defaults to
\code{FALSE} to save space.}

\item{arith_cpue_comparison}{Logical: should the unstandardized comparison be
an arithmetic 'ratio estimator' CPUE (summed catch for this species divided
by summed effort for the entire fleet) (if \code{TRUE}) or a GLM / GLMM with
only a year predictor.}

\item{parallel}{Should the various areas be fit in parallel? Make sure you
have enough memory. (Disabled for now.)}

\item{year_range}{Year range.}
}
\description{
This function helps it fit the CPUE standardization models across multiple
areas for a single species.
}
