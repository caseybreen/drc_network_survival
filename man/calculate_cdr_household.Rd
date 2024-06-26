% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_cdr_household.R
\name{calculate_cdr_household}
\alias{calculate_cdr_household}
\title{Estimate death rate using household}
\usage{
calculate_cdr_household(
  death_df,
  survey_df,
  weight_col = "weights",
  subpop = NULL
)
}
\arguments{
\item{death_df}{data.frame with death records, including a 'death_relationship/neighbour' column.}

\item{survey_df}{data.frame with survey responses, including 'num_total_hh_neighbour' and a unique identifier to match with death_df.}

\item{weight_col}{string indicating the column name in survey_df used for weighting.}

\item{subpop}{optional vector of column names in survey_df representing subpopulations.}
}
\value{
A data frame with mortality rates among neighbors, both weighted and unweighted, for each subpopulation and overall.

example
estimate_mortality_neighbor(death_df = death_data, survey_df = survey_data, weight_col = "weight_column", subpop = c("region", "age_group"))
}
\description{
Calculates death rates, optionally within subpopulations.
The function calculates both weighted and unweighted death_rate estimates
}
