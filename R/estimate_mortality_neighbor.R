#' Estimate Mortality Rate Among Neighbors
#'
#' Calculates mortality rates among neighbors based on death records and survey data, optionally within subpopulations.
#' The function allows for weighted and unweighted estimations.
#'
#' @param death_df data.frame with death records, including a 'death_relationship/neighbour' column.
#' @param survey_df data.frame with survey responses, including 'num_total_hh_neighbour' and a unique identifier to match with death_df.
#' @param weight_col string indicating the column name in survey_df used for weighting.
#' @param subpop optional vector of column names in survey_df representing subpopulations.
#'
#' @return A data frame with mortality rates among neighbors, both weighted and unweighted, for each subpopulation and overall.
#'
#' example
#' estimate_mortality_neighbor(death_df = death_data, survey_df = survey_data, weight_col = "weight_column", subpop = c("region", "age_group"))
#'
#' @export


estimate_mortality_neighbor <- function(death_df, survey_df, weight_col = "weights", subpop = NULL) {

  # Validate weight column
  if (!(weight_col %in% names(survey_df))) {
    stop("The specified weight column is not in the survey_df.")
  }

  # Handle subpopulations if provided
  if (!is.null(subpop)) {
    if (all(subpop %in% names(survey_df))) {
      survey_df <- survey_df %>% group_by(across(all_of(subpop)))
      death_df <- death_df %>% group_by(across(all_of(subpop)))
    } else {
      stop("One or more subpop names are not in the survey_df columns.")
    }
  }

  # Calculate exposure for neighbors
  exposure_hh_neighbors <- survey_df %>%
    mutate(
      exposure = lubridate::interval(as_date("2023-01-01"), as_date(start)) %/% days(1) * num_total_hh_neighbour * survey_df[[weight_col]],
      exposure_unweighted = lubridate::interval(as_date("2023-01-01"), as_date(start)) %/% days(1) * num_total_hh_neighbour
    ) %>%
    summarize(
      exposure = sum(exposure, na.rm = TRUE),
      exposure_unweighted = sum(exposure_unweighted, na.rm = TRUE)
    )

  # Calculate death counts
  death_count <- death_df %>%
    filter(`death_relationship/neighbour` == 1 | `death_relationship/household` == 1) %>%
    summarize(n = sum(.data[[weight_col]]), n_unweighted = n())


  # Combine results
  results_df <- if (!is.null(subpop)) {
    exposure_hh_neighbors %>%
      full_join(death_count, by = subpop) %>%
      mutate(
        death_rate = 10000 * n / exposure,
        death_rate_unweighted = 10000 * n_unweighted / exposure_unweighted
      )
  } else {
    exposure_hh_neighbors %>%
      bind_cols(death_count) %>%
      mutate(
        death_rate = 10000 * n / exposure,
        death_rate_unweighted = 10000 * n_unweighted / exposure_unweighted
      )
  }

  return(results_df)
}
