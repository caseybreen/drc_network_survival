#' Estimate Monthly Mortality Rate Among Neighbors
#'
#' This function calculates monthly mortality rates among neighbors based on death records and survey data. It can be tailored to specific subpopulations. The function supports both weighted and unweighted calculations.
#'
#' @param death_df data.frame with death records, including a 'death_month' column and a 'death_relationship/neighbour' indicator.
#' @param survey_df data.frame with survey responses, including 'num_total_hh_neighbour' and a unique identifier to match with death_df.
#' @param weight_col string indicating the column name in survey_df used for weighting.
#' @param subpop optional vector of column names in survey_df representing subpopulations.
#'
#' @return A data frame with monthly mortality rates among neighbors, both weighted and unweighted, for each subpopulation and overall.
#'
#' examples
#' estimate_mortality_neighbor_monthly(death_df = death_data, survey_df = survey_data, weight_col = "weight_column", subpop = c("region", "age_group"))
#'
#' @export


estimate_mortality_neighbor_monthly <- function(death_df, survey_df, weight_col = "weights", subpop = NULL) {
  # Validate weight column
  if (!(weight_col %in% names(survey_df))) {
    stop("The specified weight column is not in the survey_df.")
  }

  # Handle subpopulations if provided
  if (!is.null(subpop) && length(subpop) > 0) {
    if (all(subpop %in% names(survey_df))) {
      survey_df <- survey_df %>% group_by(across(all_of(subpop)))
      death_df <- death_df %>% group_by(across(all_of(subpop)))
    } else {
      stop("One or more subpop names are not in the survey_df columns.")
    }
  }

  # Exposure Neighbor
  exposure_neighbor <- survey_df %>%
    summarize(
      across(`2023-01-01`:`2023-06-01`, ~ sum(pmax(0, pmin(as_date(ymd(.x) %m+% months(1)), as_date(start)) - as_date(ymd(.x))) * num_total_hh_neighbour * survey_df[[weight_col]], na.rm = TRUE), .names = "{.col}")
    )

  # Convert to long format
  exposure_neighbor_long <- exposure_neighbor %>%
    pivot_longer(cols = everything(), names_to = "month", values_to = "exposure") %>%
    mutate(month = as_date(month))

  # Calculate exposure for neighbors unweighted
  exposure_neighbor_unweighted <- survey_df %>%
    summarize(
      across(`2023-01-01`:`2023-06-01`, ~ sum(pmax(0, pmin(as_date(ymd(.x) %m+% months(1)), as_date(start)) - as_date(ymd(.x))) * num_total_hh_neighbour, na.rm = TRUE), .names = "{.col}")
    )

  # Convert to long format for unweighted
  exposure_neighbor_unweighted_long <- exposure_neighbor_unweighted %>%
    pivot_longer(cols = everything(), names_to = "month", values_to = "exposure_unweighted") %>%
    mutate(month = as_date(month))

  # Combine weighted and unweighted exposures
  exposure_neighbor_long <- exposure_neighbor_long %>%
    inner_join(exposure_neighbor_unweighted_long, by = "month")

  # Death Count
  death_count <- death_df %>%
    filter(!is.na(death_month)) %>%
    filter(`death_relationship/neighbour` == 1 | `death_relationship/household` == 1) %>%
    group_by(across(all_of(c(subpop, "death_month")))) %>%
    summarize(n = sum(survey_df[[weight_col]]), n_unweighted = n(), .groups = "keep") %>%
    mutate(death_month = as.Date(death_month)) %>%
    ungroup()

  # Join Exposure and Death Counts
  results_df <- exposure_neighbor_long %>%
    left_join(death_count, by = c("month" = "death_month", subpop)) %>%
    mutate(
      death_rate = n * 10000 / exposure,
      death_rate_unweighted = n_unweighted * 10000 / exposure_unweighted
    )

  return(results_df)
}
