#' Estimate Monthly Mortality Rate for Kin
#'
#' Calculates monthly mortality rates for kin based on death records and survey data, optionally within subpopulations.
#' The function allows for weighted and unweighted estimations.
#'
#' @param death_df data.frame with death records, including a 'death_month' column.
#' @param survey_df data.frame with survey responses, including 'num_total_kin' and a unique identifier to match with death_df.
#' @param weight_col string indicating the column name in survey_df used for weighting (default is "weights").
#' @param subpop optional vector of column names in survey_df representing subpopulations (e.g., 'zone de sante').
#'
#' @return A data frame with monthly mortality rates, both weighted and unweighted, for each subpopulation and overall.
#'
#'
#' Example usage:
#' estimate_mortality_kin_monthly(death_df = death_data, survey_df = survey_data, weight_col = "weight_column", subpop = c("region", "age_group"))
#'
#' @export

estimate_mortality_kin_monthly <- function(death_df, survey_df, weight_col = "weights", subpop = NULL) {
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

  # Calculate exposure for kin
  calculate_exposure <- function(start_date, end_date) {
    sum(pmax(0, pmin(as_date(end_date), as_date(survey_df$start)) - as_date(start_date)) * survey_df$num_total_kin * survey_df[[weight_col]], na.rm = TRUE)
  }

  exposure_kin <- survey_df %>%
    summarize(
      `2023-01-01` = calculate_exposure("2023-01-01", "2023-02-01"),
      `2023-02-01` = calculate_exposure("2023-02-01", "2023-03-01"),
      `2023-03-01` = calculate_exposure("2023-03-01", "2023-04-01"),
      `2023-04-01` = calculate_exposure("2023-04-01", "2023-05-01"),
      `2023-05-01` = calculate_exposure("2023-05-01", "2023-06-01"),
      `2023-06-01` = calculate_exposure("2023-06-01", "2023-07-01")
    )

  # Convert to long format
  exposure_kin_long <- exposure_kin %>%
    pivot_longer(cols = -subpop, names_to = "month", values_to = "exposure") %>%
    mutate(month = as_date(month))

  # Calculate exposure for kin unweighted
  calculate_exposure_unweighted <- function(start_date, end_date) {
    sum(pmax(0, pmin(as_date(end_date), as_date(survey_df$start)) - as_date(start_date)) * survey_df$num_total_kin, na.rm = TRUE)
  }

  exposure_kin_unweighted <- survey_df %>%
    summarize(
      `2023-01-01` = calculate_exposure_unweighted("2023-01-01", "2023-02-01"),
      `2023-02-01` = calculate_exposure_unweighted("2023-02-01", "2023-03-01"),
      `2023-03-01` = calculate_exposure_unweighted("2023-03-01", "2023-04-01"),
      `2023-04-01` = calculate_exposure_unweighted("2023-04-01", "2023-05-01"),
      `2023-05-01` = calculate_exposure_unweighted("2023-05-01", "2023-06-01"),
      `2023-06-01` = calculate_exposure_unweighted("2023-06-01", "2023-07-01")
    )

  exposure_kin_long_unweighted <- exposure_kin_unweighted %>%
    pivot_longer(cols = -subpop, names_to = "month", values_to = "exposure_unweighted") %>%
    mutate(month = as_date(month))

  # Combine weighted and unweighted exposures
  exposure_kin_long <- exposure_kin_long %>%
    inner_join(exposure_kin_long_unweighted, by = c(subpop, "month"))

  # Calculate death counts
  death_count <- death_df %>%
    filter(!is.na(death_month)) %>%
    filter(`death_relationship/neighbour` == 0) %>%
    group_by(across(all_of(c(subpop, "death_month")))) %>%
    summarize(n = sum(.data[[weight_col]]), n_unweighted = n(), .groups = "drop") %>%
    mutate(death_month = as.Date(death_month)) %>%
    ungroup()

  # Join exposure and death counts
  results_df <- exposure_kin_long %>%
    left_join(death_count, by = c("month" = "death_month", subpop)) %>%
    mutate(
      death_rate = 10000 * n / exposure,
      death_rate_unweighted = 10000 * n_unweighted / exposure_unweighted
    )

  return(results_df)
}
