#' Estimate death rate using household data
#'
#' Calculates death rates, optionally within subpopulations.
#' The function calculates both weighted and unweighted crude death rates using household reports
#'
#' @param death_df data.frame with death records, including a 'death_relationship/household' column.
#' @param survey_df data.frame with survey responses, including 'num_total_hh' and a unique identifier to match with death_df.
#' @param weight_col string indicating the column name in survey_df used for weighting.
#' @param subpop optional vector of column names in survey_df representing subpopulations.
#'
#' @return A data frame with mortality rates among neighbors, both weighted and unweighted, for each subpopulation and overall.
#'
#' example
#' estimate_mortality_neighbor(death_df = death_data, survey_df = survey_data, weight_col = "weight_column", subpop = c("region", "age_group"))
#'
#' @export

calculate_cdr_household_monthly <- function(death_df, survey_df, weight_col = "weights", subpop = NULL) {

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

  # Calculate exposure for weighted hh
  exposure_hh <- survey_df %>%
    summarize(
      `2023-01-01` = sum(pmax(0, pmin(as_date("2023-02-01"), as_date(start)) - as_date("2023-01-01")) * num_total_hh * .data[[weight_col]], na.rm = TRUE),
      `2023-02-01` = sum(pmax(0, pmin(as_date("2023-03-01"), as_date(start)) - as_date("2023-02-01")) * num_total_hh * .data[[weight_col]], na.rm = TRUE),
      `2023-03-01` = sum(pmax(0, pmin(as_date("2023-04-01"), as_date(start)) - as_date("2023-03-01")) * num_total_hh * .data[[weight_col]], na.rm = TRUE),
      `2023-04-01` = sum(pmax(0, pmin(as_date("2023-05-01"), as_date(start)) - as_date("2023-04-01")) * num_total_hh * .data[[weight_col]], na.rm = TRUE),
      `2023-05-01` = sum(pmax(0, pmin(as_date("2023-06-01"), as_date(start)) - as_date("2023-05-01")) * num_total_hh * .data[[weight_col]], na.rm = TRUE),
      `2023-06-01` = sum(pmax(0, pmin(as_date("2023-07-01"), as_date(start)) - as_date("2023-06-01")) * num_total_hh * .data[[weight_col]], na.rm = TRUE)
    )

  # Convert to long format
  exposure_hh_long <- exposure_hh %>%
    pivot_longer(cols = -subpop, names_to = "month", values_to = "exposure") %>%
    mutate(month = as_date(month))

  # Calculate exposure for hh unweighted
  exposure_hh_unweighted <- survey_df %>%
    summarize(
      `2023-01-01` = sum(pmax(0, pmin(as_date("2023-02-01"), as_date(start)) - as_date("2023-01-01")) * num_total_hh, na.rm = TRUE),
      `2023-02-01` = sum(pmax(0, pmin(as_date("2023-03-01"), as_date(start)) - as_date("2023-02-01")) * num_total_hh, na.rm = TRUE),
      `2023-03-01` = sum(pmax(0, pmin(as_date("2023-04-01"), as_date(start)) - as_date("2023-03-01")) * num_total_hh, na.rm = TRUE),
      `2023-04-01` = sum(pmax(0, pmin(as_date("2023-05-01"), as_date(start)) - as_date("2023-04-01")) * num_total_hh, na.rm = TRUE),
      `2023-05-01` = sum(pmax(0, pmin(as_date("2023-06-01"), as_date(start)) - as_date("2023-05-01")) * num_total_hh, na.rm = TRUE),
      `2023-06-01` = sum(pmax(0, pmin(as_date("2023-07-01"), as_date(start)) - as_date("2023-06-01")) * num_total_hh, na.rm = TRUE)
    )

  exposure_hh_long_unweighted <- exposure_hh_unweighted %>%
    pivot_longer(cols = -subpop, names_to = "month", values_to = "exposure_unweighted") %>%
    mutate(month = as_date(month))

  # Combine weighted and unweighted exposures
  exposure_hh_long <- exposure_hh_long %>%
    inner_join(exposure_hh_long_unweighted, by = c(subpop, "month"))

  # Calculate death counts
  death_count <- death_df %>%
    filter(!is.na(death_month)) %>%
    filter(`death_relationship/household` == 1) %>%
    group_by(across(all_of(c(subpop, "death_month")))) %>%
    summarize(n = sum(.data[[weight_col]]), n_unweighted = n(), .groups = "drop") %>%
    mutate(death_month = as.Date(death_month)) %>%
    ungroup()

  # Check if 'subpop' is present in the data frame and update accordingly
  if (!is.null(subpop)) {
    # If 'subpop' exists and is a column in 'death_count'
    death_count <- death_count %>%
      complete(!!sym(subpop), death_month, fill = list(n = 0, n_unweighted = 0))
  } else {
    # If 'subpop' does not exist or is not a column in 'death_count'
    death_count <- death_count %>%
      complete(death_month, fill = list(n = 0, n_unweighted = 0))
  }

  # Join exposure and death counts
  results_df <- exposure_hh_long %>%
    left_join(death_count, by = c("month" = "death_month", subpop)) %>%
    mutate(
      death_rate = 10000 * n / exposure,
      death_rate_unweighted = 10000 * n_unweighted / exposure_unweighted
    )

  return(results_df)
}
