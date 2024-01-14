#' Estimate death rate using kin network
#'
#' @param death_df data.frame with deaths
#' @param survey_df data.frame with survey responses (and unique id to match to death_df)
#' @param subpop subpopulation (e.g., zone de sante)
#'
#'
#' @export

estimate_mortality_kin <- function(death_df, survey_df, weight_col = "weights", subpop = NULL) {

  ## Validate weight column
  if (!(weight_col %in% names(survey_df))) {
    stop("The specified weight column is not in the survey_df.")
  }

  if (!is.null(subpop)) {
    if (all(subpop %in% names(survey_df))) {
      survey_df <- survey_df %>% group_by(across(all_of(subpop)))
      death_df <- death_df %>% group_by(across(all_of(subpop)))
    } else {
      stop("One or more subpop names are not in the survey_df columns.")
    }
  }

  ## Calculate exposure
  exposure_kin <- survey_df %>%
    mutate(exposure = lubridate::interval(as_date("2023-01-01"), as_date(start)) %/% days(1) * num_total_kin * .data[[weight_col]]) %>%
    mutate(exposure_unweighted = lubridate::interval(as_date("2023-01-01"), as_date(start)) %/% days(1) * num_total_kin) %>%
    summarize(
      exposure = sum(exposure, na.rm = TRUE),
      exposure_unweighted = sum(exposure_unweighted, na.rm = TRUE)
    )

  ## Calculate deaths
  death_count <- death_df %>%
    filter(`death_relationship/family` == 1) %>%
    summarize(n = sum(.data[[weight_col]]), n_unweighted = n())

  # Calculate death rates
  if (!is.null(subpop)) {
    results_df <- exposure_kin %>%
      full_join(death_count, by = subpop) %>%
      mutate(
        death_rate = 10000 * n / exposure,
        death_rate_unweighted = 10000 * n_unweighted / exposure_unweighted
      )
  } else {
    results_df <- exposure_kin %>%
      bind_cols(death_count) %>%
      mutate(
        death_rate = 10000 * n / exposure,
        death_rate_unweighted = 10000 * n_unweighted / exposure_unweighted
      )
  }

  return(results_df)
}
