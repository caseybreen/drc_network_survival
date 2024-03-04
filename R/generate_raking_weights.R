#' Calculate Poststratification Weights for Survey Data
#'
#' This function calculates  weights for survey data based on demographic targets.
#'
#'
#' @param weighting_targets data.frame representing the target population distribution across demographic categories (like age, gender, and geographic zone).
#' @param survey_df data.frame with survey data, including demographic variables matching those in weighting_targets.
#'
#' @return A data frame identical to survey_df but with an additional column of poststratification weights, indicating how much each respondent's answers should count towards the overall survey results.
#'
#' example
#'  generate_raking_weights(weighting_targets = population_data, survey_df = survey_data)
#'
#' @export
# Assuming `harvest` and necessary libraries are loaded outside this function

generate_raking_weights <- function(weighting_targets, survey_df) {

  # Inner function to apply weighting
  apply_weighting <- function(zone, df, target_df) {
    margin_pop <- target_df %>%
      filter(health_zone == zone) %>%
      select(-health_zone)

    df_filtered <- df %>%
      filter(health_zone == zone) %>%
      harvest(margin_pop, convergence = c(pct = 0.001, absolute = 1e-06),max_iterations = 1000)

    return(df_filtered)
  }

  # Extract unique health zones from survey data
  health_zones <- distinct(survey_df, health_zone) %>% pull(health_zone)

  # Apply weighting and combine results
  survey_df_weighted <- map_dfr(health_zones, apply_weighting, df = survey_df, target_df = weighting_targets)

  return(survey_df_weighted)
}

