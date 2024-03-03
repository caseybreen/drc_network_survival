#' Calculate Poststratification Weights for Survey Data
#'
#' This function calculates poststratification weights for survey data based on demographic targets. It adjusts the survey sample to be more representative of the target population by considering key demographic variables such as age, gender, and geographic zones.
#'
#' Poststratification is a common technique in survey analysis to correct for sampling biases and make the sample more representative of the target population. This function aligns the survey sample distribution with known population distributions in different demographic categories.
#'
#' @param weighting_targets data.frame representing the target population distribution across demographic categories (like age, gender, and geographic zone).
#' @param survey_df data.frame with survey data, including demographic variables matching those in weighting_targets.
#'
#' @return A data frame identical to survey_df but with an additional column of poststratification weights, indicating how much each respondent's answers should count towards the overall survey results.
#'
#' example
#' poststratified_survey <- calculate_poststrat_weights(weighting_targets = population_data, survey_df = survey_data)
#'
#' @export

generate_poststrat_weights <- function(weighting_targets = weighting_targets, survey_df = survey_df) {

  # Poststratification sample
  poststrat_worldpop <- weighting_targets %>%
    group_by(health_zone, age_class, gender) %>%
    summarize(n = sum(sum_value), .groups = "drop") %>%
    group_by(health_zone) %>%
    mutate(prop_pop = n / sum(n)) %>%
    ungroup()

  # Poststratification population
  poststrat_sample <- survey_df %>%
    group_by(health_zone, age_class, gender) %>%
    summarize(n = n(), .groups = "drop") %>%
    group_by(health_zone) %>%
    mutate(prop_sample = n / sum(n)) %>%
    ungroup()

  # Poststratification weights
  poststrat_weights <- poststrat_sample %>%
    inner_join(poststrat_worldpop, by = c("age_class", "gender", "health_zone")) %>%
    mutate(weight = prop_pop / prop_sample) %>%
    dplyr::select(gender, age_class, health_zone, weight_poststrat = weight)

  # Joining the poststratification weights back into survey_df
  updated_survey_df <- survey_df %>%
    left_join(poststrat_weights, by = c("gender", "age_class", "health_zone"))

  # return updated survey df
  return(updated_survey_df)
}
