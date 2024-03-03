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

generate_raking_weights <- function(weighting_targets = weighting_targets, survey_df = survey_df) {

  # Kalemie
  margin_pop_kalemie <- weighting_targets %>%
    filter(health_zone == "Kalemie") %>%
    # mutate(proportion = round(proportion, 4)) %>%
    dplyr::select(-health_zone) %>%
    as.data.frame()

  survey_df_kalemie <- survey_df %>%
    filter(health_zone == "Kalemie")  %>%
    harvest(margin_pop_kalemie)

  # Nyunzu
  margin_pop_nyunzu <- weighting_targets %>%
    filter(health_zone == "Nyunzu") %>%
    dplyr::select(-health_zone)

  survey_df_nyunzu <- survey_df %>%
    filter(health_zone == "Nyunzu") %>%
    harvest(margin_pop_nyunzu)

  # Nyunzu
  margin_pop_nyemba <- weighting_targets %>%
    filter(health_zone == "Nyemba") %>%
    dplyr::select(-health_zone)

  survey_df_nyemba <- quota_sample_survey %>%
    filter(health_zone == "Nyemba") %>%
    harvest(margin_pop_nyunzu)


  # Combine all into one DataFrame
  survey_df_weighted <- bind_rows(survey_df_kalemie, survey_df_nyunzu, survey_df_nyemba)

  ## survey df weighted
  survey_df_weighted <- survey_df_weighted %>%
    mutate(weight_raking = weights)

  # return updated survey df
  return(survey_df_weighted)
}
