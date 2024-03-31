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


generate_ipw_weights <- function(weighting_targets_micro, survey_df) {


  # Bind rows with weighting targets, for all health zones
  weighting_df <- survey_df %>%
    select(health_zone, "hh_size_categorical", "hh_size_categorical_crude", "cooking_fuel",
           "material_house", "modern_fuel_type", "manufactured_material_house", "radio", "bed", "_id", "age_class", "gender", "age_u5_count", "age_5_18_count", "age_18plus_count") %>%
    mutate(inclusion = 1) %>%
    bind_rows(weighting_targets_micro %>%
                mutate(inclusion = 0))

  # Initialize an empty dataframe to store results
  all_zones_weighted <- data.frame()

  # Unique health zones
  health_zones <- unique(survey_df$health_zone)

  # Loop through each health zone
  for (zone in health_zones) {

    zone_df <- weighting_df %>% filter(health_zone == zone)

    # Step 1: Fit a logistic regression model for the current zone
    selection_model <- glm(inclusion ~ gender*age_class + hh_size_categorical + radio + bed + manufactured_material_house +
                             modern_fuel_type + age_u5_count + age_5_18_count + age_18plus_count, data = zone_df, family = "binomial")

    # Step 2: Predict the selection probabilities and calculate IPW for the current zone
    zone_df <- survey_df %>%
      filter(health_zone == zone) %>%
      mutate(selection_prob = predict(selection_model, newdata = ., type = "response"),
             ipw = 1 / selection_prob,
             weight_ipw = ipw / mean(ipw, na.rm = TRUE),
             weight_ipw = ifelse(weight_ipw > 10, 10, weight_ipw)) %>%
      select(health_zone, weight_ipw, uuid_ki)

    # Bind rows with the overall dataframe
    all_zones_weighted <- bind_rows(all_zones_weighted, zone_df)
  }

  ## all zones weighted - drop dupes
  all_zones_weighted <- all_zones_weighted %>%
    distinct(uuid_ki, weight_ipw)

  # Merge the original survey_df with the calculated weights
  survey_df <- survey_df %>%
    left_join(all_zones_weighted, by = "uuid_ki")

  return(survey_df)
}
