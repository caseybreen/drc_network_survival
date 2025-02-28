#' Estimate death rates
#'
#' This function computes exposure and death estimates for kin and neighbors, incorporating
#' options for bootstrapping, monthly calculations, and subpopulation analysis. It also calculates a
#' blended estimate combining kin and neighbor data, weighted by a given factor.
#' #'
#' @param death_df data.frame with death records.
#' @param survey_df data.frame with survey data.
#' @param bootstrap numeric, number of bootstrap samples to generate; NA for no bootstrapping.
#' @param monthly boolean, whether to calculate estimates on a monthly basis.
#' @param weight_col character, column name of statistical person-level weights
#' @param subpopulation optional, a vector indicating subpopulation groups for detailed analysis.
#' @param blended_weight_kin numeric, the weight to be applied to kin estimates to estimate the blended result.
#'
#' @return A data frame containing exposure and death estimates for kin, neighbor, and blended data types, including bootstrap iterations if applicable.
#'
#' Example usage:
#' results <- estimate_death_rate(death_df = death_records, survey_df = survey_data, bootstrap = 100, monthly = TRUE, subpopulation = c("urban", "rural"), blended_weight_kin = 0.5014031)
#'
#' @export


compute_cdr_comprehensive <- function(death_df, survey_df,
                                      weight_col,
                                      bootstrap = NA,
                                      monthly = FALSE,
                                      subpopulation = NULL,
                                      blended_weight_kin = 0.39375,
                                      weight_targets = weighting_targets,
                                      weight_type = "default",
                                      prob_survey_cutoff_flag = NA) {

  ## create list to store results
  results_list <- list()

  # Validate weight column
  if (!(weight_col %in% names(survey_df))) {
    stop("The specified weight column is not in the survey_df.")
  }

  if (is.na(bootstrap)) {
    # Determine the functions to use based on the 'monthly' parameter
    estimate_kin_func <- if (monthly) calculate_cdr_kin_monthly else calculate_cdr_kin
    estimate_neighbor_func <- if (monthly) calculate_cdr_neighbor_monthly else calculate_cdr_neighbor
    estimate_hh_func <- if (monthly) calculate_cdr_household else calculate_cdr_household


    # Kin result
    kin_result <- estimate_kin_func(death_df, survey_df, weight_col, subpop = subpopulation, prob_survey_cutoff = prob_survey_cutoff_flag) %>%
      mutate(type = "kin")

    # Neighbor result
    neighbor_result <- estimate_neighbor_func(death_df, survey_df, weight_col, subpop = subpopulation, prob_survey_cutoff = prob_survey_cutoff_flag) %>%
      mutate(type = "neighbor")

    # Blended result
    blended_result <- neighbor_result %>%
      mutate(
        death_rate = (kin_result$death_rate * blended_weight_kin + neighbor_result$death_rate * (1 - blended_weight_kin)),
        death_rate_unweighted = (kin_result$death_rate_unweighted * blended_weight_kin + neighbor_result$death_rate_unweighted * (1 - blended_weight_kin)),
        type = "blended"
      ) %>%
      dplyr::select(-n_deaths, -n_deaths_unweighted, -exposure, -exposure_unweighted)

    # HH estimate
    household_result <- estimate_hh_func(survey_df = survey_df, death_df = death_df, weight_col = weight_col, subpop = subpopulation, prob_survey_cutoff = prob_survey_cutoff_flag) %>%
      mutate(type = "household")

    # Store results
    results_list[["kin"]] <- kin_result
    results_list[["neighbor"]] <- neighbor_result
    results_list[["blended"]] <- blended_result
    results_list[["household"]] <- household_result
  } else {
    # Run the main code 'bootstrap' times for each bootstrap sample
    for (i in 1:bootstrap) {

      ## bootstrap survey
      boot_survey_df <- survey_df %>%
        dplyr::select(-!!weight_col) %>%
        group_by(health_zone, gender, start_month) %>%
        sample_n(size = n(), replace = TRUE) %>%
        ungroup()

      # Count and replicate data
      count_ids <- boot_survey_df %>%
        count(uuid_ki) %>%
        rename(count_column = n)

      boot_death_df_final <- death_df %>%
        dplyr::select(-!!weight_col) %>%
        inner_join(count_ids, by = "uuid_ki") %>%
        ungroup() %>%
        slice(unlist(mapply(rep, 1:n(), count_column)))

      # Check the weight type and apply the corresponding weight generation function
      if (weight_type == "poststrat") {
        boot_survey_df <- networksurvival::generate_poststrat_weights(weighting_targets = weight_targets, survey_df = boot_survey_df) %>%
          mutate(weight_col = weight_poststrat)
      } else if (weight_type == "ipw") {
        boot_survey_df <- networksurvival::generate_ipw_weights(weighting_targets = weight_targets, survey_df = boot_survey_df) %>%
          mutate(weight_col = weight_ipw)
      } else {  # This else branch can be for raking or a default case
        boot_survey_df <- boot_survey_df %>%
          mutate(weight_col = 1)  # Assuming 'weight_raking' is your column name for raking weights
      }

      ## weights
      weights <- boot_survey_df %>%
        dplyr::select(uuid_ki, weight_col) %>%
        distinct() %>%
        group_by(uuid_ki) %>%
        slice(1) %>%
        ungroup() # Ensure the grouping is removed after slicing

      ## death df
      boot_death_df_final <- boot_death_df_final %>%
        inner_join(weights, by = c("uuid_ki"))

      # Kin estimate
      estimate_kin_func <- if (monthly) calculate_cdr_kin_monthly else calculate_cdr_kin
      kin_result <- estimate_kin_func(survey_df = boot_survey_df, death_df = boot_death_df_final, weight_col = "weight_col", subpop = subpopulation, prob_survey_cutoff = prob_survey_cutoff_flag) %>%
        mutate(bootstrap_iter = i, type = "kin")

      # Neighbor estimate
      estimate_neighbor_func <- if (monthly) calculate_cdr_neighbor_monthly else calculate_cdr_neighbor
      neighbor_result <- estimate_neighbor_func(survey_df = boot_survey_df, death_df = boot_death_df_final, weight_col = "weight_col", subpop = subpopulation, prob_survey_cutoff = prob_survey_cutoff_flag) %>%
        mutate(bootstrap_iter = i, type = "neighbor")

      # HH estimate
      estimate_hh_func <- if (monthly) calculate_cdr_household_monthly else calculate_cdr_household
      household_result <- estimate_hh_func(survey_df = boot_survey_df, death_df = boot_death_df_final, weight_col = "weight_col", subpop = subpopulation,
                                           prob_survey_cutoff = prob_survey_cutoff_flag) %>%
        mutate(bootstrap_iter = i, type = "household")


      ## add blended estimate
      blended_result <- neighbor_result %>%
        mutate(
          death_rate = (kin_result$death_rate * blended_weight_kin + neighbor_result$death_rate * (1 - blended_weight_kin)),
          death_rate_unweighted = (kin_result$death_rate_unweighted * blended_weight_kin + neighbor_result$death_rate_unweighted * (1 - blended_weight_kin)),
          type = "blended"
        ) %>%
        dplyr::select(-n_deaths, -n_deaths_unweighted, -exposure, -exposure_unweighted)

      # Store results
      results_list[[paste("kin", i, sep = "_")]] <- kin_result
      results_list[[paste("neighbor", i, sep = "_")]] <- neighbor_result
      results_list[[paste("blended", i, sep = "_")]] <- blended_result
      results_list[[paste("household", i, sep = "_")]] <- household_result


      cat(i, " ")
    }
  }

  # Combine all results and return
  results_df <- bind_rows(results_list)

  # find only cols that exist
  existing_cols <- names(results_df)[names(results_df) %in% c("bootstrap_iter", "type", "month", subpopulation)]

  # Apply pivot_longer with the determined columns to exclude
  results_df <- results_df %>%
    pivot_longer(
      cols = -c(existing_cols), # This will ignore non-existent columns
      names_to = c(".value", "weights"),
      names_pattern = "(.*?)(_unweighted)?$"
    ) %>%
    mutate(weights = case_when(
      weights == "_unweighted" ~ "unweighted",
      TRUE ~ weight_col
    ))


}
