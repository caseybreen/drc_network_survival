#' Estimate death rates
#'
#' This function computes exposure and death estimates for kin and neighbors, incorporating
#' options for bootstrapping, monthly calculations, and subpopulation analysis. It also calculates a
#' blended estimate combining kin and neighbor data, weighted by a given factor.
#'
#' The function allows for a comprehensive analysis of mortality data by combining
#' different estimation methods (kin and neighbor) and enabling advanced techniques
#' like bootstrapping for robustness. The blended result, weighted by the specified
#' kin weight, offers a more nuanced understanding of mortality rates.
#'
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


estimate_death_rate <- function(death_df, survey_df, weight_col = weights, bootstrap = NA, monthly = FALSE, subpopulation = NULL, blended_weight_kin = 0.5014031) {
  results_list <- list()

  # Validate weight column
  if (!(weight_col %in% names(survey_df))) {
    stop("The specified weight column is not in the survey_df.")
  }

  if (is.na(bootstrap)) {
    # No bootstrapping, just run the main code once for each type
    kin_result <- estimate_mortality_kin(death_df, survey_df, weight_col, subpop = subpopulation) %>%
      mutate(type = "kin")

    neighbor_result <- estimate_mortality_neighbor(death_df, survey_df, weight_col, subpop = subpopulation) %>%
      mutate(type = "neighbor")

    blended_result <- neighbor_result %>%
      mutate(
        death_rate = (kin_result$death_rate * blended_weight_kin + neighbor_result$death_rate * (1 - blended_weight_kin)),
        death_rate_unweighted = (kin_result$death_rate_unweighted * blended_weight_kin + neighbor_result$death_rate_unweighted * (1 - blended_weight_kin)),
        type = "blended"
      ) %>%
      select(-n, -n_unweighted, -exposure)

    # Calculate blended estimate
    results_list[["kin"]] <- kin_result
    results_list[["neighbor"]] <- neighbor_result
    results_list[["blended"]] <- blended_result
  } else {
    # Run the main code 'bootstrap' times for each bootstrap sample
    for (i in 1:bootstrap) {
      ## bootstrap survey
      boot_survey_df <- survey_df %>%
        group_by(zone_de_sante_name, gender, start_month) %>%
        sample_n(size = n(), replace = TRUE) %>%
        ungroup()

      # Count and replicate data
      count_ids <- boot_survey_df %>%
        count(uuid_ki) %>%
        rename(count_column = n)

      boot_death_df_final <- death_df %>%
        inner_join(count_ids, by = "uuid_ki") %>%
        ungroup() %>%
        slice(unlist(mapply(rep, 1:n(), count_column)))

      # Kin estimate
      estimate_kin_func <- if (monthly) estimate_mortality_kin_monthly else estimate_mortality_kin
      kin_result <- estimate_kin_func(survey_df = boot_survey_df, death_df = boot_death_df_final, weight_col = weight_col, subpop = subpopulation) %>%
        mutate(bootstrap_iter = i, type = "kin")

      # Neighbor estimate
      estimate_neighbor_func <- if (monthly) estimate_mortality_neighbor_monthly else estimate_mortality_neighbor
      neighbor_result <- estimate_neighbor_func(survey_df = boot_survey_df, death_df = boot_death_df_final, weight_col = weight_col, subpop = subpopulation) %>%
        mutate(bootstrap_iter = i, type = "neighbor")

      ## add blended estimate
      blended_result <- neighbor_result %>%
        mutate(
          death_rate = (kin_result$death_rate * blended_weight_kin + neighbor_result$death_rate * (1 - blended_weight_kin)),
          death_rate_unweighted = (kin_result$death_rate_unweighted * blended_weight_kin + neighbor_result$death_rate_unweighted * (1 - blended_weight_kin)),
          type = "blended"
        ) %>%
        select(-n, -n_unweighted, -exposure)

      # Store results
      results_list[[paste("kin", i, sep = "_")]] <- kin_result
      results_list[[paste("neighbor", i, sep = "_")]] <- neighbor_result
      results_list[[paste("blended", i, sep = "_")]] <- blended_result

      cat(i)
    }
  }

  # Combine all results and return
  results_df <- bind_rows(results_list)

  return(results_df)
}
