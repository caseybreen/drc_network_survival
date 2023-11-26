###########################
# Estimate mortality kin 
###########################
estimate_mortality_kin <- function(death_df, survey_df, subpop = NULL) {
  
  if (!is.null(subpop)) {
    if (all(subpop %in% names(survey_df))) {
      survey_df <- survey_df %>% group_by(across(all_of(subpop)))
      death_df <- death_df %>% group_by(across(all_of(subpop)))
    } else {
      stop("One or more subpop names are not in the survey_df columns.")
    }
  }
  
  exposure_kin <- survey_df %>% 
    mutate(exposure = lubridate::interval(as_date("2023-01-01"), as_date(start)) %/% days(1) * num_total_kin * weights) %>% 
    summarize(exposure_kin = sum(exposure, na.rm = TRUE))
  
  death_count <- death_df %>% 
    filter(`death_relationship/neighbour` == 0) %>% 
    summarize(n = sum(weights), n_unweighted = n())
  
  if (!is.null(subpop)) {
    results_df <- exposure_kin %>% 
      full_join(death_count, by = subpop) %>% 
      mutate(death_rate = 10000 * n / exposure_kin)
  } else {
    results_df <- exposure_kin %>% 
      bind_cols(death_count) %>% 
      mutate(death_rate = 10000 * n / exposure_kin)
  }
  
  return(results_df)
}

################################
# Estimate mortality kin monthly
################################
estimate_mortality_kin_monthly <- function(death_df, survey_df, subpop = NULL) {
  
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
  exposure_kin <- survey_df %>%
    summarize(
      `2023-01-01` = sum(pmax(0, pmin(as_date("2023-02-01"), as_date(start)) - as_date("2023-01-01")) * num_total_kin * weights, na.rm = TRUE),
      `2023-02-01` = sum(pmax(0, pmin(as_date("2023-03-01"), as_date(start)) - as_date("2023-02-01")) * num_total_kin * weights, na.rm = TRUE),
      `2023-03-01` = sum(pmax(0, pmin(as_date("2023-04-01"), as_date(start)) - as_date("2023-03-01")) * num_total_kin * weights, na.rm = TRUE),
      `2023-04-01` = sum(pmax(0, pmin(as_date("2023-05-01"), as_date(start)) - as_date("2023-04-01")) * num_total_kin * weights, na.rm = TRUE),
      `2023-05-01` = sum(pmax(0, pmin(as_date("2023-06-01"), as_date(start)) - as_date("2023-05-01")) * num_total_kin * weights, na.rm = TRUE),
      `2023-06-01` = sum(pmax(0, pmin(as_date("2023-07-01"), as_date(start)) - as_date("2023-06-01")) * num_total_kin * weights, na.rm = TRUE)
    )
  
  
  exposure_kin_long <- exposure_kin %>% 
    pivot_longer(-subpop) %>% 
    mutate(month = lubridate::as_date(name))
  
  # Calculate death counts
  death_count <- death_df %>% 
    filter(`death_relationship/neighbour` == 0) %>% 
    count(death_month, wt = weights, name = "n") %>%
    mutate(death_month = as_date(death_month)) %>%
    group_by(death_month, across(all_of(subpop))) %>%
    summarize(n = sum(n), .groups = 'keep')
  
  # Join exposure and death counts
  results_df <- exposure_kin_long %>% 
    left_join(death_count, by = c("month" = "death_month", subpop)) %>%
    mutate(death_rate = 10000 * n / value)
  
  # Ungroup for final output
  results_df <- results_df %>% 
    ungroup() 
  
  return(results_df)
}


##############################
# Estimate mortality neighbors  
##############################

## function death 
estimate_mortality_neighbor <- function(death_df, survey_df, subpop = subpopulation) {
  
  if (!is.null(subpop)) {
    if (all(subpop %in% names(survey_df))) {
      survey_df <- survey_df %>% group_by(across(all_of(subpop)))
      death_df <- death_df %>% group_by(across(all_of(subpop)))
    } else {
      stop("One or more subpop names are not in the survey_df columns.")
    }}

  # Exposure Kin
  exposure_hh_neighbors <- survey_df  %>% 
    mutate(exposure = lubridate::interval(as_date("2023-01-01"), as_date(start)) %/% days(1) * num_total_hh_neighbour * weights,
    ) %>% 
    summarize(exposure_hh_neighbors = sum(exposure, na.rm = T))
  
  death_count <- death_df %>% 
    filter(`death_relationship/neighbour` == 1 | `death_relationship/household` == 1) %>% 
    summarize(n = sum(weights), n_unweighted = n()) 
  
  if (!is.null(subpop)) {
    results_df <- exposure_hh_neighbors %>% 
      full_join(death_count, by = subpop) %>% 
      mutate(death_rate = 10000 * n / exposure_hh_neighbors)
  } else {
    results_df <- exposure_hh_neighbors %>% 
      bind_cols(death_count) %>% 
      mutate(death_rate = 10000 * n / exposure_hh_neighbors)
  }
  
  return(results_df)
}


estimate_mortality_neighbor_monthly <- function(death_df, survey_df, subpop = NULL) {
  
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
      `2023-01-01` = sum(pmax(0, pmin(as_date("2023-02-01"), as_date(start)) - as_date("2023-01-01")) * num_total_hh_neighbour * weights, na.rm = TRUE),
      `2023-02-01` = sum(pmax(0, pmin(as_date("2023-03-01"), as_date(start)) - as_date("2023-02-01")) * num_total_hh_neighbour * weights, na.rm = TRUE),
      `2023-03-01` = sum(pmax(0, pmin(as_date("2023-04-01"), as_date(start)) - as_date("2023-03-01")) * num_total_hh_neighbour * weights, na.rm = TRUE),
      `2023-04-01` = sum(pmax(0, pmin(as_date("2023-05-01"), as_date(start)) - as_date("2023-04-01")) * num_total_hh_neighbour * weights, na.rm = TRUE),
      `2023-05-01` = sum(pmax(0, pmin(as_date("2023-06-01"), as_date(start)) - as_date("2023-05-01")) * num_total_hh_neighbour * weights, na.rm = TRUE),
      `2023-06-01` = sum(pmax(0, pmin(as_date("2023-07-01"), as_date(start)) - as_date("2023-06-01")) * num_total_hh_neighbour * weights, na.rm = TRUE)
    )
  
  exposure_neighbor_long <- exposure_neighbor %>% 
    pivot_longer(-subpop) %>% 
    mutate(month = lubridate::as_date(name))
  
  # Death Count
  death_count <- death_df %>%
    filter(!is.na(death_month)) %>%
    filter(`death_relationship/neighbour` == 1 | `death_relationship/household` == 1) %>%
    group_by(across(all_of(c(subpop, "death_month")))) %>%
    summarize(n = sum(weights), n_unweighted = n(), .groups = 'keep') %>%
    mutate(death_month = as.Date(death_month)) %>%
    ungroup()
  
  # Join Exposure and Death Counts
  results_df <- exposure_neighbor_long %>%
    left_join(death_count, by = c("month" = "death_month", subpop)) %>%
    mutate(death_rate = n*10000 / value)
  
  return(results_df)
}

