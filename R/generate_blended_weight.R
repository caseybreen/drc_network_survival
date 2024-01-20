#' Calculate Blended Weight for Kin Estimates
#'
#' This function calculates the blended weight to be applied to kin estimates in a mortality study.
#' The weight is derived based on the variance of estimates from kin and neighbor data, as well as their covariance.#'
#'#'
#' Example usage:
#' kin_blended_weight <- calculate_blended_weight_kin(mortality_estimates)
#'
#' @param main_estimate data.frame with mortality estimates, containing columns for 'type' (kin or neighbor), 'death_rate', and 'bootstrap_iter'.
#'
#' @return A single numeric value representing the blended weight for kin estimates.
#' @export

calculate_blended_weight_kin <- function(main_estimate) {
  ## estimate variance for kin
  var_kin <- main_estimate %>%
    filter(type == "kin") %>%
    group_by(type) %>%
    summarize(variance = var(death_rate)) %>%
    pull(variance)

  ## estimate variance for neighbor
  var_neighbor <- main_estimate %>%
    filter(type == "neighbor") %>%
    group_by(type) %>%
    summarize(variance = var(death_rate)) %>%
    pull(variance)

  ## estimate covariance between neighbor and kin estimates
  cov <- main_estimate %>%
    dplyr::select(type, death_rate, bootstrap_iter) %>%
    pivot_wider(values_from = death_rate, names_from = type) %>%
    summarize(covariance = cov(kin, neighbor)) %>%
    pull(covariance)

  ## calculate kin weight
  kin_weight <- (var_neighbor^2 - (cov / 2)) / (var_kin^2 + var_neighbor^2 - cov)
  return(kin_weight)
}
