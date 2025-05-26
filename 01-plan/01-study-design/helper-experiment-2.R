
#' Experiment 2: Simulate Plant Biomass Under Experimental Treatment Combinations
#'
#' Generates a simulated dataset of native plant biomass under factorial combinations of
#' nitrogen levels, soil microbes presence/absence, and the presence/absence of invasives
#' with customizable effect sizes and residual variance.
#'
#' @param n_rep Integer. Number of replicate observations per treatment combination. Default is 100.
#' @param N_vals Numeric vector. Levels of nitrogen (on log scale). Default is `log(c(4, 8, 16, 32, 64))`.
#' @param M_vals Numeric vector. Microbe treatment levels (0 = absent, 1 = present). Default is `c(0, 1)`.
#' @param I_vals Numeric vector. Invasive treatment levels (0 = native alone, 1 = invasive present). Default is `c(0, 1)`.
#' @param alpha Numeric. Intercept: log biomass when N, M, and I are all 0. Default is 1.5.
#' @param gamma1 Numeric. Effect of nitrogen (N) when M = 0 and I = 0. Default is 0.2.
#' @param gamma2 Numeric. Effect of microbes (M) when N = 0 and I = 0. Default is -0.75.
#' @param gamma3 Numeric. Effect of invasive presence (I) when N = 0 and M = 0. Default is 0.
#' @param gamma4 Numeric. Interaction effect of N × M. Default is 0.05.
#' @param gamma5 Numeric. Interaction effect of N × I. Default is 0.
#' @param gamma6 Numeric. Interaction effect of M × I. Default is 0.
#' @param gamma7 Numeric. Three-way interaction effect of N × M × I. Default is 0.4.
#' @param sigma_residual Numeric. Standard deviation of the residual error. Default is 0.10.
#'
#' @return A tibble with columns for each treatment factor (`M`, `I`, `N`), replicate ID (`rep`),
#'         and simulated biomass (`biomass`) on the natural scale.
#'
#' @examples
#' sim_data <- simulate_exp2()
#' head(sim_data)
#'
#' # Custom effect sizes and nitrogen levels
#' simulate_exp1(n_rep = 50, gamma1 = 0.1, N_vals = log(c(5, 10, 20)))
#'
#' @export
sim_exp2 <- function(n_rep = 100,
                     N_vals = log(c(4, 8, 16, 32, 64)),
                     M_vals = c(0, 1),
                     I_vals = c(0, 1),
                     alpha = 1.5,
                     gamma1 = 0.2,
                     gamma2 = -0.75,
                     gamma3 = 0,
                     gamma4 = 0.05,
                     gamma5 = 0,
                     gamma6 = 0,
                     gamma7 = 0.4,
                     sigma_residual = 0.10) {
  
  # Load necessary packages
  requireNamespace("dplyr")
  requireNamespace("tidyr")
  
  # Create all combinations of factors
  factor_combinations <- tidyr::expand_grid(
    M = M_vals,
    I = I_vals,
    N = N_vals
  )
  
  # Add replicates
  experiment_design <- factor_combinations |>
    dplyr::slice(rep(1:dplyr::n(), each = n_rep)) |>
    dplyr::mutate(rep = rep(1:n_rep, times = nrow(factor_combinations))) |>
    dplyr::tibble()
  
  # Make the intercept be zero nitrogen
  experiment_design$N <- with(experiment_design, N - min(N))
  
  # Extract and transform factors
  N <- experiment_design$N
  M <- experiment_design$M
  I <- experiment_design$I
  
  # Simulate biomass
  biomass <- exp(
    alpha +
      gamma1 * N +
      gamma2 * M +
      gamma3 * I +
      gamma4 * N * M +
      gamma5 * N * I +
      gamma6 * M * I +
      gamma7 * N * M * I +
      rnorm(length(N), mean = 0, sd = sigma_residual)
  )
  
  # Return simulation results
  dplyr::mutate(experiment_design, B = biomass)
  
}
