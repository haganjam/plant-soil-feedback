
# functions

#' Experiment 1: Simulate Plant Biomass Under Experimental Treatment Combinations
#'
#' Generates a simulated dataset of plant biomass under factorial combinations of
#' nitrogen levels, soil microbes presence/absence, and plant origin (native/invasive),
#' with customizable effect sizes and residual variance.
#'
#' @param n_rep Integer. Number of replicate observations per treatment combination. Default is 100.
#' @param N_vals Numeric vector. Levels of nitrogen (on log scale). Default is `log(c(4, 8, 16, 32, 64))`.
#' @param M_vals Numeric vector. Microbe treatment levels (0 = absent, 1 = present). Default is `c(0, 1)`.
#' @param P_vals Numeric vector. Plant origin levels (0 = native, 1 = invasive). Default is `c(0, 1)`.
#' @param alpha Numeric. Intercept: log biomass when N, M, and P are all 0. Default is 1.5.
#' @param beta1 Numeric. Effect of nitrogen (N) when M = 0 and P = 0. Default is 0.2.
#' @param beta2 Numeric. Effect of microbes (M) when N = 0 and P = 0. Default is -0.75.
#' @param beta3 Numeric. Effect of plant origin (P) when N = 0 and M = 0. Default is 0.
#' @param beta4 Numeric. Interaction effect of N × M. Default is 0.05.
#' @param beta5 Numeric. Interaction effect of N × P. Default is 0.
#' @param beta6 Numeric. Interaction effect of M × P. Default is 0.
#' @param beta7 Numeric. Three-way interaction effect of N × M × P. Default is 0.4.
#' @param sigma_residual Numeric. Standard deviation of the residual error. Default is 0.10.
#' @param verbose Logical. If `TRUE`, prints interpretation of effect sizes. Default is `TRUE`.
#'
#' @return A tibble with columns for each treatment factor (`M`, `P`, `N`), replicate ID (`rep`),
#'         and simulated biomass (`biomass`) on the natural scale.
#'
#' @examples
#' sim_data <- simulate_exp1()
#' head(sim_data)
#'
#' # Custom effect sizes and nitrogen levels
#' simulate_exp1(n_rep = 50, beta1 = 0.1, N_vals = log(c(5, 10, 20)))
#'
#' @export
sim_exp1 <- function(n_rep = 100,
                     N_vals = log(c(4, 8, 16, 32, 64)),
                     M_vals = c(0, 1),
                     P_vals = c(0, 1),
                     alpha = 1.5,
                     beta1 = 0.2,
                     beta2 = -0.75,
                     beta3 = 0,
                     beta4 = 0.05,
                     beta5 = 0,
                     beta6 = 0,
                     beta7 = 0.4,
                     sigma_residual = 0.10,
                     verbose = FALSE) {
  
  # Load necessary packages
  requireNamespace("dplyr")
  requireNamespace("tidyr")
  
  # Create all combinations of factors
  factor_combinations <- tidyr::expand_grid(
    M = M_vals,
    P = P_vals,
    N = N_vals
  )
  
  # Add replicates
  experiment_design <- factor_combinations |>
    dplyr::slice(rep(1:dplyr::n(), each = n_rep)) |>
    dplyr::mutate(rep = rep(1:n_rep, times = nrow(factor_combinations))) |>
    dplyr::tibble()
  
  # Extract and transform factors
  N <- experiment_design$N
  N <- (N - min(N))  # Make the intercept be zero nitrogen
  M <- experiment_design$M
  P <- experiment_design$P
  
  if (verbose) {
    cat(paste0("alpha: ", round(exp(alpha), 2), " mg biomass when N=0, M=0, P=0\n"))
    cat(paste0("beta1 (N): ", round((exp(beta1) - 1) * 100, 2), "% per unit N\n"))
    cat(paste0("beta2 (M): ", round((exp(beta2) - 1) * 100, 2), "% with microbes\n"))
    cat(paste0("beta3 (P): ", round((exp(beta3) - 1) * 100, 2), "% for invasive\n"))
    cat(paste0("beta4 (N×M): ", round((exp(beta4) - 1) * 100, 2), "%\n"))
    cat(paste0("beta5 (N×P): ", round((exp(beta5) - 1) * 100, 2), "%\n"))
    cat(paste0("beta6 (M×P): ", round((exp(beta6) - 1) * 100, 2), "%\n"))
    cat(paste0("beta7 (N×M×P): ", round((exp(beta7) - 1) * 100, 2), "%\n"))
  }
  
  # Simulate biomass
  biomass <- exp(
    alpha +
      beta1 * N +
      beta2 * M +
      beta3 * P +
      beta4 * N * M +
      beta5 * N * P +
      beta6 * M * P +
      beta7 * N * M * P +
      rnorm(length(N), mean = 0, sd = sigma_residual)
  )
  
  # Return simulation results
  dplyr::mutate(experiment_design, B = biomass)
}




