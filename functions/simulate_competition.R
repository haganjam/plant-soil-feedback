
# extract sigma from the fitted model
extract_model_sigma <- function(model) {
  # extract the model summary
  model_summary <- summary(model)
  # extract the sigma value
  model_summary[["sigma"]]
}

#' Simulate Biomass Under Mutual Competition Between Native and Invasive Species
#'
#' This function simulates biomass outcomes for native and invasive species
#' across nitrogen and microbial gradients, incorporating mutual competitive effects.
#' The simulation uses equilibrium biomass equations derived from a Lotka–Volterra competition model.
#'
#' @param N_vals A numeric vector of nitrogen values (e.g., log-transformed).
#' @param M_vals A numeric vector indicating microbial presence/absence (e.g., 0 = absent, 1 = present).
#' @param n_rep Integer specifying the number of replicate simulations per condition. Default is 50.
#' @param fitted_model A fitted model object used to extract biomass predictions and residual variance.
#'        Must be compatible with `extract_model_parameters()` and `extract_model_sigma()`.
#' @param alpha_IN Numeric scalar giving the per capita effect of the invasive species on the native. Default is 0.3.
#' @param alpha_NI Numeric scalar giving the per capita effect of the native species on the invasive. Default is 0.3.
#'
#' @return A data frame (tibble) with columns:
#'   \itemize{
#'     \item \code{N}: Nitrogen level
#'     \item \code{M}: Microbial presence (as character)
#'     \item \code{I}: Species identity (0 = native baseline biomass; 1 = native biomass under competition)
#'     \item \code{replicate}: Replicate ID
#'     \item \code{B}: Simulated biomass
#'   }
#'
#' @details
#' For each nitrogen-microbe combination, the function:
#' \enumerate{
#'   \item Predicts log-biomass for native and invasive species using the fitted model.
#'   \item Converts log-biomass to expected biomass by simulating residual error.
#'   \item Uses the Lotka–Volterra competition equations to estimate equilibrium biomass under mutual competition.
#'   \item Stores both the baseline biomass (\code{K_N}) and equilibrium biomass (\code{B_N*}) for the native species.
#' }
#'
#' Negative equilibrium biomasses are truncated at a minimum of 0.005 to avoid biologically implausible values.
#'
#' @seealso \code{\link{extract_model_parameters}}, \code{\link{log_biomass}}, \code{\link{extract_model_sigma}}
#'
#' @export
simulate_competition <- function(N_vals, 
                                 M_vals, 
                                 n_rep = 50,
                                 fitted_model,
                                 alpha_IN = 0.3,
                                 alpha_NI = 0.3) {
  
  # Normalize nitrogen values
  N_vals_t <- N_vals - min(N_vals)
  
  # Extract residual standard deviation
  sigma_residual <- extract_model_sigma(fitted_model)
  
  # Create all combinations of N, M, replicate
  combos <- expand.grid(
    N = N_vals_t,
    M = M_vals,
    replicate = seq_len(n_rep)
  )
  
  # convert M to a factor
  combos$M <- factor(combos$M)
  
  # Duplicate grid for native and invasive species
  combos_native <- combos
  combos_native$P <- factor(0)
  
  combos_invasive <- combos
  combos_invasive$P <- factor(1)
  
  # Predict log-biomass
  log_nat <- suppressWarnings(predict(fitted_model, newdata = combos_native))
  log_inv <- suppressWarnings(predict(fitted_model, newdata = combos_invasive))
  
  # Add stochastic residuals
  noise_nat <- rnorm(nrow(combos_native), mean = 0, sd = sigma_residual)
  noise_inv <- rnorm(nrow(combos_invasive), mean = 0, sd = sigma_residual)
  
  # Back-transform to get carrying capacities
  K_N <- 4 * exp(log_nat + noise_nat)
  K_I <- exp(log_inv + noise_inv)
  
  # Solve equilibrium biomass under Lotka–Volterra competition
  denom <- 1 - alpha_IN * alpha_NI
  B_N_star <- pmax((K_N - alpha_IN * K_I) / denom, 0.005)
  
  # Assemble results
  baseline <- transform(
    combos_native,
    I = "0",
    B = K_N
  )[, c("N", "M", "I", "replicate", "B")]
  
  competition <- transform(
    combos_native,
    I = "1",
    B = B_N_star
  )[, c("N", "M", "I", "replicate", "B")]
  
  # Combine and finalize
  results <- rbind(baseline, competition) |>
    within({
      M <- as.character(M)
      I <- as.character(I)
    }) |>
    (\(df) {
      min_nonzero <- min(df$B[df$B > 0])
      df$B <- ifelse(df$B <= 0, min_nonzero, df$B)
      df[order(df$N, df$M, df$I, df$replicate), ]
    })()
  
  # convert to tibble
  results <- dplyr::as_tibble(results)
  
  return(results)
  
}




