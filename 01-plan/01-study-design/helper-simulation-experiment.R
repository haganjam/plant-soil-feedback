
# extract parameters from the fitted model
extract_model_parameters <- function(model) {
  # extract the model coefficients
  model_coef <- model[["coefficients"]]
  names(model_coef) <- NULL
  # wrap the coefficients into a list
  list(alpha = model_coef[1],
       beta1 = model_coef[2],
       beta2 = model_coef[3],
       beta3 = model_coef[4],
       beta4 = model_coef[5],
       beta5 = model_coef[6],
       beta6 = model_coef[7],
       beta7 = model_coef[8])
}

# extract sigma from the fitted model
extract_model_sigma <- function(model) {
  # extract the model summary
  model_summary <- summary(model)
  # extract the sigma value
  model_summary[["sigma"]]
}

# Function to compute expected log-biomass
log_biomass <- function(N, M, P, model_parameters) {
  
  with(model_parameters,
       alpha +
         beta1 * N +
         beta2 * M +
         beta3 * P +
         beta4 * N * M +
         beta5 * N * P +
         beta6 * M * P +
         beta7 * N * M * P)
  
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
  
  # transform N_vals to min zero
  N_vals_t <- N_vals - min(N_vals)
  
  # extract the model parameters
  mod_pars <- extract_model_parameters(model = fitted_model)
  
  # extract the sigma_residual
  sigma_residual <- extract_model_sigma(model = fitted_model)
  
  # create an empty list
  results <- list()
  
  for (N in N_vals_t) {
    for (M in M_vals) {
      for (rep in seq_len(n_rep)) {
        
        # model-estimated log-biomass as an estimate of the carrying capacity
        log_nat <- log_biomass(N, M, 0, model_parameters = mod_pars)
        log_inv <- log_biomass(N, M, 1, model_parameters = mod_pars)
        
        # carrying capacity of native and invasives by back-transforming log-prediction
        K_N <- exp(log_nat + rnorm(1, 0, sigma_residual))
        K_I <- exp(log_inv + rnorm(1, 0, sigma_residual))
        
        # denominator
        denom <- 1 - alpha_IN * alpha_NI
        
        # solve for equilibrium biomass under mutual competition
        B_N_star <- (K_N - alpha_IN * K_I) / denom
        B_I_star <- (K_I - alpha_NI * K_N) / denom
        
        # ensure equilibrium biomass is non-negative
        B_N_star <- max(B_N_star, 0.005)
        B_I_star <- max(B_I_star, 0.005)
        
        # store the outputs
        results[[length(results) + 1]] <- tibble(
          N = N,
          M = M,
          I = 0,
          replicate = rep,
          B = K_N
        )
        results[[length(results) + 1]] <- tibble(
          N = N,
          M = M,
          I = 1,
          replicate = rep,
          B = B_N_star
        )
      }
    }
  }
  
  # bind into a data.frame
  results <-
    dplyr::bind_rows(results) |> 
    dplyr::arrange(N, M, I, replicate) |>
    dplyr::mutate(M = as.character(M),
                  I = as.character(I))
  
  # replace zeros with the minimum
  min_zero <- min(results$B[results$B > 0])
  results$B <- ifelse(results$B <= 0, min_zero, results$B)
  
  # return the results data.frame
  results
  
}