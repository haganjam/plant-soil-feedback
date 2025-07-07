
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
  
  # extract the sigma_residual
  sigma_residual <- extract_model_sigma(model = fitted_model)
  
  # create an empty list
  results <- list()
  
  for (N in N_vals_t) {
    # print(paste0("N = ", N))
    for (M in M_vals) {
      # print(paste0("M = ", M))
      for (rep in seq_len(n_rep)) {
        # print(paste0("rep = ", rep))
        
        # model-estimated log-biomass as an estimate of the carrying capacity
        suppressWarnings(log_nat <- predict(fitted_model, newdata = dplyr::tibble(N = N, M = factor(M), P = factor(0))))
        suppressWarnings(log_inv <- predict(fitted_model, newdata = dplyr::tibble(N = N, M = factor(M), P = factor(1))))
        
        # carrying capacity of native and invasives by back-transforming log-prediction
        K_N <- 4 * exp(log_nat + rnorm(1, 0, sigma_residual))
        K_I <- exp(log_inv + rnorm(1, 0, sigma_residual))
        
        # denominator
        denom <- 1 - alpha_IN * alpha_NI
        
        # solve for equilibrium biomass under mutual competition
        B_N_star <- (K_N - alpha_IN * K_I) / denom
        B_I_star <- (K_I - alpha_NI * K_N) / denom
        
        # ensure equilibrium biomass is non-negative
        B_N_star <- max(B_N_star, 0.005)
        B_I_star <- max(B_I_star, 0.005)
        
        B_N_star
        B_I_star
        
        
        # store the outputs
        results[[length(results) + 1]] <- dplyr::tibble(
          N = N,
          M = M,
          I = 0,
          replicate = rep,
          B = K_N
        )
        results[[length(results) + 1]] <- dplyr::tibble(
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

#' Simulate Biomass Under Mutual Competition (Parallelized with Progress)
#'
#' Parallelized version of `simulate_competition()` using the future/furrr package.
#' Includes progress tracking with the progressr package.
#'
#' @param N_vals A numeric vector of nitrogen values (e.g., log-transformed).
#' @param M_vals A numeric vector indicating microbial presence/absence (e.g., 0 = absent, 1 = present).
#' @param n_rep Integer specifying the number of replicate simulations per condition. Default is 50.
#' @param fitted_model A fitted model object compatible with `predict()` and custom extractors.
#' @param alpha_IN Numeric scalar giving the per capita effect of the invasive species on the native. Default is 0.3.
#' @param alpha_NI Numeric scalar giving the per capita effect of the native species on the invasive. Default is 0.3.
#'
#' @return A tibble with columns: N, M, I, replicate, B
#' @export
simulate_competition_parallel <- function(N_vals,
                                          M_vals,
                                          n_rep = 50,
                                          fitted_model,
                                          alpha_IN = 0.3,
                                          alpha_NI = 0.3) {
  
  # Load required packages
  requireNamespace("furrr", quietly = TRUE)
  requireNamespace("future", quietly = TRUE)
  requireNamespace("progressr", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  library(splines)
  
  # Set up parallel processing (use multicore on Unix/macOS)
  future::plan(future::multisession, workers = 8)
  
  # Transform N values to minimum zero
  N_vals_t <- N_vals - min(N_vals)
  
  # Build simulation grid
  sim_grid <- expand.grid(
    N = N_vals_t,
    M = M_vals,
    replicate = seq_len(n_rep),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  
  # Extract residual standard deviation from model
  sigma_residual <- extract_model_sigma(fitted_model)
  
  # Define simulation for one row
  simulate_one <- function(N, M, rep, fitted_model, sigma_residual, alpha_IN, alpha_NI) {
    suppressWarnings(
      log_nat <- predict(fitted_model, newdata = tibble::tibble(N = N, M = factor(M), P = factor(0)))
    )
    suppressWarnings(
      log_inv <- predict(fitted_model, newdata = tibble::tibble(N = N, M = factor(M), P = factor(1)))
    )
    
    K_N <- 4 * exp(log_nat + rnorm(1, 0, sigma_residual))
    K_I <- exp(log_inv + rnorm(1, 0, sigma_residual))
    
    denom <- 1 - alpha_IN * alpha_NI
    B_N_star <- max((K_N - alpha_IN * K_I) / denom, 0.005)
    B_I_star <- max((K_I - alpha_NI * K_N) / denom, 0.005)
    
    tibble::tibble(
      N = c(N, N),
      M = c(M, M),
      I = c("0", "1"),
      replicate = c(rep, rep),
      B = c(K_N, B_N_star)
    )
  }
  
  # Append parameters to grid
  sim_grid <- sim_grid |>
    dplyr::mutate(
      fitted_model = list(fitted_model),
      sigma_residual = sigma_residual,
      alpha_IN = alpha_IN,
      alpha_NI = alpha_NI
    )
  
  # Run simulations with progress tracking
  results <- progressr::with_progress({
    p <- progressr::progressor()
    
    furrr::future_pmap_dfr(
      .l = sim_grid,
      .f = ~{
        p()
        simulate_one(..1, ..2, ..3, ..4, ..5, ..6, ..7)
      },
      .options = furrr::furrr_options(seed = TRUE)
    )
  })
  
  # Format results
  results <- results |>
    dplyr::arrange(N, M, I, replicate) |>
    dplyr::mutate(
      M = as.character(M),
      I = as.character(I)
    )
  
  # Replace non-positive B values
  min_zero <- min(results$B[results$B > 0])
  results$B <- ifelse(results$B <= 0, min_zero, results$B)
  
  return(results)
  
}









