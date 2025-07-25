
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
#' @param min_native_biomass Numeric scalar giving the minimum native biomass possible Default is 18 mg
#' @param solve_simultaneous Logical (TRUE/FALSE) if solve lotka-volterra equation simultaneously
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
                                 alpha_NI = 0.3,
                                 min_native_biomass = 18,
                                 solve_simultaneous = TRUE) {
  
  # Normalize nitrogen values
  N_vals_t <- N_vals - log(4)
  
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
  
  # duplicate grid for native and invasive species
  combos_native <- combos
  combos_native$P <- factor(0)
  
  combos_invasive <- combos
  combos_invasive$P <- factor(1)
  
  # predict log-biomass
  log_nat <- suppressWarnings(predict(fitted_model, newdata = combos_native))
  log_inv <- suppressWarnings(predict(fitted_model, newdata = combos_invasive))
  
  # add stochastic residuals
  noise_add <- rnorm(nrow(combos_native), mean = 0, sd = sigma_residual)
  
  # back-transform to get carrying capacities
  K_N <- 4 * exp(log_nat + noise_add)
  K_I <- exp(log_inv + noise_add)
  
  # replicate the competition coefficients so that they can be vectorised
  alpha_IN <- rep(alpha_IN, length(K_N))
  alpha_NI <- rep(alpha_NI, length(K_N))
  
  if (solve_simultaneous) {
    
    # outcome categories
    coexist <- (alpha_IN < K_N / K_I) & (alpha_NI < K_I / K_N)
    n_wins  <- (K_N / alpha_IN > K_I) & (K_I / alpha_NI < K_N)
    i_wins  <- (K_N / alpha_IN < K_I) & (K_I / alpha_NI > K_N)
    unstable <- !(coexist | n_wins | i_wins)
    
    # check the outcome combinations
    stopifnot(sum(coexist) + sum(n_wins) + sum(i_wins) + sum(unstable) == length(K_N))
    
    # calculate denominator
    denom <- (1 - alpha_IN * alpha_NI)
    
    # initialize B_N_star
    B_N_star <- rep(NA_real_, length(K_N))
    
    # apply coexistence formula
    B_N_star[coexist] <- pmax((K_N[coexist] - alpha_IN[coexist] * K_I[coexist]) / denom[coexist], min_native_biomass)
    
    # species N wins
    B_N_star[n_wins] <- K_N[n_wins]
    
    # species I wins
    B_N_star[i_wins] <- min_native_biomass
    
    # stochastic resolution for unstable cases (biased toward higher K)
    if (any(unstable)) {
      p_N_wins <- K_N[unstable] / (K_N[unstable] + K_I[unstable])
      rand_draws <- runif(sum(unstable))
      n_winner <- rand_draws < p_N_wins
      idx_unstable <- which(unstable)
      
      B_N_star[idx_unstable[n_winner]] <- K_N[idx_unstable[n_winner]]
      B_N_star[idx_unstable[!n_winner]] <- min_native_biomass
    }
    
  } else {
    
    # assume simple linear competition without solving simultaneously
    B_N_star <- K_N - (alpha_NI * K_I)
    
  }
  
  # assemble results
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
      df$B <- ifelse(df$B < min_native_biomass, min_native_biomass, df$B)
      df[order(df$N, df$M, df$I, df$replicate), ]
    })()
  
  # convert to tibble
  results <- dplyr::as_tibble(results)
  
  return(results)
}





