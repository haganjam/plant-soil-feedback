
# load the data simulation for exp1
source(here::here("functions/sim_exp1.R"))

#' @param n_sim - number of simulations to run of the experiment
#' @param beta7_h0 - beta7 under the null hypothesis
#' @param beta7_ha - beta7 under the alternative hypothesis
# 
sim_power_exp1 <- function(
    n_sim = 100,
    alpha_rej = 0.05,
    n_rep = 5,                  
    sigma_residual = 0.15,        
    N_vals = log(c(4, 8, 16, 32, 64)),
    M_vals = c(0, 1),
    P_vals = c(0, 1),              
    alpha = 1.5,                  
    beta1 = 0.2,                  
    beta2 = -0.12,
    beta3 = 0.05,
    beta4 = 0.05,
    beta5 = 0.05,
    beta6 = 0.05,
    beta7_h0 = 0,
    beta7_ha = -0.05) {
  
  # initialize vectors to store Type I and Type II errors
  type_I_errors <- vector(length = n_sim)
  type_II_errors <- vector(length = n_sim)
  
  # simulate under H0 (Null Hypothesis)
  for (i in seq_len(n_sim)) {
    
    # generate data under H0
    data_h0 <-
      sim_exp1(n_rep = n_rep,
               N_vals = N_vals,
               M_vals = M_vals,
               P_vals = P_vals,
               alpha = alpha,
               beta1 = beta1,
               beta2 = beta1,
               beta3 = beta3,
               beta4 = beta4,
               beta5 = beta5,
               beta6 = beta6,
               beta7 = beta7_h0,
               sigma_residual = sigma_residual,
               verbose = FALSE)
    
    # fit the model under H0
    model_h0 <- lm(log(B) ~ N + M + P + N:M + N:P + M:P + N:M:P, data = data_h0)
    
    # extract the relevant p-value
    p_value_h0 <- coef(summary(model_h0))["N:M:P", "Pr(>|t|)"]
    
    # collect Type I error
    type_I_errors[i] <- as.numeric(p_value_h0 < alpha_rej)
    
  }
  
  # simulate under HA (Alternative Hypothesis)
  for (i in seq_len(n_sim)) {
    
    # generate data under HA
    data_ha <-
      sim_exp1(n_rep = n_rep,
               N_vals = N_vals,
               M_vals = M_vals,
               P_vals = P_vals,
               alpha = alpha,
               beta1 = beta1,
               beta2 = beta1,
               beta3 = beta3,
               beta4 = beta4,
               beta5 = beta5,
               beta6 = beta6,
               beta7 = beta7_ha,
               sigma_residual = sigma_residual,
               verbose = FALSE)
    
    # fit the model under H0
    model_ha <- lm(log(B) ~ N + M + P + N:M + N:P + M:P + N:M:P, data = data_ha)
    
    # extract the relevant p-value
    p_value_ha <- coef(summary(model_ha))["N:M:P", "Pr(>|t|)"]
    
    # collect Type I error
    type_II_errors[i] <- as.numeric(p_value_ha >= alpha_rej)
    
  }
  
  # calculate metrics
  type_I_error_rate <- sum(type_I_errors, na.rm = TRUE) / length(na.omit(type_I_errors))
  type_II_error_rate <- sum(type_II_errors, na.rm = TRUE) / length(na.omit(type_II_errors))
  power <- 1 - type_II_error_rate
  
  # return results
  list(
    type_I_error_rate = type_I_error_rate,
    type_II_error_rate = type_II_error_rate,
    power = power
  )
  
}




