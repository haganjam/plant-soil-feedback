
# Load required package
library(parallel)
library(dplyr)
library(tidyr)

# convert percentage to parameter for the log-scale
par_target <- function(target) log(1 + target)

alpha <- log(2.5)
beta1 <- par_target(c(0.05, 0.10, 0.15))
beta2 <- par_target(seq(0.10, 0.40, 0.10))
beta3 <- par_target(seq(-0.10, 0.30, 0.10))
beta4 <- par_target(seq(-0.10, 0.10, 0.10))
beta5 <- par_target(seq(-0.10, 0.10, 0.10))
beta6 <- par_target(seq(-0.10, 0.10, 0.10))
beta7_ha <- par_target(c(0.05, 0.10, 0.20, 0.30))

sigma_residual <- c(0.10, 0.20, 0.30)

# in addition, we set the number of replicates
n_rep <- seq(2, 5, 1)

# wrap into a simulation grid
exp1_sim_grid <- tidyr::expand_grid(n_rep,
                                    alpha, beta1, beta2, beta3,
                                    beta4, beta5, beta6,
                                    beta7_ha,
                                    sigma_residual)

# define number of cores
n_cores <- 6

# stop if number of cores too high
stopifnot(n_cores < parallel::detectCores())

# load the power functions
source(here::here("functions/sim_power_exp1.R"))

# parallelised loop
power_exp1 <- mclapply(seq_len(nrow(exp1_sim_grid)), mc.cores = n_cores, function(i) {
  
  # get the input parameters
  input_pars <- exp1_sim_grid[i, ]
  
  # run the simulation
  power_x <- sim_power_exp1(
    n_sim = 100,
    alpha_rej = 0.05,
    n_rep = input_pars$n_rep,                   
    sigma_residual = input_pars$sigma_residual,        
    N_vals = log(c(4, 8, 16, 32, 64)),
    M_vals = c(0, 1),
    P_vals = c(0, 1),              
    alpha = input_pars$alpha,                  
    beta1 = input_pars$beta1,                  
    beta2 = input_pars$beta2,
    beta3 = input_pars$beta3,
    beta4 = input_pars$beta4,
    beta5 = input_pars$beta5,
    beta6 = input_pars$beta6,
    beta7_h0 = 0,
    beta7_ha = input_pars$beta7_ha
  )
  
  # return results
  tibble(
    run = i,
    type_1_error = power_x$type_I_error_rate,
    type_2_error = power_x$type_II_error_rate,
    power = power_x$power
  )
})

# bind into a data.frame
power_exp1 <- dplyr::bind_rows(power_exp1)

# bind to the parameter data
power_exp1 <- dplyr::bind_cols(exp1_sim_grid, power_exp1)

# export the the simulated data
saveRDS(object = power_exp1,
        file = here::here("01-plan/01-study-design/jobs/power-simulations-exp1.rds"))
