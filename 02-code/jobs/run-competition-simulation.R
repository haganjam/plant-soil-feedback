# Load required packages
library(parallel)
library(dplyr)
library(here)
library(splines)

# Load the simulation function
source(here::here("functions/simulate_competition.R"))

# Set up a vector of competition values
alpha_vals <- 
  seq(0.1, 0.8, 0.05) |>
  rep(each = 1000)

# Number of cores
n_cores <- 4

# Run simulations in parallel
sim_out <- mclapply(seq_along(alpha_vals), function(i) {
  
  # For reproducibility inside mclapply
  set.seed(123 + i)
  
  # Run the simulation
  x <- simulate_competition(
    N_vals = seq(log(4) - 0.05, log(64) + 0.05, by = 0.05),
    M_vals = c(0, 1),
    n_rep = 100,
    fitted_model = mod,
    alpha_IN = alpha_vals[i],
    alpha_NI = alpha_vals[i]
  )
  
  # Add alpha metadata
  bind_cols(
    tibble(alpha_sim_rep = i, alpha_sim = alpha_vals[i]),
    x
  )
  
}, mc.cores = n_cores)

# Save the simulation output
saveRDS(
  object = sim_out,
  file = file.path(here::here("02-code/outputs"), sim_file)
)
