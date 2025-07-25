# load required packages
library(parallel)
library(dplyr)
library(here)
library(splines)

# load the simulation function
source(here::here("functions/simulate_competition.R"))

# set up a vector of competition values
alpha_vals <- 
  seq(0.2, 0.8, 0.1) |>
  rep(each = 1000)

# number of cores
n_cores <- 4

# loop over with and without simultaneous solving
sim_type <- c(TRUE, FALSE)
names(sim_type) <- c("-simultaneous-TRUE.rds", "-simultaneous-FALSE.rds")

for (k in seq_along(sim_type)) {
  
  # extract the sim_type
  sim_type_select <- sim_type[k]
  
  # run simulations in parallel: Solving simultaneously
  sim_out <- mclapply(seq_along(alpha_vals), function(i) {
    
    # for reproducibility inside mclapply
    set.seed(123 + i)
    
    # run the simulation
    x <- simulate_competition(
      N_vals = seq(log(4) - 0.05, log(64) + 0.05, by = 0.025),
      M_vals = c(0, 1),
      n_rep = 30,
      fitted_model = mod,
      alpha_IN = alpha_vals[i],
      alpha_NI = alpha_vals[i],
      min_native_biomass = 18,
      solve_simultaneous = sim_type_select
    )
    
    # add alpha metadata
    y <- 
      bind_cols(
        tibble(alpha_sim_rep = i, alpha_sim = alpha_vals[i]),
        x)
    
    # summarise across replicates within a treatment combination
    y |>
      dplyr::filter(!is.na(B)) |>
      dplyr::mutate(sim_id = paste(alpha_sim, "_", alpha_sim_rep, sep = "")) |>
      dplyr::mutate(B = log(B/4)) |>
      dplyr::group_by(sim_id, N, M, I) |>
      dplyr::summarise(B_mean = mean(B, na.rm = TRUE),
                       B_low = mean(B, na.rm = TRUE) - sd(B, na.rm = TRUE),
                       B_high = mean(B, na.rm = TRUE) + sd(B, na.rm = TRUE)) |>
      dplyr::ungroup()
    
  }, mc.cores = n_cores)
  
  # Save the simulation output
  saveRDS(
    object = sim_out,
    file = file.path(here::here("02-code/outputs"), paste0(sim_file, names(sim_type)[k]))
  )
  
}

