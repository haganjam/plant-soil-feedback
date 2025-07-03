
# data-cleaning function for experiment 1
# data = exp1 (for example)
# i_species = "Erigeron_canadensis" (for example)
clean_exp1 <- function(data,
                       i_species,
                       exclude_high_N = FALSE) {
  
  # make sure the i_species name is correct
  stopifnot(i_species %in% c("Erigeron_canadensis",
                             "Phytolacca_americana",
                             "Solidago_gigantea",
                             "Sporobolus_indicus"))
  
  # check the data is table
  stopifnot(is.data.frame(data) | dplyr::is.tbl(data))
  
  # select the relevant set-ups
  data_clean <-
    data |>
    dplyr::filter(Setup_IAN %in% c("I", "N"))
  
  # replace the All with the none
  data_clean$Invasive_species <- 
    with(data_clean,
         ifelse(Invasive_species == "All", "none", Invasive_species))
  
  # sum-up the native species
  data_clean <-
    data_clean |>
    dplyr::group_by(Setup_IAN, Nitrogen_level, Soil_conditioning,
                    Invasive_species,
                    Repetition_nr) |>
    dplyr::summarise(Shoot_dry_weight = mean(Shoot_dry_weight, na.rm = TRUE),
                     Root_dry_weight = mean(Root_dry_weight, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  # select the relevant data
  data_clean <-
    data_clean |>
    dplyr::filter(Invasive_species %in% c("none", i_species))
  
  # calculate total weight
  data_clean$total_weight <- 
    with(data_clean,
         Shoot_dry_weight + Root_dry_weight)
  
  # reorder the columns
  data_clean <-
    data_clean |>
    dplyr::select(Soil_conditioning, Setup_IAN, Nitrogen_level,
                  Repetition_nr, Shoot_dry_weight, Root_dry_weight,
                  total_weight)
  
  # rename the columns
  names(data_clean) <- c("M", "P", "N", "rep", "B_shoot", "B_root", "B")
  
  # convert the treatment variables to the values to match the data simulation
  data_clean <-
    data_clean |>
    dplyr::mutate(M = ifelse(M == "C", 0, 1),
                  P = ifelse(P == "N", 0, 1),
                  N = round(log(N), 2))
  head(data_clean)
  
  # check the design matrix
  dm <-
    data_clean |>
    dplyr::select(M, P, N) |>
    dplyr::distinct() |>
    dplyr::arrange(M, P, N)
  
  # simulate the design matrix
  M_vals <- c(0, 1)
  P_vals <- c(0, 1)
  N_vals <- c(1.39, 2.08, 2.77, 3.47, 4.16)
  
  # create all combinations
  dm_true <- expand.grid(M = M_vals, P = P_vals, N = N_vals)
  
  # arrange in the desired order: M varies slowest, then P, then N
  dm_true <- dm_true[order(dm_true$M, dm_true$P, dm_true$N), ]
  
  # convert to tibble
  dm_true <- as_tibble(dm_true)
  
  # compare the design matrices
  stopifnot(all(dm$M == dm_true$M),
            all(dm$P == dm_true$P),
            all(dm$N == dm_true$N))
  
  # give the rows id numbers
  data_clean <- 
    dplyr::bind_cols(dplyr::tibble(row_id = seq_len(nrow(data_clean))),
                     data_clean)
  
  # remove the highest level of nitrogen
  if (exclude_high_N) {
    # filter the highest nitrogen values
    data_clean <-
      data_clean |>
      dplyr::filter(N < max(N))
  }
  
  data_clean
  
}

