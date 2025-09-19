
# prepare experiment environment

# load the relevant libraries
pkgs <- c("dplyr","ggplot2","patchwork","emmeans")
for (p in pkgs) {
  cat("\n---- attaching:", p, "----\n")
  try(library(p, character.only = TRUE))
}

# get a list of the function files
fun_dir <- here::here("functions")
file_list <- list.files(
  fun_dir,
  pattern = "\\.[Rr]$",
  full.names = FALSE
)

# drop specific scripts you don't want
file_list <- setdiff(
  file_list,
  c("sim_exp1.R", "sim_exp2.R", "sim_power_exp1.R", "eda_func.R", "prep_exp_env.R")
)

# source them with guardrails so you know which one fails
for (i in file_list) {
  message("Sourcing: ", i)
  tryCatch(
    source(file.path(fun_dir, i), local = TRUE),
    error = function(e) {
      stop(sprintf("Error while sourcing %s: %s", i, conditionMessage(e)), call. = FALSE)
    }
  )
}

# load the data cleaning functions
source(here::here("02-code/02-clean-exp1.R"))
source(here::here("02-code/03-clean-exp2.R"))

# set location for outputted figures and tables
figure_table_path <- here::here("03-report/figures-tables")

# set the figure suffix
figure_suff <- function(x, y) {
  paste0(x, paste0("-exclude_high_N-", y))
}





