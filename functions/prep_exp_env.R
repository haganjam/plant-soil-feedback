
# prepare experiment environment

# load the relevant libraries
library(dplyr)
library(ggplot2)
library(patchwork)
library(emmeans)

# get a list of the function files
file_list <- list.files(here::here("functions/"))
print(file_list)

# remove the sim_exp1.R, sim_exp2.R and sim_power_exp1.R files
file_list <- file_list[!(file_list %in% c("sim_exp1.R", "sim_exp2.R", "sim_power_exp1.R"))]

# remove the eda_func.R file
file_list <- file_list[file_list != "eda_func.R"]

# remove the prep_exp_env.R script
file_list <- file_list[file_list != "prep_exp_env.R"]

# load the functions using source
for (i in file_list) {
  source(file.path(here::here("functions"), i))
}

# load the data cleaning functions
source(here::here("02-code/02-clean-exp1.R"))
source(here::here("02-code/03-clean-exp2.R"))

# set location for outputted figures and tables
figure_table_path <- here::here("03-report/figures-tables")

# set the figure suffix
figure_suff <- function(x, y) {
  paste0(x, paste0("-exlude_high_N-", y))
}





