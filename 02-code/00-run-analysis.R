
# run the analysis for all four species and produce the figures / tables

# load the relevant library
library(quarto)
library(fs)

# make the list of species to loop over
species_list <- c("Erigeron_canadensis", 
                  "Phytolacca_americana", 
                  "Solidago_gigantea",
                  "Sporobolus_indicus")

# experiment 1 analysis
for (sp in species_list) {
  
  # create an output file name
  output_file <- paste0("report-exp1-", sp, ".html")
  
  # render the relevant quarto document
  quarto_render(
    input = "02-code/05-experiment-1.qmd",
    output_file = output_file,
    execute_params = list(
      invasive_species = sp,
      exclude_highest_N = FALSE
    )
  )
  
  # move file
  file_move(output_file, here::here("02-code/quarto-renders", output_file))
  
}

# additional files
file_name <- "05-experiment-1_files"

# remove the created files if present
if (file_name %in% list.files("02-code")) {
  # delete the file
  if (file_name %in% list.files()) {
    file_delete(here::here("02-code/quarto-renders", file_name)) 
  }
  # move file
  file_move(file.path("02-code", file_name), here::here("02-code/quarto-renders", file_name))
}

# experiment 2 analysis
for (sp in species_list) {
  
  # create an output file name
  output_file <- paste0("report-exp2-", sp, ".html")
  
  # render the relevant quarto document
  quarto_render(
    input = "02-code/06-experiment-2.qmd",
    output_file = output_file,
    execute_params = list(
      invasive_species = sp,
      exclude_highest_N = FALSE
    )
  )
  
  # move file
  file_move(output_file, here::here("02-code/quarto-renders", output_file))
  
}

# additional files
file_name <- "06-experiment-2_files"

# remove the created files if present
if (file_name %in% list.files("02-code")) {
  # delete file
  if (file_name %in% list.files()) {
    file_delete(here::here("02-code/quarto-renders", file_name)) 
  } 
  # move file
  file_move(file.path("02-code", file_name), here::here("02-code/quarto-renders", file_name))
}






