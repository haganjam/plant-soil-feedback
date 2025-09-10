
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
  output_file <- paste0("report-", sp, ".html")
  
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

# remove the created files if present
if ("05-experiment-1_files" %in% list.files("02-code")) {
  # delete the file
  file_delete(here::here("02-code/quarto-renders", "05-experiment-1_files"))  
  # move file
  file_move("02-code/05-experiment-1_files", here::here("02-code/quarto-renders", "05-experiment-1_files"))
}

