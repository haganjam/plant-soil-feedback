
# run the analysis for all four species and produce the figures / tables

# load the relevant library
library(quarto)

# make the list of species to loop over
species_list <- c("Erigeron_canadensis", 
                  "Phytolacca_americana", 
                  "Solidago_gigantea",
                  "Sporobolus_indicus")

# experiment 1 analysis
for (sp in species_list) {
  output_file <- paste0("report_", sp, ".html")
  
  quarto_render(
    input = "02-code/05-experiment-1.qmd",
    output_file = output_file,
    execute_params = list(
      invasive_species = sp,
      exclude_highest_N = FALSE
    )
  )
}
