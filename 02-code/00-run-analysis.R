
# run the analysis for all four species and produce the figures / tables

# load the relevant library
library(quarto)
library(fs)

# make the list of species to loop over
species_list <- c("Erigeron_canadensis", 
                  "Phytolacca_americana", 
                  "Solidago_gigantea",
                  "Sporobolus_indicus")

# function to move and clean-up relevant files
move_and_cleanup <- function(file_name) {
  
  # check if file exists in 02-code
  if (file_name %in% list.files("02-code")) {
    
    # if file already exists in quarto-renders, delete it
    if (file_name %in% list.files("02-code/quarto-renders")) {
      file_delete(here::here("02-code/quarto-renders", file_name))
    }
    
    # move the file from 02-code to quarto-renders
    file_move(
      file.path("02-code", file_name),
      here::here("02-code/quarto-renders", file_name)
    )
  }
}

## experiment 1 analysis

# loop over each species
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

# remove the created files if present
move_and_cleanup("05-experiment-1_files")

## experiment 2 analysis

# loop over each species
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

# remove the created files if present
move_and_cleanup("06-experiment-2_files")

## individual species response analysis

# loop over each species
for (sp in species_list) {
  
  # create an output file name
  output_file <- paste0("report-ind-plot-", sp, ".html")
  
  # render the relevant quarto document
  quarto_render(
    input = "02-code/07-individual-species-responses.qmd",
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
move_and_cleanup("07-individual-species-responses_files")

## simulation analysis

# build expected filenames
expected_files <- c(
  paste0(species_list, "-exclude_high_N-FALSE-simulation-output-simultaneous-FALSE.rds"),
  paste0(species_list, "-exclude_high_N-FALSE-simulation-output-simultaneous-TRUE.rds")
)

# check presence
present <- 
  file.exists(file.path(here::here("02-code/outputs"), 
                        expected_files))

if (all(present)) {
  
  # loop over each species
  for (sp in species_list) {
    
    # create an output file name
    output_file <- paste0("report-simulation-", sp, ".html")
    
    # render the relevant quarto document
    quarto_render(
      input = "02-code/08-simulation-experiment.qmd",
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
  move_and_cleanup("08-simulation-experiment_files")
  
} else {
  
  message("Simulation analysis not performed! Run simulations individually, see README")
  
}
