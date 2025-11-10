## -------------------------------------------------------------------
## Script name: set_environment
## Purpose of script: Setting up working environment before any functions
## Author: Peter Kedron
## Date Created: 2025-06-19
## Last Updated: 2025-08-28

## Inputs:
## code_dir (character): The directory for all scripts.

## Outputs:
## No output. Set up the computational environment.

## WARNING:
## Using groundhog may disrupt the usage due to permission or restart
## issues. The package may also cause issues with different R versions.
## -------------------------------------------------------------------

set_environment <- function(code_dir, date = "2025-06-01"){
  
  # Install groundhog
  if (!require("groundhog", character.only = TRUE)) {
    install.packages("groundhog")
    require("groundhog")
  }
  
  # Load libraries with specific version by groundhog
  pkgs <- c("plyr", "dplyr", "readxl", "here", "ggplot2", "tidyr", "irr")
  groundhog.library(pkgs, date)
  rm(pkgs)
  
  # Load functions
  functions <- c("clean_data", "describe_data", "compare_ratings", "create_table2","create_figure1")
  for (func in file.path(here(), code_dir, 
                         sprintf("%s.R", functions)
  )
  ){source(func)}
}