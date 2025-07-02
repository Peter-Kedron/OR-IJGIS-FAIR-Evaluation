## -------------------------------------------------------------------
## Script name: control_file
## Purpose of script: Record sequence of statistical analysis
## Author: Peter Kedron
## Date Created: 2025-07-02

## Inputs:
## code_dir (character): The directory for all scripts.

## Outputs:
## No output. Set up the computational environment.

## WARNING:
## Using groundhog may disrupt the usage due to permission or restart
## issues. The package may also cause issues with different R versions.
## -------------------------------------------------------------------

# Load functions used in analysis
source("set_environment.r")
source("clean_data.r")

set_environment("procedure/code")
clean_data("data/raw/public", "data/derived/public")