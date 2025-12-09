## -------------------------------------------------------------------
## Script name: control_analysis
## Purpose of script: Execute descriptive analysis of audit data
## Author: Peter Kedron
## Date Created: 2025-07-02
## Last Updated: 2025-12-08

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
source("describe_data.r")
source("compare_ratings.r")
source("create_table2.r")
source("create_figure1.r")

# Set up environment
set_environment("procedure/code")

# Clean and prepare data
dat_analysis <- clean_data("data/raw/public", "data/derived/public")

# Conduct descriptive analysis
descriptive_results <- describe_data("data/derived/public", 
                                     "results/figures", 
                                     "results/tables")

period_comparison <- compare_ratings("data/derived/public", "results/tables")

# Create reproducibility summary tables and figures
reproducibility_summary <- create_table2("data/derived/public", 
                                         "results/tables")

figure1_plot <- create_figure1("results/tables", "results/figures")
