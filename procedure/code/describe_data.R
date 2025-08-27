## -----------------------------------------------------------------------------
## Script name: describe_data
## Purpose of script: Execute descriptive analysis of audit data
## Author: Peter Kedron
## Date Created: 2025-07-02
## Last Updated: 2025-08-27

## Inputs:
## code_dir (character): The directory for all scripts.

## Outputs:
## No output. Set up the computational environment.

## WARNING:
## Using groundhog may disrupt the usage due to permission or restart
## issues. The package may also cause issues with different R versions.
## -----------------------------------------------------------------------------

describe_data <- function(source_dir, dest_dir_fig, dest_dir_table) {
  
  # Load data and select variables for analysis --------------------------------
  
  # Load the analytical dataset
  dat <- read.csv(file.path(here(), source_dir, "dat_analysis.csv"), 
                  header = TRUE, stringsAsFactors = FALSE)
  
  # Create five-star reproducibility rating system -------------------------
  
  cat("Creating five-star reproducibility ratings...\n")
  
  # Initialize rating column
  dat$reproducibility_rating <- 0
  
  # Check if required variables exist (handle both hyphen and period formats)
  required_vars <- c("dr.1", "cr.1", "dr.2", "dr.f.1", "cr.a.2", "cr.i.1", 
                     "cr.f.1", "dr.r.2", "dr.i.1", "cr.i.3", "cr.i.4")
  
  # Alternative names if hyphens are used in original data
  alt_vars <- c("dr-1", "cr-1", "dr-2", "dr-f-1", "cr-a-2", "cr-i-1", 
                "cr-f-1", "dr-r-2", "dr-i-1", "cr-i-3", "cr-i-4")
  
  # Function to get variable value (handles both naming conventions)
  get_var_value <- function(data, var_name, alt_name) {
    if (var_name %in% names(data)) {
      return(data[[var_name]])
    } else if (alt_name %in% names(data)) {
      return(data[[alt_name]])
    } else {
      return(rep(NA, nrow(data)))
    }
  }
  
  # Get values for all required variables
  dr_1 <- get_var_value(dat, "dr.1", "dr-1")
  cr_1 <- get_var_value(dat, "cr.1", "cr-1")
  dr_2 <- get_var_value(dat, "dr.2", "dr-2")
  dr_f_1 <- get_var_value(dat, "dr.f.1", "dr-f-1")
  cr_a_2 <- get_var_value(dat, "cr.a.2", "cr-a-2")
  cr_i_1 <- get_var_value(dat, "cr.i.1", "cr-i-1")
  cr_f_1 <- get_var_value(dat, "cr.f.1", "cr-f-1")
  dr_r_2 <- get_var_value(dat, "dr.r.2", "dr-r-2")
  dr_i_1 <- get_var_value(dat, "dr.i.1", "dr-i-1")
  cr_i_3 <- get_var_value(dat, "cr.i.3", "cr-i-3")
  cr_i_4 <- get_var_value(dat, "cr.i.4", "cr-i-4")
  
  # Apply rating system
  for (i in 1:nrow(dat)) {
    
    # One-star: dr-1 = YES AND cr-1 = YES
    if (!is.na(dr_1[i]) && !is.na(cr_1[i]) && 
        dr_1[i] == "Yes" && cr_1[i] == "Yes") {
      dat$reproducibility_rating[i] <- 1
      
      # Two-star: One-star + dr-2 = YES
      if (!is.na(dr_2[i]) && dr_2[i] == "Yes") {
        dat$reproducibility_rating[i] <- 2
        
        # Three-star: Two-star + dr-f-1 = YES AND cr-a-2 = YES AND cr-i-1 = YES AND cr-f-1 = YES AND dr-r-2 = YES
        if (!is.na(dr_f_1[i]) && dr_f_1[i] == "Yes" &&
            !is.na(cr_a_2[i]) && cr_a_2[i] == "Yes" &&
            !is.na(cr_i_1[i]) && cr_i_1[i] == "Yes" &&
            !is.na(cr_f_1[i]) && cr_f_1[i] == "Yes" &&
            !is.na(dr_r_2[i]) && dr_r_2[i] == "Yes") {
          dat$reproducibility_rating[i] <- 3
          
          # Four-star: Three-star + dr-i-1 = YES
          if (!is.na(dr_i_1[i]) && dr_i_1[i] == "Yes") {
            dat$reproducibility_rating[i] <- 4
            
            # Five-star: Four-star + cr-i-3 = YES AND cr-i-4 = YES
            if (!is.na(cr_i_3[i]) && cr_i_3[i] == "Yes" &&
                !is.na(cr_i_4[i]) && cr_i_4[i] == "Yes") {
              dat$reproducibility_rating[i] <- 5
            }
          }
        }
      }
    }
  }
  
  # Save the rated dataset
  write.csv(dat, file.path(here(source_dir), "dat_analysis_rated.csv"), row.names = FALSE)
  
  # Print rating distribution
  rating_table <- table(dat$reproducibility_rating, useNA = "ifany")
  cat("\nReproducibility Rating Distribution:\n")
  for (rating in names(rating_table)) {
    count <- rating_table[rating]
    pct <- round((count / nrow(dat)) * 100, 1)
    if (rating == "0") {
      cat("0-star (No rating):", count, "(", pct, "%)\n")
    } else {
      cat(rating, "-star:", count, "(", pct, "%)\n")
    }
  }
  cat("\n")
  
  # Initialize results list
  results <- list()
  
  # Set up variable exclusion lists (now including reproducibility_rating in analysis)
  exclude_vars <- c("progress", "duration", "qualtrics_id", "ID", "doi", 
                    "comments", "cr.3", "cr.3.text", "cr.4.text", "cr.1.2", 
                    "cr.f.1.1", "dr.1.1", "dr.1.2.1", "dr.2.1", 
                    "dr.f.1.1", "dr.r.1.1")
  
  # Get all variable names and apply exclusion (reproducibility_rating will be included)
  all_vars <- names(dat)
  summary_vars <- all_vars[!all_vars %in% exclude_vars]
  
  # Function to create summary statistics for a dataset
  create_summary_stats <- function(data_subset, subset_name = "") {
    
    # Initialize summary table
    summary_table <- data.frame(
      Variable = character(0),
      Response = character(0),
      Count = numeric(0),
      Percent_of_Valid = numeric(0),
      Percent_of_Total = numeric(0),
      N_Valid = numeric(0),
      N_Missing = numeric(0),
      Percent_Missing = numeric(0),
      stringsAsFactors = FALSE
    )
    
    # Process each variable
    for (var in summary_vars) {
      if (var %in% names(data_subset)) {
        # Get the variable data
        var_data <- data_subset[[var]]
        
        # Calculate basic statistics
        n_total <- length(var_data)
        n_valid <- sum(!is.na(var_data) & var_data != "" & var_data != "NA")
        n_missing <- n_total - n_valid
        pct_missing <- round((n_missing / n_total) * 100, 1)
        
        # For categorical variables, get frequency counts
        if (n_valid > 0) {
          # Clean the data (remove NA, empty strings, "NA" strings)
          clean_data <- var_data[!is.na(var_data) & var_data != "" & var_data != "NA"]
          
          if (length(clean_data) > 0) {
            # Get frequency table
            freq_table <- table(clean_data)
            
            # Create summary rows for each response category
            for (response in names(freq_table)) {
              count <- as.numeric(freq_table[response])
              pct_of_valid <- round((count / n_valid) * 100, 1)
              pct_of_total <- round((count / n_total) * 100, 1)
              
              summary_row <- data.frame(
                Variable = var,
                Response = response,
                Count = count,
                Percent_of_Valid = pct_of_valid,
                Percent_of_Total = pct_of_total,
                N_Valid = n_valid,
                N_Missing = n_missing,
                Percent_Missing = pct_missing,
                stringsAsFactors = FALSE
              )
              summary_table <- rbind(summary_table, summary_row)
            }
          }
          
          # Add missing data row if there are missing values
          if (n_missing > 0) {
            missing_row <- data.frame(
              Variable = var,
              Response = "Missing/NA",
              Count = n_missing,
              Percent_of_Valid = NA,
              Percent_of_Total = round((n_missing / n_total) * 100, 1),
              N_Valid = n_valid,
              N_Missing = n_missing,
              Percent_Missing = pct_missing,
              stringsAsFactors = FALSE
            )
            summary_table <- rbind(summary_table, missing_row)
          }
        } else {
          # All data is missing
          missing_row <- data.frame(
            Variable = var,
            Response = "All Missing",
            Count = n_missing,
            Percent_of_Valid = NA,
            Percent_of_Total = 100.0,
            N_Valid = 0,
            N_Missing = n_missing,
            Percent_Missing = 100.0,
            stringsAsFactors = FALSE
          )
          summary_table <- rbind(summary_table, missing_row)
        }
      }
    }
    
    return(summary_table)
  }
  
  # Function to create overview table from summary statistics
  create_overview_table <- function(summary_table) {
    if (nrow(summary_table) > 0 && "Variable" %in% names(summary_table)) {
      overview_table <- summary_table %>%
        group_by(Variable) %>%
        summarise(
          N_Valid = first(N_Valid),
          N_Missing = first(N_Missing),
          Percent_Missing = first(Percent_Missing),
          N_Categories = n() - ifelse(any(Response == "Missing/NA"), 1, 0),
          Most_Common = Response[which.max(ifelse(Response == "Missing/NA", 0, Count))],
          Most_Common_Pct = max(ifelse(Response == "Missing/NA", 0, Percent_of_Valid)),
          .groups = 'drop'
        )
    } else {
      overview_table <- data.frame(
        Variable = character(0),
        N_Valid = numeric(0),
        N_Missing = numeric(0),
        Percent_Missing = numeric(0),
        N_Categories = numeric(0),
        Most_Common = character(0),
        Most_Common_Pct = numeric(0),
        stringsAsFactors = FALSE
      )
    }
    return(overview_table)
  }
  
  # Calculate summary statistics -----------------------------------------------
  
  # Create complete dataset analysis
  cat("Creating summary statistics for complete dataset...\n")
  summary_table_all <- create_summary_stats(dat, "all")
  overview_table_all <- create_overview_table(summary_table_all)
  
  # Save complete dataset tables
  write.csv(summary_table_all, file.path(here(dest_dir_table), "summary_statistics_all.csv"), row.names = FALSE)
  write.csv(overview_table_all, file.path(here(dest_dir_table), "variable_overview_all.csv"), row.names = FALSE)
  
  results$summary_table_all <- summary_table_all
  results$overview_table_all <- overview_table_all
  
  # Create pre-policy analysis
  if ("pre.post" %in% names(dat)) {
    dat_pre <- dat[dat$pre.post == "pre-policy" & !is.na(dat$pre.post), ]
    cat("Creating summary statistics for pre-policy period (n =", nrow(dat_pre), ")...\n")
    
    if (nrow(dat_pre) > 0) {
      summary_table_pre <- create_summary_stats(dat_pre, "pre")
      overview_table_pre <- create_overview_table(summary_table_pre)
      
      # Save pre-policy tables
      write.csv(summary_table_pre, file.path(here(dest_dir_table), "summary_statistics_pre.csv"), row.names = FALSE)
      write.csv(overview_table_pre, file.path(here(dest_dir_table), "variable_overview_pre.csv"), row.names = FALSE)
      
      results$summary_table_pre <- summary_table_pre
      results$overview_table_pre <- overview_table_pre
    } else {
      cat("Warning: No pre-policy data found.\n")
    }
    
    # Create post-policy analysis
    dat_post <- dat[dat$pre.post == "post-policy" & !is.na(dat$pre.post), ]
    cat("Creating summary statistics for post-policy period (n =", nrow(dat_post), ")...\n")
    
    if (nrow(dat_post) > 0) {
      summary_table_post <- create_summary_stats(dat_post, "post")
      overview_table_post <- create_overview_table(summary_table_post)
      
      # Save post-policy tables
      write.csv(summary_table_post, file.path(here(dest_dir_table), "summary_statistics_post.csv"), row.names = FALSE)
      write.csv(overview_table_post, file.path(here(dest_dir_table), "variable_overview_post.csv"), row.names = FALSE)
      
      results$summary_table_post <- summary_table_post
      results$overview_table_post <- overview_table_post
    } else {
      cat("Warning: No post-policy data found.\n")
    }
  } else {
    cat("Warning: pre.post variable not found in dataset.\n")
  }
  
  # Create year-specific analyses
  if ("year_after_policy" %in% names(dat)) {
    # Get unique years (excluding NA)
    unique_years <- unique(dat$year_after_policy)
    unique_years <- unique_years[!is.na(unique_years) & unique_years != "NA"]
    unique_years <- sort(unique_years)
    
    cat("\nCreating year-specific summary statistics...\n")
    
    for (year in unique_years) {
      # Filter data for this year
      dat_year <- dat[dat$year_after_policy == year & !is.na(dat$year_after_policy) & dat$year_after_policy != "NA", ]
      
      cat("Creating summary statistics for year", year, "after policy (n =", nrow(dat_year), ")...\n")
      
      if (nrow(dat_year) > 0) {
        # Create summary statistics
        summary_table_year <- create_summary_stats(dat_year, paste0("year_", year))
        overview_table_year <- create_overview_table(summary_table_year)
        
        # Save year-specific tables
        year_suffix <- paste0("_year", year)
        write.csv(summary_table_year, 
                  file.path(here(dest_dir_table), paste0("summary_statistics", year_suffix, ".csv")), 
                  row.names = FALSE)
        write.csv(overview_table_year, 
                  file.path(here(dest_dir_table), paste0("variable_overview", year_suffix, ".csv")), 
                  row.names = FALSE)
        
        # Store in results
        results[[paste0("summary_table", year_suffix)]] <- summary_table_year
        results[[paste0("overview_table", year_suffix)]] <- overview_table_year
      } else {
        cat("Warning: No data found for year", year, "after policy.\n")
      }
    }
    
    # Print summary of years processed
    cat("\nProcessed years after policy:", paste(unique_years, collapse = ", "), "\n")
    
  } else {
    cat("Warning: year_after_policy variable not found in dataset.\n")
  }
  
  cat("\nSummary statistics completed!\n")
  cat("Files saved to:", dest_dir_table, "\n")
  cat("Complete dataset:\n")
  cat("- summary_statistics_all.csv & variable_overview_all.csv\n")
  cat("Pre/Post policy:\n")
  cat("- summary_statistics_pre.csv & variable_overview_pre.csv\n")
  cat("- summary_statistics_post.csv & variable_overview_post.csv\n")
  if ("year_after_policy" %in% names(dat)) {
    cat("Year-specific files:\n")
    for (year in unique_years) {
      cat("- summary_statistics_year", year, ".csv & variable_overview_year", year, ".csv\n")
    }
  }
  
  return(results)
}