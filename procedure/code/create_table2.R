## -----------------------------------------------------------------------------
## Script name: create_table2
## Purpose of script: Create reproducibility rating summary table by time period
## Author: Peter Kedron
## Date Created: 2025-07-02
## Last Updated: 2025-08-27

## Inputs:
## source_dir (character): The directory containing the dat_analysis_rated.csv file
## dest_dir_table (character): The destination directory for table files

## Outputs:
## Reproducibility summary table saved as CSV file in dest_dir_table
## Returns a data.frame containing summary statistics

## -----------------------------------------------------------------------------

create_table2 <- function(source_dir, dest_dir_table) {
  
  # Load data ----------------------------------------------------------------
  
  # Load the rated dataset
  dat_rated <- read.csv(file.path(here(), source_dir, "dat_analysis_rated.csv"), 
                        header = TRUE, stringsAsFactors = FALSE)
  
  cat("Loaded dat_analysis_rated.csv with", nrow(dat_rated), "rows and", ncol(dat_rated), "columns\n\n")
  
  # Function to calculate reproducibility statistics for a dataset subset
  calculate_repro_stats <- function(data_subset, period_name) {
    
    # Get reproducibility ratings
    ratings <- data_subset$reproducibility_rating
    
    # Calculate descriptive statistics
    mean_rating <- round(mean(ratings, na.rm = TRUE), 2)
    median_rating <- median(ratings, na.rm = TRUE)
    sd_rating <- round(sd(ratings, na.rm = TRUE), 2)
    
    # Count occurrences of each rating (0-5 stars)
    rating_counts <- table(factor(ratings, levels = 0:5))
    
    # Create result row
    result <- data.frame(
      Time_Period = period_name,
      N = length(ratings),
      Mean = mean_rating,
      Median = median_rating,
      SD = sd_rating,
      Zero_Star = as.numeric(rating_counts[1]),
      One_Star = as.numeric(rating_counts[2]),
      Two_Star = as.numeric(rating_counts[3]),
      Three_Star = as.numeric(rating_counts[4]),
      Four_Star = as.numeric(rating_counts[5]),
      Five_Star = as.numeric(rating_counts[6]),
      stringsAsFactors = FALSE
    )
    
    return(result)
  }
  
  # Create summary table -----------------------------------------------------
  
  cat("Creating reproducibility rating summary by time period...\n")
  
  # Initialize results table
  summary_table <- data.frame()
  
  # 1. Post-policy period
  dat_post <- dat_rated[dat_rated$pre.post == "post-policy" & !is.na(dat_rated$pre.post), ]
  # Filter out NA reproducibility ratings
  dat_post_filtered <- dat_post[!is.na(dat_post$reproducibility_rating), ]
  
  if (nrow(dat_post_filtered) > 0) {
    cat("Processing post-policy period (n =", nrow(dat_post), 
        ", filtered n =", nrow(dat_post_filtered), ")...\n")
    post_stats <- calculate_repro_stats(dat_post_filtered, "Post-policy")
    summary_table <- rbind(summary_table, post_stats)
  } else {
    cat("Warning: No post-policy data found after filtering.\n")
  }
  
  # 2. Pre-policy period
  dat_pre <- dat_rated[dat_rated$pre.post == "pre-policy" & !is.na(dat_rated$pre.post), ]
  # Filter out NA reproducibility ratings
  dat_pre_filtered <- dat_pre[!is.na(dat_pre$reproducibility_rating), ]
  
  if (nrow(dat_pre_filtered) > 0) {
    cat("Processing pre-policy period (n =", nrow(dat_pre), 
        ", filtered n =", nrow(dat_pre_filtered), ")...\n")
    pre_stats <- calculate_repro_stats(dat_pre_filtered, "Pre-policy")
    summary_table <- rbind(summary_table, pre_stats)
  } else {
    cat("Warning: No pre-policy data found after filtering.\n")
  }
  
  # 3. Year-specific analyses (Years 1-5)
  if ("year_after_policy" %in% names(dat_rated)) {
    unique_years <- sort(unique(dat_rated$year_after_policy))
    unique_years <- unique_years[!is.na(unique_years)]
    
    cat("Processing year-specific data for years:", paste(unique_years, collapse = ", "), "\n")
    
    for (year in unique_years) {
      dat_year <- dat_rated[dat_rated$year_after_policy == year & !is.na(dat_rated$year_after_policy), ]
      # Filter out NA reproducibility ratings
      dat_year_filtered <- dat_year[!is.na(dat_year$reproducibility_rating), ]
      
      if (nrow(dat_year_filtered) > 0) {
        cat("Processing year", year, "after policy (n =", nrow(dat_year), 
            ", filtered n =", nrow(dat_year_filtered), ")...\n")
        year_stats <- calculate_repro_stats(dat_year_filtered, paste0("Year ", year))
        summary_table <- rbind(summary_table, year_stats)
      } else {
        cat("Warning: No data found for year", year, "after policy after filtering.\n")
      }
    }
  } else {
    cat("Warning: year_after_policy variable not found in dataset.\n")
  }
  
  # Save and display results -------------------------------------------------
  
  # Save the table
  output_file <- file.path(here(dest_dir_table), "reproducibility_summary_table.csv")
  write.csv(summary_table, output_file, row.names = FALSE)
  
  # Create formatted console output
  cat("\nFormatted Summary Table:\n")
  cat(paste(rep("=", 100), collapse = ""), "\n")
  
  # Print header
  cat(sprintf("%-15s %5s %6s %7s %5s %9s %8s %8s %10s %9s %9s\n",
              "Time_Period", "N", "Mean", "Median", "SD", 
              "0-Star", "1-Star", "2-Star", "3-Star", "4-Star", "5-Star"))
  cat(paste(rep("-", 100), collapse = ""), "\n")
  
  # Print each row
  for (i in 1:nrow(summary_table)) {
    row <- summary_table[i, ]
    cat(sprintf("%-15s %5d %6.2f %7.0f %5.2f %9d %8d %8d %10d %9d %9d\n",
                row$Time_Period, row$N, row$Mean, row$Median, row$SD,
                row$Zero_Star, row$One_Star, row$Two_Star, 
                row$Three_Star, row$Four_Star, row$Five_Star))
  }
  
  cat("\nSummary completed!\n")
  cat("Table saved as:", output_file, "\n")
  
  return(summary_table)
}