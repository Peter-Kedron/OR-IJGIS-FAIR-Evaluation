## -----------------------------------------------------------------------------
## Script name: compare_ratings
## Purpose of script: Statistical comparison of pre-policy and post-policy reproducibility ratings
## Author: Peter Kedron
## Date Created: 2025-09-03
## Last Updated: ---

## Inputs:
## source_dir (character): The directory containing the dat_analysis_rated.csv file
## dest_dir_table (character): The destination directory for table files

## Outputs:
## Statistical comparison results saved as CSV files in dest_dir_table
## Returns a list containing comparison statistics and test results

## -----------------------------------------------------------------------------

compare_ratings <- function(source_dir, dest_dir_table) {
  
  # Load data ----------------------------------------------------------------
  
  # Load the rated dataset
  dat_rated <- read.csv(file.path(here(), source_dir, "dat_analysis_rated.csv"), 
                        header = TRUE, stringsAsFactors = FALSE)
  
  cat("Loaded dat_analysis_rated.csv with", nrow(dat_rated), "rows and", ncol(dat_rated), "columns\n\n")
  
  # Apply the same exclusion criteria as describe_data.R
  # Exclude articles where d_filter == "No" AND c_filter == "No" (these have reproducibility_rating = NA)
  dat_filtered <- dat_rated[!is.na(dat_rated$reproducibility_rating), ]
  
  cat("Original dataset:", nrow(dat_rated), "articles\n")
  cat("After filtering NA ratings (d_filter=No AND c_filter=No excluded):", nrow(dat_filtered), "articles\n")
  
  # Additional check - verify exclusion logic matches describe_data.R
  excluded_articles <- dat_rated[is.na(dat_rated$reproducibility_rating), ]
  if (nrow(excluded_articles) > 0) {
    # Check if these are indeed the d_filter=No AND c_filter=No articles
    excluded_check <- (!is.na(excluded_articles$d_filter) & excluded_articles$d_filter == "No") & 
      (!is.na(excluded_articles$c_filter) & excluded_articles$c_filter == "No")
    cat("Excluded articles verification: ", sum(excluded_check), "out of", nrow(excluded_articles), 
        "excluded articles match d_filter=No AND c_filter=No criteria\n")
  }
  cat("\n")
  
  # Separate pre-policy and post-policy data
  pre_policy <- dat_filtered[dat_filtered$pre.post == "pre-policy" & !is.na(dat_filtered$pre.post), ]
  post_policy <- dat_filtered[dat_filtered$pre.post == "post-policy" & !is.na(dat_filtered$pre.post), ]
  
  cat("=== SAMPLE SIZES ===\n")
  cat("Pre-policy articles:", nrow(pre_policy), "\n")
  cat("Post-policy articles:", nrow(post_policy), "\n\n")
  
  # Initialize results list
  results <- list()
  
  # Extract reproducibility ratings
  pre_ratings <- pre_policy$reproducibility_rating
  post_ratings <- post_policy$reproducibility_rating
  
  # Calculate descriptive statistics -----------------------------------------
  
  cat("=== DESCRIPTIVE STATISTICS ===\n")
  
  # Pre-policy statistics
  pre_stats <- data.frame(
    Period = "Pre-policy",
    N = length(pre_ratings),
    Mean = round(mean(pre_ratings, na.rm = TRUE), 3),
    Median = median(pre_ratings, na.rm = TRUE),
    SD = round(sd(pre_ratings, na.rm = TRUE), 3),
    Min = min(pre_ratings, na.rm = TRUE),
    Max = max(pre_ratings, na.rm = TRUE),
    Q1 = quantile(pre_ratings, 0.25, na.rm = TRUE),
    Q3 = quantile(pre_ratings, 0.75, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Post-policy statistics  
  post_stats <- data.frame(
    Period = "Post-policy",
    N = length(post_ratings),
    Mean = round(mean(post_ratings, na.rm = TRUE), 3),
    Median = median(post_ratings, na.rm = TRUE),
    SD = round(sd(post_ratings, na.rm = TRUE), 3),
    Min = min(post_ratings, na.rm = TRUE),
    Max = max(post_ratings, na.rm = TRUE),
    Q1 = quantile(post_ratings, 0.25, na.rm = TRUE),
    Q3 = quantile(post_ratings, 0.75, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Combine and display
  comparison_stats <- rbind(pre_stats, post_stats)
  print(comparison_stats)
  cat("\n")
  
  results$comparison_stats <- comparison_stats
  
  # Calculate frequency distributions ----------------------------------------
  
  cat("=== FREQUENCY DISTRIBUTIONS ===\n")
  
  # Pre-policy distribution
  cat("Pre-policy Rating Distribution:\n")
  pre_table <- table(pre_ratings)
  pre_prop <- round(prop.table(pre_table) * 100, 1)
  for(i in 1:length(pre_table)) {
    rating <- names(pre_table)[i]
    count <- pre_table[i]
    pct <- pre_prop[i]
    cat(sprintf("%s-star: %d (%.1f%%)\n", rating, count, pct))
  }
  cat("\n")
  
  # Post-policy distribution
  cat("Post-policy Rating Distribution:\n")
  post_table <- table(post_ratings)
  post_prop <- round(prop.table(post_table) * 100, 1)
  for(i in 1:length(post_table)) {
    rating <- names(post_table)[i]
    count <- post_table[i]
    pct <- post_prop[i]
    cat(sprintf("%s-star: %d (%.1f%%)\n", rating, count, pct))
  }
  cat("\n")
  
  results$pre_distribution <- data.frame(Rating = names(pre_table), 
                                         Count = as.numeric(pre_table), 
                                         Percentage = as.numeric(pre_prop),
                                         stringsAsFactors = FALSE)
  results$post_distribution <- data.frame(Rating = names(post_table), 
                                          Count = as.numeric(post_table), 
                                          Percentage = as.numeric(post_prop),
                                          stringsAsFactors = FALSE)
  
  # Perform statistical tests ---------------------------------------------
  
  cat("=== STATISTICAL TESTS ===\n")
  
  # Mann-Whitney U Test (Wilcoxon Rank Sum Test)
  wilcox_result <- wilcox.test(post_ratings, pre_ratings, alternative = "two.sided")
  
  cat("Mann-Whitney U Test (Wilcoxon Rank Sum Test):\n")
  cat("Null hypothesis: No difference in reproducibility ratings between periods\n")
  cat("Alternative hypothesis: Difference exists between periods\n")
  cat(sprintf("W statistic: %.2f\n", wilcox_result$statistic))
  cat(sprintf("p-value: %.6f\n", wilcox_result$p.value))
  cat(sprintf("Significance level: %s\n", ifelse(wilcox_result$p.value < 0.001, "p < 0.001***", 
                                                 ifelse(wilcox_result$p.value < 0.01, "p < 0.01**",
                                                        ifelse(wilcox_result$p.value < 0.05, "p < 0.05*", "Not significant")))))
  cat("\n")
  
  # Effect size (rank-biserial correlation)
  n1 <- length(pre_ratings)
  n2 <- length(post_ratings)
  U <- wilcox_result$statistic
  r <- 1 - (2*U)/(n1*n2)
  cat(sprintf("Effect size (rank-biserial correlation): %.3f\n", r))
  cat("Interpretation: |r| < 0.1 (negligible), 0.1-0.3 (small), 0.3-0.5 (medium), >0.5 (large)\n\n")
  
  results$wilcox_test <- list(
    statistic = wilcox_result$statistic,
    p_value = wilcox_result$p.value,
    effect_size = r
  )
  
  # Chi-square test for independence
  rating_levels <- sort(unique(c(pre_ratings, post_ratings)))
  contingency_table <- matrix(0, nrow = 2, ncol = length(rating_levels))
  rownames(contingency_table) <- c("Pre-policy", "Post-policy")
  colnames(contingency_table) <- paste0(rating_levels, "-star")
  
  for(i in 1:length(rating_levels)) {
    contingency_table[1, i] <- sum(pre_ratings == rating_levels[i])
    contingency_table[2, i] <- sum(post_ratings == rating_levels[i])
  }
  
  cat("Contingency Table:\n")
  print(contingency_table)
  cat("\n")
  
  # Perform chi-square test
  chi_result <- chisq.test(contingency_table)
  cat("Chi-square Test of Independence:\n")
  cat("Null hypothesis: Rating distributions are independent of time period\n")
  cat("Alternative hypothesis: Rating distributions depend on time period\n")
  cat(sprintf("Chi-square statistic: %.3f\n", chi_result$statistic))
  cat(sprintf("Degrees of freedom: %d\n", chi_result$parameter))
  cat(sprintf("p-value: %.6f\n", chi_result$p.value))
  cat(sprintf("Significance level: %s\n", ifelse(chi_result$p.value < 0.001, "p < 0.001***", 
                                                 ifelse(chi_result$p.value < 0.01, "p < 0.01**",
                                                        ifelse(chi_result$p.value < 0.05, "p < 0.05*", "Not significant")))))
  
  # Cramer's V (effect size for chi-square)
  cramers_v <- sqrt(chi_result$statistic / (sum(contingency_table) * (min(nrow(contingency_table), ncol(contingency_table)) - 1)))
  cat(sprintf("Cramer's V (effect size): %.3f\n", cramers_v))
  cat("Interpretation: V < 0.1 (negligible), 0.1-0.3 (small), 0.3-0.5 (medium), >0.5 (large)\n\n")
  
  results$chi_test <- list(
    statistic = chi_result$statistic,
    df = chi_result$parameter,
    p_value = chi_result$p.value,
    cramers_v = cramers_v
  )
  results$contingency_table <- contingency_table
  
  # Calculate improvement metrics ------------------------------------------
  
  cat("=== IMPROVEMENT ANALYSIS ===\n")
  
  # Calculate mean difference
  mean_diff <- mean(post_ratings) - mean(pre_ratings)
  cat(sprintf("Mean difference (Post - Pre): %.3f\n", mean_diff))
  
  # Percentage improvement in mean rating
  pct_improvement <- (mean_diff / mean(pre_ratings)) * 100
  cat(sprintf("Percentage improvement in mean rating: %.1f%%\n", pct_improvement))
  
  # Improvement in high-quality ratings (3+ stars)
  pre_high_quality <- sum(pre_ratings >= 3) / length(pre_ratings) * 100
  post_high_quality <- sum(post_ratings >= 3) / length(post_ratings) * 100
  high_quality_diff <- post_high_quality - pre_high_quality
  
  cat(sprintf("Pre-policy 3+ star ratings: %.1f%%\n", pre_high_quality))
  cat(sprintf("Post-policy 3+ star ratings: %.1f%%\n", post_high_quality))
  cat(sprintf("Improvement in 3+ star ratings: %.1f percentage points\n", high_quality_diff))
  
  # Improvement in 0-star ratings (reduction is good)
  pre_zero_star <- sum(pre_ratings == 0) / length(pre_ratings) * 100
  post_zero_star <- sum(post_ratings == 0) / length(post_ratings) * 100
  zero_star_change <- post_zero_star - pre_zero_star
  
  cat(sprintf("Pre-policy 0-star ratings: %.1f%%\n", pre_zero_star))
  cat(sprintf("Post-policy 0-star ratings: %.1f%%\n", post_zero_star))
  cat(sprintf("Change in 0-star ratings: %.1f percentage points\n", zero_star_change))
  
  # Store improvement metrics
  improvement_metrics <- data.frame(
    Metric = c("Mean_Difference", "Percent_Improvement", "High_Quality_Pre", "High_Quality_Post", 
               "High_Quality_Change", "Zero_Star_Pre", "Zero_Star_Post", "Zero_Star_Change"),
    Value = c(mean_diff, pct_improvement, pre_high_quality, post_high_quality, 
              high_quality_diff, pre_zero_star, post_zero_star, zero_star_change),
    stringsAsFactors = FALSE
  )
  results$improvement_metrics <- improvement_metrics
  
  # Generate summary interpretation ----------------------------------------
  
  cat("\n=== SUMMARY AND INTERPRETATION ===\n")
  cat("Statistical Significance:\n")
  if(wilcox_result$p.value < 0.05) {
    cat("- Mann-Whitney U test shows SIGNIFICANT difference between periods\n")
  } else {
    cat("- Mann-Whitney U test shows NO significant difference between periods\n")
  }
  
  if(chi_result$p.value < 0.05) {
    cat("- Chi-square test shows rating distributions SIGNIFICANTLY differ\n")
  } else {
    cat("- Chi-square test shows rating distributions do NOT significantly differ\n")
  }
  
  cat("\nPractical Significance:\n")
  cat(sprintf("- Mean reproducibility rating improved by %.3f points (%.1f%%)\n", mean_diff, pct_improvement))
  cat(sprintf("- High-quality reproducibility (3+ stars) improved by %.1f percentage points\n", high_quality_diff))
  
  if(mean_diff > 0) {
    cat("- Overall trend indicates IMPROVEMENT in reproducibility practices\n")
  } else if(mean_diff < 0) {
    cat("- Overall trend indicates DECLINE in reproducibility practices\n")
  } else {
    cat("- No change in average reproducibility practices\n")
  }
  
  # Save results to files -------------------------------------------------
  
  write.csv(comparison_stats, file.path(here(), dest_dir_table, "reproducibility_comparison_stats.csv"), row.names = FALSE)
  write.csv(contingency_table, file.path(here(), dest_dir_table, "reproducibility_contingency_table.csv"), row.names = TRUE)
  write.csv(improvement_metrics, file.path(here(), dest_dir_table, "reproducibility_improvement_metrics.csv"), row.names = FALSE)
  
  # Year-by-year analysis for post-policy period ---------------------------
  
  if ("year_after_policy" %in% names(dat_filtered)) {
    cat("\n=== YEAR-BY-YEAR POST-POLICY ANALYSIS ===\n")
    
    # Get unique years
    unique_years <- sort(unique(dat_filtered$year_after_policy))
    unique_years <- unique_years[!is.na(unique_years)]
    
    cat("Analyzing years:", paste(unique_years, collapse = ", "), "\n\n")
    
    # Initialize containers for year-by-year results
    yearly_stats_list <- list()
    yearly_vs_pre_tests <- list()
    progressive_tests <- list()
    
    # Create year-specific datasets
    year_data <- list()
    for (year in unique_years) {
      year_data[[paste0("Year_", year)]] <- dat_filtered[dat_filtered$year_after_policy == year & 
                                                           !is.na(dat_filtered$year_after_policy), ]
    }
    
    # Calculate descriptive statistics for each year
    cat("=== YEARLY DESCRIPTIVE STATISTICS ===\n")
    yearly_comparison_stats <- data.frame()
    
    for (year in unique_years) {
      year_ratings <- year_data[[paste0("Year_", year)]]$reproducibility_rating
      
      year_stats <- data.frame(
        Period = paste0("Year ", year),
        N = length(year_ratings),
        Mean = round(mean(year_ratings, na.rm = TRUE), 3),
        Median = median(year_ratings, na.rm = TRUE),
        SD = round(sd(year_ratings, na.rm = TRUE), 3),
        Min = min(year_ratings, na.rm = TRUE),
        Max = max(year_ratings, na.rm = TRUE),
        Q1 = quantile(year_ratings, 0.25, na.rm = TRUE),
        Q3 = quantile(year_ratings, 0.75, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      
      yearly_comparison_stats <- rbind(yearly_comparison_stats, year_stats)
      yearly_stats_list[[paste0("Year_", year)]] <- year_stats
    }
    
    print(yearly_comparison_stats)
    cat("\n")
    
    # Progressive year-by-year comparisons (Year 1 vs 2, Year 2 vs 3, etc.)
    cat("=== PROGRESSIVE YEAR-BY-YEAR COMPARISONS ===\n")
    
    for (i in 1:(length(unique_years)-1)) {
      year1 <- unique_years[i]
      year2 <- unique_years[i+1]
      
      ratings1 <- year_data[[paste0("Year_", year1)]]$reproducibility_rating
      ratings2 <- year_data[[paste0("Year_", year2)]]$reproducibility_rating
      
      cat(sprintf("Comparing Year %s (n=%d) vs Year %s (n=%d):\n", 
                  year1, length(ratings1), year2, length(ratings2)))
      
      # Wilcoxon test
      wilcox_yearly <- wilcox.test(ratings2, ratings1, alternative = "two.sided")
      
      # Effect size
      n1_year <- length(ratings1)
      n2_year <- length(ratings2)
      U_year <- wilcox_yearly$statistic
      r_year <- 1 - (2*U_year)/(n1_year*n2_year)
      
      cat(sprintf("  Wilcoxon test: W = %.2f, p = %.4f", wilcox_yearly$statistic, wilcox_yearly$p.value))
      if (wilcox_yearly$p.value < 0.001) {
        cat(" ***")
      } else if (wilcox_yearly$p.value < 0.01) {
        cat(" **")
      } else if (wilcox_yearly$p.value < 0.05) {
        cat(" *")
      }
      cat(sprintf(", Effect size (r) = %.3f\n", r_year))
      
      # Mean difference
      mean_diff_year <- mean(ratings2) - mean(ratings1)
      cat(sprintf("  Mean difference: %.3f\n", mean_diff_year))
      
      # Store results
      progressive_tests[[paste0("Year", year1, "_vs_Year", year2)]] <- list(
        comparison = paste0("Year ", year1, " vs Year ", year2),
        wilcox_stat = wilcox_yearly$statistic,
        p_value = wilcox_yearly$p.value,
        effect_size = r_year,
        mean_difference = mean_diff_year
      )
      
      cat("\n")
    }
    
    # Compare each post-policy year to pre-policy period
    cat("=== EACH YEAR COMPARED TO PRE-POLICY ===\n")
    
    for (year in unique_years) {
      year_ratings <- year_data[[paste0("Year_", year)]]$reproducibility_rating
      
      cat(sprintf("Comparing Pre-policy (n=%d) vs Year %s (n=%d):\n", 
                  length(pre_ratings), year, length(year_ratings)))
      
      # Wilcoxon test
      wilcox_vs_pre <- wilcox.test(year_ratings, pre_ratings, alternative = "two.sided")
      
      # Effect size
      n1_pre <- length(pre_ratings)
      n2_year <- length(year_ratings)
      U_vs_pre <- wilcox_vs_pre$statistic
      r_vs_pre <- 1 - (2*U_vs_pre)/(n1_pre*n2_year)
      
      cat(sprintf("  Wilcoxon test: W = %.2f, p = %.4f", wilcox_vs_pre$statistic, wilcox_vs_pre$p.value))
      if (wilcox_vs_pre$p.value < 0.001) {
        cat(" ***")
      } else if (wilcox_vs_pre$p.value < 0.01) {
        cat(" **")
      } else if (wilcox_vs_pre$p.value < 0.05) {
        cat(" *")
      }
      cat(sprintf(", Effect size (r) = %.3f\n", r_vs_pre))
      
      # Mean difference
      mean_diff_vs_pre <- mean(year_ratings) - mean(pre_ratings)
      pct_improvement_vs_pre <- (mean_diff_vs_pre / mean(pre_ratings)) * 100
      
      cat(sprintf("  Mean difference: %.3f (%.1f%% improvement)\n", mean_diff_vs_pre, pct_improvement_vs_pre))
      
      # High-quality ratings comparison
      pre_high_qual <- sum(pre_ratings >= 3) / length(pre_ratings) * 100
      year_high_qual <- sum(year_ratings >= 3) / length(year_ratings) * 100
      high_qual_diff <- year_high_qual - pre_high_qual
      
      cat(sprintf("  3+ star ratings: Pre=%.1f%%, Year %s=%.1f%%, Difference=%.1f pp\n", 
                  pre_high_qual, year, year_high_qual, high_qual_diff))
      
      # Store results
      yearly_vs_pre_tests[[paste0("Year_", year)]] <- list(
        comparison = paste0("Pre-policy vs Year ", year),
        wilcox_stat = wilcox_vs_pre$statistic,
        p_value = wilcox_vs_pre$p.value,
        effect_size = r_vs_pre,
        mean_difference = mean_diff_vs_pre,
        percent_improvement = pct_improvement_vs_pre,
        high_quality_change = high_qual_diff
      )
      
      cat("\n")
    }
    
    # Create summary tables for year-by-year results
    
    # Progressive comparisons summary
    if (length(progressive_tests) > 0) {
      progressive_summary <- data.frame(
        Comparison = sapply(progressive_tests, function(x) x$comparison),
        Wilcox_Statistic = sapply(progressive_tests, function(x) round(x$wilcox_stat, 2)),
        P_Value = sapply(progressive_tests, function(x) round(x$p_value, 4)),
        Effect_Size = sapply(progressive_tests, function(x) round(x$effect_size, 3)),
        Mean_Difference = sapply(progressive_tests, function(x) round(x$mean_difference, 3)),
        stringsAsFactors = FALSE
      )
      
      write.csv(progressive_summary, file.path(here(), dest_dir_table, "progressive_year_comparisons.csv"), row.names = FALSE)
    }
    
    # Year vs pre-policy summary
    if (length(yearly_vs_pre_tests) > 0) {
      yearly_vs_pre_summary <- data.frame(
        Year = sapply(yearly_vs_pre_tests, function(x) gsub("Pre-policy vs ", "", x$comparison)),
        Wilcox_Statistic = sapply(yearly_vs_pre_tests, function(x) round(x$wilcox_stat, 2)),
        P_Value = sapply(yearly_vs_pre_tests, function(x) round(x$p_value, 4)),
        Effect_Size = sapply(yearly_vs_pre_tests, function(x) round(x$effect_size, 3)),
        Mean_Difference = sapply(yearly_vs_pre_tests, function(x) round(x$mean_difference, 3)),
        Percent_Improvement = sapply(yearly_vs_pre_tests, function(x) round(x$percent_improvement, 1)),
        High_Quality_Change = sapply(yearly_vs_pre_tests, function(x) round(x$high_quality_change, 1)),
        stringsAsFactors = FALSE
      )
      
      write.csv(yearly_vs_pre_summary, file.path(here(), dest_dir_table, "yearly_vs_pre_policy_comparisons.csv"), row.names = FALSE)
    }
    
    # Save yearly descriptive stats
    write.csv(yearly_comparison_stats, file.path(here(), dest_dir_table, "yearly_descriptive_statistics.csv"), row.names = FALSE)
    
    # Store in results
    results$yearly_stats <- yearly_stats_list
    results$progressive_comparisons <- progressive_tests
    results$yearly_vs_pre_comparisons <- yearly_vs_pre_tests
    results$yearly_descriptive_stats <- yearly_comparison_stats
    
    cat("Year-by-year analysis completed!\n")
    cat("Additional files saved:\n")
    cat("- yearly_descriptive_statistics.csv\n")
    cat("- progressive_year_comparisons.csv\n") 
    cat("- yearly_vs_pre_policy_comparisons.csv\n")
    
  } else {
    cat("Warning: year_after_policy variable not found for year-by-year analysis.\n")
  }
  
  cat("\nComparison completed!\n")
  cat("Results saved to:", dest_dir_table, "\n")
  cat("- reproducibility_comparison_stats.csv\n")
  cat("- reproducibility_contingency_table.csv\n")
  cat("- reproducibility_improvement_metrics.csv\n")
  
  return(results)
}