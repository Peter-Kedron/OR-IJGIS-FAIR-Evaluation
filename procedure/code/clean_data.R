## -----------------------------------------------------------------------------
## Script name: clean_data
## Purpose of script: Prepare data for analysis
## Author: Peter Kedron, Zijun Li
## Date Created: 2025-06-19
## Date Updated: 2025-11-08

## Inputs:
## source_dir (character): The directory containing original qualtric .csv
##
## dest_dir (character): The destination directory for saved files

## Outputs:
## Returns a data.frame of cleaned data and a .csv file named "dat_analysis_*".
## -----------------------------------------------------------------------------

clean_data <- function(source_dir, dest_dir){
  
  # Load data, Read the raw CSV output from qualtrics
  dat_qualtrics <- data.frame(read.csv(file.path(here(source_dir),
                                                 sprintf("qualtrics_screener_2025_11_08.csv")
  ),
  header = TRUE)
  )
  
  # Remove unwanted columns by column names
  data <- dat_qualtrics[, !(names(dat_qualtrics) %in% c("StartDate", 
                                                        "EndDate",
                                                        "Status",
                                                        "Finished",
                                                        "IPAddress",
                                                        "RecordedDate",
                                                        "RecipientFirstName",
                                                        "RecipientLastName",
                                                        "RecipientEmail",
                                                        "ExternalReference",
                                                        "LocationLatitude",
                                                        "LocationLongitude",
                                                        "DistributionChannel",
                                                        "UserLanguage",
                                                        "First",
                                                        "Last")
  )
  ]
  
  
  # Rename specific columns----------------------------------
  names(data)[names(data) == "Progress"] <- "progress"
  names(data)[names(data) == "Duration..in.seconds."] <- "duration"
  names(data)[names(data) == "ResponseId"] <- "qualtrics_id"
  names(data)[names(data) == "ID"] <- "id"
  names(data)[names(data) == "DOI"] <- "doi"
  # names(data)[names(data) == "Data_Filter"] <- "d-filter"
  # names(data)[names(data) == "Synth_Data"] <- "d-synth"
  # names(data)[names(data) == "Code_Filter"] <- "c-filter"
  names(data)[names(data) == "Q30"] <- "comments"
  names(data)[names(data) == "SCR.1"] <- "cr-1"
  names(data)[names(data) == "SCR.1.1"] <- "cr-1-1"
  names(data)[names(data) == "SCR.1.2"] <- "cr-1-2"
  names(data)[names(data) == "SCR.3"] <- "cr-3"
  names(data)[names(data) == "SCR.3_5_TEXT"] <- "cr-3-text"
  names(data)[names(data) == "SCR.4"] <- "cr-4"
  names(data)[names(data) == "SCR.4_3_TEXT"] <- "cr-4-text"
  names(data)[names(data) == "DCR.1._1"] <- "cr-f-1"
  names(data)[names(data) == "DCR.1._2"] <- "cr-f-2"
  names(data)[names(data) == "DCR.1.1"] <- "cr-f-1-1"
  names(data)[names(data) == "DCR.2_1"] <- "cr-a-1"
  names(data)[names(data) == "DCR.2_2"] <- "cr-a-2"
  names(data)[names(data) == "DCR.3_1"] <- "cr-i-1"
  names(data)[names(data) == "DCR.3_2"] <- "cr-i-2"
  # Are the available code used in the publication Interoperable? - Publication or code specifies computing infrastructure used
  names(data)[names(data) == "DCR.3_3"] <- "cr-i-3"
  names(data)[names(data) == "DCR.3_4"] <- "cr-i-4"
  names(data)[names(data) == "DCR.4_1"] <- "cr-r-1"
  names(data)[names(data) == "DCR.4_2"] <- "cr-r-2"
  names(data)[names(data) == "DCR.4_3"] <- "cr-r-3"
  names(data)[names(data) == "DCR.4.1"] <- "cr-r-1-1"
  names(data)[names(data) == "SDR.1"] <- "dr-1"
  names(data)[names(data) == "SDR.1.1"] <- "dr-1-1"
  names(data)[names(data) == "SDR.1.2"] <- "dr-1-2"
  names(data)[names(data) == "SDR.1.2_5_TEXT"] <- "dr-1-2-1"
  # Is a metadata provided for the data?
  names(data)[names(data) == "SDR.2"] <- "dr-2"
  names(data)[names(data) == "SDR.2.1"] <- "dr-2-1"
  names(data)[names(data) == "FDR.1_1"] <- "dr-f-1"
  names(data)[names(data) == "FDR.1_2"] <- "dr-f-2"
  names(data)[names(data) == "FDR.1_3"] <- "dr-f-3"
  names(data)[names(data) == "FDR.1.1"] <- "dr-f-1-1"
  names(data)[names(data) == "FDR.2_1"] <- "dr-a-1"
  names(data)[names(data) == "FDR.2_2"] <- "dr-a-2"
  names(data)[names(data) == "FDR.3_1"] <- "dr-i-1"
  names(data)[names(data) == "FDR.3_2"] <- "dr-i-2"
  names(data)[names(data) == "FDR.4_1"] <- "dr-r-1"
  names(data)[names(data) == "FDR.4_2"] <- "dr-r-2"
  names(data)[names(data) == "FDR.4_3"] <- "dr-r-3"
  names(data)[names(data) == "FDR.4.1"] <- "dr-r-1-1"
  
  # Keep only rows where 'id' is not NA and not an empty string
  dat_clean <- data[!(is.na(data$id) | data$id == ""), ]
  
  # Calculate Inter-rater Metrics-----------------------------------------------
  
  # Identify Joint Review records based on ID pattern
  # Joint Review IDs are 9000+ higher than original IDs (e.g., 9122 corresponds to ID 122, 9089 to 89)

  # Find all IDs that are >= 9000
  joint_ids <- dat_clean$id[dat_clean$id >= 9000]
  
  # Calculate corresponding original IDs by subtracting 9000
  original_ids <- joint_ids - 9000
  
  # Create a mapping of Joint Review ID to Original ID
  id_pairs <- data.frame(
    joint_id = joint_ids,
    original_id = original_ids
  )
  
  cat("\nFound", nrow(id_pairs), "records with duplicate ratings for inter-rater reliability\n")
  
  # Define rating columns to analyze
  rating_cols <- c("cr-1", "cr-1-1", "cr-1-2", "cr-3", "cr-4",
                   "cr-f-1", "cr-f-2", "cr-f-1-1", 
                   "cr-a-1", "cr-a-2",
                   "cr-i-1", "cr-i-2", "cr-i-3", "cr-i-4",
                   "cr-r-1", "cr-r-2", "cr-r-3", "cr-r-1-1",
                   "dr-1", "dr-1-1", "dr-1-2", "dr-2", "dr-2-1",
                   "dr-f-1", "dr-f-2", "dr-f-3", "dr-f-1-1",
                   "dr-a-1", "dr-a-2",
                   "dr-i-1", "dr-i-2",
                   "dr-r-1", "dr-r-2", "dr-r-3", "dr-r-1-1")
  
  if (nrow(id_pairs) > 0) {
    # Initialize results storage
    irr_results <- data.frame(
      variable = character(),
      percent_agreement = numeric(),
      cohens_kappa = numeric(),
      n_pairs = integer(),
      stringsAsFactors = FALSE
    )
    
    # Calculate metrics for each rating column
    for (col in rating_cols) {
      if (col %in% colnames(dat_clean)) {
        # Storage for valid rating pairs
        rater1_ratings <- c()
        rater2_ratings <- c()
        
        # Loop through each ID pair
        for (i in 1:nrow(id_pairs)) {
          joint_id <- id_pairs$joint_id[i]
          orig_id <- id_pairs$original_id[i]
          
          # Get the ratings from both raters
          joint_rating <- dat_clean[dat_clean$id == joint_id, col]
          orig_rating <- dat_clean[dat_clean$id == orig_id, col]
          
          # Only include if both ratings exist and are not NA
          if (length(joint_rating) == 1 && length(orig_rating) == 1 &&
              !is.na(joint_rating) && !is.na(orig_rating)) {
            rater1_ratings <- c(rater1_ratings, as.character(orig_rating))
            rater2_ratings <- c(rater2_ratings, as.character(joint_rating))
          }
        }
        
        # Calculate metrics if we have valid pairs
        if (length(rater1_ratings) > 0) {
          # Percent Agreement
          agreements <- rater1_ratings == rater2_ratings
          percent_agree <- sum(agreements, na.rm = TRUE) / length(agreements) * 100
          
          # Cohen's Kappa
          ratings_matrix <- cbind(rater1_ratings, rater2_ratings)
          
          tryCatch({
            kappa_result <- kappa2(ratings_matrix, weight = "unweighted")
            kappa_value <- kappa_result$value
          }, error = function(e) {
            kappa_value <- NA
          })
          
          # Store results
          irr_results <- rbind(irr_results, data.frame(
            variable = col,
            percent_agreement = round(percent_agree, 2),
            cohens_kappa = round(kappa_value, 3),
            n_pairs = length(agreements),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    # Save inter-rater reliability results
    if (nrow(irr_results) > 0) {
      irr_file <- file.path(here(), "results/tables", "inter_rater_reliability.csv")
      write.csv(irr_results, irr_file, row.names = FALSE)
      cat("\nInter-rater reliability metrics calculated for", nrow(id_pairs), "ID pairs\n")
      cat("Results saved to:", irr_file, "\n\n")
      
      # Print summary statistics
      cat("Summary of Inter-rater Reliability:\n")
      cat("Mean Percent Agreement:", round(mean(irr_results$percent_agreement, na.rm = TRUE), 2), "%\n")
      cat("Mean Cohen's Kappa:", round(mean(irr_results$cohens_kappa, na.rm = TRUE), 3), "\n\n")
      
      cat("First 10 variables:\n")
      print(head(irr_results, 10))
    } else {
      cat("\nNo valid rating pairs found for inter-rater reliability analysis\n")
    }
  } else {
    cat("\nNo Joint Review records found for inter-rater reliability analysis\n")
  }
  
  # Implement corrections from team review -------------------------------------
  
  # Replace rows corrected during team review 
  # Dynamically identify all Joint Review IDs based on ID pattern (>= 9000)
  joint_ids_for_remap <- dat_clean$id[dat_clean$id >= 9000]
  original_ids_for_remap <- joint_ids_for_remap - 9000
  
  # Remove the original records (keep only Joint Review versions)
  dat_clean <- subset(dat_clean, !(id %in% original_ids_for_remap))
  
  # Remap Joint Review IDs to original IDs
  dat_clean$id <- mapvalues(dat_clean$id, 
                            from = joint_ids_for_remap, 
                            to = original_ids_for_remap)
  
  # Apply metadata question correction identified during team review
  subsequent_metadata_cols <- c("dr-2-1", "dr-f-3", "dr-a-1", "dr-i-1", "dr-i-2")
  dat_clean <- dat_clean %>%
    mutate(across(all_of(subsequent_metadata_cols),
                  ~ ifelse(`dr-2` == "No", NA, .)))
  
  # Index papers as pre- or post-policy based on Vol & Issue -------------------
  # Load review lists
  review_data <- read_excel(file.path(here(), 
                                      source_dir, 
                                      "IJGIS_article_list.xlsx"), 
                            sheet = "pre_policy_review")
  
  review_data2 <- read_excel(file.path(here(), 
                                       source_dir, 
                                       "IJGIS_article_list.xlsx"), 
                             sheet = "IJGIS")
  
  # Filter IDs where "to review" is "complete"
  pre_policy_ids <- review_data$ID[review_data$`To_Review` == "Complete"]
  
  # Assign the "pre- & post-" labels for matching IDs
  dat_clean$`pre-post` <- NA
  dat_clean$`pre-post`[dat_clean$id %in% pre_policy_ids] <- "pre-policy"
  dat_clean$`pre-post`[is.na(dat_clean$`pre-post`)] <- "post-policy"
  
  # Add year after column
  dat_clean$year_after_policy <- NA
  
  # Filter IDs for year 1 (vol 34 issue 9–12, vol 35 issue 1–8)
  First_ids <- review_data2 %>%
    filter((vol == 34 & issue >= 9 & issue <= 12) |
             (vol == 35 & issue >= 1 & issue <= 8)) %>%
    pull(ID)
  
  # Assign "1" to year_after_policy for matching IDs
  dat_clean$year_after_policy[as.character(dat_clean$id) %in% as.character(First_ids)] <- "1"
  
  # Filter IDs for year 2 (vol 35 issue 9–12, vol 36 issue 1–8)
  Second_ids <- review_data2 %>%
    filter((vol == 35 & issue >= 9 & issue <= 12) |
             (vol == 36 & issue >= 1 & issue <= 8)) %>%
    pull(ID)
  
  # Assign "2" to year_after_policy for matching IDs
  dat_clean$year_after_policy[dat_clean$id %in% Second_ids] <- "2"
  
  # Filter IDs for year 3 (vol 36 issue 9–12, vol 37 issue 1–8)
  Third_ids <- review_data2 %>%
    filter((vol == 36 & issue >= 9 & issue <= 12) |
             (vol == 37 & issue >= 1 & issue <= 8)) %>%
    pull(ID)
  
  # Assign "3" to year_after_policy for matching IDs
  dat_clean$year_after_policy[dat_clean$id %in% Third_ids] <- "3"
  
  # Filter IDs for year 4 (vol 37 issue 9–12, vol 38 issue 1–8)
  Forth_ids <- review_data2 %>%
    filter((vol == 37 & issue >= 9 & issue <= 12) |
             (vol == 38 & issue >= 1 & issue <= 8)) %>%
    pull(ID)
  
  # Assign "4" to year_after_policy for matching IDs
  dat_clean$year_after_policy[dat_clean$id %in% Forth_ids] <- "4"
  
  # Filter IDs for year 5 (vol 38 issue 9–10)
  Fifth_ids <- review_data2 %>%
    filter(vol == 38 & issue >= 9 & issue <= 10) %>%
    pull(ID)
  
  # Assign "5" to year_after_policy for matching IDs
  dat_clean$year_after_policy[dat_clean$id %in% Fifth_ids] <- "5"
  
  
  # Save new CSV as modify id Data------------------------------------------
  f_name <- file.path(here(),dest_dir, "dat_analysis.csv")
  write.csv(dat_clean, f_name, row.names = FALSE)
  
  # Return data.frame for analysis
  return(dat_clean)
}