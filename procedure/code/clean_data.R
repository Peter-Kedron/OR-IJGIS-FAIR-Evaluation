## -----------------------------------------------------------------------------
## Script name: clean_data
## Purpose of script: Prepare data for analysis
## Author: Peter Kedron, Zijun Li
## Date Created: 2025-06-19

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
                                                 sprintf("qualtrics_screener_2025_06_19.csv")
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
  names(data)[names(data) == "ID."] <- "id"
  names(data)[names(data) == "DOI"] <- "doi"
  names(data)[names(data) == "Data.Filter."] <- "d_filter"
  names(data)[names(data) == "Synth.data."] <- "d_synth"
  names(data)[names(data) == "Code.Filter."] <- "c_filter"
  names(data)[names(data) == "Q30"] <- "comments"
  names(data)[names(data) == "SCR.1"] <- "cr-1"
  names(data)[names(data) == "SCR.1.1."] <- "cr-1-1	"
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
  names(data)[names(data) == "FDR.4.1."] <- "dr-r-1-1"

  # Keep only rows where 'id' is not NA and not an empty string
  dat_clean <- data[!(is.na(data$ID) | data$ID == ""), ]
  
  # Replace row corrected during team review 
  dat_clean <- subset(dat_clean, !(ID %in% c(122, 143, 162, 268, 335, 366, 378, 415)))
  old_values <- c(9122, 9143, 9162, 9268, 9335, 9366, 9378, 9415)
  new_values <- c(122, 143, 162, 268, 335, 366, 378, 415)
  dat_clean$ID <- mapvalues(dat_clean$ID, from = old_values, to = new_values)
  
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
  dat_clean$`pre-post`[dat_clean$ID %in% pre_policy_ids] <- "pre-policy"
  dat_clean$`pre-post`[is.na(dat_clean$`pre-post`)] <- "post-policy"
  
  # Add year after column
  dat_clean$year_after_policy <- NA
  
  # Filter IDs for year 1 (vol 34 issue 9–12, vol 35 issue 1–8)
  First_ids <- review_data2 %>%
    filter((vol == 34 & issue >= 9 & issue <= 12) |
             (vol == 35 & issue >= 1 & issue <= 8)) %>%
    pull(ID)
  
  # Assign "1" to year_after_policy for matching IDs
  dat_clean$year_after_policy[as.character(dat_clean$ID) %in% as.character(First_ids)] <- "1"
  
  # Filter IDs for year 2 (vol 35 issue 9–12, vol 36 issue 1–8)
  Second_ids <- review_data2 %>%
    filter((vol == 35 & issue >= 9 & issue <= 12) |
             (vol == 36 & issue >= 1 & issue <= 8)) %>%
    pull(ID)
  
  # Assign "2" to year_after_policy for matching IDs
  dat_clean$year_after_policy[dat_clean$ID %in% Second_ids] <- "2"
  
  # Filter IDs for year 3 (vol 36 issue 9–12, vol 37 issue 1–8)
  Third_ids <- review_data2 %>%
    filter((vol == 36 & issue >= 9 & issue <= 12) |
             (vol == 37 & issue >= 1 & issue <= 8)) %>%
    pull(ID)
  
  # Assign "3" to year_after_policy for matching IDs
  dat_clean$year_after_policy[dat_clean$ID %in% Third_ids] <- "3"
  
  # Filter IDs for year 4 (vol 37 issue 9–12, vol 38 issue 1–8)
  Forth_ids <- review_data2 %>%
    filter((vol == 37 & issue >= 9 & issue <= 12) |
             (vol == 38 & issue >= 1 & issue <= 8)) %>%
    pull(ID)
  
  # Assign "4" to year_after_policy for matching IDs
  dat_clean$year_after_policy[dat_clean$ID %in% Forth_ids] <- "4"
  
  # Filter IDs for year 5 (vol 38 issue 9–10)
  Fifth_ids <- review_data2 %>%
    filter(vol == 38 & issue >= 9 & issue <= 10) %>%
    pull(ID)
  
  # Assign "5" to year_after_policy for matching IDs
  dat_clean$year_after_policy[dat_clean$ID %in% Fifth_ids] <- "5"
  
  
  # Save new CSV as modify id Data------------------------------------------
  f_name <- file.path(here(),dest_dir, "dat_analysis.csv")
  write.csv(dat_clean, f_name, row.names = FALSE)
  
  # Return data.frame for analysis
  return(dat_clean)
}
