## -----------------------------------------------------------------------------
## Script name: create_figure1
## Purpose of script: Create horizontal bar chart of reproducibility criteria compliance
## Author: Peter Kedron
## Date Created: 2025-09-03
## Last Updated: ---

## Inputs:
## source_dir_table (character): The directory containing summary statistics files
## dest_dir_fig (character): The destination directory for figure files

## Outputs:
## Horizontal bar chart saved as PNG file in dest_dir_fig
## Returns the ggplot object for further customization

## -----------------------------------------------------------------------------

create_figure1 <- function(source_dir_table, dest_dir_fig) {
  
  # Load data ----------------------------------------------------------------
  
  # Load the post-policy summary statistics
  summary_post <- read.csv(file.path(here(), source_dir_table, "summary_statistics_post.csv"), 
                           header = TRUE, stringsAsFactors = FALSE)
  
  cat("Loaded summary_statistics_post.csv with", nrow(summary_post), "rows\n")
  
  # Prepare data for visualization -------------------------------------------
  
  # Define the variables in the specified order (top to bottom)
  variables_order <- c("dr.1", "cr.1", "cr.r.1", "dr.2", "dr.f.1", 
                       "cr.a.2", "cr.i.1", "cr.i.2", "cr.f.1", "dr.r.2", 
                       "dr.i.1", "cr.i.3", "cr.i.4")
  
  # Create display labels for the variables
  variable_labels <- c("dr.1" = "Data available",
                       "cr.1" = "Code available", 
                       "cr.r.1" = "Data & code licensed",
                       "dr.2" = "Metadata available",
                       "dr.f.1" = "Data identifier",
                       "cr.a.2" = "Code commented", 
                       "cr.i.1" = "Packages identified",
                       "cr.i.2" = "Package versions",
                       "cr.f.1" = "Code identifier",
                       "dr.r.2" = "Metadata describe attributes",
                       "dr.i.1" = "Metadata formatted",
                       "cr.i.3" = "Computing infrastructure", 
                       "cr.i.4" = "Containerized"
  )
  
  # Define FAIR labels for each variable (in the specified order)
  fair_labels <- c("F, A", "F, A", "R", "I, R", 
                   "F, A", 
                   "A, R", 
                   "I", 
                   "I", 
                   "F, A", 
                   "R", 
                   "I, R", 
                   "I, R", 
                   "I, R")
  
  # Extract data for "Yes" responses and missing values
  chart_data <- data.frame()
  
  for (i in 1:length(variables_order)) {
    var <- variables_order[i]
    
    # Get "Yes" responses
    yes_row <- summary_post[summary_post$Variable == var & summary_post$Response == "Yes", ]
    
    # Get missing data info (use any row for this variable to get N_Missing)
    missing_info <- summary_post[summary_post$Variable == var, ][1, ]
    
    if (nrow(yes_row) > 0) {
      chart_row <- data.frame(
        Variable = var,
        Label = variable_labels[var],
        FAIR = fair_labels[i],
        Yes_Count = yes_row$Count,
        Yes_Percent = yes_row$Percent_of_Valid,
        N_Missing = missing_info$N_Missing,
        stringsAsFactors = FALSE
      )
    } else {
      # Handle case where there are no "Yes" responses
      chart_row <- data.frame(
        Variable = var,
        Label = variable_labels[var],
        FAIR = fair_labels[i],
        Yes_Count = 0,
        Yes_Percent = 0,
        N_Missing = ifelse(is.na(missing_info$N_Missing), 0, missing_info$N_Missing),
        stringsAsFactors = FALSE
      )
    }
    
    chart_data <- rbind(chart_data, chart_row)
  }
  
  # Order data for plotting (reverse order since ggplot plots bottom to top)
  chart_data$Label <- factor(chart_data$Label, levels = rev(variable_labels[variables_order]))
  chart_data$Variable <- factor(chart_data$Variable, levels = rev(variables_order))
  
  cat("Prepared data for", nrow(chart_data), "variables\n")
  
  # Create Tufte-style horizontal bar chart ----------------------------------
  
  cat("Creating horizontal bar chart...\n")
  
  # Create the plot
  p <- ggplot(chart_data, aes(x = Yes_Percent, y = Label)) +
    
    # Horizontal bars
    geom_col(fill = "grey70", width = 0.7, color = "white", size = 0.5) +
    
    # Add percentage labels at the end of bars (adjusted position)
    geom_text(aes(label = paste0(round(Yes_Percent, 1), "%")), 
              hjust = -0.05, vjust = 0.5, size = 3, color = "black") +
    
    # Add variable ID codes to the far left
    geom_text(aes(x = -35, label = Variable), 
              hjust = 0.5, vjust = 0.5, size = 3, color = "grey40") +
    
    # Add FAIR labels 
    geom_text(aes(x = -22, label = FAIR), 
              hjust = 0.5, vjust = 0.5, size = 3, color = "grey40") +
    
    # Add missing value counts
    geom_text(aes(x = -12, label = N_Missing), 
              hjust = 0.5, vjust = 0.5, size = 3, color = "grey40") +
    
    # Tufte-style theme
    theme_minimal() +
    theme(
      # Remove all grid lines (including y-axis lines)
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      # Clean axis
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      
      # Text formatting
      axis.title.x = element_text(size = 10, color = "grey30"),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 9, color = "black", hjust = 1),
      axis.text.x = element_text(size = 8, color = "grey50"),
      
      # Plot area - minimal margins but enough space for labels
      plot.margin = margin(15, 20, 10, 5),
      panel.background = element_blank(),
      plot.background = element_blank()
    ) +
    
    # Labels and scales (removed title)
    labs(
      x = "Percent Yes"
    ) +
    
    # Extend x-axis to accommodate labels and prevent cutoff
    scale_x_continuous(
      limits = c(-42, max(chart_data$Yes_Percent) + 8),
      breaks = seq(0, 100, 20),
      labels = paste0(seq(0, 100, 20), "%"),
      expand = c(0, 0)
    )
  
  # Add column headers for ID, FAIR and Omitted
  p <- p + 
    annotate(
      "text",
      x = -35, y = length(variables_order) + 0.4, 
      label = "ID", 
      hjust = 0.5, vjust = 0.5, 
      size = 3, color = "grey40", fontface = "bold"
    ) +
    annotate(
      "text",
      x = -22, y = length(variables_order) + 0.4, 
      label = "FAIR", 
      hjust = 0.5, vjust = 0.5, 
      size = 3, color = "grey40", fontface = "bold"
    ) +
    annotate(
      "text",
      x = -12, y = length(variables_order) + 0.4, 
      label = "Omit", 
      hjust = 0.5, vjust = 0.5, 
      size = 3, color = "grey40", fontface = "bold"
    )
  
  # Display the plot
  print(p)
  
  # Save the figure ----------------------------------------------------------
  
  output_file <- file.path(here(), dest_dir_fig, "figure1_reproducibility_practices.png")
  
  ggsave(
    filename = output_file,
    plot = p,
    width = 6.5, 
    height = 5, 
    dpi = 300,
    bg = "white"
  )
  
  cat("Figure saved successfully!\n")
  cat("File saved as:", output_file, "\n")
  cat("Dimensions: 6.5 x 5 inches at 300 DPI\n")
  
  return(p)
}