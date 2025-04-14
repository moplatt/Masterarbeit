# ---------------------------- create plots over time for the results of snowcover raster comparison

#        in this script the .csv files with the comparison statistics for every timestep are imported and can be visualized
#        using a defined function - function create plot -

library(ggplot2)
library(dplyr)
library(tidyr)

setwd("L:/T/95_FG_ENTW_PRJ/30_BAU/2022-2025_Langfristprognose_YETI")
source("R_Scripts/snowcover/function_create plot.R")  # Load the custom function

# Define the seasons
seasons <- list(
  "Winter_22-23" = "winter_2022-23",
  "Winter_23-24" = "winter_2023-24"
)

for (season_name in names(seasons)) {
  
  season_folder <- seasons[[season_name]]
  print(paste("Processing season:", season_name))
  
  # Load data
  df_SG <- read.csv(paste0("Ergebnisse/snowcover/statistics_df/", season_folder, "/statistics_yeti_SG_df.csv"))
  df_SG$Date <- as.Date(df_SG$Date)  # Convert to Date
  
  df_S1 <- read.csv(paste0("Ergebnisse/snowcover/statistics_df/", season_folder, "/statistics_yeti_S1_df.csv"))
  df_S1$Date <- as.Date(df_S1$Date)  # Convert to Date
  
  df_all <- merge(df_S1[, c("Date", "AOI", "cover_S1")], 
                  df_SG[, c("Date", "AOI", "cover_YETI_100m", "cover_SG")], 
                  by = c("Date", "AOI"), 
                  all = TRUE)
  
  # Define custom theme
  custom_theme <- theme_bw() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14)
    )
  
  # AOI List
  AOIs <- c("Achensee", "Kuehtai", "Kaunertal")
  
  # Plot for SnowGrid and Sentinel 1 comparisons
  for (aoi in AOIs) {
    
    data_subset <- subset(df_all, AOI == aoi)
    
    plot <- ggplot(data_subset, aes(x = Date)) +
      geom_line(aes(y = cover_S1, color = "cover_S1"), size = 1) +
      geom_line(aes(y = cover_YETI_100m, color = "cover_YETI"), size = 1) +
      geom_line(aes(y = cover_SG, color = "cover_SG"), size = 1) +
      labs(title = paste0(aoi, " snow cover - ", season_name), 
           x = "Datum", 
           y = "% cover", 
           color = "Legende") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
      custom_theme
    
    # Save plot
    output_file <- paste0("Ergebnisse/snowcover/plots/", aoi, "_cover_", season_folder, ".jpg")
    ggsave(output_file, width = 10, height = 6, dpi = 300)
    
  }
}

print("All plots have been generated and saved successfully.")
