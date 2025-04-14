
# letzte Bearbeitung: 24.03.2025
# von Moritz Plattner

# this script loads all the available preprocessed data for the Meteo Stations. All extracted raster values, all stations data incl. the deltasnow SWE data.
# The data gets processed and plotted over time from Okt - Aug in one plot for each Station. 
# The Plots get saved as .jpg files. The combined data from all stations gets saved as .csv files.

# Load required libraries
library(dplyr)
library(ggplot2)


rm(list = ls())

seasons <- c("2022-23", "2023-24")
season <- "2022-23"

for (season in seasons) { # start loop over seasons
  season_2 <- ifelse(season == "2022-23", "22-23", "23-24")
  
  setwd(paste0("L:/T/95_FG_ENTW_PRJ/30_BAU/2022-2025_Langfristprognose_YETI/Pointdata/data_Winter_", season_2))
  
  ### load deltasnow data ------------------------------------------------
  
  # Set the output path where the generated .csv files are stored
  OUTPUT_PFAD <- paste0("deltasnow")
  
  # Define the station names and their corresponding output filenames
  station_files <- list(
    "Mittelbergferner" = "Mittelbergferner_SWE.csv",
    "Alpeiner Alpl" = "AlpeinerAlpl_SWE.csv",
    "Weisssee" = "Weisssee_SWE.csv",
    "Kraftwerk Kuehtai" = "KraftwerkKuehtai_SWE.csv",
    "Erfurter Huette" = "ErfurterHütte_SWE.csv"
  )
  
  # Initialize a list to store the loaded dataframes
  deltasnow_data <- list()
  
  # Load each file into the list
  for (station_name in names(station_files)) {
    file_path <- file.path(OUTPUT_PFAD, station_files[[station_name]])
    
    print(paste("Loading data for:", station_name, "Season:", season))
    data <- read.csv(file_path)
    data$Station <- station_name  # Add a column for the station name
    deltasnow_data[[station_name]] <- data
  }
  
  # Combine all dataframes into one
  deltasnow_all <- bind_rows(deltasnow_data)
  deltasnow_all <- subset(deltasnow_all, format(as.Date(date), "%m-%d") != "09-30")
  deltasnow_all$Date <- as.Date(deltasnow_all$date, format = "%Y-%m-%d") # convert to date format
  deltasnow_all <- deltasnow_all[, !names(deltasnow_all) %in% "date"]
  
  ### load raster data -------------------------------------------
  
  PD_raster <- read.csv("point_values_raster.csv")
  PD_raster$Date <- gsub("_", "-", PD_raster$Date)  # replace underscores with dashes
  PD_raster$Date <- as.Date(PD_raster$Date, format = "%Y-%m-%d") # convert to date format

  ### load SWE station data --------------------------------------
  
  SWE_stations <- read.csv(paste0("SWE_stations.csv"))
  # SWE_stations$Date <- gsub("_", "-", SWE_stations$Date)
  SWE_stations$Date <- as.Date(SWE_stations$Date, format = "%Y-%m-%d") # convert to date format
  
  # modify Kuehtai and Weisssee separately to merge later
  KK_data <- SWE_stations[, c("Date", "KK_SWE_Kissen", "KK_SWE_calc")]
  Weisssee_data <- SWE_stations[, c("Date", "SWE_Weisssee_calc")]
  
  # Check if the column EH_SWE_Kissen exists in the SWE_stations dataframe
  if ("EH_SWE_Kissen" %in% colnames(SWE_stations)) {
    EH_data <- SWE_stations[, c("Date", "EH_SWE_Kissen")]  # Extract the required columns
  } else {
    message("EH_SWE_Kissen not found in SWE_stations for season:", season)
  }
  
  KK_data$Station <- "Kraftwerk Kuehtai"
  Weisssee_data$Station <- "Weisssee"
  # Check if EH_data exists
  if (exists("EH_data")) {
    # If EH_data exists, add the Station column
    EH_data$Station <- "Erfurter Huette"
  } else {
    # If EH_data does not exist, print a message and proceed without it
    message("EH_data not found for season:", season)
  }
  
  # Rename columns
  colnames(KK_data) <- c("Date", "SWE_Kissen", "SWE_calc", "Station")
  colnames(Weisssee_data) <- c("Date", "SWE_calc", "Station")
  # If EH_data is available, rename its columns
  # If EH_data exists, rename its columns
  if (exists("EH_data")) {
    colnames(EH_data) <- c("Date", "SWE_Kissen", "Station")
  }
  
  
  ### combine all to one file and save as .csv file ---------------------------
  
  # Merge deltasnow_all and PD_raster by 'Date' and 'Station'
  SWE_all <- merge(deltasnow_all[, c("Date", "delta.snow", "Station")], 
                       PD_raster[, c("Date", "Station", "YETI", "SnowGrid", "S1")],
                       by = c("Date", "Station"), 
                       all = TRUE)
  
  # merge Kuehtai and Weisssee
  SWE_all <- merge(SWE_all, KK_data, by = c("Date", "Station"), all = TRUE)
  SWE_all <- merge(SWE_all, Weisssee_data, by = c("Date", "Station"), all = TRUE)
  
  # If EH_data exists, merge it and combine the SWE_Kissen columns
  if (exists("EH_data") && is.data.frame(EH_data)) {
    # Merge EH_data into SWE_all
    SWE_all <- merge(SWE_all, EH_data, by = c("Date", "Station"), all = TRUE)
    
    # Combine the SWE_Kissen columns from KK_data, Weisssee_data, and EH_data
    SWE_all$SWE_Kissen <- ifelse(!is.na(SWE_all$SWE_Kissen.x), SWE_all$SWE_Kissen.x, SWE_all$SWE_Kissen.y)
    
    # Remove the original SWE_Kissen.x and SWE_Kissen.y columns after combining
    SWE_all <- SWE_all[, !grepl("SWE_Kissen.x|SWE_Kissen.y", colnames(SWE_all))]
  }
  
  SWE_all$SWE_calc <- ifelse(!is.na(SWE_all$SWE_calc.x), SWE_all$SWE_calc.x, SWE_all$SWE_calc.y) # combine SWE_calc columns
  SWE_all <- SWE_all[, !grepl("SWE_calc.x|SWE_calc.y", colnames(SWE_all))]
  
  SWE_all$YETI <- as.numeric(SWE_all$YETI)
  write.csv(SWE_all, "SWE_all.csv", row.names = F)
  
  
  
  # ----------------    PLOT all data series over time and save the plots   -----------------------
  
  # Define a custom theme for readability
  custom_theme <- theme_bw() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5), # Title style
      axis.title.x = element_text(size = 14), # X-axis title size
      axis.title.y = element_text(size = 14), # Y-axis title size
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1), # X-axis text
      axis.text.y = element_text(size = 12), # Y-axis text
      legend.title = element_text(size = 16), # Legend title size
      legend.text = element_text(size = 14)   # Legend text size
    )
  
  mittelbergferner_data <- subset(SWE_all, Station == "Mittelbergferner")
  
  # Plot Mittelbergferner and save
  ggplot(mittelbergferner_data, aes(x = Date)) +
    geom_line(aes(y = delta.snow, color = "delta.snow"), size = 1) +
    geom_line(aes(y = YETI, color = "YETI"), size = 1) +
    geom_line(aes(y = SnowGrid, color = "SnowGrid"), size = 1) +
    geom_line(aes(y = S1, color = "S1"), size = 1) +
    labs(title = paste0("SWE for Mittelbergferner - Winter ", season), 
         x = "Datum", 
         y = "SWE [mm]", 
         color = "Legende") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month")+
    ylim(0,1300)+
    custom_theme
  ggsave("plots/Mittelbergferner_SWE.jpg", width = 10, height = 6, dpi = 300)
  
  AA_data <- subset(SWE_all, Station == "Alpeiner Alpl")
  
  # Plot Alpeiner Alpl and save
  ggplot(AA_data, aes(x = Date)) +
    geom_line(aes(y = delta.snow, color = "delta.snow"), size = 1) +
    geom_line(aes(y = YETI, color = "YETI"), size = 1) +
    geom_line(aes(y = SnowGrid, color = "SnowGrid"), size = 1) +
    geom_line(aes(y = S1, color = "S1"), size = 1) +
    labs(title = paste0("SWE for Alpeiner Alpl - Winter ", season), 
         x = "Datum", 
         y = "SWE [mm]", 
         color = "Legende") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month")+
    ylim(0,1300)+
    custom_theme
  ggsave("plots/AlpeinerAlpl_SWE.jpg", width = 10, height = 6, dpi = 300)
  
  WS_data <- subset(SWE_all, Station == "Weisssee")
  
  # Plot Weisssee and save
  ggplot(WS_data, aes(x = Date)) +
    geom_line(aes(y = delta.snow, color = "delta.snow"), size = 1) +
    geom_line(aes(y = YETI, color = "YETI"), size = 1) +
    geom_line(aes(y = SnowGrid, color = "SnowGrid"), size = 1) +
    geom_line(aes(y = S1, color = "S1"), size = 1) +
    geom_line(aes(y = SWE_calc, color = "SWE_calc"), size = 1) +
    labs(title = paste0("SWE Time for Weisssee - Winter ", season), 
         x = "Datum", 
         y = "SWE [mm]", 
         color = "Legende") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month")+
    ylim(0,1300)+
    custom_theme
  ggsave("plots/Weisssee_SWE.jpg", width = 10, height = 6, dpi = 300)
  
  EH_data <- subset(SWE_all, Station == "Erfurter Huette")
  
  # Plot Erfurter Huette
  # Start the ggplot
  plot <- ggplot(EH_data, aes(x = Date)) +
    geom_line(aes(y = delta.snow, color = "delta.snow"), size = 1) +
    geom_line(aes(y = YETI, color = "YETI"), size = 1) +
    geom_line(aes(y = SnowGrid, color = "SnowGrid"), size = 1) +
    geom_line(aes(y = S1, color = "S1"), size = 1) +
    
    labs(title = paste0("SWE for Erfurter Huette - Winter ", season), 
         x = "Datum", 
         y = "SWE [mm]", 
         color = "Legende") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
    ylim(0, 1300) +
    custom_theme
  
  # Add SWE_Kissen if it exists
  if("SWE_Kissen" %in% colnames(EH_data)) {
    plot <- plot + geom_line(aes(y = SWE_Kissen, color = "SWE_Kissen"), size = 1)
  }
  # save the plot
  ggsave("plots/ErfurterHuette_SWE.jpg", plot = plot, width = 10, height = 6, dpi = 300)
  
  KK_data <- subset(SWE_all, Station == "Kraftwerk Kuehtai")
  
  # Plot Kuehtai and save
  ggplot(KK_data, aes(x = Date)) +
    geom_line(aes(y = delta.snow, color = "delta.snow"), size = 1) +
    geom_line(aes(y = YETI, color = "YETI"), size = 1) +
    geom_line(aes(y = SnowGrid, color = "SnowGrid"), size = 1) +
    geom_line(aes(y = S1, color = "S1"), size = 1) +
    geom_line(aes(y = SWE_calc, color = "SWE_calc"), size = 1) +
    geom_line(aes(y = SWE_Kissen, color = "SWE_Kissen"), size = 1) +
    labs(title = paste0("SWE for Kraftwerk Kuehtai - Winter ", season), 
         x = "Datum", 
         y = "SWE [mm]", 
         color = "Legende") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month")+
    ylim(0,1300)+
    custom_theme
  ggsave("plots/KraftwerkKuehtai_SWE.jpg", width = 10, height = 6, dpi = 300)

  message(paste0(season,"processed successfully"))
}
