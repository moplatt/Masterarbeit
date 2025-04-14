



# Load  libraries
library(dplyr)
library(ggplot2)
library(gridGraphics)

# # # #    SCHNEEHÖHE  ->    SWE     # # # #

# Load required libraries

rm(list = ls())
setwd("L:/T/95_FG_ENTW_PRJ/30_BAU/2022-2025_Langfristprognose_YETI")

seasons <- c("22-23", "23-24")

for (season in seasons) {
  
  # Set the input and output paths for each season
  INPUT_PFAD <- paste0("Pointdata/data_Winter_", season, "/stations")
  output_folder <- paste0("Pointdata/data_Winter_", season)
  output_file <- paste0(output_folder, "/SWE_stations.csv")
  
  weisssee_files <- list(
    "WS_Schneehöhe_HS" = "1_107_Weißsee_Fagge_Schneehöhe_HS_TagMittel.csv",
    "WS_RHO_010" = "1_107_Weißsee_Fagge_Schneedichte_RHO.010_TagMittel.csv",
    "WS_RHO_030" = "1_107_Weißsee_Fagge_Schneedichte_RHO.030_TagMittel.csv",
    "WS_RHO_050" = "1_107_Weißsee_Fagge_Schneedichte_RHO.050_TagMittel.csv"
  )
  
  erfurter_files <- list(
    "EH_SWE_Kissen" = "1_022_ErfurterHütte_Achensee_SWE.csv"
  )
  
  kuehtai_files <- list(
    "KK_HS" = "1_055_KraftwerkKühtai_Längentalbach_Schneehöhe_HS_TagMittel.csv",
    "KK_Dichte" = "1_755_Schneeforschungsstation_Längentalbach_Schneedichte_RHO_TagMittel.csv",
    "KK_SWE_Kissen" = "1_755_Schneeforschungsstation_Längentalbach_Schneewasserwert_SW.Schneekissen_TagMittel.csv"
  )
  
  ### WEISSSEE ----------------------------------
  weisssee_data <- list()
  
  # Loop over files and load them into separate dataframes
  for (var_name in names(weisssee_files)) {
    file_path <- file.path(INPUT_PFAD, weisssee_files[[var_name]])
    
    # Read CSV, skipping first 12 lines, no headers, keeping first 3 columns
    df <- read.csv(file_path, skip = 12, header = FALSE, sep = ";")[, c(1,3)]
    
      # Assign meaningful column names
    colnames(df) <- c("Date", var_name)
    
    # Change the Date format from d.m.y to %Y.%m.%d
    df$Date <- format(as.Date(df$Date, format = "%d.%m.%Y"), "%Y.%m.%d")
    df[[var_name]] <- as.numeric(gsub(",", ".", df[[var_name]]))
    df[[var_name]] <- as.numeric(df[[var_name]])
    # Store in list
    weisssee_data[[var_name]] <- df
  }
  
  # Merge all dataframes by Date
  ws <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), weisssee_data)
  
  ws$Date <- as.Date(ws$Date, format = "%Y.%m.%d")
  
  # Compute SWE_Weisssee in mm based on given conditions
  ws$SWE_Weisssee_calc <- with(ws, ifelse(
    !is.na(WS_Schneehöhe_HS) & WS_Schneehöhe_HS < 50 & !is.na(WS_RHO_010), 
    (WS_Schneehöhe_HS * WS_RHO_010) / 100,  # Convert to mm
    ifelse(!is.na(WS_Schneehöhe_HS) & WS_Schneehöhe_HS >= 80 & !is.na(WS_RHO_050) & !is.na(WS_RHO_030) & !is.na(WS_RHO_010), 
           (WS_Schneehöhe_HS * (WS_RHO_050 + WS_RHO_030 + WS_RHO_010)) / 3 / 100,  # Convert to mm
           ifelse(!is.na(WS_Schneehöhe_HS) & WS_Schneehöhe_HS >= 50 & WS_Schneehöhe_HS < 80 & !is.na(WS_RHO_030) & !is.na(WS_RHO_010), 
                  (WS_Schneehöhe_HS * (WS_RHO_030 + WS_RHO_010)) / 2 / 100, NA  # Convert to mm
           )
    )
  ))
  
  ### KUEHTAI -----------------------------------
  
  kuehtai_data <- list()
  
  # Loop over files and load them into separate dataframes
  for (var_name in names(kuehtai_files)) {
    file_path <- file.path(INPUT_PFAD, kuehtai_files[[var_name]])
    
    # Read CSV, skipping first 12 lines, no headers, keeping first 3 columns
    df <- read.csv(file_path, skip = 12, header = FALSE, sep = ";")[, c(1,3)]
    
    # Assign meaningful column names
    colnames(df) <- c("Date", var_name)
    
    # Change the Date format from d.m.y to %Y.%m.%d
    df$Date <- format(as.Date(df$Date, format = "%d.%m.%Y"), "%Y.%m.%d")
    df[[var_name]] <- as.numeric(gsub(",", ".", df[[var_name]]))
    df[[var_name]] <- as.numeric(df[[var_name]])
    # Store in list
    kuehtai_data[[var_name]] <- df
  }
  
  # Merge all dataframes by Date
  kue <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), kuehtai_data)
  
  # Set all negative Dichte values to 0
  kue$KK_Dichte <- ifelse(kue$KK_Dichte < 0, 0, kue$KK_Dichte)
  
  # Compute KK_SWE
  kue$KK_SWE_calc <- with(kue, ifelse(!is.na(KK_HS) & !is.na(KK_Dichte), 
                                 (KK_HS * KK_Dichte) / 100, 
                                 NA))  # Convert to mm
  
  # Convert Date column to Date type
  kue$Date <- as.Date(kue$Date, format="%Y.%m.%d")
  
  # Create the plot
  ggplot(kue, aes(x = Date)) +
    geom_line(aes(y = KK_SWE_calc, color = "KK_SWE_calc"), size = 1) + 
    geom_line(aes(y = KK_SWE_Kissen, color = "KK_SWE_Kissen"), size = 1) + 
    labs(title = "SWE vs. SWE Kissen Over Time", 
         x = "Date", 
         y = "SWE (mm)", 
         color = "Legend") +
    theme_minimal()
  
  ### ERFURTER HUETTE ---------------------------
  
  EH_data <- list()
  
  # Loop over files and load them into separate dataframes if they exist
  for (var_name in names(erfurter_files)) {
    file_path <- file.path(INPUT_PFAD, erfurter_files[[var_name]])
    
    # Check if file exists
    if (file.exists(file_path)) {
      # Read CSV, keeping only required columns
      df <- read.csv(file_path, header = TRUE, sep = ",")[, c(56, 37)]
      
      # Assign meaningful column names
      colnames(df) <- c("Date", var_name)  # Ensure "Date" is assigned correctly
      
      # Remove tab characters from Date column
      df$Date <- gsub("\t", "", df$Date)
      
      # Convert Date column to Date format
      df$Date <- as.Date(df$Date, format = "%Y-%m-%d-%H-%M")  
      
      # Format Date to "YYYY.MM.DD"
      df$Date <- format(df$Date, "%Y.%m.%d")
      
      # Convert variable column to numeric
      df[[var_name]] <- as.numeric(df[[var_name]])
      
      # Store cleaned dataframe in list
      EH_data[[var_name]] <- df
    } else {
      message(paste("File not found:", file_path))
    }
  }
  
  # Merge all dataframes by Date if EH_data is not empty
  if (length(EH_data) > 0) {
    eh <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), EH_data)
    
    # Convert Date column to Date type after merging
    eh$Date <- as.Date(eh$Date, format = "%Y.%m.%d")
  } else {
    message("No valid files found. EH_data is empty. Skipping Erfurter Hütte data.")
    eh <- NULL  # Set eh to NULL so it doesn't cause errors later on
  }
  
  # ------------------------------------ combine all SWE data! -----------------------------------------
  
  str(eh)
  str(kue)
  str(ws)
  
  # Checking if EH data exists before merging
  if (!is.null(eh)) {
    SWE_stations <- merge(eh[, c("Date", "EH_SWE_Kissen")], 
                          kue[, c("Date", "KK_SWE_Kissen", "KK_SWE_calc")], 
                          by = "Date", 
                          all = TRUE)
  } else {
    SWE_stations <- kue[, c("Date", "KK_SWE_Kissen", "KK_SWE_calc")]
  }
  
  SWE_stations <- merge(SWE_stations, 
                        ws[, c("Date", "SWE_Weisssee_calc")], 
                        by = "Date", 
                        all = TRUE)
  
  # View the structure of the new dataframe
  str(SWE_stations)
  
  # Save the dataframe to a CSV file
  write.csv(SWE_stations, file = output_file, row.names = FALSE)
  
  # Confirm that the file is saved
  message("File saved to: ", output_file)
  
}

#   # Melt the data to long format for ggplot
#   library(reshape2)
#   SWE_stations_long <- melt(SWE_stations, id.vars = "Date", 
#                             measure.vars = c("SWE_Weisssee_calc", "KK_SWE_Kissen", "KK_SWE_calc", "EH_SWE_Kissen"),
#                             variable.name = "SWE_Type", value.name = "Value")
#   
#   # Plot the data
#   ggplot(SWE_stations_long, aes(x = Date, y = Value, color = SWE_Type)) +
#     geom_line() +
#     labs(title = "SWE Values Over Time",
#          x = "Date",
#          y = "SWE (mm)",
#          color = "SWE Type") +
#     theme_minimal() +
#     scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week") +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

