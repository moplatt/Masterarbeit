
# letzte Bearbeitung: 24.03.2025
# von Moritz Plattner

# # # #    SCHNEEHÖHE  ->    SWE     # # # #

# Load required libraries
library(nixmass)
library(gridGraphics)
library(ggplot2)

rm(list = ls())

# Set the input and output paths
INPUT_PFAD <- "L:/T/95_FG_ENTW_PRJ/30_BAU/2022-2025_Langfristprognose_YETI/Pointdata/data_Winter_23-24"
OUTPUT_PFAD <- "L:/T/95_FG_ENTW_PRJ/30_BAU/2022-2025_Langfristprognose_YETI/Pointdata/data_Winter_23-24/deltasnow"

# Define filenames and their corresponding station names
files_and_names <- list(
  "1_153_Mittelbergferner_Pitze_Schneehöhe_HS_TagMittel.csv" = "Mittelbergferner",
  "1_143_AlpeinerAlpl_Oberbergbach_Schneehöhe_HS_TagMittel.csv" = "AlpeinerAlpl",
  "1_107_Weißsee_Fagge_Schneehöhe_HS_TagMittel.csv" = "Weisssee",
  "1_055_KraftwerkKühtai_Längentalbach_Schneehöhe_HS_TagMittel.csv" = "KraftwerkKuehtai",
  "1_022_ErfurterHütte_Achensee_Schneehöhe_HS_TagMittel.csv" = "ErfurterHütte"
)

# Process each file
for (input_file in names(files_and_names)) {
  station_name <- files_and_names[[input_file]]  # Get the corresponding station name
  
  print(paste("Processing:", station_name))
  
  # Read the data
  data <- read.csv(file.path(INPUT_PFAD, "stations", input_file), header = FALSE, skip = 12, sep = ";")
  
  # Clean and prepare the data
  data <- data[,-2]                    # Remove column 2
  data <- data[,-3:-10]                # Remove columns 3 to 10
  colnames(data) <- c("date", "hs")    # Define column names
  
  # Convert HS data
  data$hs <- as.numeric(gsub(",", ".", data$hs))  # Replace ',' with '.' and convert to numeric
  data$hs <- data$hs / 100                        # Convert mm to meters
  data$hs[data$hs < 0] <- 0                       # Replace negative values with 0
  data$hs[is.na(data$hs)] <- 0                    # Replace NA with 0
  
  # Convert date column to Date format
  data$date <- as.Date(data$date, format = "%d.%m.%Y")
  
  # Add a value of 0 for the date 30.09.2023 if it does not exist
  additional_row <- data.frame(
    date = as.Date("2023-09-30"),
    hs = 0
  )
  data <- rbind(data, additional_row)
  data <- data[order(data$date), ]  # Ensure data is sorted by date
  
  data$date <- as.character(data$date)
  
  # Run the model
  model_output <- nixmass(data, model = "delta.snow", verbose = TRUE)
  
  # Create a dataframe for SWE and date
  swe_dataframe <- data.frame(
    date = as.Date(model_output$date),
    swe = model_output$swe
  )
  swe_dataframe$delta.snow <- round(swe_dataframe$delta.snow,2) # auf zwei Nachkommastellen runden
  
  # Write the output to a CSV file
  output_file <- file.path(OUTPUT_PFAD, paste0(station_name, "_SWE.csv"))
  write.csv(swe_dataframe, output_file, row.names = FALSE)
  
  print(paste("Output written to:", output_file))
}

print("Processing complete for all files.")


plot(swe_dataframe)
