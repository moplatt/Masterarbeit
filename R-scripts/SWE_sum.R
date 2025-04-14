
# letzte Bearbeitung: 24.03.2025
# von Moritz Plattner

library(raster)
library(sf)
library(ggplot2)
library(terra)

setwd("L:/T/95_FG_ENTW_PRJ/30_BAU/2022-2025_Langfristprognose_YETI")
rm(list = ls())

# Define seasons
seasons <- list(
  "Winter_22-23" = list(start_date = as.Date("2022-10-01"), end_date = as.Date("2023-08-31")),
  "Winter_23-24" = list(start_date = as.Date("2023-10-01"), end_date = as.Date("2024-08-31"))
)

# --------------------------------------------------
Achensee <- vect("Daten/shapefiles/HOPI_Achensee.shp")
Kuehtai <- vect("Daten/shapefiles/Kuehtai_2.shp")
Kaunertal <- vect("Daten/shapefiles/Kaunertal.shp")

original_crs <- 31251  # Austria GK WEST (EPSG:31255)
target_crs <- 32632  # UTM Zone 32N (EPSG:32632)

# Function to calculate the mean of SWE for the AOIs
calculate_avg_swe <- function(raster_data, aoi) {
  if (is.null(raster_data) || ncell(raster_data) == 0) {
    return(NA_real_)
  }
  
  # Extract values within AOI (returns a list of numeric vectors)
  values_list <- raster::extract(raster_data, aoi)
  values <- unlist(values_list)
  
  # Filter out NA values
  valid_values <- values[!is.na(values)]
  
  if (length(valid_values) == 0) {
    return(NA_real_)
  }
  
  # SWE is in mm, so just return average per m²
  avg_swe_mm_per_m2 <- mean(valid_values)  # mm per m²
  
  return(avg_swe_mm_per_m2)
}




for (season_name in names(seasons)) {
  
  print(paste("Processing season:", season_name))
  
  start_date <- seasons[[season_name]]$start_date
  end_date <- seasons[[season_name]]$end_date
  output_folder <- paste0("Ergebnisse/SWE_sum/data_", season_name)
  output_file <- paste0(output_folder, "/swe_sum.csv")
  
  # Create sequence of dates
  dates <- format(seq(from = start_date, to = end_date, by = "day"), "%Y_%m_%d")
  
  all_data <- list()
  
  for (date in dates) {
    
    print(date)
    
    date2 <- gsub("_", "-", date)
    date3 <- gsub("_", "", date)
    
    # Construct file paths dynamically based on season
    year_range <- ifelse(season_name == "Winter_22-23", "2022-10-01_bis_2023-08-31", "2023-10-01_bis_2024-08-31")
    data_name_season <- ifelse(season_name == "Winter_22-23", "2022-23", "2023-24")
    
    # Construct file paths
    file_paths <- list(
      YETI_Achensee = paste0("Daten/Nachrechnung_", data_name_season, "/", year_range, "/01_ses_nordost/SES_SWE_", date2, "T0600.tif"),
      YETI_zentral  = paste0("Daten/Nachrechnung_", data_name_season, "/", year_range, "/03_ses_zentralraum/SES_SWE_", date2, "T0600.tif"),
      SG = paste0("Daten/snowgrid/SWE_2022-09_bis_2024-11/tif_files/", date3, "_SNOWGRID_SWE.tif"),
      sentinel1 = paste0("Daten/S1_SWE_Tirol/raster_files/SWE_", date3, "_500m.tif")
    )
    
    # Initialize variables for raster layers
    rasters <- list()
    
    # Load rasters if they exist
    for (name in names(file_paths)) {
      if (file.exists(file_paths[[name]])) {
        rasters[[name]] <- raster(file_paths[[name]])
      } else {
        print(paste("File does not exist, skipping:", file_paths[[name]]))
      }
    }
    
    # Check if at least one raster is loaded
    if (length(rasters) == 0) {
      print(paste("No rasters available for date:", date, "- skipping."))
      next
    }
    
    # Mosaikieren der YETI-Raster
    rasters_YETI <- c()
    
    if (!is.null(rasters$YETI_Achensee)) {
      rasters_YETI <- append(rasters_YETI, rasters$YETI_Achensee)
    }
    if (!is.null(rasters$YETI_zentral)) {
      rasters_YETI <- append(rasters_YETI, rasters$YETI_zentral)
    }
    
    if (length(rasters_YETI) == 2) {
      YETI <- mosaic(rasters_YETI[[1]], rasters_YETI[[2]], fun = mean)  # Mittelwert bei Überlappung
    } else if (length(rasters_YETI) == 1) {
      YETI <- rasters_YETI[[1]]
    } else {
      YETI <- NULL
    }
    
    # Set projection for YETI if it exists
    if (!is.null(YETI)) {
      projection(YETI) <- projection(Kuehtai)
      rasters$YETI <- YETI
    }
    
    # Reproject SG if it exists
    if (!is.null(rasters$SG)) {
      rasters$SG <- projectRaster(rasters$SG, crs = crs(Achensee))
    }
    
    # Raster in ein terra-Raster umwandeln
    SG <- if (!is.null(rasters$SG)) rast(rasters$SG)
    S1 <- if (!is.null(rasters$sentinel1)) rast(rasters$sentinel1)
    YETI <- if (!is.null(rasters$YETI)) rast(rasters$YETI)
    
    if (is.null(SG) && is.null(S1) && is.null(YETI)) {
      print(paste("No valid rasters available for date:", date, "- skipping calculations."))
      next
    }
    
    # Calculate sum of SWE for each AOI (Achensee, Kuehtai, Kaunertal)
    sum_YETI_Achensee <- if (!is.null(YETI)) calculate_avg_swe(YETI, Achensee)
    sum_YETI_Kuehtai <- if (!is.null(YETI)) calculate_avg_swe(YETI, Kuehtai)
    sum_YETI_Kaunertal <- if (!is.null(YETI)) calculate_avg_swe(YETI, Kaunertal)
    
    sum_SG_Achensee <- if (!is.null(SG)) calculate_avg_swe(SG, Achensee)
    sum_SG_Kuehtai <- if (!is.null(SG)) calculate_avg_swe(SG, Kuehtai)
    sum_SG_Kaunertal <- if (!is.null(SG)) calculate_avg_swe(SG, Kaunertal)
    
    sum_S1_Achensee <- if (!is.null(S1)) calculate_avg_swe(S1, Achensee)
    sum_S1_Kuehtai <- if (!is.null(S1)) calculate_avg_swe(S1, Kuehtai)
    sum_S1_Kaunertal <- if (!is.null(S1)) calculate_avg_swe(S1, Kaunertal)
    
    # Create a data frame for the current date with summed SWE for each AOI
    date_data <- data.frame(
      Date = date2,
      YETI_Achensee = ifelse(is.null(sum_YETI_Achensee), NA, sum_YETI_Achensee),
      YETI_Kuehtai = ifelse(is.null(sum_YETI_Kuehtai), NA, sum_YETI_Kuehtai),
      YETI_Kaunertal = ifelse(is.null(sum_YETI_Kaunertal), NA, sum_YETI_Kaunertal),
      SnowGrid_Achensee = ifelse(is.null(sum_SG_Achensee), NA, sum_SG_Achensee),
      SnowGrid_Kuehtai = ifelse(is.null(sum_SG_Kuehtai), NA, sum_SG_Kuehtai),
      SnowGrid_Kaunertal = ifelse(is.null(sum_SG_Kaunertal), NA, sum_SG_Kaunertal),
      S1_Achensee = ifelse(is.null(sum_S1_Achensee), NA, sum_S1_Achensee),
      S1_Kuehtai = ifelse(is.null(sum_S1_Kuehtai), NA, sum_S1_Kuehtai),
      S1_Kaunertal = ifelse(is.null(sum_S1_Kaunertal), NA, sum_S1_Kaunertal)
    )
    
    # Remove any ID columns, if they exist
    date_data <- date_data[, !grepl("ID", colnames(date_data))]
    
    all_data[[date]] <- date_data
    }
  
  str(all_data)
  
  # Combine all data for the current season
  final_df <- do.call(rbind, all_data)
  
  # Ensure output folder exists
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # Save the data to CSV for the current season
  write.csv(final_df, file = output_file, row.names = FALSE)
  
  print(paste("Saved data for season:", season_name))
}


library(tidyverse)

# List of AOIs
aois <- c("Achensee", "Kuehtai", "Kaunertal")
seasons_to_plot <- names(seasons)

# Function to generate plots per AOI
plot_aoi <- function(aoi_name, season_name) {
  
  # Load CSV data
  file_path <- paste0("Ergebnisse/SWE_sum/data_", season_name, "/swe_sum.csv")
  df <- read.csv(file_path)
  
  # Convert Date to proper format
  df$Date <- as.Date(gsub("_", "-", df$Date))
  
  # Filter and reshape using dplyr/tidyr explicitly
  df_long <- df %>%
    dplyr::select(Date, dplyr::contains(aoi_name)) %>%
    tidyr::pivot_longer(
      cols = -Date,
      names_to = "Source",
      values_to = "SWE"
    )
  
  # Plot
  ggplot(df_long, aes(x = Date, y = SWE, color = Source)) +
    geom_line(size = 0.8, alpha = 0.8, na.rm = TRUE) +
    labs(title = paste("SWE Zeitreihe -", aoi_name, season_name),
         x = "Datum", y = "SWE (Summe)",
         color = "Datenquelle") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(filename = paste0("Ergebnisse/SWE_sum/plots/", season_name, "_", aoi_name, ".png"),
         plot = last_plot(), width = 10, height = 6)
  
}

# Plot for each AOI and season
for (aoi in aois) {
  for (season in seasons_to_plot) {
    print(plot_aoi(aoi, season))
  }
}






