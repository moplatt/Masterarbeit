
# letzte Bearbeitung: 24.03.2025
# von Moritz Plattner

#### Extract point data out of a yeti raster ###

library(raster)
library(sf)
library(ggplot2)
library(terra)


setwd("L:/T/95_FG_ENTW_PRJ/30_BAU/2022-2025_Langfristprognose_YETI")
rm(list = ls())

# -----------------------  !!!  adjust the date here !!!    -------------------------

# Define seasons
seasons <- list(
  "Winter_22-23" = list(start_date = as.Date("2022-10-01"), end_date = as.Date("2023-08-31")),
  "Winter_23-24" = list(start_date = as.Date("2023-10-01"), end_date = as.Date("2024-08-31"))
)

# --------------------------------------------------
Achensee      <- st_read("Daten/shapefiles/HOPI_Achensee.shp", quiet = T)
Kuehtai       <- st_read("Daten/shapefiles/Kuehtai_2.shp", quiet = T)
Kaunertal     <- st_read("Daten/shapefiles/Kaunertal.shp", quiet = T)

Koordinaten <- read.csv2("Pointdata/Koordinaten.txt", sep = ";", header = TRUE)

original_crs <- 31251  # Austria GK WEST (EPSG:31255)
target_crs <- 32632  # UTM Zone 32N (EPSG:32632)

sf_object <- st_as_sf(Koordinaten, coords = c("Rechtswert", "Hochwert"), crs = original_crs)
Koordinaten <- st_transform(sf_object, crs = target_crs)
Koordinaten$Koordinatensystem <- "UTM 32"

for (season_name in names(seasons)) {
  
  print(paste("Processing season:", season_name))
  
  start_date <- seasons[[season_name]]$start_date
  end_date <- seasons[[season_name]]$end_date
  output_folder <- paste0("Pointdata/data_", season_name)
  output_file <- paste0(output_folder, "/point_values_raster.csv")
  
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
  
    # Extract values using st_coordinates
    EV_YETI <- if (!is.null(rasters$YETI)) terra::extract(rasters$YETI, st_coordinates(Koordinaten)) else NA
    EV_SG <- if (!is.null(rasters$SG)) terra::extract(rasters$SG, st_coordinates(Koordinaten)) else NA
    EV_S1 <- if (!is.null(rasters$sentinel1)) terra::extract(rasters$sentinel1, st_coordinates(Koordinaten)) else NA
    
    # Create a data frame for the current date
    date_data <- data.frame(
      Station = Koordinaten$Station,
      Date = date,
      YETI = EV_YETI,
      SnowGrid = EV_SG,
      S1 = EV_S1
    )
    
    all_data[[date]] <- date_data
  }
  
  
  final_df <- do.call(rbind, all_data)
    
  
  
  # Ensure output folder exists
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  write.csv(final_df, file = output_file, row.names = FALSE)
  
  print(paste("Saved data for season:", season_name))
}

# plot(sentinel1)
# plot(st_geometry(Koordinaten), add = TRUE, col = "red", pch = 16, cex = 2)



# Convert Date to Date format
final_df$Date <- as.Date(final_df$Date, format = "%Y_%m_%d")

mittelbergferner_df <- subset(final_df, Station == "Erfurter Huette 2")


# Create the plot
plot(mittelbergferner_df$Date, mittelbergferner_df$YETI, type = "o", col = "blue", 
     xlab = "Date", ylab = "Values", main = "YETI and SnowGrid Values for Mittelbergferner Station", ylim = c(0,1200))
lines(mittelbergferner_df$Date, mittelbergferner_df$SnowGrid, type = "o", col = "red")
lines(mittelbergferner_df$Date, mittelbergferner_df$S1, type = "o", col = "green4")


legend("topright", legend = c("YETI", "SnowGrid", "S1"), col = c("blue", "red", "green4"), lty = 1, pch = 1)


