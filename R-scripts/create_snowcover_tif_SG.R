

# ----------    this scipt creates snowcover tif rasters of Sentinel 1 SWE output files for multiple dates   -----------------------
# ----------    the threshold is set to >5 mm = snow

library(terra)
library(sf)

setwd("L:/T/95_FG_ENTW_PRJ/30_BAU/2022-2025_Langfristprognose_YETI")
rm(list = ls())

# define date! -----------------------------------

# date <- "2024_02_05"
# dates <- c("2023_12_07", "2024_01_11", "2024_02_05", "2024_02_25", "2024_02_29", "2024_03_14", "2024_04_07")
# Define the start and end dates

start_date <- as.Date("2024-08-01")
end_date <- as.Date("2024-08-31")

# Create a sequence of dates
dates <- seq(from = start_date, to = end_date, by = "day")

# Format the dates as "YYYY_MM_DD"
dates <- format(dates, "%Y_%m_%d")

# --------------------------------------------------
target_crs <- "EPSG:32632"

rcl <- matrix(c(-Inf, 5, 0, 5, Inf, 1), ncol = 3, byrow = TRUE)

YETI_Achensee <- rast("Ergebnisse/snowcover/raster_files/YETI/2023_12_07_snowcover_Achensee_YETI_100.tif") # load for resampling to 500m
YETI_Kuehtai <- rast("Ergebnisse/snowcover/raster_files/YETI/2023_12_07_snowcover_Kuehtai_YETI_100.tif")
YETI_Kaunertal <- rast("Ergebnisse/snowcover/raster_files/YETI/2023_12_07_snowcover_Kaunertal_YETI_100.tif")

# load all shapefiles
Achensee      <- vect("Daten/shapefiles/HOPI_Achensee.shp")
Kuehtai       <- vect("Daten/shapefiles/Kuehtai_2.shp")
Kaunertal     <- vect("Daten/shapefiles/Kaunertal.shp")

for (date in dates) {
  
  date2 <- gsub("_","-",date)
  date3 <- gsub("_","",date)
  
  # Construct the file path
  file_path <- paste0("Daten/snowgrid/SWE_2022-09_bis_2024-11/tif_files/", date3, "_SNOWGRID_SWE.tif")
  
  # Check if the file exists
  if (!file.exists(file_path)) {
    print(paste("File does not exist, skipping:", file_path))
    next
  }
  
  # Load SG data
  SG <- rast(file_path)

  # set Projection & extent
  SG <- project(SG, target_crs)

  SG_Achensee <- crop(SG, Achensee)
  SG_Achensee <- resample(SG_Achensee, YETI_Achensee)
  SG_Achensee <- mask(SG_Achensee,Achensee)
  
  SG_Kuehtai <- crop(SG, Kuehtai)
  SG_Kuehtai <- resample(SG_Kuehtai, YETI_Kuehtai) 
  SG_Kuehtai <- mask(SG_Kuehtai, Kuehtai)
  
  SG_Kaunertal <- crop(SG, Kaunertal)
  SG_Kaunertal <- resample(SG_Kaunertal, YETI_Kaunertal)
  SG_Kaunertal <- mask(SG_Kaunertal, Kaunertal)

  # Reclassify using classify() from terra
  SG_Achensee_100 <- classify(SG_Achensee, rcl)
  writeRaster(SG_Achensee_100, paste0("Ergebnisse/snowcover/raster_files/SnowGrid/", date,"_snowcover_Achensee_SG_100.tif"), datatype="INT1U", overwrite = TRUE)
  
  S1_Kuehtai_100 <- classify(SG_Kuehtai, rcl)
  writeRaster(S1_Kuehtai_100, paste0("Ergebnisse/snowcover/raster_files/SnowGrid/", date,"_snowcover_Kuehtai_SG_100.tif"), datatype="INT1U", overwrite = TRUE)
  
  S1_Kaunertal_100 <- classify(SG_Kaunertal, rcl)
  writeRaster(S1_Kaunertal_100, paste0("Ergebnisse/snowcover/raster_files/SnowGrid/", date,"_snowcover_Kaunertal_SG_100.tif"), datatype="INT1U", overwrite = TRUE)
  
  print(date)
}


