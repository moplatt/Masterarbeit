

# ----------    this scipt creates snowcover tif rasters of YETI SWE output files for multiple dates   -----------------------
# ----------    the threshold is set to >5 mm = snow
library(terra)
library(sf)

rm(list = ls())
setwd("L:/T/95_FG_ENTW_PRJ/30_BAU/2022-2025_Langfristprognose_YETI")

# define date! -----------------------------------

# dates <- c("2023_12_07", "2024_01_11", "2024_02_05", "2024_02_25", "2024_02_29", "2024_03_14", "2024_04_07")
# Define the start and end dates

start_date <- as.Date("2024-07-01")
end_date <- as.Date("2024-07-31")

# Create a sequence of dates
dates <- seq(from = start_date, to = end_date, by = "day")

# Format the dates as "YYYY_MM_DD"
dates <- format(dates, "%Y_%m_%d")
# date <- "2022_12_15"
# --------------------------------------------------
target_crs <- "EPSG:32632"

S1_Achensee <- rast("Ergebnisse/snowcover/raster_files/Sentinel1/2023_12_07_snowcover_Achensee_S1_500.tif") # load for resampling to 500m
S1_Kuehtai <- rast("Ergebnisse/snowcover/raster_files/Sentinel1/2023_12_07_snowcover_Kuehtai_S1_500.tif")
S1_Kaunertal <- rast("Ergebnisse/snowcover/raster_files/Sentinel1/2023_12_07_snowcover_Kaunertal_S1_500.tif")

# load all shapefiles
Achensee      <- vect("Daten/shapefiles/HOPI_Achensee.shp")
Kuehtai       <- vect("Daten/shapefiles/Kuehtai_2.shp")
Kaunertal     <- vect("Daten/shapefiles/Kaunertal.shp")

for (date in dates) {
  
  date2 <- gsub("_","-",date)
  date3 <- gsub("_","",date)
  print(date)                         # progress
  
  # load all YETI data
  YETI_Achensee <- rast(list.files(paste0("Daten/Nachrechnung_2023-24/2023-10-01_bis_2024-08-31/01_ses_nordost"), pattern = paste0("SES_SWE_", date2, "T.*.tif"), full.names = TRUE)[1])
  YETI_zentral  <- rast(list.files(paste0("Daten/Nachrechnung_2023-24/2023-10-01_bis_2024-08-31/03_ses_zentralraum"), pattern = paste0("SES_SWE_", date2, "T.*.tif"), full.names = TRUE)[1])
  
  # set Projection & extent
  crs(YETI_Achensee) <- target_crs # defines a projection for YETI_Achensee, as it has no crs defined!
  crs(YETI_zentral) <- target_crs
  
  YETI_Kuehtai <- crop(YETI_zentral, Kuehtai)
  YETI_Kaunertal <- crop(YETI_zentral, Kaunertal)
  
  YETI_Kuehtai <- mask(YETI_Kuehtai, Kuehtai)
  YETI_Kaunertal <- mask(YETI_Kaunertal, Kaunertal)
  
  # Reclassify using classify() from terra
  rcl <- matrix(c(-Inf, 5, 0, 5, Inf, 1), ncol = 3, byrow = TRUE)
  YETI_Achensee_100 <- classify(YETI_Achensee, rcl)
  writeRaster(YETI_Achensee_100, paste0("Ergebnisse/snowcover/raster_files/YETI/", date,"_snowcover_Achensee_YETI_100.tif"),datatype="INT1U", overwrite = TRUE)
  
  YETI_Kuehtai_100 <- classify(YETI_Kuehtai, rcl)
  writeRaster(YETI_Kuehtai_100, paste0("Ergebnisse/snowcover/raster_files/YETI/", date,"_snowcover_Kuehtai_YETI_100.tif"), datatype="INT1U", overwrite = TRUE)
  
  YETI_Kaunertal_100 <- classify(YETI_Kaunertal, rcl)
  writeRaster(YETI_Kaunertal_100, paste0("Ergebnisse/snowcover/raster_files/YETI/", date,"_snowcover_Kaunertal_YETI_100.tif"), datatype="INT1U", overwrite = TRUE)
  
  # print("done_100m")
  # --------------   500m for comparison with S1   ---------------
  if (date <= "2024_02_29") {
  
    YETI_Achensee_500 <- resample(YETI_Achensee_100, S1_Achensee, method = "near")
    YETI_Achensee_500 <- mask(YETI_Achensee_500, S1_Achensee, maskvalue=NA)
    writeRaster(YETI_Achensee_500, paste0("Ergebnisse/snowcover/raster_files/YETI/", date,"_snowcover_Achensee_YETI_500.tif"), datatype="INT1U", overwrite = TRUE)
    
    YETI_Kuehtai_500 <- resample(YETI_Kuehtai_100, S1_Kuehtai, method = "near")
    YETI_Kuehtai_500 <- mask(YETI_Kuehtai_500, S1_Kuehtai, maskvalue=NA)
    writeRaster(YETI_Kuehtai_500, paste0("Ergebnisse/snowcover/raster_files/YETI/", date,"_snowcover_Kuehtai_YETI_500.tif"), datatype="INT1U", overwrite = TRUE)
    
    YETI_Kaunertal_500 <- resample(YETI_Kaunertal_100, S1_Kaunertal, method = "near")
    YETI_Kaunertal_500 <- mask(YETI_Kaunertal_500, S1_Kaunertal, maskvalue=NA)
    writeRaster(YETI_Kaunertal_500, paste0("Ergebnisse/snowcover/raster_files/YETI/", date,"_snowcover_Kaunertal_YETI_500.tif"), datatype="INT1U", overwrite = TRUE)
    # print("done_500m")
    
  }
}

