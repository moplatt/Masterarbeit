

# ----------    this scipt creates snowcover tif rasters of Sentinel 1 SWE output files for multiple dates   -----------------------
# ----------    the threshold is set to >5 mm = snow

library(terra)

setwd("L:/T/95_FG_ENTW_PRJ/30_BAU/2022-2025_Langfristprognose_YETI")
rm(list = ls())

# define date! -----------------------------------

# date <- "2024_02_05"
# dates <- c("2023_12_07", "2024_01_11", "2024_02_05", "2024_02_25", "2024_02_29")
# Define the start and end dates

start_date <- as.Date("2023-08-01")
end_date <- as.Date("2023-08-31")

# Create a sequence of dates
dates <- seq(from = start_date, to = end_date, by = "day")

# Format the dates as "YYYY_MM_DD"
dates <- format(dates, "%Y_%m_%d")

# --------------------------------------------------
target_crs <- "EPSG:32632"

# load all shapefiles
Achensee      <- vect("Daten/shapefiles/HOPI_Achensee.shp")
Kuehtai       <- vect("Daten/shapefiles/Kuehtai_2.shp")
Kaunertal     <- vect("Daten/shapefiles/Kaunertal.shp")

rcl <- matrix(c(-Inf, 5, 0,
                5, Inf, 1),
              ncol = 3, byrow = TRUE)

for (date in dates) {
  
  date2 <- gsub("_","-",date)
  date3 <- gsub("_","",date)
  
  # load all S1 data
  sentinel1      <- rast(paste0("Daten/S1_SWE_Tirol/raster_files/SWE_", date3, "_500m.tif"))

  # set Projection & extent
  S1_Achensee <- crop(sentinel1, Achensee)
  S1_Achensee <- mask(S1_Achensee,Achensee)
  
  S1_Kuehtai <- crop(sentinel1, Kuehtai)
  S1_Kuehtai <- mask(S1_Kuehtai, Kuehtai)
  
  S1_Kaunertal <- crop(sentinel1, Kaunertal)
  S1_Kaunertal <- mask(S1_Kaunertal, Kaunertal)
  
  # Reclassify using classify() from terra
  S1_Achensee_500 <- classify(S1_Achensee, rcl)
  writeRaster(S1_Achensee_500, paste0("Ergebnisse/snowcover/raster_files/Sentinel1/", date,"_snowcover_Achensee_S1_500.tif"), datatype="INT1U", overwrite = TRUE)
  
  S1_Kuehtai_500 <- classify(S1_Kuehtai, rcl)
  writeRaster(S1_Kuehtai_500, paste0("Ergebnisse/snowcover/raster_files/Sentinel1/", date,"_snowcover_Kuehtai_S1_500.tif"), datatype="INT1U", overwrite = TRUE)
  
  S1_Kaunertal_500 <- classify(S1_Kaunertal, rcl)
  writeRaster(S1_Kaunertal_500, paste0("Ergebnisse/snowcover/raster_files/Sentinel1/", date,"_snowcover_Kaunertal_S1_500.tif"), datatype="INT1U", overwrite = TRUE)
  
  print(date)
}