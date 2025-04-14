
#             -------------   create dataframes for the snowcover comparison   -----------------------

#             this script creates a .csv file with the statistics for every timestep, comparing the snowcover tif files of yeti with
#             S1 and SNOWGRID snowcover tif files. The statistics are: F1, F2, F3, NSE, R², % snowcover of YETI, % snowcover of the second file.
#             The calculation of statistics is done by the outsourced function -function_statistics_sim_obs-
#             The .csv files are imported in the next script -plot_statistics- and can be visualized over time there!

library(terra)
library(raster)
library(sf)
library(RColorBrewer)
library(ggplot2)
library(dplyr)

rm(list = ls())

##   FSC Vergleich    ##    ----     Sentinel 1    ------

setwd("L:/T/95_FG_ENTW_PRJ/30_BAU/2022-2025_Langfristprognose_YETI")
source("R_Scripts/snowcover/function_statistics_sim_obs.R")
source("R_Scripts/snowcover/function_2_statistics_sim_obs.R")

# define date! -----------------------------------

# Define seasons with start and end dates
seasons <- list(
  "winter_2022-23" = list(start = as.Date("2022-10-01"), end = as.Date("2023-08-31")),
  "winter_2023-24" = list(start = as.Date("2023-10-01"), end = as.Date("2024-08-31"))
)

# date <- "2024_02_05"
# dates <- c("2023_12_07", "2024_01_11", "2024_02_05", "2024_02_25", "2024_02_29")
# Define the start and end dates

# start_date <- as.Date("2022-10-01")
# end_date <- as.Date("2023-07-31")
# 
# # Create a sequence of dates
# dates <- seq(from = start_date, to = end_date, by = "day")
# 
# # Format the dates as "YYYY_MM_DD"
# dates <- format(dates, "%Y_%m_%d")

# --------------------------------------------------

AOIs <- c("Achensee", "Kuehtai", "Kaunertal")

# Loop over each season
for (season_name in names(seasons)) {
  
  season_dates <- seq(from = seasons[[season_name]]$start, to = seasons[[season_name]]$end, by = "day")
  dates <- format(season_dates, "%Y_%m_%d")  # Format as "YYYY_MM_DD"
  
  results_S1 <- list()
  results_SG <- list()
  missing_dates <- c()
  
  # Loop over each date
  for (date in dates) {
    for (aoi in AOIs) {
      
      date2 <- gsub("_", "-", date)
      date3 <- gsub("_", "", date)
      
      # ----- Sentinel-1 Processing -----
      obs_S1_path <- paste0("Ergebnisse/snowcover/raster_files/Sentinel1/", date, "_snowcover_", aoi, "_S1_500.tif")
      sim_YETI_500_path <- paste0("Ergebnisse/snowcover/raster_files/YETI/", date, "_snowcover_", aoi, "_YETI_500.tif")
      
      if (file.exists(obs_S1_path) && file.exists(sim_YETI_500_path)) {
        obs_S1 <- rast(obs_S1_path)
        sim_YETI_500 <- rast(sim_YETI_500_path)
        results_S1[[paste(date, aoi, sep = "_")]] <- process_sim_obs(sim_YETI_500, obs_S1, aoi)
      } else {
        missing_dates <- c(missing_dates, paste("S1", date, aoi, sep = "_"))
      }
      
      # ----- SnowGrid Processing -----
      obs_SG_path <- paste0("Ergebnisse/snowcover/raster_files/SnowGrid/", date, "_snowcover_", aoi, "_SG_100.tif")
      sim_YETI_100_path <- paste0("Ergebnisse/snowcover/raster_files/YETI/", date, "_snowcover_", aoi, "_YETI_100.tif")
      
      if (file.exists(obs_SG_path) && file.exists(sim_YETI_100_path)) {
        obs_SG <- rast(obs_SG_path)
        sim_YETI_100 <- rast(sim_YETI_100_path)
        results_SG[[paste(date, aoi, sep = "_")]] <- process_sim_obs(sim_YETI_100, obs_SG, aoi)
      } else {
        missing_dates <- c(missing_dates, paste("SG", date, aoi, sep = "_"))
      }
    }
    print(date)
  }
  
  # ---- Create DataFrames ----
  results_S1_df <- bind_rows(lapply(results_S1, as.data.frame), .id = "Date_AOI")
  colnames(results_S1_df) <- c("date_aoi", "F1", "F2", "F3", "NSE", "R2", "cover_S1", "cover_YETI_500m", "AOI")
  
  results_SG_df <- bind_rows(lapply(results_SG, as.data.frame), .id = "Date_AOI")
  colnames(results_SG_df) <- c("date_aoi", "F1", "F2", "F3", "NSE", "R2", "cover_SG", "cover_YETI_100m", "AOI")
  
  # Add missing dates with NA values
  if (length(missing_dates) > 0) {
    missing_df <- data.frame(
      date_aoi = missing_dates,
      F1 = NA, F2 = NA, F3 = NA, NSE = NA, R2 = NA, 
      cover_S1 = NA, cover_YETI_500m = NA, AOI = NA
    )
    results_S1_df <- bind_rows(results_S1_df, missing_df)
  }
  
  # Format date column and reorder
  results_S1_df <- results_S1_df %>%
    mutate(
      Date = sub("^(\\d{4}_\\d{2}_\\d{2})_.*$", "\\1", date_aoi),
      Date = as.Date(Date, format = "%Y_%m_%d")
    ) %>%
    dplyr::select(Date, AOI, F1, F2, F3, NSE, R2, cover_S1, cover_YETI_500m) %>%
    arrange(Date)
  
  results_SG_df <- results_SG_df %>%
    mutate(
      Date = sub("^(\\d{4}_\\d{2}_\\d{2})_.*$", "\\1", date_aoi),
      Date = as.Date(Date, format = "%Y_%m_%d")
    ) %>%
    dplyr::select(Date, AOI, F1, F2, F3, NSE, R2, cover_SG, cover_YETI_100m) %>%
    arrange(Date)
  
  # ---- Save Results ----
  write.csv(results_S1_df, paste0("Ergebnisse/snowcover/statistics_df/", season_name, "/statistics_yeti_S1_df.csv"), row.names = FALSE)
  write.csv(results_SG_df, paste0("Ergebnisse/snowcover/statistics_df/", season_name, "/statistics_yeti_SG_df.csv"), row.names = FALSE)
  
  print(paste("Finished processing for", season_name))
}




