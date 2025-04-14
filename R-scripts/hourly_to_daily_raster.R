
# keep only one file for each day, if possible 06:00 Uhr.

# Required libraries
library(stringr)
library(dplyr)

setwd("L:/T/95_FG_ENTW_PRJ/30_BAU/2022-2025_Langfristprognose_YETI")

path <-"Daten/Nachrechnung_2022-23/2022-10-01_bis_2023-07-31/03_ses_zentralraum/out"

tif_files <- list.files(path, pattern = "\\.tif$", full.names = TRUE)

# Extract date and time information from filenames
file_info <- data.frame(
  file_path = tif_files,
  file_name = basename(tif_files),
  date = str_extract(tif_files, "SWE_\\d{4}-\\d{2}-\\d{2}"),  # Extract date (YYYY-MM-DD)
  time = str_extract(tif_files, "T\\d{4}")                    # Extract time (e.g., T0600)
)

# Remove "T" from time for easier handling
file_info$time <- sub("T", "", file_info$time)
file_info$date <- sub("SWE_", "", file_info$date)

# Select one file per day (prefer 0600 if available)
selected_files <- file_info %>%
  group_by(date) %>%
  slice(if (any(time == "0600")) which(time == "0600")[1] else 1) %>%
  ungroup()

# Files to keep
keep_files <- selected_files$file_path

# Files to delete
delete_files <- setdiff(tif_files, keep_files)

# Delete unwanted files
file.remove(delete_files)

# Print results
cat("Kept files:\n", paste(keep_files, collapse = "\n"), "\n")
cat("Deleted files:\n", paste(delete_files, collapse = "\n"), "\n")
