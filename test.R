# Load libraries
library(tidyverse)  # For data manipulation
library(ntpuR)      # Assuming it provides relevant tools

# File path
file_path <- "臺北市都市設計及土地使用開發許可審議委員會_112年度委員會幹事及委員名單_v1130319  (1).csv"

# Check if the file exists
if (!file.exists(file_path)) {
  stop("File not found. Please check the file path or upload the file again.")
} else {
  message("File found. Proceeding to read the file...")
}

# Attempt to read the file
tryCatch({
  tidy_data <- read_csv(file_path, locale = locale(encoding = "UTF-8"))
  message("File successfully read as CSV!")
}, error = function(e) {
  message("Reading as CSV failed. Trying with Big5 encoding...")
  tidy_data <- read_csv(file_path, locale = locale(encoding = "Big5"))
})

# Glimpse the data to inspect its structure
glimpse(tidy_data)

