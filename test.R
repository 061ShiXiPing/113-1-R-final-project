# Load required libraries
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

# Rename columns for easier handling
tidy_data <- tidy_data %>%
  rename(
    index = 項次,
    role = 委員會職務,
    name = 姓名,
    organization = 服務單位,
    position = 現職,
    gender = 性別,
    gender_code = 性別代碼
  )

# Handle missing values in the 'gender' column
tidy_data <- tidy_data %>%
  mutate(gender = ifelse(is.na(gender), "Unknown", gender))

# Fix encoding issues in the 'position' column
tidy_data <- tidy_data %>%
  mutate(position = str_replace_all(position, "[^[:print:]]", ""))

# Check for missing values
missing_summary <- tidy_data %>%
  summarize(across(everything(), ~ sum(is.na(.))))

# Verify unique and sequential index
index_check <- tidy_data %>%
  summarize(
    is_unique = n_distinct(index) == n(),
    is_sequential = all(diff(sort(index)) == 1)
  )

# Gender distribution summary
gender_summary <- tidy_data %>%
  count(gender, name = "count")

# Role distribution summary
role_summary <- tidy_data %>%
  count(role, name = "count") %>%
  arrange(desc(count))

# Gender and role combined distribution
role_gender_summary <- tidy_data %>%
  count(role, gender, name = "count") %>%
  arrange(desc(count))

# Output summaries for verification
print("Missing Value Summary:")
print(missing_summary)

print("Index Check:")
print(index_check)

print("Gender Distribution:")
print(gender_summary)

print("Role Distribution:")
print(role_summary)

print("Role and Gender Combined Distribution:")
print(role_gender_summary)

# Glimpse cleaned data
glimpse(tidy_data)

# Export cleaned data
write_csv(tidy_data, "cleaned_data.csv")

message("Data parsing and cleaning complete. Cleaned data saved as 'cleaned_data.csv'.")

