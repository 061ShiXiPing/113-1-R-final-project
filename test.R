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

# Translation dictionary for column names
translation_dict <- c(
  項次 = "index",
  委員會職務 = "role",
  姓名 = "name",
  服務單位 = "organization",
  現職 = "position",
  性別 = "gender",
  性別代碼 = "gender_code"
)

# Rename columns using the translation dictionary
tidy_data <- tidy_data %>%
  rename_with(~ translation_dict[.x], .cols = everything())

# Translation dictionary for gender values
gender_translation <- c(
  "男" = "Male",
  "女" = "Female",
  "未知" = "Unknown"
)

# Translate gender values
tidy_data <- tidy_data %>%
  mutate(gender = recode(gender, !!!gender_translation))

# Translation dictionary for role values (if applicable)
role_translation <- c(
  "委員" = "Member",
  "主席" = "Chairperson",
  "副主席" = "Vice Chairperson"
)

# Translate roles
tidy_data <- tidy_data %>%
  mutate(role = recode(role, !!!role_translation))

# Fix encoding issues in the 'position' column (Regex example)
tidy_data <- tidy_data %>%
  mutate(position = str_replace_all(position, "[^[:print:]]", ""))

# Check for missing values (Summarize Part)
missing_summary <- tidy_data %>%
  summarize(across(everything(), ~ sum(is.na(.))))  # Summarizes missing values

# Verify unique and sequential index (Summarize Part)
index_check <- tidy_data %>%
  summarize(
    is_unique = n_distinct(index) == n(),
    is_sequential = all(diff(sort(index)) == 1)
  )  # Summarizes index validity

# Gender distribution summary (Summarize Part)
gender_summary <- tidy_data %>%
  count(gender, name = "count")  # Summarizes gender distribution

# Role distribution summary (Summarize Part)
role_summary <- tidy_data %>%
  count(role, name = "count") %>%
  arrange(desc(count))  # Summarizes role distribution

# Gender and role combined distribution (Summarize Part)
role_gender_summary <- tidy_data %>%
  count(role, gender, name = "count") %>%
  arrange(desc(count))  # Summarizes role and gender combined distribution

# Pivot data: Convert roles into columns for analysis
tidy_pivot <- tidy_data %>%
  pivot_wider(names_from = role, values_from = name, values_fill = NA)

# Output summaries for verification
print("Translated Column Names:")
print(colnames(tidy_data))

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

print("Pivoted Data Preview:")
print(head(tidy_pivot))

# Glimpse cleaned data
glimpse(tidy_data)

# Export cleaned data
write_csv(tidy_data, "cleaned_data.csv")
write_csv(tidy_pivot, "pivoted_data.csv")  # Export pivoted data

message("Data parsing, cleaning, translating, and pivoting complete. Files saved as 'cleaned_data.csv' and 'pivoted_data.csv'.")
