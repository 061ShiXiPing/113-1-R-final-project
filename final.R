library(tidyverse)
library(ntpuR)


file_path <- "臺北市役男徵集概況統計.csv"


tidy_data <- read_csv(file_path, locale = locale(encoding = "Big5"))


head(tidy_data)

