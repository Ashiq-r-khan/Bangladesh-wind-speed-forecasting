# ============================================================================
# Script: Aggregate 64 District Wind Speed Data for National Analysis
# Data: NASA MERRA-2 Wind Speed (1981-2025) for Bangladesh
# Author: Research Project - Wind Speed Analysis
# ============================================================================

# Load required libraries
library(tidyverse)
library(data.table)
library(lubridate)

# Set working directory
data_path <- "E:/wind speed data"
output_path <- "E:/A paper work/Aggregated_Data"

# Create output directory if it doesn't exist
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

# ============================================================================
# FUNCTION 1: Read single district file
# ============================================================================
read_district_wind <- function(filepath, district_name) {
  # Skip header lines (9 lines) and read data
  data <- fread(filepath, skip = 9, header = TRUE)
  
  # Add district identifier
  data$District <- district_name
  
  # Create proper date column
  data$Date <- as.Date(paste(data$YEAR, data$MO, data$DY, sep = "-"))
  
  # Handle missing values (-999)
  data$WS10M[data$WS10M == -999] <- NA
  
  # Select relevant columns
  data <- data %>% select(District, Date, YEAR, MO, DY, WS10M)
  
  return(data)
}

# ============================================================================
# FUNCTION 2: Get all district CSV files
# ============================================================================
# List all CSV files in the data directory
csv_files <- list.files(data_path, pattern = "*.csv$", full.names = TRUE)

# Extract district names from filenames
district_names <- tools::file_path_sans_ext(basename(csv_files))

cat(sprintf("Found %d district CSV files\n", length(csv_files)))

# ============================================================================
# STEP 1: Read and combine all district data
# ============================================================================
cat("\nReading all district files...\n")

# Read all files and combine
all_districts_data <- map2_dfr(
  csv_files, 
  district_names, 
  ~read_district_wind(.x, .y),
  .progress = TRUE
)

cat(sprintf("Total records: %d\n", nrow(all_districts_data)))
cat(sprintf("Date range: %s to %s\n", 
            min(all_districts_data$Date), 
            max(all_districts_data$Date)))

# Save complete combined dataset
fwrite(all_districts_data, 
       file.path(output_path, "all_districts_daily_windspeed.csv"))
cat("\n✓ Saved: all_districts_daily_windspeed.csv\n")

# ============================================================================
# STEP 2: National Daily Average
# ============================================================================
cat("\nCalculating national daily averages...\n")

national_daily <- all_districts_data %>%
  group_by(Date, YEAR, MO, DY) %>%
  summarise(
    WS10M_mean = mean(WS10M, na.rm = TRUE),
    WS10M_sd = sd(WS10M, na.rm = TRUE),
    WS10M_min = min(WS10M, na.rm = TRUE),
    WS10M_max = max(WS10M, na.rm = TRUE),
    Districts_reporting = sum(!is.na(WS10M)),
    .groups = "drop"
  )

fwrite(national_daily, 
       file.path(output_path, "national_daily_windspeed.csv"))
cat("✓ Saved: national_daily_windspeed.csv\n")

# ============================================================================
# STEP 3: Monthly Aggregation (District & National)
# ============================================================================
cat("\nCalculating monthly averages...\n")

# District-level monthly
district_monthly <- all_districts_data %>%
  group_by(District, YEAR, MO) %>%
  summarise(
    WS10M_monthly_mean = mean(WS10M, na.rm = TRUE),
    WS10M_monthly_sd = sd(WS10M, na.rm = TRUE),
    WS10M_monthly_min = min(WS10M, na.rm = TRUE),
    WS10M_monthly_max = max(WS10M, na.rm = TRUE),
    Days_recorded = sum(!is.na(WS10M)),
    .groups = "drop"
  ) %>%
  mutate(YearMonth = as.Date(paste(YEAR, MO, "01", sep = "-")))

fwrite(district_monthly, 
       file.path(output_path, "district_monthly_windspeed.csv"))
cat("✓ Saved: district_monthly_windspeed.csv\n")

# National monthly
national_monthly <- district_monthly %>%
  group_by(YEAR, MO, YearMonth) %>%
  summarise(
    WS10M_national_mean = mean(WS10M_monthly_mean, na.rm = TRUE),
    WS10M_national_sd = sd(WS10M_monthly_mean, na.rm = TRUE),
    WS10M_national_min = min(WS10M_monthly_mean, na.rm = TRUE),
    WS10M_national_max = max(WS10M_monthly_mean, na.rm = TRUE),
    Districts_count = n(),
    .groups = "drop"
  )

fwrite(national_monthly, 
       file.path(output_path, "national_monthly_windspeed.csv"))
cat("✓ Saved: national_monthly_windspeed.csv\n")

# ============================================================================
# STEP 4: Annual Aggregation (District & National)
# ============================================================================
cat("\nCalculating annual averages...\n")

# District-level annual
district_annual <- all_districts_data %>%
  group_by(District, YEAR) %>%
  summarise(
    WS10M_annual_mean = mean(WS10M, na.rm = TRUE),
    WS10M_annual_sd = sd(WS10M, na.rm = TRUE),
    WS10M_annual_min = min(WS10M, na.rm = TRUE),
    WS10M_annual_max = max(WS10M, na.rm = TRUE),
    Days_recorded = sum(!is.na(WS10M)),
    .groups = "drop"
  )

fwrite(district_annual, 
       file.path(output_path, "district_annual_windspeed.csv"))
cat("✓ Saved: district_annual_windspeed.csv\n")

# National annual
national_annual <- district_annual %>%
  group_by(YEAR) %>%
  summarise(
    WS10M_national_mean = mean(WS10M_annual_mean, na.rm = TRUE),
    WS10M_national_sd = sd(WS10M_annual_mean, na.rm = TRUE),
    WS10M_national_min = min(WS10M_annual_mean, na.rm = TRUE),
    WS10M_national_max = max(WS10M_annual_mean, na.rm = TRUE),
    Districts_count = n(),
    .groups = "drop"
  )

fwrite(national_annual, 
       file.path(output_path, "national_annual_windspeed.csv"))
cat("✓ Saved: national_annual_windspeed.csv\n")

# ============================================================================
# STEP 5: Seasonal Aggregation (Monsoon vs Dry season)
# ============================================================================
cat("\nCalculating seasonal patterns...\n")

# Define seasons for Bangladesh
# Pre-monsoon: March-May
# Monsoon: June-September
# Post-monsoon: October-November
# Winter/Dry: December-February

seasonal_data <- all_districts_data %>%
  mutate(
    Season = case_when(
      MO %in% c(12, 1, 2) ~ "Winter",
      MO %in% c(3, 4, 5) ~ "Pre-Monsoon",
      MO %in% c(6, 7, 8, 9) ~ "Monsoon",
      MO %in% c(10, 11) ~ "Post-Monsoon"
    )
  ) %>%
  group_by(District, YEAR, Season) %>%
  summarise(
    WS10M_seasonal_mean = mean(WS10M, na.rm = TRUE),
    WS10M_seasonal_sd = sd(WS10M, na.rm = TRUE),
    Days_recorded = sum(!is.na(WS10M)),
    .groups = "drop"
  )

fwrite(seasonal_data, 
       file.path(output_path, "district_seasonal_windspeed.csv"))
cat("✓ Saved: district_seasonal_windspeed.csv\n")

# National seasonal
national_seasonal <- seasonal_data %>%
  group_by(YEAR, Season) %>%
  summarise(
    WS10M_national_mean = mean(WS10M_seasonal_mean, na.rm = TRUE),
    WS10M_national_sd = sd(WS10M_seasonal_mean, na.rm = TRUE),
    Districts_count = n(),
    .groups = "drop"
  )

fwrite(national_seasonal, 
       file.path(output_path, "national_seasonal_windspeed.csv"))
cat("✓ Saved: national_seasonal_windspeed.csv\n")

# ============================================================================
# STEP 6: District Summary Statistics (Complete Time Series)
# ============================================================================
cat("\nCalculating district-level summary statistics...\n")

district_summary <- all_districts_data %>%
  group_by(District) %>%
  summarise(
    WS10M_overall_mean = mean(WS10M, na.rm = TRUE),
    WS10M_overall_median = median(WS10M, na.rm = TRUE),
    WS10M_overall_sd = sd(WS10M, na.rm = TRUE),
    WS10M_overall_min = min(WS10M, na.rm = TRUE),
    WS10M_overall_max = max(WS10M, na.rm = TRUE),
    WS10M_Q25 = quantile(WS10M, 0.25, na.rm = TRUE),
    WS10M_Q75 = quantile(WS10M, 0.75, na.rm = TRUE),
    Total_days = n(),
    Missing_days = sum(is.na(WS10M)),
    Data_availability_pct = (1 - sum(is.na(WS10M))/n()) * 100,
    First_date = min(Date),
    Last_date = max(Date),
    .groups = "drop"
  ) %>%
  arrange(desc(WS10M_overall_mean))

fwrite(district_summary, 
       file.path(output_path, "district_summary_statistics.csv"))
cat("✓ Saved: district_summary_statistics.csv\n")

# ============================================================================
# STEP 7: Generate Summary Report
# ============================================================================
cat("\n============================================================\n")
cat("AGGREGATION SUMMARY REPORT\n")
cat("============================================================\n\n")

cat(sprintf("Total districts processed: %d\n", length(unique(all_districts_data$District))))
cat(sprintf("Date range: %s to %s\n", 
            min(all_districts_data$Date), 
            max(all_districts_data$Date)))
cat(sprintf("Total days: %d\n", 
            as.numeric(max(all_districts_data$Date) - min(all_districts_data$Date))))
cat(sprintf("Total records: %s\n", 
            format(nrow(all_districts_data), big.mark = ",")))

cat("\nNational Statistics (Overall):\n")
cat(sprintf("  Mean wind speed: %.2f m/s\n", 
            mean(all_districts_data$WS10M, na.rm = TRUE)))
cat(sprintf("  Std deviation: %.2f m/s\n", 
            sd(all_districts_data$WS10M, na.rm = TRUE)))
cat(sprintf("  Range: %.2f - %.2f m/s\n", 
            min(all_districts_data$WS10M, na.rm = TRUE),
            max(all_districts_data$WS10M, na.rm = TRUE)))

cat("\nTop 5 Windiest Districts (Overall Mean):\n")
print(head(district_summary %>% select(District, WS10M_overall_mean), 5))

cat("\nFiles created in:", output_path, "\n")
cat("  1. all_districts_daily_windspeed.csv (Complete dataset)\n")
cat("  2. national_daily_windspeed.csv (Daily national averages)\n")
cat("  3. district_monthly_windspeed.csv (Monthly by district)\n")
cat("  4. national_monthly_windspeed.csv (Monthly national)\n")
cat("  5. district_annual_windspeed.csv (Annual by district)\n")
cat("  6. national_annual_windspeed.csv (Annual national)\n")
cat("  7. district_seasonal_windspeed.csv (Seasonal by district)\n")
cat("  8. national_seasonal_windspeed.csv (Seasonal national)\n")
cat("  9. district_summary_statistics.csv (Overall statistics)\n")

cat("\n✓ Aggregation complete!\n")
cat("============================================================\n")
