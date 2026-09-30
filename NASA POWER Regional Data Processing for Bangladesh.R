# NASA POWER Regional Data Processing for Bangladesh
# Author: Ashik | Date: January 27, 2026
# Run this in RStudio on your local computer

# Load required libraries
library(tidyverse)
library(lubridate)

# Step 1: Set your file paths with CORRECT filenames
base_path <- "E:/A paper work/Climate_Indices"

ps_file <- file.path(base_path, "POWER_Regional_Monthly_19810101_20241231_020d50N_026d70N_088d00E_092d80E_LST.csv")
t2m_file <- file.path(base_path, "POWER_Regional_Monthly_19810101_20241231_020d50N_026d70N_088d00E_092d80E_LST (1).csv")
rh2m_file <- file.path(base_path, "POWER_Regional_Monthly_19810101_20241231_020d50N_026d70N_088d00E_092d80E_LST (2).csv")

cat("📁 Reading files from:\n")
cat("  ", base_path, "\n\n")

# Verify files exist
cat("🔍 Checking if files exist:\n")
cat("  PS file:", ifelse(file.exists(ps_file), "✅ Found", "❌ NOT FOUND"), "\n")
cat("  T2M file:", ifelse(file.exists(t2m_file), "✅ Found", "❌ NOT FOUND"), "\n")
cat("  RH2M file:", ifelse(file.exists(rh2m_file), "✅ Found", "❌ NOT FOUND"), "\n\n")

# Step 2: Function to process each NASA POWER regional file
process_nasa_power <- function(filepath, param_name) {
  cat("Processing:", basename(filepath), "\n")
  
  # Read file, skip first 10 rows (metadata header)
  df <- read.csv(filepath, skip = 10, stringsAsFactors = FALSE)
  
  # Rename columns
  colnames(df) <- c("PARAMETER", "YEAR", "LAT", "LON", "JAN", "FEB", "MAR", 
                    "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", "ANN")
  
  # Remove unnecessary columns
  df <- df %>% select(-PARAMETER, -ANN)
  
  # Reshape from wide to long format
  df_long <- df %>%
    pivot_longer(
      cols = c(JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC),
      names_to = "MONTH_NAME",
      values_to = param_name
    )
  
  # Convert month names to numbers
  month_map <- c("JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6,
                 "JUL" = 7, "AUG" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)
  
  df_long$MONTH <- month_map[df_long$MONTH_NAME]
  df_long <- df_long %>% select(-MONTH_NAME)
  
  # Calculate NATIONAL AVERAGE
  national_avg <- df_long %>%
    group_by(YEAR, MONTH) %>%
    summarise(!!param_name := round(mean(.data[[param_name]], na.rm = TRUE), 2),
              .groups = "drop")
  
  cat("  ✓ Extracted", param_name, ",", nrow(national_avg), "records\n\n")
  
  return(national_avg)
}

# Step 3: Process all 3 files
cat("🔄 Processing files...\n\n")
df_ps <- process_nasa_power(ps_file, "PS")
df_t2m <- process_nasa_power(t2m_file, "T2M")
df_rh2m <- process_nasa_power(rh2m_file, "RH2M")

# Step 4: Merge all 3 parameters
cat("🔗 Merging PS, T2M, and RH2M...\n")
national_climate <- df_ps %>%
  full_join(df_t2m, by = c("YEAR", "MONTH")) %>%
  full_join(df_rh2m, by = c("YEAR", "MONTH"))

# Step 5: Sort and create Date column
national_climate <- national_climate %>%
  arrange(YEAR, MONTH)

national_climate$Date <- ymd(paste0(national_climate$YEAR, "-", 
                                    sprintf("%02d", national_climate$MONTH), "-01"))

# Reorder columns
national_climate <- national_climate %>%
  select(Date, Year = YEAR, Month = MONTH, PS, T2M, RH2M)

# Step 6: Display results
cat("\n", rep("=", 70), "\n", sep = "")
cat("📊 NATIONAL CLIMATE DATASET - BANGLADESH (1981-2024)\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("First 10 rows:\n")
print(head(national_climate, 10))

cat("\nLast 10 rows:\n")
print(tail(national_climate, 10))

cat("\nTotal records:", nrow(national_climate), "\n")
cat("Date range:", as.character(min(national_climate$Date)), "to", 
    as.character(max(national_climate$Date)), "\n")

# Step 7: Summary statistics
cat("\n📈 Summary Statistics:\n")
print(summary(national_climate[, c("PS", "T2M", "RH2M")]))

# Step 8: Check for missing values
cat("\n🔍 Missing values check:\n")
print(colSums(is.na(national_climate)))

# Step 9: Save output
output_path <- file.path(base_path, "National_Climate_Bangladesh_1981_2024.csv")
write.csv(national_climate, output_path, row.names = FALSE)

cat("\n✅ SUCCESS! File saved to:\n")
cat("  ", output_path, "\n\n")

cat("📦 Output File Structure:\n")
cat("   Columns:", paste(colnames(national_climate), collapse = ", "), "\n")
cat("   • PS   : Surface Pressure (kPa)\n")
cat("   • T2M  : Temperature at 2 Meters (°C)\n")
cat("   • RH2M : Relative Humidity at 2 Meters (%)\n")
cat("\n✅ COMPLETE! Next: EXTEND NATIONAL CLIMATE DATASET TO 2025.R\n")
