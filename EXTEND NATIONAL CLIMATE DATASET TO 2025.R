# ═══════════════════════════════════════════════════════════════════════════════
# STEP 1: EXTEND NATIONAL CLIMATE DATASET TO 2025
# Adding new NASA POWER file (1981–2025) to existing National_Climate_Bangladesh
# Author: Ashik | Updated: 2026
# ═══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(lubridate)

# ── File Paths ────────────────────────────────────────────────────────────────
base_path <- "E:/A paper work/Climate_Indices"

# Existing national climate dataset (PS, T2M, RH2M — 1981–2024)
existing_file <- file.path(base_path, "National_Climate_Bangladesh_1981_2024.csv")

# NEW NASA POWER file (1981–2025, updated bounding box)
new_nasa_file <- file.path(base_path,
                           "POWER_Regional_Monthly_19810101_20251231_020d74N_026d63N_088d01E_092d68E_LST.csv")

# Output file
output_file <- file.path(base_path, "National_Climate_Bangladesh_1981_2025.csv")

# ── Verify files exist ────────────────────────────────────────────────────────
cat("📋 Checking files...\n")
cat("  Existing dataset :", ifelse(file.exists(existing_file),  "✔ Found", "✘ NOT FOUND"), "\n")
cat("  New NASA POWER   :", ifelse(file.exists(new_nasa_file),  "✔ Found", "✘ NOT FOUND"), "\n\n")

if (!file.exists(existing_file) | !file.exists(new_nasa_file)) {
  stop("❌ One or more required files not found. Check paths above.")
}

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 1A: Load the existing National Climate dataset (1981–2024)
# ═══════════════════════════════════════════════════════════════════════════════
cat("📂 Loading existing National_Climate_Bangladesh_1981_2024.csv...\n")
df_existing <- read.csv(existing_file, stringsAsFactors = FALSE)
df_existing$Date <- ymd(df_existing$Date)

cat("  ✔ Loaded:", nrow(df_existing), "rows |",
    as.character(min(df_existing$Date)), "to", as.character(max(df_existing$Date)), "\n")
cat("  Columns:", paste(colnames(df_existing), collapse = ", "), "\n\n")

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 1B: Process the new NASA POWER file — auto-detect variable(s)
# ═══════════════════════════════════════════════════════════════════════════════
cat("🔍 Reading new NASA POWER file and detecting variables...\n")

# Read raw file skipping the 10-row metadata header
df_raw <- read.csv(new_nasa_file, skip = 10, stringsAsFactors = FALSE)

# Rename standard columns
colnames(df_raw) <- c("PARAMETER", "YEAR", "LAT", "LON",
                      "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                      "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", "ANN")

# Detect all unique variables in this file
detected_vars <- unique(trimws(df_raw$PARAMETER))
cat("  Detected variable(s):", paste(detected_vars, collapse = ", "), "\n\n")

# ─── Map NASA POWER parameter codes to clean column names ────────────────────
# Extend this lookup if your new file contains additional variables
param_map <- c(
  "PS"         = "PS",
  "T2M"        = "T2M",
  "RH2M"       = "RH2M",
  "WS10M"      = "WS10M",
  "PRECTOTCORR"= "PREC",
  "ALLSKY_SFC_SW_DWN" = "SW_RAD"
)

month_map <- c(JAN=1, FEB=2, MAR=3, APR=4, MAY=5, JUN=6,
               JUL=7, AUG=8, SEP=9, OCT=10, NOV=11, DEC=12)

# ── Helper: process one parameter block from the raw file ─────────────────────
process_one_param <- function(df_raw, param_code) {
  col_name <- ifelse(param_code %in% names(param_map),
                     param_map[[param_code]], param_code)
  
  df_param <- df_raw %>%
    filter(trimws(PARAMETER) == param_code) %>%
    select(-PARAMETER, -ANN) %>%
    pivot_longer(cols = c(JAN, FEB, MAR, APR, MAY, JUN,
                          JUL, AUG, SEP, OCT, NOV, DEC),
                 names_to  = "MONTH_NAME",
                 values_to = col_name) %>%
    mutate(MONTH = month_map[MONTH_NAME]) %>%
    select(-MONTH_NAME, -LAT, -LON) %>%
    group_by(YEAR, MONTH) %>%
    summarise(!!col_name := round(mean(.data[[col_name]], na.rm = TRUE), 2),
              .groups = "drop")
  
  cat("  ✔", param_code, "→ column '", col_name, "' |",
      nrow(df_param), "grid-averaged records\n")
  return(df_param)
}

# ── Process every detected variable ──────────────────────────────────────────
cat("⚙️  Processing variable(s)...\n")
list_processed <- lapply(detected_vars, function(p) process_one_param(df_raw, p))

# Merge all processed variables into one wide data frame
df_new <- list_processed[[1]]
if (length(list_processed) > 1) {
  for (i in 2:length(list_processed)) {
    df_new <- full_join(df_new, list_processed[[i]], by = c("YEAR", "MONTH"))
  }
}

# Add Date column
df_new <- df_new %>%
  arrange(YEAR, MONTH) %>%
  mutate(Date = ymd(paste0(YEAR, "-", sprintf("%02d", MONTH), "-01")))

cat("\n  New file covers:", as.character(min(df_new$Date)),
    "to", as.character(max(df_new$Date)), "\n")
cat("  Total records in new file:", nrow(df_new), "\n\n")

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 1C: Identify what the new file contributes
#   Strategy:
#     • For rows already in the existing dataset  → keep existing values
#       (they were built from the original 3-file pipeline; no need to overwrite)
#     • For NEW rows (i.e., 2025 months not yet in the existing file) → add them
#   This preserves the integrity of your 1981–2024 baseline entirely.
# ═══════════════════════════════════════════════════════════════════════════════
existing_dates  <- df_existing$Date
new_dates       <- df_new$Date
dates_to_append <- new_dates[!new_dates %in% existing_dates]

cat("📅 Date comparison:\n")
cat("  Existing dataset ends   :", as.character(max(existing_dates)), "\n")
cat("  New file ends           :", as.character(max(new_dates)), "\n")
cat("  New months to append    :", length(dates_to_append), "\n\n")

if (length(dates_to_append) == 0) {
  cat("⚠️  No new months to append — the existing dataset already covers this range.\n")
  cat("   If you intended to REPLACE existing values, set force_overwrite = TRUE below.\n")
  df_extended <- df_existing
} else {
  
  # ── Figure out which columns the new file provides ──────────────────────────
  # Map detected_vars → clean column names (same mapping used above)
  new_col_names <- sapply(detected_vars, function(p) {
    ifelse(p %in% names(param_map), param_map[[p]], p)
  })
  
  # Columns present in BOTH existing and new (e.g. PS, T2M, RH2M)
  shared_cols <- intersect(new_col_names, colnames(df_existing))
  # Columns NEW only (variables not previously in the existing dataset)
  brand_new_cols <- setdiff(new_col_names, colnames(df_existing))
  
  cat("🔗 Column alignment:\n")
  cat("  Shared columns (will be extended) :", paste(shared_cols,    collapse = ", "), "\n")
  cat("  Brand-new columns (will be joined) :", paste(brand_new_cols, collapse = ", "), "\n\n")
  
  # Build rows to append: select only the 2025 (new) months
  df_append <- df_new %>%
    filter(Date %in% dates_to_append) %>%
    mutate(Year  = YEAR,
           Month = MONTH) %>%
    select(Date, Year, Month, any_of(c(shared_cols, brand_new_cols)))
  
  # Fill any columns in df_existing that are missing in df_append with NA
  for (col in setdiff(colnames(df_existing), colnames(df_append))) {
    df_append[[col]] <- NA_real_
  }
  
  # Reorder to match existing column order, then add any brand-new columns at the end
  existing_cols_ordered <- colnames(df_existing)
  df_append <- df_append %>%
    select(all_of(existing_cols_ordered), any_of(brand_new_cols))
  
  # If brand-new columns exist, also add them as NA to the existing dataset
  if (length(brand_new_cols) > 0) {
    for (col in brand_new_cols) {
      df_existing[[col]] <- NA_real_
    }
    # Back-fill brand-new columns in existing rows using the new file's data
    df_backfill <- df_new %>%
      select(Date, all_of(brand_new_cols))
    df_existing <- df_existing %>%
      left_join(df_backfill %>% rename_with(~ paste0(.x, "_new"), all_of(brand_new_cols)),
                by = "Date")
    for (col in brand_new_cols) {
      df_existing[[col]] <- coalesce(df_existing[[col]],
                                     df_existing[[paste0(col, "_new")]])
      df_existing[[paste0(col, "_new")]] <- NULL
    }
  }
  
  # Stack: existing rows + new 2025 rows
  df_extended <- bind_rows(df_existing, df_append) %>%
    arrange(Date)
  
  cat("✅ Extended dataset:\n")
  cat("  Rows:", nrow(df_existing), "(existing) +", nrow(df_append), "(new) =",
      nrow(df_extended), "total\n")
  cat("  Date range:", as.character(min(df_extended$Date)), "to",
      as.character(max(df_extended$Date)), "\n\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 1D: Validation
# ═══════════════════════════════════════════════════════════════════════════════
cat(rep("=", 70), "\n", sep = "")
cat("📊 FINAL DATASET SUMMARY\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("Columns:", paste(colnames(df_extended), collapse = ", "), "\n")
cat("Total records:", nrow(df_extended), "\n")
cat("Date range:", as.character(min(df_extended$Date)), "to",
    as.character(max(df_extended$Date)), "\n\n")

cat("📌 Missing values check:\n")
print(colSums(is.na(df_extended)))

cat("\n📌 Summary statistics for key variables:\n")
vars_to_summarise <- intersect(c("PS", "T2M", "RH2M"), colnames(df_extended))
if (length(vars_to_summarise) > 0) print(summary(df_extended[, vars_to_summarise]))

cat("\n📌 First 6 rows:\n")
print(head(df_extended, 6))
cat("\n📌 Last 6 rows (should include 2025):\n")
print(tail(df_extended, 6))

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 1E: Save output
# ═══════════════════════════════════════════════════════════════════════════════
write.csv(df_extended, output_file, row.names = FALSE)
cat("\n✔ SAVED → ", output_file, "\n")
cat("  Ready for EXOGENOUS DATASET PREPARATION (Step 2)\n\n")