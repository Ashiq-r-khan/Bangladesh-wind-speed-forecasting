# ═══════════════════════════════════════════════════════════════════════════════
# STEP 2: EXOGENOUS DATASET PREPARATION (1981–2025)
# Merging: NASA POWER (PS, T2M, RH2M, WD2M) + Climate Indices (ENSO, IOD)
# Author: Ashik | Updated: 2026
# Run AFTER EXTEND NATIONAL CLIMATE DATASET TO 2025.R
# ═══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(lubridate)

# ── File Paths ────────────────────────────────────────────────────────────────
base_path <- "E:/A paper work/Climate_Indices"

# File 1: Extended NASA POWER variables (PS, T2M, RH2M, WD2M) — created in Step 1
climate_file  <- file.path(base_path, "National_Climate_Bangladesh_1981_2025.csv")

# File 2: Climate Indices (ENSO, IOD) — your existing file
indices_file  <- file.path(base_path, "climate_indices_complete.csv")

# Output
output_file   <- file.path(base_path, "Exogenous_Variables_Bangladesh_1981_2025.csv")

# ── Verify files ──────────────────────────────────────────────────────────────
cat("📋 Checking input files...\n")
cat("  NASA POWER climate :", ifelse(file.exists(climate_file),  "✔ Found", "✘ NOT FOUND"), "\n")
cat("  Climate indices    :", ifelse(file.exists(indices_file),  "✔ Found", "✘ NOT FOUND"), "\n\n")

if (!file.exists(climate_file) | !file.exists(indices_file)) {
  stop("❌ One or more required files not found. Run Step 1 first.")
}

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 2A: Load both datasets
# ═══════════════════════════════════════════════════════════════════════════════
cat("📂 Loading datasets...\n")

df_climate <- read.csv(climate_file, stringsAsFactors = FALSE)
df_climate$Date <- ymd(df_climate$Date)
cat("  ✔ NASA POWER loaded :", nrow(df_climate), "rows |",
    as.character(min(df_climate$Date)), "to", as.character(max(df_climate$Date)), "\n")
cat("  Columns:", paste(colnames(df_climate), collapse = ", "), "\n\n")

df_indices <- read.csv(indices_file, stringsAsFactors = FALSE)
df_indices$Date <- ymd(df_indices$Date)
cat("  ✔ Climate indices loaded:", nrow(df_indices), "rows |",
    as.character(min(df_indices$Date)), "to", as.character(max(df_indices$Date)), "\n")
cat("  Columns:", paste(colnames(df_indices), collapse = ", "), "\n\n")

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 2B: Select ENSO and IOD variables from the climate indices file
#   Core variables: Nino34_Anomaly, IOD_DMI
#   All lagged variables (lag2–lag6) for both, plus interaction terms
# ═══════════════════════════════════════════════════════════════════════════════

# Identify all columns matching the pattern for ENSO and IOD
all_index_cols <- colnames(df_indices)

# Core identifiers
id_cols <- c("Date", "Year", "Month")

# Core ENSO/IOD and all their lagged / interaction variants
enso_cols <- grep("Nino34|ENSO|nino|enso", all_index_cols, value = TRUE, ignore.case = TRUE)
iod_cols  <- grep("IOD|iod",               all_index_cols, value = TRUE, ignore.case = TRUE)

selected_index_cols <- unique(c(id_cols, enso_cols, iod_cols))

cat("📌 Selected climate index columns:\n")
cat("  ENSO-related :", paste(enso_cols, collapse = ", "), "\n")
cat("  IOD-related  :", paste(iod_cols,  collapse = ", "), "\n\n")

df_indices_clean <- df_indices %>% select(all_of(selected_index_cols))

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 2C: Merge NASA POWER + Climate Indices
# ═══════════════════════════════════════════════════════════════════════════════
cat("🔗 Merging datasets by Date / Year / Month...\n")

# Use left_join so we keep ALL months from the climate (NASA POWER) dataset.
# Months in 2025 not yet covered by climate_indices will have NA for ENSO/IOD.
complete_data <- df_climate %>%
  left_join(
    df_indices_clean,
    by = intersect(c("Date", "Year", "Month"), colnames(df_indices_clean))
  ) %>%
  arrange(Date)

cat("  ✔ Merged:", nrow(complete_data), "rows\n")
cat("  Date range:", as.character(min(complete_data$Date)), "to",
    as.character(max(complete_data$Date)), "\n\n")

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 2D: Handle 2025 months where climate indices may be missing
# ═══════════════════════════════════════════════════════════════════════════════
cat("📅 Checking coverage by year...\n")
coverage <- complete_data %>%
  group_by(Year) %>%
  summarise(
    n_months        = n(),
    nasa_power_ok   = sum(!is.na(PS)),
    enso_ok         = sum(!is.na(Nino34_Anomaly)),
    iod_ok          = sum(!is.na(IOD_DMI)),
    .groups = "drop"
  ) %>%
  filter(Year >= 2023)

print(coverage)
cat("\n")

# Rows with complete NASA POWER but missing climate indices (likely 2025)
rows_missing_indices <- complete_data %>%
  filter(!is.na(PS) & (is.na(Nino34_Anomaly) | is.na(IOD_DMI))) %>%
  select(Date, Year, Month, Nino34_Anomaly, IOD_DMI)

if (nrow(rows_missing_indices) > 0) {
  cat("⚠️  Months with missing ENSO/IOD indices:\n")
  print(rows_missing_indices)
  cat("\n")
  cat("   These are likely 2025 months not yet in climate_indices_complete.csv.\n")
  cat("   Options:\n")
  cat("   (A) Keep NAs → safe for exploratory analysis, but ML models may need imputation.\n")
  cat("   (B) Impute via seasonal climatological mean → recommended for ML model training.\n")
  cat("   (C) Restrict analysis to rows with complete cases (1981–2024).\n\n")
  
  # ── Option B: Seasonal climatological mean imputation ──────────────────────
  # For each calendar month, use the long-term mean of Nino34 and IOD_DMI
  # from the full historical record (1981–last complete year)
  cat("   → Applying Option B: seasonal climatological mean imputation for 2025...\n")
  
  clim_means <- complete_data %>%
    filter(!is.na(Nino34_Anomaly) & !is.na(IOD_DMI)) %>%
    group_by(Month) %>%
    summarise(
      Nino34_clim_mean = round(mean(Nino34_Anomaly, na.rm = TRUE), 3),
      IOD_clim_mean    = round(mean(IOD_DMI,        na.rm = TRUE), 3),
      .groups = "drop"
    )
  
  complete_data <- complete_data %>%
    left_join(clim_means, by = "Month") %>%
    mutate(
      Nino34_Anomaly_orig = Nino34_Anomaly,
      IOD_DMI_orig        = IOD_DMI,
      Nino34_Anomaly = coalesce(Nino34_Anomaly, Nino34_clim_mean),
      IOD_DMI        = coalesce(IOD_DMI,        IOD_clim_mean),
      # Flag imputed rows (useful for model transparency)
      indices_imputed = is.na(Nino34_Anomaly_orig) | is.na(IOD_DMI_orig)
    ) %>%
    select(-Nino34_clim_mean, -IOD_clim_mean,
           -Nino34_Anomaly_orig, -IOD_DMI_orig)
  
  n_imputed <- sum(complete_data$indices_imputed)
  cat("   ✔ Imputed", n_imputed, "row(s) with climatological monthly means.\n")
  cat("   Column 'indices_imputed' added (TRUE = imputed; use as a quality flag).\n\n")
} else {
  # No missing rows — still add the flag column as FALSE for consistency
  complete_data <- complete_data %>% mutate(indices_imputed = FALSE)
  cat("✔ No missing climate indices — all rows are complete.\n\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 2E: Final validation
# ═══════════════════════════════════════════════════════════════════════════════
cat(rep("=", 70), "\n", sep = "")
cat("📊 EXOGENOUS DATASET: FINAL SUMMARY\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("Columns (", ncol(complete_data), "):\n", sep = "")
for (col in colnames(complete_data)) {
  cat("  •", col, "\n")
}

cat("\nTotal records:", nrow(complete_data), "months\n")
cat("Date range   :", as.character(min(complete_data$Date)), "to",
    as.character(max(complete_data$Date)), "\n\n")

cat("📌 Missing values check:\n")
mv <- colSums(is.na(complete_data))
print(mv[mv > 0])
if (sum(mv) == 0) cat("  None — dataset is fully complete.\n")

cat("\n📌 Summary statistics:\n")
core_vars <- intersect(c("PS", "T2M", "RH2M", "Nino34_Anomaly", "IOD_DMI"),
                       colnames(complete_data))
print(summary(complete_data[, core_vars]))

cat("\n📌 Last 12 rows (2025 extension check):\n")
print(tail(complete_data, 12))

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 2F: Save output
# ═══════════════════════════════════════════════════════════════════════════════
write.csv(complete_data, output_file, row.names = FALSE)

cat("\n✔ SAVED → ", output_file, "\n\n")

cat("📦 Dataset contents:\n")
cat("   ┌── Identifiers         : Date, Year, Month\n")
cat("   ├── NASA POWER variables: PS (kPa), T2M (°C), RH2M (%), WD2M (°)\n")
if (any(grepl("WS10M|PREC|SW_RAD", colnames(complete_data)))) {
  extra_vars <- grep("WS10M|PREC|SW_RAD", colnames(complete_data), value = TRUE)
  cat("   ├── Additional NASA POWER:", paste(extra_vars, collapse = ", "), "\n")
}
cat("   ├── ENSO / IOD variables: Nino34_Anomaly, IOD_DMI (+ lagged terms if present)\n")
cat("   └── Quality flag        : indices_imputed (TRUE = climatological mean used)\n\n")

cat("🏁 NEXT STEP: run the PATCH script in R program/.\n")
cat("   It trims this file to 1981–2024 and saves it as ML_Input/00c_exogenous_variables_1981_2024.csv\n")
cat("   for ML feature engineering (Random Forest, XGBoost, SVR, LSTM, Hybrid).\n\n")