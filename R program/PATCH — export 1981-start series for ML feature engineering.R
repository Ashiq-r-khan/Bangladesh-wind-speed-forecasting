###############################################################################
## PATCH - export 1981-start series for ML feature engineering
## Writes to SARIMA_Output/ML_Input/. Touches nothing else.
## SARIMA results unchanged.
###############################################################################

ROOT    <- "E:/A paper work"
OUTDIR  <- file.path(ROOT, "SARIMA_Output")
DATADIR <- file.path(OUTDIR, "Data")
MLDIR   <- file.path(OUTDIR, "ML_Input")

if (!dir.exists(DATADIR)) stop("Run the main SARIMA script first: ", DATADIR)
if (!dir.exists(MLDIR)) dir.create(MLDIR, recursive = TRUE)

cat("\n=== ML INPUT EXPORT ===\n")
cat("Output ->", MLDIR, "\n\n")

## ---------------------------------------------------------------------------
## 1. Locate the same source file the main script used
## ---------------------------------------------------------------------------
cands <- list.files(ROOT, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
cands <- cands[!grepl("SARIMA_Output", cands, fixed = TRUE)]
hit   <- cands[grepl("national.*month|month.*national", basename(cands), ignore.case = TRUE)]
if (length(hit) == 0) hit <- cands[grepl("national", basename(cands), ignore.case = TRUE)]
if (length(hit) == 0) stop("No national monthly CSV found under: ", ROOT)
DATA_FILE <- hit[1]
cat("[1] Source:", basename(DATA_FILE), "\n")

raw <- read.csv(DATA_FILE, stringsAsFactors = FALSE)
names(raw) <- trimws(names(raw))

tcol <- grep("WS10M|wind.*speed|^WS$|windspeed", names(raw), ignore.case = TRUE, value = TRUE)
tcol <- tcol[!grepl("lag|roll|std|anom|dir|WD", tcol, ignore.case = TRUE)]
if (length(tcol) == 0) stop("Target column not found.")
TARGET <- tcol[1]
cat("    target:", TARGET, "\n")

ycol <- grep("^year$|^YEAR$|^yr$", names(raw), ignore.case = TRUE, value = TRUE)
mcol <- grep("^month$|^mon$|^mo$", names(raw), ignore.case = TRUE, value = TRUE)
dcol <- grep("^date$|^ym$|^time$", names(raw), ignore.case = TRUE, value = TRUE)

if (length(ycol) && length(mcol)) {
  raw$.Y <- as.integer(raw[[ycol[1]]])
  raw$.M <- as.integer(raw[[mcol[1]]])
} else if (length(dcol)) {
  dd <- as.Date(raw[[dcol[1]]])
  if (all(is.na(dd))) dd <- as.Date(paste0(raw[[dcol[1]]], "-01"))
  raw$.Y <- as.integer(format(dd, "%Y"))
  raw$.M <- as.integer(format(dd, "%m"))
} else {
  stop("Could not build a date. Need Year+Month or a Date column.")
}

raw <- raw[order(raw$.Y, raw$.M), ]
raw$Date <- as.Date(sprintf("%04d-%02d-01", raw$.Y, raw$.M))
raw <- raw[!is.na(raw[[TARGET]]), ]

## ---------------------------------------------------------------------------
## 2. 1981-01 to 2024-12 (12 months runway + the 1982-2024 analysis window)
## ---------------------------------------------------------------------------
cat("[2] Building 1981-start series\n")

lo <- as.Date("1981-01-01")
hi <- as.Date("2024-12-01")
d81 <- raw[raw$Date >= lo & raw$Date <= hi, c("Date", TARGET)]
names(d81)[2] <- "WS"

expected <- seq(lo, hi, by = "month")
if (nrow(d81) != length(expected) || any(d81$Date != expected)) {
  stop("Series is not a complete monthly sequence 1981-01 to 2024-12")
}

d81$Role <- ifelse(d81$Date < as.Date("1982-01-01"), "Feature_runway",
                   ifelse(d81$Date <= as.Date("2022-12-01"), "Train", "Holdout"))

write.csv(d81, file.path(MLDIR, "00b_analysis_series_1981_2024.csv"), row.names = FALSE)
cat("  [csv] ML_Input/00b_analysis_series_1981_2024.csv\n")
cat("    total rows:", nrow(d81), "\n")
print(table(factor(d81$Role, levels = c("Feature_runway", "Train", "Holdout"))))

## ---------------------------------------------------------------------------
## 3. Exogenous variables 1981-01 to 2024-12 (trimmed from the 2025 dataset)
## ---------------------------------------------------------------------------
cat("[3] Building exogenous series\n")

EXO_FILE <- file.path(ROOT, "Climate_Indices", "Exogenous_Variables_Bangladesh_1981_2025.csv")
if (!file.exists(EXO_FILE)) stop("Exogenous source not found: ", EXO_FILE)
exo_cols <- c("Date", "Year", "Month", "PS", "T2M", "RH2M", "WD2M", "Nino34_SST", "Nino34_Anomaly",
              "ENSO_Phase", "ENSO_Strength", "IOD_DMI", "IOD_Phase", "IOD_Strength")
exo <- read.csv(EXO_FILE, stringsAsFactors = FALSE)
exo$Date <- as.Date(exo$Date)
exo <- exo[exo$Date >= lo & exo$Date <= hi, exo_cols]
if (!identical(exo$Date, d81$Date)) stop("Exogenous dates do not match the 1981-2024 target series")
if (anyNA(exo[c("PS", "T2M", "RH2M", "WD2M", "Nino34_Anomaly", "IOD_DMI")])) stop("NA in exogenous model columns")

write.csv(exo, file.path(MLDIR, "00c_exogenous_variables_1981_2024.csv"), row.names = FALSE, quote = FALSE)
cat("  [csv] ML_Input/00c_exogenous_variables_1981_2024.csv\n")
cat("    total rows:", nrow(exo), "\n")

## ---------------------------------------------------------------------------
## 4. Copy the SARIMA forecast files Colab needs to merge into
## ---------------------------------------------------------------------------
cat("[4] Copying SARIMA forecast files for merge\n")

need <- c("08b_holdout_forecast_sarima.csv", "10c_cv_forecasts_sarima.csv")
for (f in need) {
  src <- file.path(DATADIR, f)
  if (file.exists(src)) {
    file.copy(src, file.path(MLDIR, f), overwrite = TRUE)
    cat("  [copy] ML_Input/", f, "\n", sep = "")
  } else {
    cat("  [MISSING] ", f, " - re-run the main script\n", sep = "")
  }
}

## ---------------------------------------------------------------------------
## 5. Manifest (lets you detect stale copies later)
## ---------------------------------------------------------------------------
files <- list.files(MLDIR, pattern = "\\.csv$", full.names = TRUE)
files <- files[basename(files) != "MANIFEST.csv"]

manifest <- data.frame(
  File = basename(files),
  Rows = sapply(files, function(p) nrow(read.csv(p, stringsAsFactors = FALSE))),
  Source_modified = sapply(basename(files), function(f) {
    s <- if (f == "00c_exogenous_variables_1981_2024.csv") EXO_FILE else file.path(DATADIR, f)
    if (file.exists(s)) format(file.mtime(s), "%Y-%m-%d %H:%M:%S") else "generated here"
  }),
  Exported = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  row.names = NULL
)
write.csv(manifest, file.path(MLDIR, "MANIFEST.csv"), row.names = FALSE)
cat("  [csv] ML_Input/MANIFEST.csv\n")

cat("\n=== DONE ===\n")
cat("After lag-12 / roll-12 construction, drop the 12 runway rows:\n")
cat("  Matrix : 1982-01 to 2024-12 = 516 rows\n")
cat("  Train  : 1982-01 to 2022-12 = 492\n")
cat("  Holdout: 2023-01 to 2024-12 = 24\n\n")