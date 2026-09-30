###############################################################################
## DIEBOLD-MARIANO TESTS - SARIMA vs ML/DL models
## Inputs : ML_Output/Data/04_all_forecasts_merged_holdout.csv
##          ML_Output/Data/05_all_forecasts_merged_cv.csv
## Outputs: DM_Test_Output/Data/ and DM_Test_Output/Graph/
###############################################################################

## ---------------------------------------------------------------------------
## 0. SETUP - edit ML_DIR to wherever you put the downloaded ML_Output/Data
## ---------------------------------------------------------------------------
ML_DIR  <- "E:/A paper work/ML_Output/Data"
OUTROOT <- "E:/A paper work/DM_Test_Output"
DATADIR <- file.path(OUTROOT, "Data")
GRAPHDIR<- file.path(OUTROOT, "Graph")

for (d in c(OUTROOT, DATADIR, GRAPHDIR))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)

if (!requireNamespace("forecast", quietly = TRUE)) install.packages("forecast")
suppressPackageStartupMessages(library(forecast))

sav <- function(df, name) {
  write.csv(df, file.path(DATADIR, name), row.names = FALSE)
  cat("  [csv] Data/", name, "\n", sep = "")
}

BASE   <- "SARIMA"
MODELS <- c("Random Forest", "XGBoost", "SVR", "LSTM", "Hybrid")

cat("\n=== DIEBOLD-MARIANO TESTS ===\n")
cat("Input :", ML_DIR, "\n")
cat("Output:", OUTROOT, "\n\n")

## ---------------------------------------------------------------------------
## 1. LOAD
## ---------------------------------------------------------------------------
cat("[1] Loading forecasts\n")

f_hold <- file.path(ML_DIR, "04_all_forecasts_merged_holdout.csv")
f_cv   <- file.path(ML_DIR, "05_all_forecasts_merged_cv.csv")
if (!file.exists(f_hold)) stop("Not found: ", f_hold)
if (!file.exists(f_cv))   stop("Not found: ", f_cv)

hold <- read.csv(f_hold, stringsAsFactors = FALSE)
cv   <- read.csv(f_cv,   stringsAsFactors = FALSE)
hold$Date <- as.Date(hold$Date)
cv$Date   <- as.Date(cv$Date)

cat("  holdout:", nrow(hold), "rows |  cv:", nrow(cv), "rows\n")
stopifnot(nrow(hold) == 24)

## error columns in the wide holdout file
err_col <- function(m) if (m == BASE) "Error" else paste0(gsub(" ", "_", m), "_Error")
for (m in c(BASE, MODELS))
  if (!err_col(m) %in% names(hold)) stop("Missing column: ", err_col(m))

## ---------------------------------------------------------------------------
## 2. VALIDITY CHECK - are the loss differentials autocorrelated?
##    The standard DM statistic assumes no serial correlation at horizon h.
##    If d_t is autocorrelated, variance is understated and p-values are wrong.
## ---------------------------------------------------------------------------
cat("[2] Checking loss-differential autocorrelation\n")

e_base <- hold[[err_col(BASE)]]
val <- data.frame()

for (m in MODELS) {
  d_t <- e_base^2 - hold[[err_col(m)]]^2          # squared-error differential
  lb  <- Box.test(d_t, lag = 6, type = "Ljung-Box")
  a1  <- acf(d_t, plot = FALSE, lag.max = 1)$acf[2]
  val <- rbind(val, data.frame(
    Comparison = paste(BASE, "vs", m),
    Mean_d = round(mean(d_t), 6),
    ACF_lag1 = round(a1, 4),
    LjungBox6_stat = round(lb$statistic, 4),
    LjungBox6_p = round(lb$p.value, 4),
    Assumption = ifelse(lb$p.value > 0.05,
                        "OK - differentials uncorrelated",
                        "VIOLATED - interpret DM p-values with caution"),
    row.names = NULL))
}
sav(val, "01_dm_validity_loss_differential_acf.csv")
print(val[, c("Comparison", "ACF_lag1", "LjungBox6_p", "Assumption")])

## ---------------------------------------------------------------------------
## 3. DM TESTS - HOLDOUT (n = 24)
##    h = 1 with the Harvey-Leybourne-Newbold small-sample correction
##    (dm.test applies HLN by default). h = 12 reported as a sensitivity check
##    because these are multi-step forecasts from a fixed origin.
## ---------------------------------------------------------------------------
cat("[3] DM tests on holdout (n = 24)\n")

run_dm <- function(e1, e2, h, lab) {
  t <- tryCatch(dm.test(e1, e2, alternative = "two.sided", h = h, power = 2),
                error = function(e) NULL)
  if (is.null(t)) return(NULL)
  data.frame(Comparison = lab, h = h,
             DM_stat = round(as.numeric(t$statistic), 4),
             p_value = round(t$p.value, 4), row.names = NULL)
}

dm_hold <- data.frame()
for (m in MODELS) {
  for (h in c(1, 12)) {
    r <- run_dm(e_base, hold[[err_col(m)]], h, paste(BASE, "vs", m))
    if (!is.null(r)) {
      rb <- sqrt(mean(e_base^2)); rm_ <- sqrt(mean(hold[[err_col(m)]]^2))
      r$RMSE_SARIMA <- round(rb, 4)
      r$RMSE_other  <- round(rm_, 4)
      r$Better <- ifelse(rm_ < rb, m, BASE)
      dm_hold <- rbind(dm_hold, r)
    }
  }
}

## Holm correction, applied within each h separately
dm_hold$p_adj_holm <- NA
for (h in unique(dm_hold$h)) {
  i <- dm_hold$h == h
  dm_hold$p_adj_holm[i] <- round(p.adjust(dm_hold$p_value[i], method = "holm"), 4)
}
dm_hold$Signif_raw <- cut(dm_hold$p_value, c(-Inf, .01, .05, .10, Inf),
                          labels = c("***", "**", "*", "ns"))
dm_hold$Signif_adj <- cut(dm_hold$p_adj_holm, c(-Inf, .01, .05, .10, Inf),
                          labels = c("***", "**", "*", "ns"))
dm_hold$Verdict <- ifelse(dm_hold$p_adj_holm > 0.05,
                          "No significant difference",
                          paste(dm_hold$Better, "significantly better"))

sav(dm_hold, "02_dm_holdout_vs_sarima.csv")
print(dm_hold[dm_hold$h == 1,
              c("Comparison","DM_stat","p_value","p_adj_holm","Better","Verdict")])

## ---------------------------------------------------------------------------
## 4. DM TESTS - CROSS-VALIDATION POOL (n = 120)
##    Five non-overlapping 24-month windows pooled. More power than n=24.
## ---------------------------------------------------------------------------
cat("[4] DM tests on pooled CV (n = 120)\n")

get_cv_err <- function(m) {
  s <- cv[cv$Model == m, c("Fold", "Date", "Error")]
  s <- s[order(s$Fold, s$Date), ]
  s$Error
}

e_base_cv <- get_cv_err(BASE)
cat("  SARIMA CV errors:", length(e_base_cv), "\n")

dm_cv <- data.frame()
for (m in MODELS) {
  e_m <- get_cv_err(m)
  if (length(e_m) != length(e_base_cv)) {
    cat("  SKIP", m, "- length mismatch\n"); next
  }
  r <- run_dm(e_base_cv, e_m, 1, paste(BASE, "vs", m))
  if (!is.null(r)) {
    rb <- sqrt(mean(e_base_cv^2)); rm_ <- sqrt(mean(e_m^2))
    r$RMSE_SARIMA <- round(rb, 4)
    r$RMSE_other  <- round(rm_, 4)
    r$Better <- ifelse(rm_ < rb, m, BASE)
    dm_cv <- rbind(dm_cv, r)
  }
}
dm_cv$p_adj_holm <- round(p.adjust(dm_cv$p_value, method = "holm"), 4)
dm_cv$Signif_adj <- cut(dm_cv$p_adj_holm, c(-Inf, .01, .05, .10, Inf),
                        labels = c("***", "**", "*", "ns"))
dm_cv$Verdict <- ifelse(dm_cv$p_adj_holm > 0.05,
                        "No significant difference",
                        paste(dm_cv$Better, "significantly better"))
sav(dm_cv, "03_dm_cv_pooled_vs_sarima.csv")
print(dm_cv[, c("Comparison","DM_stat","p_value","p_adj_holm","Better","Verdict")])

## ---------------------------------------------------------------------------
## 5. PER-FOLD DM (robustness: is any result driven by one fold?)
## ---------------------------------------------------------------------------
cat("[5] Per-fold DM tests\n")

dm_fold <- data.frame()
for (f in sort(unique(cv$Fold))) {
  eb <- cv$Error[cv$Model == BASE & cv$Fold == f]
  for (m in MODELS) {
    em <- cv$Error[cv$Model == m & cv$Fold == f]
    if (length(em) != length(eb)) next
    r <- run_dm(eb, em, 1, paste(BASE, "vs", m))
    if (!is.null(r)) {
      r$Fold <- f
      r$Better <- ifelse(sqrt(mean(em^2)) < sqrt(mean(eb^2)), m, BASE)
      dm_fold <- rbind(dm_fold, r)
    }
  }
}
dm_fold <- dm_fold[, c("Fold","Comparison","DM_stat","p_value","Better")]
sav(dm_fold, "04_dm_per_fold.csv")

wins <- as.data.frame(table(dm_fold$Comparison, dm_fold$Better))
names(wins) <- c("Comparison", "Winner", "Folds_won")
wins <- wins[wins$Folds_won > 0, ]
sav(wins, "04b_dm_fold_win_counts.csv")
print(wins)

## ---------------------------------------------------------------------------
## 6. FIGURE - DM statistics with significance bands
## ---------------------------------------------------------------------------
cat("[6] Figure\n")

h1 <- dm_hold[dm_hold$h == 1, ]
lab <- gsub("SARIMA vs ", "", h1$Comparison)

png(file.path(GRAPHDIR, "Fig1_dm_statistics.png"),
    width = 2400, height = 1300, res = 300)
par(mfrow = c(1, 2), mar = c(7, 4, 3, 1))

bp <- barplot(h1$DM_stat, names.arg = lab, las = 2, cex.names = .8,
              ylab = "DM statistic", col = "grey70", border = NA,
              main = "(a) Holdout (n = 24)",
              ylim = range(c(h1$DM_stat, -2.5, 2.5)) * 1.2)
abline(h = c(-1.96, 0, 1.96), lty = c(2, 1, 2),
       col = c("red", "grey40", "red"))
text(bp, h1$DM_stat, as.character(h1$Signif_adj), pos = 3, cex = .8)

bp2 <- barplot(dm_cv$DM_stat, names.arg = gsub("SARIMA vs ", "", dm_cv$Comparison),
               las = 2, cex.names = .8, ylab = "DM statistic",
               col = "grey70", border = NA, main = "(b) Pooled CV (n = 120)",
               ylim = range(c(dm_cv$DM_stat, -2.5, 2.5)) * 1.2)
abline(h = c(-1.96, 0, 1.96), lty = c(2, 1, 2),
       col = c("red", "grey40", "red"))
text(bp2, dm_cv$DM_stat, as.character(dm_cv$Signif_adj), pos = 3, cex = .8)
dev.off()
cat("  [png] Graph/Fig1_dm_statistics.png\n")

## ---------------------------------------------------------------------------
## 7. VERDICT
## ---------------------------------------------------------------------------
beat_h  <- sum(h1$p_adj_holm < 0.05 & h1$Better != BASE)
beat_cv <- sum(dm_cv$p_adj_holm < 0.05 & dm_cv$Better != BASE)
worse_h <- sum(h1$p_adj_holm < 0.05 & h1$Better == BASE)

verdict <- data.frame(
  Item = c("Holdout n", "CV pooled n", "Models tested",
           "Models significantly BETTER than SARIMA (holdout)",
           "Models significantly WORSE than SARIMA (holdout)",
           "Models significantly BETTER than SARIMA (CV)",
           "Correction applied", "Headline"),
  Value = c(nrow(hold), length(e_base_cv), length(MODELS),
            beat_h, worse_h, beat_cv, "Holm",
            ifelse(beat_h == 0 && beat_cv == 0,
                   "No ML model significantly outperforms SARIMA",
                   "At least one ML model significantly outperforms SARIMA")))
sav(verdict, "05_SUMMARY_dm.csv")

cat("\n=== DONE ===\n")
print(verdict)
cat("\nNote: n = 24 gives the holdout test low power. Non-significance means\n")
cat("insufficient evidence of a difference, NOT evidence of equivalence.\n\n")