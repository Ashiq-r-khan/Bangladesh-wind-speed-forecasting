###############################################################################
##  SARIMA ANALYSIS — Monthly Wind Speed, Bangladesh (1982–2024)
##  Data characterization -> identification -> selection -> diagnostics
##  -> holdout -> holdout diagnostics -> rolling-origin CV
##
##  Outputs:  E:/A paper work/SARIMA_Output/Data/    (all CSV)
##            E:/A paper work/SARIMA_Output/Graph/   (all PNG)
##
##  Author: Ashiqur Rahman Khan | MBSTU
###############################################################################

## ---------------------------------------------------------------------------
## 0. SETUP
## ---------------------------------------------------------------------------
ROOT    <- "E:/A paper work"
OUTDIR  <- file.path(ROOT, "SARIMA_Output")
DATADIR <- file.path(OUTDIR, "Data")
GRAPHDIR<- file.path(OUTDIR, "Graph")
for (d in c(OUTDIR, DATADIR, GRAPHDIR))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)

pkgs <- c("forecast", "tseries", "lmtest", "sandwich")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages({
  library(forecast); library(tseries); library(lmtest); library(sandwich)
})

## Analysis window ------------------------------------------------------------
START      <- c(1982, 1)      # start 1982 -> no refit needed later
TRAIN_END  <- c(2022, 12)     # 492 months
TEST_START <- c(2023, 1)
TEST_END   <- c(2024, 12)     # 24-month holdout
FREQ       <- 12

## Final model order (set BOTH to NULL to auto-adopt the AICc-best grid model)
FINAL_ORDER    <- c(0, 1, 1)
FINAL_SEASONAL <- c(0, 1, 1)

sav <- function(df, name) {
  write.csv(df, file.path(DATADIR, name), row.names = FALSE)
  cat("  [csv] Data/", name, "\n", sep = "")
}
fig <- function(name, w = 2400, h = 1600) {
  png(file.path(GRAPHDIR, name), width = w, height = h, res = 300)
}
endfig <- function(name) { dev.off(); cat("  [png] Graph/", name, "\n", sep = "") }

cat("\n=== SARIMA ANALYSIS =========================================\n")
cat("Data  ->", DATADIR, "\n")
cat("Graph ->", GRAPHDIR, "\n\n")

## ---------------------------------------------------------------------------
## 1. LOAD DATA  (auto-detects the national monthly file + column names)
## ---------------------------------------------------------------------------
cat("[1] Loading data\n")

cands <- list.files(ROOT, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
cands <- cands[!grepl("SARIMA_Output", cands, fixed = TRUE)]   # ignore own output
hit   <- cands[grepl("national.*month|month.*national", basename(cands), ignore.case = TRUE)]
if (length(hit) == 0) hit <- cands[grepl("national", basename(cands), ignore.case = TRUE)]
if (length(hit) == 0) stop("No national monthly CSV found under: ", ROOT)
DATA_FILE <- hit[1]
cat("  file:", basename(DATA_FILE), "\n")

raw <- read.csv(DATA_FILE, stringsAsFactors = FALSE)
names(raw) <- trimws(names(raw))

## --- target column ---
tcol <- grep("WS10M|wind.*speed|^WS$|windspeed", names(raw), ignore.case = TRUE, value = TRUE)
tcol <- tcol[!grepl("lag|roll|std|anom|dir|WD", tcol, ignore.case = TRUE)]
if (length(tcol) == 0) stop("Target column not found. Columns: ", paste(names(raw), collapse = ", "))
TARGET <- tcol[1]
cat("  target:", TARGET, "\n")

## --- date construction ---
ycol <- grep("^year$|^YEAR$|^yr$", names(raw), ignore.case = TRUE, value = TRUE)
mcol <- grep("^month$|^mon$|^mo$", names(raw), ignore.case = TRUE, value = TRUE)
dcol <- grep("^date$|^ym$|^time$", names(raw), ignore.case = TRUE, value = TRUE)

if (length(ycol) && length(mcol)) {
  raw$.Y <- as.integer(raw[[ycol[1]]]); raw$.M <- as.integer(raw[[mcol[1]]])
} else if (length(dcol)) {
  dd <- as.Date(raw[[dcol[1]]])
  if (all(is.na(dd))) dd <- as.Date(paste0(raw[[dcol[1]]], "-01"))
  raw$.Y <- as.integer(format(dd, "%Y")); raw$.M <- as.integer(format(dd, "%m"))
} else stop("Could not build a date. Need Year+Month or a Date column.")

raw <- raw[order(raw$.Y, raw$.M), ]
raw$Date <- as.Date(sprintf("%04d-%02d-01", raw$.Y, raw$.M))
raw <- raw[!is.na(raw[[TARGET]]), ]

## --- restrict to analysis window & verify continuity ---
lo <- as.Date(sprintf("%04d-%02d-01", START[1], START[2]))
hi <- as.Date(sprintf("%04d-%02d-01", TEST_END[1], TEST_END[2]))
dat <- raw[raw$Date >= lo & raw$Date <= hi, c("Date", TARGET)]
names(dat)[2] <- "WS"

expected <- seq(lo, hi, by = "month")
if (nrow(dat) != length(expected) || any(dat$Date != expected))
  stop("Series is not a complete monthly sequence ", format(lo), " to ", format(hi))

cat("  n =", nrow(dat), "months |", format(min(dat$Date)), "->", format(max(dat$Date)), "\n")

## --- ts objects ---
ws_all <- ts(dat$WS, start = START, frequency = FREQ)
ws_tr  <- window(ws_all, start = START,     end = TRAIN_END)
ws_te  <- window(ws_all, start = TEST_START, end = TEST_END)
h      <- length(ws_te)
mo     <- as.integer(format(dat$Date, "%m"))
cat("  train:", length(ws_tr), " | holdout:", h, "\n")

dat$Split <- ifelse(dat$Date <= as.Date(sprintf("%04d-%02d-01", TRAIN_END[1], TRAIN_END[2])),
                    "Train", "Holdout")
sav(dat, "00_analysis_series_1982_2024.csv")   # <- feed this exact file to Python

## ---------------------------------------------------------------------------
## 2. DATA CHARACTERIZATION  (descriptive; full series)
## ---------------------------------------------------------------------------
cat("[2] Data characterization\n")

fig("Fig0a_series_overview.png", 2600, 1500)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(ws_all, ylab = "Wind speed (m/s)", xlab = "Year", col = "grey30", lwd = 0.9,
     main = "(a) Monthly mean wind speed, 1982-2024")
tt <- as.numeric(time(ws_all))
abline(lm(as.numeric(ws_all) ~ tt), col = "red", lwd = 2)
lines(lowess(tt, as.numeric(ws_all), f = 0.20), col = "blue", lwd = 2, lty = 2)
legend("topright", c("Observed", "Linear trend", "LOWESS"),
       col = c("grey30", "red", "blue"), lty = c(1, 1, 2),
       lwd = c(0.9, 2, 2), bty = "n", cex = 0.7)
boxplot(dat$WS ~ mo, xlab = "Month", ylab = "Wind speed (m/s)",
        main = "(b) Distribution by calendar month",
        col = "grey90", border = "grey35", outpch = 20, outcex = 0.6)
lines(1:12, tapply(dat$WS, mo, mean), col = "red", lwd = 2, type = "b", pch = 19)
legend("topright", "Monthly mean", col = "red", lty = 1, lwd = 2,
       pch = 19, bty = "n", cex = 0.7)
endfig("Fig0a_series_overview.png")

sav(data.frame(
  Month = 1:12, Name = month.abb,
  Mean   = round(tapply(dat$WS, mo, mean),   4),
  SD     = round(tapply(dat$WS, mo, sd),     4),
  Min    = round(tapply(dat$WS, mo, min),    4),
  Median = round(tapply(dat$WS, mo, median), 4),
  Max    = round(tapply(dat$WS, mo, max),    4),
  n      = as.integer(table(mo)), row.names = NULL),
  "01_monthly_summary_statistics.csv")

## --- STL decomposition (descriptive only; feeds no model) ---
stl_fit <- stl(ws_all, s.window = "periodic", robust = TRUE)
cmp     <- stl_fit$time.series

fig("Fig0b_stl_decomposition.png", 2400, 2000)
par(mar = c(4, 4, 2, 1))
plot(stl_fit, main = "STL decomposition of monthly mean wind speed (1982-2024)")
endfig("Fig0b_stl_decomposition.png")

sav(data.frame(Date = dat$Date,
               Observed  = round(as.numeric(ws_all), 6),
               Trend     = round(as.numeric(cmp[, "trend"]), 6),
               Seasonal  = round(as.numeric(cmp[, "seasonal"]), 6),
               Remainder = round(as.numeric(cmp[, "remainder"]), 6)),
    "02_stl_components.csv")

v_rem <- var(cmp[, "remainder"])
F_S <- max(0, 1 - v_rem / var(cmp[, "seasonal"] + cmp[, "remainder"]))
F_T <- max(0, 1 - v_rem / var(cmp[, "trend"]    + cmp[, "remainder"]))
strength_tab <- data.frame(
  Measure = c("Seasonal strength (F_S)", "Trend strength (F_T)",
              "Seasonal amplitude (peak-trough, m/s)",
              "Variance share: seasonal (%)", "Variance share: trend (%)",
              "Variance share: remainder (%)"),
  Value = round(c(F_S, F_T,
                  max(cmp[, "seasonal"]) - min(cmp[, "seasonal"]),
                  100 * var(cmp[, "seasonal"]) / var(ws_all),
                  100 * var(cmp[, "trend"])    / var(ws_all),
                  100 * v_rem                  / var(ws_all)), 4),
  Interpretation = c(ifelse(F_S > 0.6, "Strongly seasonal", "Weakly seasonal"),
                     ifelse(F_T > 0.6, "Strong trend", "Weak trend"),
                     "", "", "", ""), row.names = NULL)
sav(strength_tab, "02b_stl_strength_measures.csv")
print(strength_tab)
cat("  NOTE: STL is descriptive only - not used in model fitting.\n")

## ---------------------------------------------------------------------------
## 3. STATIONARITY + IDENTIFICATION  (TRAIN ONLY — leakage guard)
## ---------------------------------------------------------------------------
cat("[3] Stationarity & identification (training window only)\n")

st <- function(x, lab) {
  a <- suppressWarnings(adf.test(x)); k <- suppressWarnings(kpss.test(x, null = "Level"))
  data.frame(Series = lab,
             ADF_stat = round(a$statistic, 4), ADF_p = round(a$p.value, 4),
             ADF_conclusion  = ifelse(a$p.value < 0.05, "Stationary", "Non-stationary"),
             KPSS_stat = round(k$statistic, 4), KPSS_p = round(k$p.value, 4),
             KPSS_conclusion = ifelse(k$p.value < 0.05, "Non-stationary", "Stationary"),
             row.names = NULL)
}
stat_tab <- rbind(
  st(ws_tr,                       "Level"),
  st(diff(ws_tr),                 "First difference (d=1)"),
  st(diff(ws_tr, lag = 12),       "Seasonal difference (D=1)"),
  st(diff(diff(ws_tr, lag = 12)), "First + seasonal difference"))
stat_tab$Suggested_d <- ndiffs(ws_tr)
stat_tab$Suggested_D <- nsdiffs(ws_tr)
sav(stat_tab, "03_stationarity_tests.csv")
cat("  ndiffs =", ndiffs(ws_tr), " nsdiffs =", nsdiffs(ws_tr), "\n")

fig("Fig0c_identification_acf_pacf.png", 2600, 2400)
par(mfrow = c(4, 2), mar = c(4, 4, 3, 1))
Acf (ws_tr, lag.max = 48, main = "(a) Level - ACF")
Pacf(ws_tr, lag.max = 48, main = "(b) Level - PACF")
d1 <- diff(ws_tr)
Acf (d1, lag.max = 48, main = "(c) First difference (d=1) - ACF")
Pacf(d1, lag.max = 48, main = "(d) First difference (d=1) - PACF")
D1 <- diff(ws_tr, lag = 12)
Acf (D1, lag.max = 48, main = "(e) Seasonal difference (D=1) - ACF")
Pacf(D1, lag.max = 48, main = "(f) Seasonal difference (D=1) - PACF")
dD <- diff(diff(ws_tr, lag = 12))
Acf (dD, lag.max = 48, main = "(g) d=1 + D=1 - ACF")
Pacf(dD, lag.max = 48, main = "(h) d=1 + D=1 - PACF")
endfig("Fig0c_identification_acf_pacf.png")

## ---------------------------------------------------------------------------
## 4. MODEL SELECTION — exhaustive grid + auto.arima cross-check
## ---------------------------------------------------------------------------
cat("[4] Grid search (this takes a few minutes)\n")

d <- ndiffs(ws_tr); D <- nsdiffs(ws_tr)
grid <- expand.grid(p = 0:3, q = 0:3, P = 0:2, Q = 0:2)
res  <- data.frame()

for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  if (g$p + g$q + g$P + g$Q == 0) next
  fit <- tryCatch(
    Arima(ws_tr, order = c(g$p, d, g$q),
          seasonal = list(order = c(g$P, D, g$Q), period = FREQ),
          method = "ML"),
    error = function(e) NULL)
  if (is.null(fit)) next
  lb <- Box.test(residuals(fit), lag = 24, type = "Ljung-Box",
                 fitdf = g$p + g$q + g$P + g$Q)
  res <- rbind(res, data.frame(
    Model = sprintf("SARIMA(%d,%d,%d)(%d,%d,%d)[12]", g$p, d, g$q, g$P, D, g$Q),
    p = g$p, d = d, q = g$q, P = g$P, D = D, Q = g$Q,
    n_par = g$p + g$q + g$P + g$Q,
    AIC = round(fit$aic, 3), AICc = round(fit$aicc, 3), BIC = round(fit$bic, 3),
    sigma2 = round(fit$sigma2, 6), loglik = round(fit$loglik, 3),
    LjungBox24_p = round(lb$p.value, 4),
    Residuals_WhiteNoise = ifelse(lb$p.value > 0.05, "Yes", "No")))
  if (i %% 20 == 0) cat("  ", i, "/", nrow(grid), "\r")
}
res_aicc <- res[order(res$AICc), ]
res_bic  <- res[order(res$BIC),  ]
sav(res_aicc,          "04_model_selection_full_grid.csv")
sav(head(res_aicc, 15),"04b_model_selection_top15_AICc.csv")
sav(head(res_bic,  15),"04c_model_selection_top15_BIC.csv")

auto <- auto.arima(ws_tr, seasonal = TRUE, stepwise = FALSE,
                   approximation = FALSE, ic = "aicc")
cat("\n  auto.arima :", paste(arimaorder(auto), collapse = ","), "\n")
cat("  AICc-best  :", res_aicc$Model[1], " (AICc =", res_aicc$AICc[1], ")\n")
cat("  BIC-best   :", res_bic$Model[1],  " (BIC  =", res_bic$BIC[1],  ")\n")

if (is.null(FINAL_ORDER)) {
  FINAL_ORDER    <- c(res_aicc$p[1], res_aicc$d[1], res_aicc$q[1])
  FINAL_SEASONAL <- c(res_aicc$P[1], res_aicc$D[1], res_aicc$Q[1])
}
final_lab <- sprintf("SARIMA(%d,%d,%d)(%d,%d,%d)[12]",
                     FINAL_ORDER[1], FINAL_ORDER[2], FINAL_ORDER[3],
                     FINAL_SEASONAL[1], FINAL_SEASONAL[2], FINAL_SEASONAL[3])
cat("  ADOPTED    :", final_lab, "\n")
if (!identical(final_lab, res_aicc$Model[1]))
  cat("  NOTE: adopted model is not the AICc minimum - see 04c (BIC ranking).\n")

## ---------------------------------------------------------------------------
## 5. CANDIDATE COMPARISON — top AICc models vs adopted, on holdout + CV
## ---------------------------------------------------------------------------
cat("[5] Candidate forecast comparison\n")

metrics <- function(a, f, lab) {
  e <- a - f
  data.frame(Model = lab,
             RMSE = round(sqrt(mean(e^2)), 4),
             MAE  = round(mean(abs(e)), 4),
             MAPE = round(100 * mean(abs(e / a)), 4),
             R2   = round(1 - sum(e^2) / sum((a - mean(a))^2), 4),
             MASE = round(mean(abs(e)) /
                            mean(abs(diff(as.numeric(ws_tr), lag = 12))), 4),
             n = length(a), row.names = NULL)
}

cand <- unique(c(final_lab, head(res_aicc$Model, 5)))
K <- 5; W <- 24
cand_tab <- data.frame()

for (m in cand) {
  rw <- res_aicc[res_aicc$Model == m, ][1, ]
  ord <- c(rw$p, rw$d, rw$q); sea <- c(rw$P, rw$D, rw$Q)
  
  fk <- tryCatch(Arima(ws_tr, order = ord,
                       seasonal = list(order = sea, period = FREQ), method = "ML"),
                 error = function(e) NULL)
  if (is.null(fk)) next
  hm <- metrics(as.numeric(ws_te), as.numeric(forecast(fk, h = h)$mean), m)
  
  cvr <- c()
  for (k in K:1) {
    te_end <- length(ws_all) - (K - k) * W; te_start <- te_end - W + 1
    tr_k <- ts(as.numeric(ws_all)[1:(te_start - 1)], start = START, frequency = FREQ)
    ak   <- as.numeric(ws_all)[te_start:te_end]
    f2 <- tryCatch(Arima(tr_k, order = ord,
                         seasonal = list(order = sea, period = FREQ), method = "ML"),
                   error = function(e) NULL)
    if (is.null(f2)) next
    cvr <- c(cvr, sqrt(mean((ak - as.numeric(forecast(f2, h = W)$mean))^2)))
  }
  
  cand_tab <- rbind(cand_tab, data.frame(
    Model = m, n_par = rw$n_par, AICc = rw$AICc, BIC = rw$BIC,
    Holdout_RMSE = hm$RMSE, Holdout_MAE = hm$MAE,
    Holdout_MAPE = hm$MAPE, Holdout_R2 = hm$R2,
    CV_RMSE_mean = round(mean(cvr), 4), CV_RMSE_sd = round(sd(cvr), 4),
    Adopted = ifelse(m == final_lab, "YES", ""), row.names = NULL))
}
cand_tab <- cand_tab[order(cand_tab$Holdout_RMSE), ]
sav(cand_tab, "05_candidate_model_comparison.csv")
print(cand_tab)

## ---------------------------------------------------------------------------
## 6. FINAL FIT + COEFFICIENTS
## ---------------------------------------------------------------------------
cat("[6] Fitting final model\n")

fit <- Arima(ws_tr, order = FINAL_ORDER,
             seasonal = list(order = FINAL_SEASONAL, period = FREQ), method = "ML")

ct <- coeftest(fit)
coef_tab <- data.frame(
  Model = final_lab, Term = rownames(ct),
  Estimate = round(ct[, 1], 5), Std_Error = round(ct[, 2], 5),
  z_value  = round(ct[, 3], 4), p_value = round(ct[, 4], 5),
  Signif = cut(ct[, 4], c(-Inf, .001, .01, .05, .1, Inf),
               labels = c("***", "**", "*", ".", "ns")), row.names = NULL)
sav(coef_tab, "06_final_model_coefficients.csv")
sav(data.frame(Model = final_lab,
               AIC = round(fit$aic, 3), AICc = round(fit$aicc, 3),
               BIC = round(fit$bic, 3), sigma2 = round(fit$sigma2, 6),
               loglik = round(fit$loglik, 3),
               n_train = length(ws_tr), n_parameters = length(coef(fit))),
    "06b_final_model_fit_statistics.csv")
print(coef_tab)

## ---------------------------------------------------------------------------
## 7. IN-SAMPLE (TRAIN) RESIDUAL DIAGNOSTICS
## ---------------------------------------------------------------------------
cat("[7] Training residual diagnostics\n")

r  <- residuals(fit)
df <- length(coef(fit))

lb <- do.call(rbind, lapply(c(6, 12, 18, 24, 36), function(L) {
  b <- Box.test(r, lag = L, type = "Ljung-Box", fitdf = df)
  data.frame(Test = "Ljung-Box", Lag = L, Statistic = round(b$statistic, 4),
             df = L - df, p_value = round(b$p.value, 4),
             Conclusion = ifelse(b$p.value > 0.05,
                                 "No autocorrelation (adequate)",
                                 "Autocorrelation remains"), row.names = NULL)
}))

sw <- shapiro.test(as.numeric(r)); jb <- jarque.bera.test(as.numeric(r))
norm_tab <- data.frame(
  Test = c("Shapiro-Wilk", "Jarque-Bera"),
  Statistic = round(c(sw$statistic, jb$statistic), 4),
  p_value   = round(c(sw$p.value, jb$p.value), 4),
  Conclusion = ifelse(c(sw$p.value, jb$p.value) > 0.05, "Normal", "Non-normal"))

archlm <- function(e, L = 12) {
  e2 <- as.numeric(e)^2; n <- length(e2)
  X <- embed(e2, L + 1)
  m <- lm(X[, 1] ~ X[, -1]); R2 <- summary(m)$r.squared
  s <- (n - L) * R2
  data.frame(Test = "ARCH-LM", Lag = L, Statistic = round(s, 4),
             p_value = round(pchisq(s, L, lower.tail = FALSE), 4),
             Conclusion = ifelse(pchisq(s, L, lower.tail = FALSE) > 0.05,
                                 "Homoscedastic", "ARCH effects present"))
}
arch_tab <- archlm(r, 12)

sav(lb,       "07_train_residual_ljungbox.csv")
sav(norm_tab, "07b_train_residual_normality.csv")
sav(arch_tab, "07c_train_residual_archlm.csv")
sav(data.frame(Date = dat$Date[dat$Split == "Train"],
               Actual = as.numeric(ws_tr),
               Fitted = round(as.numeric(fitted(fit)), 6),
               Residual = round(as.numeric(r), 6)),
    "07d_train_fitted_residuals.csv")
print(lb); print(norm_tab)

fig("Fig1_train_residual_diagnostics.png", 2400, 2000)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(r, ylab = "Residual", xlab = "Year", main = "(a) Residuals over time",
     col = "grey25"); abline(h = 0, col = "red", lty = 2)
Acf(r,  lag.max = 36, main = "(b) Residual ACF")
Pacf(r, lag.max = 36, main = "(c) Residual PACF")
qqnorm(r, main = "(d) Normal Q-Q", pch = 20, col = "grey30"); qqline(r, col = "red")
endfig("Fig1_train_residual_diagnostics.png")

## ---------------------------------------------------------------------------
## 8. HOLDOUT FORECAST + METRICS
## ---------------------------------------------------------------------------
cat("[8] Holdout forecast (24 months)\n")

fc <- forecast(fit, h = h, level = c(80, 95))
a  <- as.numeric(ws_te); f <- as.numeric(fc$mean); e <- a - f

hold <- metrics(a, f, final_lab)
sav(hold, "08_holdout_metrics_sarima.csv")

fc_tab <- data.frame(
  Date = dat$Date[dat$Split == "Holdout"],
  Actual = round(a, 6), SARIMA_Forecast = round(f, 6), Error = round(e, 6),
  Lo80 = round(as.numeric(fc$lower[, 1]), 6), Hi80 = round(as.numeric(fc$upper[, 1]), 6),
  Lo95 = round(as.numeric(fc$lower[, 2]), 6), Hi95 = round(as.numeric(fc$upper[, 2]), 6),
  Within_95PI = a >= as.numeric(fc$lower[, 2]) & a <= as.numeric(fc$upper[, 2]))
sav(fc_tab, "08b_holdout_forecast_sarima.csv")   # <- merge ML forecasts into this

cov95 <- round(100 * mean(fc_tab$Within_95PI), 2)
cat("  RMSE =", hold$RMSE, " MAE =", hold$MAE,
    " MAPE =", hold$MAPE, " R2 =", hold$R2, "\n")
cat("  95% PI coverage:", cov95, "%\n")

## error split by holdout year (2023 vs 2024)
yr <- as.integer(format(fc_tab$Date, "%Y"))
sav(do.call(rbind, lapply(sort(unique(yr)), function(y) {
  ey <- e[yr == y]
  data.frame(Year = y, n = length(ey),
             Mean_Error = round(mean(ey), 5),
             RMSE = round(sqrt(mean(ey^2)), 5),
             MAE  = round(mean(abs(ey)), 5), row.names = NULL)
})), "08c_holdout_error_by_year.csv")

sav(data.frame(Level = c("80%", "95%"),
               Nominal = c(80, 95),
               Empirical = c(
                 round(100 * mean(a >= fc$lower[, 1] & a <= fc$upper[, 1]), 2),
                 cov95),
               n = h, row.names = NULL), "08d_holdout_pi_coverage.csv")

fig("Fig2_observed_vs_forecast.png", 2600, 1500)
par(mar = c(4, 4, 3, 1))
plot(window(ws_all, start = c(2015, 1)), ylab = "Wind speed (m/s)", xlab = "Year",
     main = paste("Holdout forecast:", final_lab), col = "grey20", lwd = 1.6,
     ylim = range(window(ws_all, start = c(2015, 1)), fc$lower, fc$upper))
polygon(c(time(fc$mean), rev(time(fc$mean))),
        c(fc$upper[, 2], rev(fc$lower[, 2])), col = rgb(0, 0, 1, .12), border = NA)
lines(fc$mean, col = "blue", lwd = 2)
abline(v = 2023, col = "red", lty = 2)
legend("topright", c("Observed", "SARIMA forecast", "95% PI", "Train/holdout split"),
       col = c("grey20", "blue", rgb(0, 0, 1, .3), "red"),
       lty = c(1, 1, 1, 2), lwd = c(1.6, 2, 8, 1), bty = "n", cex = .8)
endfig("Fig2_observed_vs_forecast.png")

## ---------------------------------------------------------------------------
## 9. HOLDOUT (TEST) ERROR DIAGNOSTICS
## ---------------------------------------------------------------------------
cat("[9] Holdout error diagnostics\n")

lb_te <- do.call(rbind, lapply(c(6, 12), function(L) {
  b <- Box.test(e, lag = L, type = "Ljung-Box")
  data.frame(Test = "Ljung-Box (holdout errors)", Lag = L,
             Statistic = round(b$statistic, 4), p_value = round(b$p.value, 4),
             Conclusion = ifelse(b$p.value > 0.05,
                                 "Errors uncorrelated (no unexploited structure)",
                                 "Serial correlation remains"), row.names = NULL)
}))

tt2 <- t.test(e)
bias_tab <- data.frame(Test = "Mean error = 0 (t-test)",
                       Mean_Error = round(mean(e), 5),
                       t_stat = round(tt2$statistic, 4), p_value = round(tt2$p.value, 4),
                       CI_lower = round(tt2$conf.int[1], 5),
                       CI_upper = round(tt2$conf.int[2], 5),
                       Conclusion = ifelse(tt2$p.value > 0.05, "Unbiased", "Biased"),
                       row.names = NULL)

mz  <- lm(a ~ f); V <- vcovHC(mz, type = "HC1")
b   <- coef(mz); Rm <- diag(2); rv <- c(0, 1)
W2  <- as.numeric(t(Rm %*% b - rv) %*% solve(Rm %*% V %*% t(Rm)) %*% (Rm %*% b - rv))
Fst <- W2 / 2; pF <- pf(Fst, 2, length(a) - 2, lower.tail = FALSE)
mz_tab <- data.frame(
  Test = "Mincer-Zarnowitz", Alpha = round(b[1], 5), Beta = round(b[2], 5),
  R2 = round(summary(mz)$r.squared, 4), F_stat = round(Fst, 4),
  p_value = round(pF, 4),
  Conclusion = ifelse(pF > 0.05, "Forecast efficient (alpha=0, beta=1 not rejected)",
                      "Efficiency rejected"), row.names = NULL)

mo_te <- as.integer(format(fc_tab$Date, "%m"))
aovm  <- summary(aov(e ~ factor(mo_te)))[[1]]
seas_tab <- data.frame(Test = "Seasonality in holdout errors (ANOVA by month)",
                       F_stat = round(aovm$`F value`[1], 4),
                       p_value = round(aovm$`Pr(>F)`[1], 4),
                       Conclusion = ifelse(aovm$`Pr(>F)`[1] > 0.05,
                                           "No residual seasonality",
                                           "Residual seasonality present"),
                       row.names = NULL)

sav(lb_te,    "09_holdout_error_ljungbox.csv")
sav(bias_tab, "09b_holdout_error_bias_test.csv")
sav(mz_tab,   "09c_holdout_mincer_zarnowitz.csv")
sav(seas_tab, "09d_holdout_error_seasonality.csv")
print(lb_te); print(bias_tab); print(mz_tab)
cat("  NOTE: n = 24 -> low power; report as supporting evidence.\n")

fig("Fig3_holdout_error_diagnostics.png", 2400, 2000)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(fc_tab$Date, e, type = "h", lwd = 3, col = "grey30",
     xlab = "", ylab = "Error (m/s)", main = "(a) Holdout errors over time")
abline(h = 0, col = "red", lty = 2)
Acf(e, lag.max = 12, main = "(b) ACF of holdout errors")
boxplot(e ~ mo_te, xlab = "Month", ylab = "Error (m/s)",
        main = "(c) Errors by calendar month", col = "grey90")
abline(h = 0, col = "red", lty = 2)
plot(f, a, pch = 19, col = "grey25", xlab = "Forecast (m/s)", ylab = "Actual (m/s)",
     main = "(d) Mincer-Zarnowitz")
abline(0, 1, col = "red", lty = 2); abline(mz, col = "blue", lwd = 1.8)
legend("topleft", c("45-degree line", "Fitted"), col = c("red", "blue"),
       lty = c(2, 1), bty = "n", cex = .75)
endfig("Fig3_holdout_error_diagnostics.png")

## ---------------------------------------------------------------------------
## 10. ROLLING-ORIGIN CROSS-VALIDATION (5 folds x 24 months, expanding window)
## ---------------------------------------------------------------------------
cat("[10] Rolling-origin cross-validation\n")

cv <- data.frame(); cvf <- data.frame()
for (k in K:1) {
  te_end   <- length(ws_all) - (K - k) * W
  te_start <- te_end - W + 1
  tr_k <- ts(as.numeric(ws_all)[1:(te_start - 1)], start = START, frequency = FREQ)
  ak   <- as.numeric(ws_all)[te_start:te_end]
  
  fk <- tryCatch(Arima(tr_k, order = FINAL_ORDER,
                       seasonal = list(order = FINAL_SEASONAL, period = FREQ),
                       method = "ML"), error = function(e) NULL)
  if (is.null(fk)) next
  pk <- as.numeric(forecast(fk, h = W)$mean)
  
  m <- metrics(ak, pk, final_lab)
  cv <- rbind(cv, data.frame(
    Fold = K - k + 1, Train_n = length(tr_k),
    Test_start = format(dat$Date[te_start], "%Y-%m"),
    Test_end   = format(dat$Date[te_end],   "%Y-%m"),
    RMSE = m$RMSE, MAE = m$MAE, MAPE = m$MAPE, R2 = m$R2))
  cvf <- rbind(cvf, data.frame(Fold = K - k + 1, Date = dat$Date[te_start:te_end],
                               Actual = round(ak, 6), Forecast = round(pk, 6),
                               Error = round(ak - pk, 6)))
}
cv <- cv[order(cv$Fold), ]

cv_sum <- data.frame(
  Model = final_lab, n_folds = nrow(cv), Test_window = W,
  RMSE_mean = round(mean(cv$RMSE), 4), RMSE_sd = round(sd(cv$RMSE), 4),
  MAE_mean  = round(mean(cv$MAE),  4), MAE_sd  = round(sd(cv$MAE),  4),
  MAPE_mean = round(mean(cv$MAPE), 4), MAPE_sd = round(sd(cv$MAPE), 4),
  R2_mean   = round(mean(cv$R2),   4), R2_sd   = round(sd(cv$R2),   4))

sav(cv,     "10_cv_fold_metrics_sarima.csv")
sav(cv_sum, "10b_cv_summary_sarima.csv")
sav(cvf,    "10c_cv_forecasts_sarima.csv")   # <- for DM tests against ML later
print(cv); print(cv_sum)

fig("Fig4_cv_fold_rmse.png", 2200, 1400)
par(mar = c(4, 4, 3, 1))
bp <- barplot(cv$RMSE, names.arg = paste0("Fold ", cv$Fold, "\n", cv$Test_start),
              ylab = "RMSE (m/s)", col = "grey75", border = NA,
              main = paste("Rolling-origin CV RMSE -", final_lab),
              ylim = c(0, max(cv$RMSE) * 1.25), cex.names = .75)
text(bp, cv$RMSE, round(cv$RMSE, 4), pos = 3, cex = .75)
abline(h = mean(cv$RMSE), col = "blue", lty = 2, lwd = 1.6)
abline(h = hold$RMSE,     col = "red",  lty = 3, lwd = 1.6)
legend("topleft", c(sprintf("CV mean = %.4f", mean(cv$RMSE)),
                    sprintf("Holdout = %.4f", hold$RMSE)),
       col = c("blue", "red"), lty = c(2, 3), lwd = 1.6, bty = "n", cex = .8)
endfig("Fig4_cv_fold_rmse.png")

## ---------------------------------------------------------------------------
## 11. MASTER SUMMARY
## ---------------------------------------------------------------------------
sav(data.frame(
  Item = c("Data file", "Target variable", "Analysis period", "Training window",
           "Holdout window", "AICc-best model", "BIC-best model", "Adopted model",
           "AICc (adopted)", "BIC (adopted)", "Ljung-Box(24) train p",
           "Shapiro-Wilk p", "ARCH-LM(12) p",
           "Holdout RMSE", "Holdout MAE", "Holdout MAPE (%)", "Holdout R2",
           "Holdout MASE", "95% PI coverage (%)",
           "CV mean RMSE", "CV RMSE sd"),
  Value = c(basename(DATA_FILE), TARGET,
            paste(format(min(dat$Date)), "to", format(max(dat$Date))),
            paste(length(ws_tr), "months (1982-01 to 2022-12)"),
            paste(h, "months (2023-01 to 2024-12)"),
            res_aicc$Model[1], res_bic$Model[1], final_lab,
            round(fit$aicc, 3), round(fit$bic, 3),
            lb$p_value[lb$Lag == 24],
            norm_tab$p_value[norm_tab$Test == "Shapiro-Wilk"],
            arch_tab$p_value,
            hold$RMSE, hold$MAE, hold$MAPE, hold$R2, hold$MASE, cov95,
            cv_sum$RMSE_mean, cv_sum$RMSE_sd)),
  "11_SUMMARY_sarima.csv")

cat("\n=== DONE ====================================================\n")
cat("Data : ", length(list.files(DATADIR,  pattern = "csv$")), " csv\n", sep = "")
cat("Graph: ", length(list.files(GRAPHDIR, pattern = "png$")), " png\n", sep = "")