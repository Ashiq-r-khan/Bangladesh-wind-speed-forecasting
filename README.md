# 🌬️ Long-Term Trend Analysis and Multi-Model Forecasting of Monthly Wind Speed in Bangladesh (1981–2025)

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
![Status: In Progress](https://img.shields.io/badge/Status-In%20Progress-yellow)
![Language: R & Python](https://img.shields.io/badge/Language-R%20%7C%20Python-green)

> A comparative study of SARIMA and Machine Learning approaches (Random Forest, XGBoost, SVR, LSTM, Hybrid) for monthly wind speed forecasting across 64 districts of Bangladesh, with applications to agriculture and disaster management.

---

## 📌 Project Overview

This project analyzes **44 years (1981–2025)** of monthly wind speed data sourced from **NASA POWER** across all 64 districts of Bangladesh. It establishes a **SARIMA baseline** and benchmarks it against modern machine learning models, incorporating climate teleconnection indices (ENSO, IOD) as exogenous features.

**Author:** Ashik  
**Institution:** Mawlana Bhashani Science and Technology University (MBSTU), Tangail, Bangladesh  
**Degree:** B.Sc. in Statistics (Final Year Project)  
**Target Publication:** Regional journal (e.g., *Climate*, *Energy Reports*, *IJRED*)

---

## 📊 Baseline Results — SARIMA(0,1,1)(0,1,1)[12]

| Metric | Value |
|--------|-------|
| MAPE   | 6.64% |
| RMSE   | 0.248 |
| R²     | 0.8515 |
| Train  | 1981–2022 |
| Test   | 2023–2025 |

---

## 🗂️ Dataset Description

| File | Description |
|------|-------------|
| `data/raw/national_monthly_windspeed.csv` | Monthly national aggregate wind speed (mean, SD, min, max) across 64 districts, 1981–2025 |
| `data/raw/district_monthly_windspeed_cleaned.csv` | District-level monthly wind speed for all 64 districts, 1981–2025 |
| `data/raw/Complete_SARIMAX_Variables_Bangladesh.csv` | Exogenous variables: PS, T2M, RH2M, Niño3.4, IOD DMI (1981–2024) |
| `data/climate_indices/nino34_processed.csv` | Monthly Niño 3.4 SST anomalies and ENSO phase classification (1981–2025) |
| `data/climate_indices/iod_dmi_processed.csv` | Monthly IOD DMI index and phase classification (1981–2025) |
| `data/climate_indices/climate_indices_complete.csv` | Combined ENSO + IOD with lags and interaction terms |

> **Data Source:** [NASA POWER](https://power.larc.nasa.gov/) | NOAA ENSO/IOD indices

---

## 🤖 Models Compared

| Model | Type | Key Features Used |
|-------|------|-------------------|
| SARIMA(0,1,1)(0,1,1)[12] | Statistical baseline | Seasonal differencing |
| Random Forest | ML Ensemble | Lag-1/2/3/6/12, rolling mean, season, climate indices |
| XGBoost | ML Gradient Boosting | Same as RF + lagged ENSO/IOD (2–6 months) |
| SVR | ML Kernel Method | Same feature set |
| LSTM | Deep Learning | Sequence-based temporal patterns |
| Hybrid SARIMA+XGBoost | Combined | SARIMA residuals modeled by XGBoost |

---

## 🧪 Validation Strategy

- **Rolling-origin cross-validation** — 5 folds × 24-month test windows
- **Evaluation Metrics:** RMSE, MAE, MAPE, R²
- **Statistical significance:** Diebold-Mariano (DM) test for model comparison

---

## 🔬 Key Features Engineered

- Lag features: Wind speed at lag-1, 2, 3, 6, 12 months
- Rolling statistics: 3-month and 12-month rolling mean
- Temporal features: Month, Season (Pre-monsoon, Monsoon, Post-monsoon, Winter)
- Climate teleconnections: ENSO (Niño3.4), IOD (DMI) — raw + lagged 2–6 months
- Atmospheric covariates: Surface pressure (PS), Temperature (T2M), Humidity (RH2M)

---

## 📈 Extensions & Applied Chapters

1. **Mann-Kendall trend test + Sen's slope** — National and district-level series
2. **District-level case studies** — 3–6 representative districts (coastal, inland, hilly)
3. **Cyclone month error analysis** — Model reliability during extreme wind events
4. **Wind–rice yield linkage** — Applied chapter for agricultural implications
5. **Spray advisory threshold analysis** — Practical implication for crop protection

---

## 📁 Repository Structure
