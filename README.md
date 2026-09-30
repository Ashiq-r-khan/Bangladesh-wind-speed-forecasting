# Monthly wind speed forecasting in Bangladesh: SARIMA vs machine learning

Code, data and outputs for comparing a seasonal ARIMA model with five machine learning models on national monthly mean wind speed in Bangladesh, 1982–2024.

The target is the monthly mean of daily 10 m wind speed (WS10M) from NASA POWER, averaged over 64 district points. SARIMA runs in R. Random Forest, XGBoost, SVR, LSTM and a hybrid SARIMA+XGBoost run in Python. All six are compared on a 24-month holdout, a 5-fold rolling-origin cross-validation and Diebold-Mariano tests.

## Results

Holdout, January 2023 to December 2024 (24 months):

| Rank | Model | RMSE (m/s) | MAE (m/s) | MAPE (%) | R² |
|---|---|---|---|---|---|
| 1 | Random Forest | 0.2360 | 0.1679 | 6.44 | 0.8279 |
| 2 | SARIMA(0,1,1)(0,1,1)[12] | 0.2469 | 0.1648 | 6.38 | 0.8116 |
| 3 | LSTM | 0.2496 | 0.1753 | 6.99 | 0.8076 |
| 4 | XGBoost | 0.2575 | 0.1774 | 6.70 | 0.7952 |
| 5 | Hybrid SARIMA+XGBoost | 0.2799 | 0.2043 | 7.99 | 0.7580 |
| 6 | SVR | 0.3693 | 0.2651 | 9.99 | 0.5788 |

Rolling-origin cross-validation, 5 folds of 24 months:

| Model | RMSE mean | RMSE SD | MAE mean | MAPE mean (%) | R² mean |
|---|---|---|---|---|---|
| Random Forest | 0.2639 | 0.0575 | 0.2106 | 8.16 | 0.8191 |
| SARIMA | 0.2672 | 0.0416 | 0.1978 | 7.49 | 0.8169 |
| XGBoost | 0.2718 | 0.0389 | 0.2146 | 8.19 | 0.8071 |
| Hybrid | 0.2896 | 0.0387 | 0.2265 | 8.54 | 0.7839 |
| LSTM | 0.3076 | 0.0457 | 0.2359 | 9.20 | 0.7570 |
| SVR | 0.3341 | 0.0710 | 0.2644 | 10.29 | 0.7048 |

Diebold-Mariano tests against SARIMA (squared-error loss, Holm correction): no model is significantly better than SARIMA, on the holdout (n = 24) or on the pooled cross-validation forecasts (n = 120). SVR is significantly worse on the pooled cross-validation (adjusted p = 0.0015).

Random Forest has the lowest RMSE, but its gap to SARIMA is small and not significant. SARIMA has the lowest holdout MAE and MAPE with only two parameters.

## Study design

- Train: 1982-01 to 2022-12 (492 months)
- Holdout: 2023-01 to 2024-12 (24 months)
- 1981 is only a 12-month runway for the lag-12 and rolling-12 features. It is never predicted.
- Cross-validation: expanding window, 5 folds of 24 months. Fold 1 is the holdout window and fold 5 is 2015-01 to 2016-12.
- SARIMA order (0,1,1)(0,1,1)[12] was chosen on the training data by BIC.
- SARIMA forecasts all 24 holdout months from December 2022. The ML models forecast one month ahead, using observed lags and same-month covariates.

The ML models use 21 predictors:

| Group | Features |
|---|---|
| AR lags | `WS_lag1`, `WS_lag2`, `WS_lag3`, `WS_lag6`, `WS_lag12` |
| Rolling | `WS_roll3`, `WS_roll12`, `WS_std3` |
| Calendar | `Month` |
| Meteorological | `PS`, `T2M`, `RH2M`, `WD2M` |
| Teleconnections | `Nino34_Anomaly`, `IOD_DMI` |
| ENSO lags | `Nino34_lag2`, `Nino34_lag3`, `Nino34_lag6` |
| IOD lags | `IOD_lag2`, `IOD_lag3`, `IOD_lag6` |

### Leakage checks

An earlier version of the pipeline built the rolling features with right-aligned windows, so they included the wind speed of the month being predicted. This was found and fixed. Rolling features are now built after `.shift(1)`, every scaler is fit on training rows only, and section 4 of the notebook runs an audit that stops the run if either rule is broken.

## Data

| File | Content | Source |
|---|---|---|
| `Aggregated_Data/national_monthly_windspeed.csv` | National monthly WS10M (mean, SD, min, max over 64 districts) | NASA POWER daily point data, aggregated by `Data aggregation.R` |
| `Climate_Indices/POWER_Regional_Monthly_..._20241231_...` (3 files) | Monthly PS, T2M and RH2M on a regional grid, 1981–2024 | NASA POWER |
| `Climate_Indices/POWER_Regional_Monthly_..._20251231_...` | Monthly WD2M on a regional grid, 1981–2025 | NASA POWER |
| `Climate_Indices/nino34.data`, `iod_dmi.data` | Raw monthly Niño 3.4 and IOD DMI series | NOAA |
| `Climate_Indices/climate_indices_complete.csv` | Niño 3.4 anomaly and IOD DMI with phases and lags | Built from the two NOAA files |

The raw daily files for the 64 districts are not included. `Data aggregation.R` expects one CSV per district in a single folder, in the standard NASA POWER daily point format. The daily WS10M series for each district can be downloaded as point data from the [NASA POWER Data Access Viewer](https://power.larc.nasa.gov/data-access-viewer/).

The script that built `climate_indices_complete.csv` from the NOAA files is not in this repository yet.

## Repository structure

```
Data aggregation.R                          daily district files -> Aggregated_Data/
NASA POWER Regional Data Processing for Bangladesh.R
EXTEND NATIONAL CLIMATE DATASET TO 2025.R
EXOGENOUS DATASET PREPARATION (1981–2025).R
Aggregated_Data/                            national monthly wind speed
Climate_Indices/                            NASA POWER covariates, ENSO and IOD
R program/
    SARIMA ANALYSIS — Monthly Wind Speed, Bangladesh (1982–2024).R
    PATCH — export 1981-start series for ML feature engineering.R
    DM test script.R
Python programming/
    Step_2_ML_Forecasting_Bangladesh_Wind.ipynb
SARIMA_Output/
    Data/  Graph/                           SARIMA results and figures
    ML_Input/                               inputs for the notebook
ML_Output/
    Data/  Graph/                           ML results and figures
DM_Test_Output/
    Data/  Graph/                           Diebold-Mariano results
```

## How to run

Every script uses absolute Windows paths (`E:/A paper work/...`). Change `ROOT`, `base_path`, `output_path` or `ML_DIR` at the top of each script to your own folder.

Steps 1 to 4 prepare the inputs. Their outputs are already in the repository, so you can start from step 5.

1. `Data aggregation.R`: reads the daily district files and writes `Aggregated_Data/national_monthly_windspeed.csv`. Set `data_path` to the folder with the raw files.
2. `NASA POWER Regional Data Processing for Bangladesh.R`: averages the three regional files and writes `National_Climate_Bangladesh_1981_2024.csv` (PS, T2M, RH2M).
3. `EXTEND NATIONAL CLIMATE DATASET TO 2025.R`: adds WD2M from the 2025 file and writes `National_Climate_Bangladesh_1981_2025.csv`.
4. `EXOGENOUS DATASET PREPARATION (1981–2025).R`: joins the NASA POWER variables with ENSO and IOD and writes `Exogenous_Variables_Bangladesh_1981_2025.csv`.
5. The SARIMA ANALYSIS script in `R program/`: stationarity tests, model selection, diagnostics, holdout forecast and cross-validation. Writes `SARIMA_Output/Data` and `SARIMA_Output/Graph`.
6. The PATCH script in `R program/`: writes the four notebook inputs and a manifest to `SARIMA_Output/ML_Input`.
7. `Python programming/Step_2_ML_Forecasting_Bangladesh_Wind.ipynb`: run on Google Colab. It mounts Google Drive, looks for a folder in My Drive whose name contains "paper publication", and reads `ML_Input` from inside it. Copy the project folder there first. Writes `ML_Output`.
8. `R program/DM test script.R`: reads the merged forecasts from `ML_Output/Data` and writes `DM_Test_Output`.

## Requirements

R: `tidyverse`, `data.table`, `lubridate`, `forecast`, `tseries`, `lmtest`, `sandwich`

Python, as run on Colab: numpy 2.1.3, pandas 2.2.3, xgboost 3.4.1, tensorflow 2.20.0, scikit-learn, statsmodels, matplotlib

## Author

Md. Ashiqur Rahman Khan, B.Sc. in Statistics, Mawlana Bhashani Science and Technology University (MBSTU), Tangail, Bangladesh. Supervisor: Somaresh Kumar Mondal.

## Citation

A citation will be added here after the paper is published.

## License

Copyright (c) 2026 Md. Ashiqur Rahman Khan. All rights reserved.

The code is shared so the analysis can be read and checked. It is not licensed for reuse, modification or redistribution. If you want to use any part of it, please contact me first.

NASA POWER and NOAA data are subject to their providers' terms.
