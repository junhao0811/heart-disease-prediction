# Heart Disease Prediction & Statistical Modelling in R

Statistical modelling and machine learning in R, from probability models through to predictive classifiers.

## Projects

### 🫀 `heart-disease-and-regression/` — the main project
| Script | What it does |
|---|---|
| `heart_disease_classification.R` | Predicts heart disease with a **decision tree** (`rpart`) and **stepwise logistic regression** (BIC), evaluated with ROC/AUC (`pROC`) and **bootstrap confidence intervals** for individual patients' risk |
| `concrete_strength_regression.R` | **Multiple linear regression** predicting concrete compressive strength from its ingredients |
| `mass_spec_knn.R` | **k-nearest-neighbour regression** (`kknn`) to smooth mass-spectrometry signals, with cross-validated *k* and bootstrap CIs |

### 🎲 `probability-models/`
- `afl_win_loss_streaks.R`: how often Port Adelaide (AFL) wins or loses after a previous win or loss
- `dog_bite_poisson.R`: **Poisson model** of daily dog-bite hospital admissions

### ⛽ `hypothesis-testing/`
- `fuel_economy_analysis.R`: estimates and **hypothesis tests** comparing fuel efficiency of all-wheel-drive and part-time four-wheel-drive vehicles

Each folder has a `report.pdf` with the full write-up and results.

## Run it
Open R in a project folder and `source()` a script. Datasets are included.
Packages: `rpart`, `boot`, `kknn`, `pROC`.
