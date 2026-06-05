# MS Business Analytics — Drexel University

Graduate coursework in data science, machine learning, statistics, and business analytics. All code is my own work from homework assignments, quizzes, exams, and course projects.

**Program:** MS in Business Analytics  
**School:** Drexel University, LeBow College of Business  
**Languages:** R, Python, SQL

---

## Courses with Code

### BSAN 615 — Data Visualization & Analytics
**Language:** R (`ggplot2`, `dplyr`, `tidyr`)  
**What it covered:** Principles of effective data visualization, critique of existing charts, and building polished, publication-ready graphics in R.

**Code:**
- `assignment_1/coding_assignment1.R` — Avocado market trend visualizations
- `assignment_2/assignment_2.R` — Vehicle fleet maintenance cost analysis
- `assignment_3/assignment_3.R` — Multi-variable chart critique and redesign
- `midterm/midterm.R` — Midterm exam (in-class)
- `BSAN615_FINAL/BSAN615_final.R` — Final project: Venture capital investment trend analysis
- `final_charts.R` — Supplemental chart exports

---

### MIS 612 — Aligning Information Systems & Business Strategies
**Language:** Python / Jupyter Notebooks (`pandas`, `scikit-learn`, `xgboost`)  
**What it covered:** Applied machine learning to real business datasets; alignment of data science outputs with strategic decision-making.

**Code:**
- `corbett_austin_assignment1.ipynb` — Classification on chronic kidney disease dataset
- `corbett_austin_assignment2.ipynb` — Telco churn prediction
- `corbett_austin_assignment3.ipynb` — Employee attrition modeling
- `xgboost_ml_desc.ipynb` — XGBoost deep-dive and model interpretation

---

### MIS 636 — Python Programming for Business Applications
**Language:** Python / Jupyter Notebooks  
**What it covered:** Python fundamentals through a business lens — data manipulation, automation, and machine learning workflows.

**Code:**
- `Assignment_1(3).ipynb` — Python fundamentals and business data tasks
- `Assignment_3.ipynb` — Advanced data processing assignment
- `xgboost_ml_v3-ba.ipynb` — End-to-end XGBoost ML pipeline (1,200+ lines)

---

### MIS 652 — Business Agility and IT
**Language:** R, Python (`ranger`, `xgboost`, `caret`, `pROC`)  
**What it covered:** Agile frameworks applied to IT strategy; capstone group project built a wildfire risk scoring system using satellite data.

**Code:**
- `data/wildfire_risk_model.R` — Random Forest + XGBoost wildfire risk scoring model using MODIS satellite detections (2014–2024) and MTBS fire perimeter data; temporal block cross-validation
- `data/wildfire_insurance_analysis.py` — Insurance exposure analysis tied to wildfire risk index
- `data/gridmet_integration.R` — Climate/weather data ingestion from GridMET API
- `data/test_climateR.R` / `test_gridmet.R` — API integration test scripts

---

### STAT 610 — Statistics for Business Analytics
**Language:** R (`lm`, `aov`, base stats)  
**What it covered:** Probability, hypothesis testing, ANOVA, simple and multiple linear regression.

**Code:**
- `corbett_austin_midterm.R` — Midterm exam (regression, ANOVA, distributions)
- `corbett_austin_final.R` — Final exam (multiple regression, model diagnostics)

---

### STAT 640 — Predictive Analytics and Machine Learning
**Language:** R (`caret`, `xgboost`, `catboost`, `dplyr`, `ggplot2`)  
**What it covered:** Full ML pipeline from EDA through model deployment — decision trees, random forests, boosting, and ensembles. Included a live Kaggle competition (product return prediction).

**Code — Weekly Assignments:**
- `wk2_assignment/wk2_assignment.R` — Exploratory data analysis and 2D visualization
- `wk3_assignment/wk3_assignment.R` — Decision trees and resampling
- `wk4_assignment/wk4_assignment.R` — Random forests
- `wk5_assignment/wk5_assignment.R` — Boosting methods
- `wk6_assignment/wk6_assignment.R` — Model tuning and comparison
- `wk7_assignment/wk7_assignment.R` — Ensemble methods

**Code — Kaggle Competition (Product Return Prediction):**
- `kaggle_competition/preprocessing.R` — Full preprocessing pipeline
- `kaggle_competition/feature_engineering_v2.R` / `v3.R` — Feature engineering iterations
- `kaggle_competition/modeling_v3.R` — Model training and evaluation
- `kaggle_competition/xg_boost_v2.R` / `xg_boost_fine.R` — XGBoost experiments
- `kaggle_competition/cat_boost_base.R` — CatBoost baseline
- `kaggle_competition/cat_xg_blend.R` — Model blending (XGBoost + CatBoost ensemble)
- `kaggle_competition/model_fold.R` — K-fold cross-validation framework

---

### STAT 645 — Time Series Forecasting
**Language:** R (`fpp3`, `fable`, `feasts`, `tsibble`)  
**What it covered:** Classical and modern time series methods — decomposition, ETS, ARIMA, regression with ARIMA errors, and forecast evaluation.

**Code — Homework:**
- `hw_1/quiz_1.R` — Chapter 1 exercises (time series basics, decomposition)
- `hw_2/` — 20+ individual scripts covering ACF/PACF, Ljung-Box tests, trend and seasonal modeling (chapters 1–4)
- `hw_3/hw_3.R` + diagnostics — ETS models, TSLM, residual diagnostics
- `hw_4/hw_4.R` — ARIMA modeling

**Code — Assessments:**
- `hw_2/quiz_2.R` — Quiz 2
- `quiz3/quiz_3.R` — Quiz 3

**Code — Final Project:**
- `stat645_final/stat645_final_project.R` — Forecasting Rhode Island median home listing prices (FRED/Zillow data); full pipeline including EDA, train/test split, ETS vs. ARIMA comparison, residual diagnostics, and 12-month forward forecast

---

## Skills & Tools

| Area | Tools |
|------|-------|
| Data Visualization | ggplot2, plotly, R base graphics |
| Machine Learning | xgboost, catboost, caret, scikit-learn, ranger |
| Time Series | fpp3, fable, feasts, tsibble, forecast |
| Statistical Modeling | lm, aov, glm, ARIMA, ETS |
| Data Wrangling | dplyr, tidyr, pandas, data.table, lubridate |
| Languages | R, Python, SQL (coursework) |

---

## Notable Projects

**Wildfire Risk Scoring Model** *(MIS 652)*  
Predicts wildfire risk (0–100) for any California/West Coast grid cell using MODIS satellite fire detections and MTBS burn perimeter records. Compares Random Forest, XGBoost, and logistic regression baselines under temporal block cross-validation.

**Rhode Island Home Price Forecasting** *(STAT 645)*  
End-to-end time series analysis of FRED median listing price data. Benchmarks multiple ETS and ARIMA specifications, evaluates holdout accuracy, and produces a 12-month forward forecast with confidence intervals.

**Kaggle Competition — Product Return Prediction** *(STAT 640)*  
Built a full ML pipeline (preprocessing → feature engineering → XGBoost/CatBoost ensemble) to predict e-commerce product returns. Iterated through multiple model versions and blending strategies.

**Venture Capital Investment Visualization** *(BSAN 615)*  
Designed a suite of polished ggplot2 charts analyzing VC investment trends by sector, stage, and geography for a final project presentation.

---

## Non-Coding Courses

| Course | Description |
|--------|-------------|
| ENTP 540 — Approaches to Entrepreneurship | Readings and discussion on entrepreneurial theory |
| ENTP 690 — The Lean Launch | Lean startup methodology; built FishTrackPro (computer vision fishing app) as a venture concept |
| MGMT 715 — Business Consulting | Consulting engagement with Jawnt (transit benefits company) |
| MIS 632 — Database Analysis and Design | Database design principles, ER modeling, SQL |
| MIS 642 — Emerging Technologies | Research on drone swarm intelligence and enterprise adoption |
| OPR 601 — Managerial Decision Models & Simulation | Decision analysis and simulation modeling |
