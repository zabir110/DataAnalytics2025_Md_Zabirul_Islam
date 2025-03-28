
Summary of Lab Tasks Completed
==============================

Overall Goal:
-------------
You worked with multiple datasets and applied classification and regression techniques to explore and model housing and wine-related data. The analyses included data preprocessing, model training, hyperparameter tuning, evaluation, and comparison across multiple algorithms.

1. Wine Dataset Classification
------------------------------
- Dataset: Wine characteristics with class labels.
- Models:
  - SVM (Linear kernel) and SVM (Radial kernel) with hyperparameter tuning using tune.svm.
  - k-Nearest Neighbors (k-NN) with k = 5.
- Features Used: All features is selected subset including "Alcohol", "Flavanoids", "Color intensity", "Hue", and "Proline".
- Evaluation Metrics: Precision, Recall, and F1 Score using confusionMatrix() from the caret package.
- Fixes Applied:
  - Standardized feature names with make.names() to resolve prediction errors.
  - Harmonized column names between training and testing sets to avoid SVM formula errors.

2. NY Housing Dataset Regression
--------------------------------
- Dataset: Real estate data with features like PRICE, PROPERTYSQFT, and geospatial information.
- Preprocessing:
  - Removed outliers (top 1% of price).
  - Removed extreme or erroneous entries (e.g., square footage = 2184.207862).
  - Removed missing values.
  - Created log-transformed variables: logPRICE, logPROPERTYSQFT.
- Models:
  - SVM Regression: svm(logPRICE ~ logPROPERTYSQFT).
  - Linear Regression: lm(logPRICE ~ logPROPERTYSQFT).
- Evaluation:
  - Plotted predicted vs actual log prices.
  - Residual plots for both models to visually assess prediction errors.

Summary of Skills and Methods Used
----------------------------------
- Data Cleaning & Feature Engineering
- Supervised Learning:
  - Classification: SVM, k-NN
  - Regression: SVM Regression, Linear Regression
- Model Tuning (tune.svm)
- Evaluation Metrics:
  - Precision, Recall, F1
  - Residual analysis
- Data visualization using ggplot2
- Variable transformation (log-scale)
