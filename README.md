# COBRA Study

## Overview
The COBRA Study examines the effects of cognitive states on compliance, careless responding, and delayed responding in an Ecological Momentary Assessment (EMA) study. This repository contains R and Python code for analyzing these effects, including scripts for preprocessing, mixed-effects models, visualizations, and supplementary analyses to ensure reproducibility and support further research.

## Project Structure
- **scripts/**: Includes R scripts for preprocessing, analysis, and visualization.
  - `preprocessing.ipynb`: Handles preprocessing, feature engineering, data cleaning, and preparation of datasets for analysis.
  - `analysis.R`: Implements mixed-effects models to analyze next-prompt straight-lining, compliance, and response time using generalized linear models (GLMs) and linear mixed-effects models (LMMs). Outputs include coefficients, odds ratios, and confidence intervals.
  - `visualization.R`: Generates plots and visual summaries of results, including correlation matrices and other visualizations relevant to the analyses.
- **README.md**: This file, providing an overview of the project.

## Analysis Scripts
### 1. **preprocessing.ipynb**
This Jupyter Notebook contains code for:
- Reading and cleaning the data.
- Standardizing key variables (e.g., sleep quality, stress) for analysis.
- Handling missing values using imputation and outlier detection.
- Feature engineering

### Python Requirements
For the preprocessing Python script, you will need:
- `pandas`
- `numpy`
- `seaborn`
- `scikit-learn`
- `matplotlib`
- `tqdm`
- `openpyxl`

### 2. **analysis.R**
This script performs the following analyses:
- **Next-prompt straight-lining model**: Analyzes the effects of cognitive states (e.g., sleep quality, stress, affect) on the likelihood of straight-lining responses.
- **Next-prompt compliance model**: Evaluates factors influencing compliance in the EMA study.
- **TPI (Time Per Item) model**: Analyzes time taken per survey item, including sensitivity analyses for upper fence criteria.
- **Response time model**: Assesses factors affecting response delay, modeling response times using log-transformed values.

Outputs from this script include:
- Coefficients, odds ratios, and confidence intervals saved as CSV files for each model.

### 3. **visualization.R**
This script generates various plots and visualizations, including:
- Correlation matrices for cognitive and motivational predictors.
- Graphical summaries of model results to facilitate understanding and interpretation of findings.

## Requirements
To run the analysis and visualization scripts, ensure you have the following packages installed in R:
- `tidyverse`
- `arrow`
- `nlme`
- `readr`
- `car`
- `ggplot2`
- `lme4`
- `sjstats`
- `effectsize`
- `gap`
- `DHARMa`
- `lmerTest`
- `effects`
- `MuMIn`
- `dsem`
- `moments`
- `sjPlot`
