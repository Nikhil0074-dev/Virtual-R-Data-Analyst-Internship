# Week 1: Data Cleaning and Preliminary Analysis with R

End-to-end cleaning, preprocessing and exploratory analysis of the Titanic passenger dataset (891 rows, 12 variables) in R.

## What is covered
- Missing-value detection (including hidden blank strings) and a test of whether missingness is random
- Imputation: mode (Embarked), group median by Title x Pclass (Age), class median (zero Fare), deck labels (Cabin)
- Feature engineering: Title, FamilySize, IsAlone, HasCabin, Deck, missing-value flags
- Outlier detection (IQR and z-score), winsorising and log transform for Fare
- Normalisation (min-max, z-score) and encoding (binary, ordinal, one-hot)
- EDA: str(), summary(), descriptive statistics, 11 figures, correlations, chi-square / Wilcoxon / t-tests, logistic regression

## Run it
1. Install packages: `install.packages(c("ggplot2","dplyr","tidyr","corrplot","gridExtra","scales"))`
2. Keep `analysis.R` and `titanic.csv` in the same folder
3. In R: `source("analysis.R")`

Outputs: `output/titanic_clean.csv`, `output/titanic_encoded.csv`, `figures/*.png`.
Full write-up: `Week1_Data_Cleaning_Report.docx`.
