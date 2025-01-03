# PuscasuAlberto_TFM
Predicting Tobacco Smoke Exposure in Children: A Machine Learning Approach Using Urine Metabolite Analysis and Sociodemographic Factors
This repository contains the data analysis and machine learning pipeline for analyzing tobacco smoke exposure through urinary metabolites.
Repository Structure
Data
Contains all raw and processed datasets used in this thesis:

Metadata_ECHOES: Unprocessed metadata questionnaire responses
Urinary_density: Specific gravity (SG) measurements for sample standardization
Data_NTs_Alberto: Raw urinary metabolite concentrations (ppm)
Statistics_df: Processed metadata and metabolite concentrations ready for analysis

Python Notebooks
Collection of Google Colab notebooks used for data analysis. Note: Path modifications may be required for local execution.

Metadata_preprocessing: Metadata processing and preparation
NTs_preprocessing: Metabolite level assessment, missing value imputation, and SG standardization
EDA: Exploratory data analysis for pattern and outlier detection
Biostatistics_analysis: Statistical analysis of exposure group differences
Machine_Learning: Training and evaluation of tobacco smoke exposure prediction models

Analysis Results
Distribution Plots

Metabolite concentration distributions across different transformation methods
Normality assessment visualizations

Violin Plots

Pre and post-processing metabolite distributions
Statistical analysis results

Multivariate Analyses

MFA (Multiple Factor Analysis)

Results for metabolite-only and combined metabolite-metadata analyses
Includes R script


MGA (Mixed Graphical Models)

MGM results for metadata and combined analyses per exposure group
Includes R script


PCA (Principal Component Analysis)

Analysis with and without outliers
PCA plots, scree plots, and bi-plot arrows
Includes both metabolite-only and combined analyses



Clustering Analysis

UnsupervisedClustering

Silhouette score plots
K-means and DB clustering results



Data Processing

ImputationMethods

MNAR imputation results for metabolites under LOD/LOQ
ANOVA results and boxplot visualizations


Z-scores

Z-score values across exposure groups
LogFC change visualization



Machine Learning

Multinomial Logistic Regression hyperparameter tuning results
Linear SVM coefficients

Usage

Clone this repository
For notebook execution:

Upload notebooks to Google Colab
Connect to Google Drive
Adjust data paths as needed
Execute notebooks in the following order:

Metadata_preprocessing
NTs_preprocessing
EDA
Biostatistics_analysis
Machine_Learning





Dependencies

Python notebooks require Google Colab environment
R scripts included in MFA and MGA folders
