
# R codes for "Contribution of Digital Sleep-Wake Cycle Metrics to Dementia Prediction in Older Adults"

This repository provides the R code used in the paper "Contribution of Digital Sleep-Wake Cycle Metrics to Dementia Prediction in Older Adults" published in "name journal" on "date". Please cite the work: 

"citation"

- The main analysis use the UK Biobank cohort dataset.
- An external validation is done using Whitehall II accelerometer-substudy dataset.

## Step 0 - Load data and functions 

- 0_0_aux_functions.R
- 0_0_GGIR_extraction.R


## Step 1 - Building datasets  

At this step we merge accelerometer-derived metrics with sociodemographics, health and behavioural covariates as well as with the outcome of interest. 

- 1_1_data_UKB.R
- 1_2_data_WII.R

## Step 2 - Exploratory descriptive analysis 

At this step we do exploratory descriptive analysis (EDA) for the covariates of interest and we plot spline cox regression for the accelerometer-derived metrics. 

- 2_1_EDA_UKB.R
- 2_2_WII_UKB.R
- 3_1_Cox_UKB.R
  
## Step 3 - Model training 

At this step we do model training, lets say we pre estimate the tuning parameters k and eta.  

- 4_1_sPLSDR_tuning_UKB.R 

## Step 4 - Model fitting

- 5_1_sPLSDR_scores_UKBB.R
- 5_2_sPLSDR_scores_WII.R

## Step 5 - Model fitting

- 6_1_sPLSDR_stratified_UKB.R
- 6_1_sPLSDR_stratified_UKB.R

## Step 6 - Model fitting

- 7_1_sPLSDR_mcvd_UKB.R
- 7_1_sPLSDR_mcvd_UKB.R

## Step 7 - Posthoc analysis 

### Step 7.1

- 8-1_posthoc_EDA_UKB.R 

### Step 7.2

- 9-1_posthoc_sPLS_models_UKB.R
- 9-2_posthoc_sPLS_models_WII.R

### Step 7.3 

- 11-1_posthoc_sPLS_lag_WII.R 
- 11-2_posthoc_sPLS_lag_UKB.R

