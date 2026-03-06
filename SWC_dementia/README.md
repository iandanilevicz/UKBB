
# R codes for "Contribution of Digital Sleep-Wake Cycle Metrics to Dementia Prediction in Older Adults"

This repository provides the R code used in the paper "Contribution of Digital Sleep-Wake Cycle Metrics to Dementia Prediction in Older Adults" published in "name journal" on "date". Please cite the work: 

"citation"

- The main analysis use the UK Biobank cohort dataset.
- An external validation is done using Whitehall II accelerometer-substudy dataset.

## Step 0 - Load data and functions 

At this step, we load the accelerometer data using the R package GGIR and also load several functions that will be useful for subsequent analyses. 

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

At this step, we perform model training. The sparse partial least squares deviance residual regression (sPLSDR) is a machine-learning (ML) method that requires two tuning parameters: k (number of components) and eta (level of sparsity).

- 4_1_sPLSDR_tuning_UKB.R 

## Step 4 - Model fitting

Given k (number of components) and eta (level of sparsity) we can fit a model for UK biobank (main analysis) and Whitehall II (external validation). 

- 5_1_sPLSDR_scores_UKBB.R
- 5_2_sPLSDR_scores_WII.R

## Step 5 - Stratified models

To verify the robustness of the findings, we stratified our analyses.

- 6_1_sPLSDR_stratified_UKB.R
- 6_1_sPLSDR_stratified_UKB.R

## Step 6 - Model fitting

To verify the specificity of the findings, we replicate the analyses on a different outcome.

- 7_1_sPLSDR_mcvd_UKB.R
- 7_1_sPLSDR_mcvd_UKB.R

## Step 7 - Posthoc analysis 

Here we perform some additional analyses that were not initially planned but were valuable suggestions from the reviewers.

### Step 7.1

To verify whether the selected sample from the UK Biobank is similar to the general UK Biobank population, we conducted an exploratory data analysis (EDA) comparing the two samples.

- 8-1_posthoc_EDA_UKB.R 

### Step 7.2

To assess the impact of digital sleep–wake cycle (SWC) components, we fit simpler models with fewer covariates.

- 9-1_posthoc_sPLS_models_UKB.R
- 9-2_posthoc_sPLS_models_WII.R

### Step 7.3 

To verify the robustness of the findings, we conducted analyses using a different definition of the event (applying a two-year lag).

- 10-1_posthoc_sPLS_lag_WII.R 
- 10-2_posthoc_sPLS_lag_UKB.R

