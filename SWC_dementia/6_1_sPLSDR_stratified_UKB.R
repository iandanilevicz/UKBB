# ---------------------------------------------------------------------------- #
# Script : 6_1_sPLSDR_stratified_UKB.R
# Author : I M Danilevicz 
# Remark : Part of the work available in:
#          C Cavailles, IM Danilevicz, S Vidil et al (2026). 
#          "Contribution of Digital Sleep-Wake Cycle Metrics to Dementia Prediction in Older Adults", (under review)
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Set local language as english to be able to read dates properly
# Clear the Environment 
rm(list=ls())
Sys.setlocale("LC_ALL","English")

# ---------------------------------------------------------------------------- #
# Packages 
library(ggplot2)
library(dplyr)
library(patchwork)
library(grid)
library(compareC)
library(tidyverse)
library(survival)

# ---------------------------------------------------------------------------- #
# Folders
dir_load = "D:/..."
dir_fun  = "D:/..."
dir_cv   = "D:/..."
dir_res  = "D:/..."

# ---------------------------------------------------------------------------- #
# Functions
setwd(dir_fun)
source("0-0_aux_functions.R")
# ---------------------------------------------------------------------------- #
# Data
setwd(dir_load)
dat0 = read.csv2("data_UKB_clean.csv", header = TRUE, sep=",", dec=".")
X <- dat0 %>% 
  select(starts_with("cov"), starts_with("y"), starts_with("rar_"), 
         starts_with("mb_"), starts_with("ct_"), starts_with("spt_"))
X_acc = dat0 %>% select(starts_with("rar_"), starts_with("mb_"), starts_with("ct_"), starts_with("spt_"))
S_acc                  = as.data.frame(scale(X_acc))
S_acc$mb_dur_lipa_sq   = scale(S_acc$mb_dur_lipa^2)
S_acc$mb_mdb_lipa_sq   = scale(S_acc$mb_mdb_lipa^2)
S_acc$mb_mdb_mvpa_sq   = scale(S_acc$mb_mdb_mvpa^2)
S_acc$spt_dur_sleep_sq = scale(S_acc$spt_dur_sleep^2)
R_acc = rescale_mat(S_acc, sigma=6, scale=FALSE)
n = dim(R_acc)[1]
Cas = dat0$y 
T   = dat0$y_time 
y = dat0$y
y_time = dat0$y_time
Y = cbind(X$y_time,X$y); colnames(Y) = c("time", "event")
X$y = dat0$y
X$y_time = dat0$y_time
X$cov1_sex     = as.factor(X$cov1_sex) 
X$cov1_edu     = as.factor(X$cov1_edu)
X$cov1_marcoh  = as.factor(X$cov1_marcoh) 
X$cov1_work    = as.factor(X$cov1_work) 
X$cov1_smoke   = as.factor(X$cov1_smoke) 
X$cov1_veg     = as.factor(X$cov1_veg) 
X$cov1_alcohol = as.factor(X$cov1_alcohol) 
X$cov1_bmic    = factor(ifelse(X$cov1_bmi<25,1,ifelse(X$cov1_bmi>=30,3,2))) 
X$cov1_hypt    = as.factor(X$cov1_hypt) 
X$cov1_diab    = as.factor(X$cov1_diab) 
X$cov1_hypl    = as.factor(X$cov1_hypl) 
X$cov1_ncdc    = factor(ifelse(X$cov1_Nchronic==0,0, ifelse(X$cov1_Nchronic==1,1,2))) 
X$cov1_cns     = as.factor(X$cov1_cns) 

# ---------------------------------------------------------------------------- #
# Reference model 
cov0_acc0 = "cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns"

cov0_acc1 = "sdt_score1 + cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns"

cov0_acc2 = "sdt_score1 + sdt_score2 + cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns"

cov0_Acc0 = c("cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh", "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns")

cov0_Acc1 = c("sdt_score1", "cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh", "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns")

cov0_Acc2 = c("sdt_score1", "sdt_score2", "cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh", "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns")


# ---------------------------------------------------------------------------- #
# Model fit
D = 2
eta = 0.85
setwd(dir_res)
load(file="W_star_ukb85.rda")
full_list = colnames(R_acc)
Wtemp     = build_matrix(full_list, W_star)
Xscores   =  as.matrix(R_acc) %*% Wtemp
X$score1 = Xscores[,1]
X$score2 = Xscores[,2]
X$sdt_score1 = scale(X$score1)
X$sdt_score2 = scale(X$score2)


# ---------------------------------------------------------------------------- #
# Subgroup 1 - Age (1 = youngers, 2=olders) 
age_cut = 70
W = X %>% filter(cov1_age < age_cut)
mod_acc0_age1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc0)), data = W)
mod_acc1_age1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc1)), data = W)
mod_acc2_age1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc2)), data = W)
n_age1 = dim(W)[1]
M0_age1 = compare_models_123(data=W, y_str="y", time_str="y_time", cov_str1=cov0_acc0, cov_str2=cov0_acc1, cov_str3=cov0_acc2, 
                             cov_list1=cov0_Acc0, cov_list2=cov0_Acc1, cov_list3=cov0_Acc2, 
                             folloup=9.6*365, precision = 0.1,  do_sens=FALSE)
W = X %>% filter(cov1_age >= age_cut)
mod_acc0_age2 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc0)), data = W)
mod_acc1_age2 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc1)), data = W)
mod_acc2_age2 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc2)), data = W)
n_age2 = dim(W)[1]
M0_age2 = compare_models_123(data=W, y_str="y", time_str="y_time", cov_str1=cov0_acc0, cov_str2=cov0_acc1, cov_str3=cov0_acc2, 
                             cov_list1=cov0_Acc0, cov_list2=cov0_Acc1, cov_list3=cov0_Acc2, 
                             folloup=9.6*365, precision = 0.1,  do_sens=FALSE)

# ---------------------------------------------------------------------------- #
# Subgroup 2 - Sex (0 = men, 1=women)
W = X %>% filter(cov1_sex == 0)
mod_acc0_sex0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc0)), data = W)
mod_acc1_sex0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc1)), data = W)
mod_acc2_sex0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc2)), data = W)
n_sex0 = dim(W)[1]
M0_sex0 = compare_models_123(data=W, y_str="y", time_str="y_time", cov_str1=cov0_acc0, cov_str2=cov0_acc1, cov_str3=cov0_acc2, 
                             cov_list1=cov0_Acc0, cov_list2=cov0_Acc1, cov_list3=cov0_Acc2, 
                             folloup=9.6*365, precision = 0.1,  do_sens=FALSE)
W = X %>% filter(cov1_sex == 1)
mod_acc0_sex1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc0)), data = W)
mod_acc1_sex1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc1)), data = W)
mod_acc2_sex1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc2)), data = W)
n_sex1 = dim(W)[1]
M0_sex1 = compare_models_123(data=W, y_str="y", time_str="y_time", cov_str1=cov0_acc0, cov_str2=cov0_acc1, cov_str3=cov0_acc2, 
                             cov_list1=cov0_Acc0, cov_list2=cov0_Acc1, cov_list3=cov0_Acc2, 
                             folloup=9.6*365, precision = 0.1,  do_sens=FALSE)

# ---------------------------------------------------------------------------- #
# Subgroup 3 - Education (0, 1, 2 )
W = X %>% filter(cov1_edu == 0)
mod_acc0_edu0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc0)), data = W)
mod_acc1_edu0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc1)), data = W)
mod_acc2_edu0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc2)), data = W)
n_edu0 = dim(W)[1]
M0_edu0 = compare_models_123(data=W, y_str="y", time_str="y_time", cov_str1=cov0_acc0, cov_str2=cov0_acc1, cov_str3=cov0_acc2, 
                             cov_list1=cov0_Acc0, cov_list2=cov0_Acc1, cov_list3=cov0_Acc2, 
                             folloup=9.6*365, precision = 0.1,  do_sens=FALSE)
W = X %>% filter(cov1_edu == 1)
mod_acc0_edu1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc0)), data = W)
mod_acc1_edu1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc1)), data = W)
mod_acc2_edu1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc2)), data = W)
n_edu1 = dim(W)[1]
M0_edu1 = compare_models_123(data=W, y_str="y", time_str="y_time", cov_str1=cov0_acc0, cov_str2=cov0_acc1, cov_str3=cov0_acc2, 
                             cov_list1=cov0_Acc0, cov_list2=cov0_Acc1, cov_list3=cov0_Acc2, 
                             folloup=9.6*365, precision = 0.1,  do_sens=FALSE)
W = X %>% filter(cov1_edu == 2)
mod_acc0_edu2 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc0)), data = W)
mod_acc1_edu2 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc1)), data = W)
mod_acc2_edu2 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc2)), data = W)
n_edu2 = dim(W)[1]
M0_edu2 = compare_models_123(data=W, y_str="y", time_str="y_time", cov_str1=cov0_acc0, cov_str2=cov0_acc1, cov_str3=cov0_acc2, 
                             cov_list1=cov0_Acc0, cov_list2=cov0_Acc1, cov_list3=cov0_Acc2, 
                             folloup=9.6*365, precision = 0.1,  do_sens=FALSE)

# ---------------------------------------------------------------------------- #
# Subgroup 3 - N chronic diseases (0, 1, 2 or more)
W = X %>% filter(cov1_Nchronic == 0)
mod_acc0_ncd0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc0)), data = W)
mod_acc1_ncd0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc1)), data = W)
mod_acc2_ncd0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc2)), data = W)
n_ncd0 = dim(W)[1]
M0_ncd0 = compare_models_123(data=W, y_str="y", time_str="y_time", cov_str1=cov0_acc0, cov_str2=cov0_acc1, cov_str3=cov0_acc2, 
                             cov_list1=cov0_Acc0, cov_list2=cov0_Acc1, cov_list3=cov0_Acc2, 
                             folloup=9.6*365, precision = 0.1,  do_sens=FALSE)
W = X %>% filter(cov1_Nchronic >= 1)
mod_acc0_ncd1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc0)), data = W)
mod_acc1_ncd1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc1)), data = W)
mod_acc2_ncd1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc2)), data = W)
n_ncd1 = dim(W)[1]
M0_ncd1 = compare_models_123(data=W, y_str="y", time_str="y_time", cov_str1=cov0_acc0, cov_str2=cov0_acc1, cov_str3=cov0_acc2, 
                             cov_list1=cov0_Acc0, cov_list2=cov0_Acc1, cov_list3=cov0_Acc2, 
                             folloup=9.6*365, precision = 0.1,  do_sens=FALSE)


# ---------------------------------------------------------------------------- #
# Subgroup 6 - APOE (0 , 1 or 2)
W = X %>% filter(cov4_apoe == 0)
mod_acc0_apoe0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc0)), data = W)
mod_acc1_apoe0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc1)), data = W)
mod_acc2_apoe0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc2)), data = W)
n_apoe0 = dim(W)[1]
M0_apoe0 = compare_models_123(data=W, y_str="y", time_str="y_time", cov_str1=cov0_acc0, cov_str2=cov0_acc1, cov_str3=cov0_acc2, 
                              cov_list1=cov0_Acc0, cov_list2=cov0_Acc1, cov_list3=cov0_Acc2, 
                              folloup=9.6*365, precision = 0.1,  do_sens=FALSE)
W = X %>% filter(cov4_apoe >= 1)
mod_acc0_apoe1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc0)), data = W)
mod_acc1_apoe1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc1)), data = W)
mod_acc2_apoe1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc2)), data = W)
n_apoe1 = dim(W)[1]
M0_apoe1 = compare_models_123(data=W, y_str="y", time_str="y_time", cov_str1=cov0_acc0, cov_str2=cov0_acc1, cov_str3=cov0_acc2, 
                              cov_list1=cov0_Acc0, cov_list2=cov0_Acc1, cov_list3=cov0_Acc2, 
                              folloup=9.6*365, precision = 0.1,  do_sens=FALSE)

# ---------------------------------------------------------------------------- #
setwd(dir_res)
save.image("ukbb_stratified_models.RData")
# ---------------------------------------------------------------------------- #
# Model summary

# H0 same covariates as UK Biobank
H0_age1 = compare_HR_1_12(mod1=mod_acc1_age1, mod2=mod_acc2_age1, alpha=0.05, round=3)
H0_age2 = compare_HR_1_12(mod1=mod_acc1_age2, mod2=mod_acc2_age2, alpha=0.05, round=3)

H0_sex0 = compare_HR_1_12(mod1=mod_acc1_sex0, mod2=mod_acc2_sex0, alpha=0.05, round=3)
H0_sex1 = compare_HR_1_12(mod1=mod_acc1_sex1, mod2=mod_acc2_sex1, alpha=0.05, round=3)

H0_edu0 = compare_HR_1_12(mod1=mod_acc1_edu0, mod2=mod_acc2_edu0, alpha=0.05, round=3)
H0_edu1 = compare_HR_1_12(mod1=mod_acc1_edu1, mod2=mod_acc2_edu1, alpha=0.05, round=3)
H0_edu2 = compare_HR_1_12(mod1=mod_acc1_edu2, mod2=mod_acc2_edu2, alpha=0.05, round=3)

H0_ncd0 = compare_HR_1_12(mod1=mod_acc1_ncd0, mod2=mod_acc2_ncd0, alpha=0.05, round=3)
H0_ncd1 = compare_HR_1_12(mod1=mod_acc1_ncd1, mod2=mod_acc2_ncd1, alpha=0.05, round=3)

H0_apoe0 = compare_HR_1_12(mod1=mod_acc1_apoe0, mod2=mod_acc2_apoe0, alpha=0.05, round=3)
H0_apoe1 = compare_HR_1_12(mod1=mod_acc1_apoe1, mod2=mod_acc2_apoe1, alpha=0.05, round=3)


n_age1
n_age2
n_sex0
n_sex1
n_ncd0
n_ncd1

n_apoe0
n_apoe1

n_edu0
n_edu1
n_edu2

H0_age1
H0_age2
H0_sex0
H0_sex1
H0_edu0
H0_edu1
H0_edu2
H0_ncd0
H0_ncd1

H0_apoe0
H0_apoe1


M0_age1
M0_age2
M0_sex0
M0_sex1
M0_edu0
M0_edu1
M0_edu2
M0_ncd0
M0_ncd1

M0_apoe0
M0_apoe1


# ---------------------------------------------------------------------------- #


