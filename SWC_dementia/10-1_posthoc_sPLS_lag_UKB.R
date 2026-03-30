# ---------------------------------------------------------------------------- #
# Script : 10-1_posthoc_sPLS_lag_UKB.R
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
library(compareC)
library(tidyverse)
library(survival)
# ---------------------------------------------------------------------------- #
# Folders
dir_load = "D:/..."
dir_fun  = "D:/..."
dir_cv   = "D:/..."
dir_res  = "D:/..."
dir_posthoc  = "D:/..."

# ---------------------------------------------------------------------------- #
# Functions
setwd(dir_fun)
source("0-0_aux_functions.R")
# ---------------------------------------------------------------------------- #
# Data
setwd(dir_load)
dat0 = read.csv2("data_UKB_clean.csv", header = TRUE, sep=",", dec=".", quote = "\"")

X <- dat0 %>% select(starts_with("cov"), starts_with("y"), starts_with("rar_"),
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
X$cov4_apoe     = X$cov4_apoe 

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
# model  
cov3_acc0 = "cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns + cov4_apoe"

cov3_acc1 = "sdt_score1 + cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns + cov4_apoe"

cov3_acc2 = "sdt_score1 + sdt_score2 + cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns + cov4_apoe"

cov3_Acc0 = c("cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh", "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns", "cov4_apoe")

cov3_Acc1 = c("sdt_score1", "cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh", "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns", "cov4_apoe")

cov3_Acc2 = c("sdt_score1", "sdt_score2", "cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh", "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns", "cov4_apoe")




# ---------------------------------------------------------------------------- #
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
# minimal lag time for dementia 
X$y = ifelse(X$y == 1 & X$y_time/365.25 < 2, 0, X$y)


# ---------------------------------------------------------------------------- #
mod_acc0_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc0)), data = X)
mod_acc1_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc1)), data = X)
mod_acc2_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc2)), data = X)

mod_acc0_cov3 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov3_acc0)), data = X)
mod_acc1_cov3 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov3_acc1)), data = X)
mod_acc2_cov3 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov3_acc2)), data = X)

# ---------------------------------------------------------------------------- #
# Model summary
H0 = compare_HR_1_12(mod1=mod_acc1_cov0, mod2=mod_acc2_cov0, alpha=0.05, round=3)
H3 = compare_HR_1_12(mod1=mod_acc1_cov3, mod2=mod_acc2_cov3, alpha=0.05, round=3)


# H0 Hazard ratio of M0 model with basic covariates 
H0
mod_acc1_cov0$n
mod_acc1_cov0$nevent
age_at_event(X$cov1_age, X$y_time, X$y, X$cov1_age)

# H3 Hazard ratio of M3 model with basic covariates + Apoe 
H3 
mod_acc1_cov3$n
mod_acc1_cov3$nevent
age_at_event(X$cov1_age, X$y_time, X$y, X$cov4_apoe)
# ---------------------------------------------------------------------------- #
# Statistics 
# C-index
M0 = compare_models_123(data=X, y_str="y", time_str="y_time", cov_str1=cov0_acc0, cov_str2=cov0_acc1, cov_str3=cov0_acc2, 
                        cov_list1=cov0_Acc0, cov_list2=cov0_Acc1, cov_list3=cov0_Acc2, 
                        folloup=quantile(X$y_time,probs=0.9), precision = 0.0001, round=8, do_sens=0.75)

M3 = compare_models_123(data=X, y_str="y", time_str="y_time", cov_str1=cov3_acc0, cov_str2=cov3_acc1, cov_str3=cov3_acc2, 
                        cov_list1=cov3_Acc0, cov_list2=cov3_Acc1, cov_list3=cov3_Acc2, 
                        folloup=quantile(X$y_time,probs=0.9), precision = 0.0001, round=8, do_sens=0.75)
# M0 Performance statistics of M0 model with basic covariates  
# M3 Performance statistics of M3 model with basic covariates + Apoe 
round(M0, 3)
round(M3, 3) 

setwd(dir = dir_res)
write.csv(M0, file = "m0_ukb_lag.csv", row.names = TRUE)
write.csv(H0, file = "h0_ukb_lag.csv", row.names = TRUE)

# ---------------------------------------------------------------------------- #


