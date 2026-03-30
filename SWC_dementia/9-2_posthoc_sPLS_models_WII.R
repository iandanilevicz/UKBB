# ---------------------------------------------------------------------------- #
# Script : 9-2_posthoc_sPLS_models_WII.R
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
dat0 = read.csv2("data_WII_clean.csv", header = TRUE, sep=",", dec=".")
X <- dat0 %>% 
  select(starts_with("cov"), starts_with("y"), starts_with("rar_"), 
         starts_with("mb_"), starts_with("ct_"), starts_with("spt_"), "fptau")

X_acc = dat0 %>% select(starts_with("rar_"), starts_with("mb_"), starts_with("ct_"), starts_with("spt_"))
S_acc                  = as.data.frame(scale(X_acc))
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
X$cov2_adl     = as.factor(X$cov1_adl) 
X$cov2_dep     = as.factor(X$cov1_ghq) 
X$cov3_ptau     = log(X$fptau) 


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
mod_age = coxph(as.formula(paste0("Surv(y_time, y) ~ cov1_age")), data = X)
mod_age_comp = coxph(as.formula(paste0("Surv(y_time, y) ~ cov1_age + sdt_score1 + sdt_score2")), data = X)
mod_age_sex = coxph(as.formula(paste0("Surv(y_time, y) ~ cov1_age + cov1_sex")), data = X)
mod_age_edu = coxph(as.formula(paste0("Surv(y_time, y) ~ cov1_age + cov1_edu")), data = X)
mod_age_apoe = coxph(as.formula(paste0("Surv(y_time, y) ~ cov1_age + cov4_apoe")), data = X)
mod_age_mmse = coxph(as.formula(paste0("Surv(y_time, y) ~ cov1_age + cov2_mmse")), data = X)
mod_age_tau = coxph(as.formula(paste0("Surv(y_time, y) ~ cov1_age + cov3_ptau")), data = X)


# ---------------------------------------------------------------------------- #
# Statistics 
# C-index
M0 = compare_models_123(data=X, y_str="y", time_str="y_time", 
                        cov_str1="cov1_age", 
                        cov_str2="cov1_age + sdt_score1 + sdt_score2", 
                        cov_str3="cov1_age + cov1_sex", 
                        cov_list1="cov1_age", 
                        cov_list2=c("cov1_age", "sdt_score1", "sdt_score2"), 
                        cov_list3=c("cov1_age", "cov1_sex"), 
                        folloup=quantile(X$y_time,probs=0.9), precision = 0.001, round=8, do_sens=FALSE)

M1 = compare_models_123(data=X, y_str="y", time_str="y_time", 
                        cov_str1="cov1_age", 
                        cov_str2="cov1_age + sdt_score1 + sdt_score2", 
                        cov_str3="cov1_age + cov1_edu", 
                        cov_list1="cov1_age", 
                        cov_list2=c("cov1_age", "sdt_score1", "sdt_score2"), 
                        cov_list3=c("cov1_age", "cov1_edu"), 
                        folloup=quantile(X$y_time,probs=0.9), precision = 0.001, round=8, do_sens=FALSE)

X2 = X[!is.na(X$cov4_apoe), ]
M0apoe = compare_models_123(data=X2, y_str="y", time_str="y_time", 
                        cov_str1="cov1_age", 
                        cov_str2="cov1_age + sdt_score1 + sdt_score2", 
                        cov_str3="cov1_age + cov1_sex", 
                        cov_list1="cov1_age", 
                        cov_list2=c("cov1_age", "sdt_score1", "sdt_score2"), 
                        cov_list3=c("cov1_age", "cov1_sex"), 
                        folloup=quantile(X$y_time,probs=0.9), precision = 0.001, round=8, do_sens=FALSE)

M1apoe = compare_models_123(data=X2, y_str="y", time_str="y_time", 
                        cov_str1="cov1_age", 
                        cov_str2="cov1_age + sdt_score1 + sdt_score2", 
                        cov_str3="cov1_age + cov1_edu", 
                        cov_list1="cov1_age", 
                        cov_list2=c("cov1_age", "sdt_score1", "sdt_score2"), 
                        cov_list3=c("cov1_age", "cov1_edu"), 
                        folloup=quantile(X$y_time,probs=0.9), precision = 0.001, round=8, do_sens=FALSE)
M2 = compare_models_123(data=X2, y_str="y", time_str="y_time", 
                        cov_str1="cov1_age", 
                        cov_str2="cov1_age + sdt_score1 + sdt_score2", 
                        cov_str3="cov1_age + cov4_apoe", 
                        cov_list1="cov1_age", 
                        cov_list2=c("cov1_age", "sdt_score1", "sdt_score2"), 
                        cov_list3=c("cov1_age", "cov4_apoe"), 
                        folloup=quantile(X$y_time,probs=0.9), precision = 0.001, round=8, do_sens=FALSE)

X3 = X[!is.na(X$cov2_mmse), ]
M0mmse = compare_models_123(data=X3, y_str="y", time_str="y_time", 
                           cov_str1="cov1_age", 
                           cov_str2="cov1_age + sdt_score1 + sdt_score2", 
                           cov_str3="cov1_age + cov1_sex", 
                           cov_list1="cov1_age", 
                           cov_list2=c("cov1_age", "sdt_score1", "sdt_score2"), 
                           cov_list3=c("cov1_age", "cov1_sex"), 
                           folloup=quantile(X$y_time,probs=0.9), precision = 0.001, round=8, do_sens=FALSE)

M1mmse = compare_models_123(data=X3, y_str="y", time_str="y_time", 
                           cov_str1="cov1_age", 
                           cov_str2="cov1_age + sdt_score1 + sdt_score2", 
                           cov_str3="cov1_age + cov1_edu", 
                           cov_list1="cov1_age", 
                           cov_list2=c("cov1_age", "sdt_score1", "sdt_score2"), 
                           cov_list3=c("cov1_age", "cov1_edu"), 
                           folloup=quantile(X$y_time,probs=0.9), precision = 0.001, round=8, do_sens=FALSE)

M3 = compare_models_123(data=X3, y_str="y", time_str="y_time", 
                        cov_str1="cov1_age", 
                        cov_str2="cov1_age + sdt_score1 + sdt_score2", 
                        cov_str3="cov1_age + cov2_mmse", 
                        cov_list1="cov1_age", 
                        cov_list2=c("cov1_age", "sdt_score1", "sdt_score2"), 
                        cov_list3=c("cov1_age", "cov2_mmse"), 
                        folloup=quantile(X$y_time,probs=0.9), precision = 0.001, round=8, do_sens=FALSE)

X4 = X[!is.na(X$cov3_ptau), ]
M0tau = compare_models_123(data=X4, y_str="y", time_str="y_time", 
                           cov_str1="cov1_age", 
                           cov_str2="cov1_age + sdt_score1 + sdt_score2", 
                           cov_str3="cov1_age + cov1_sex", 
                           cov_list1="cov1_age", 
                           cov_list2=c("cov1_age", "sdt_score1", "sdt_score2"), 
                           cov_list3=c("cov1_age", "cov1_sex"), 
                           folloup=quantile(X$y_time,probs=0.9), precision = 0.001, round=8, do_sens=FALSE)

M1tau = compare_models_123(data=X4, y_str="y", time_str="y_time", 
                           cov_str1="cov1_age", 
                           cov_str2="cov1_age + sdt_score1 + sdt_score2", 
                           cov_str3="cov1_age + cov1_edu", 
                           cov_list1="cov1_age", 
                           cov_list2=c("cov1_age", "sdt_score1", "sdt_score2"), 
                           cov_list3=c("cov1_age", "cov1_edu"), 
                           folloup=quantile(X$y_time,probs=0.9), precision = 0.001, round=8, do_sens=FALSE)

M4 = compare_models_123(data=X4, y_str="y", time_str="y_time", 
                        cov_str1="cov1_age", 
                        cov_str2="cov1_age + sdt_score1 + sdt_score2", 
                        cov_str3="cov1_age + cov3_ptau", 
                        cov_list1="cov1_age", 
                        cov_list2=c("cov1_age", "sdt_score1", "sdt_score2"), 
                        cov_list3=c("cov1_age", "cov3_ptau"), 
                        folloup=quantile(X$y_time,probs=0.9), precision = 0.001, round=8, do_sens=FALSE)

# ---------------------------------------------------------------------------- #


round(M0, 3)
round(M1, 3) 

round(M0apoe, 3)
round(M1apoe, 3) 
round(M2, 3) 

round(M0mmse, 3)
round(M1mmse, 3) 
round(M3, 3) 

round(M0tau, 3)
round(M1tau, 3) 
round(M4, 3) 




# ---------------------------------------------------------------------------- #

