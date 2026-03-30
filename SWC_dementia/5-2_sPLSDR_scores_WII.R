# ---------------------------------------------------------------------------- #
# Script : 5-2_sPLSDR_scores_WII.R
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

# ---------------------------------------------------------------------------- #
# Functions
setwd(dir_fun)
source("0-0_aux_functions_v3.R")
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
#X$cov2_adl     = as.factor(X$cov1_adl) 
X$cov2_dep     = as.factor(X$cov1_ghq) 
X$cov3_ptau     = log(X$fptau) 

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

cov0_Acc0 = c("cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh",  "cov1_smoke", 
"cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns")

cov0_Acc1 = c("sdt_score1", "cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh",  "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns")

cov0_Acc2 = c("sdt_score1", "sdt_score2", "cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh",  "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns")

# ---------------------------------------------------------------------------- #
# Model Whitehall  
cov1_acc0 = "cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
 cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns + cov2_dep + cov2_mmse"

cov1_acc1 = "sdt_score1 + cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
 cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns + cov2_dep + cov2_mmse"

cov1_acc2 = "sdt_score1 + sdt_score2 + cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
 cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns + cov2_dep + cov2_mmse"

cov1_Acc0 = c("cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh",  "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns",
              "cov2_dep", "cov2_mmse")

cov1_Acc1 = c("sdt_score1", "cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh",  "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns",
              "cov2_dep", "cov2_mmse")

cov1_Acc2 = c("sdt_score1", "sdt_score2", "cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh",  "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns",
              "cov2_dep", "cov2_mmse")

# ---------------------------------------------------------------------------- #
# Model Tau   
cov2_acc0 = "cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns + cov2_dep + cov2_mmse + cov3_ptau"

cov2_acc1 = "sdt_score1 + cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns + cov2_dep + cov2_mmse + cov3_ptau"

cov2_acc2 = "sdt_score1 + sdt_score2 + cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
 cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns + cov2_dep + cov2_mmse + cov3_ptau"

cov2_Acc0 = c("cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh", "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns",
              "cov2_dep", "cov2_mmse", "cov3_ptau")

cov2_Acc1 = c("sdt_score1", "cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh",  "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns",
              "cov2_dep", "cov2_mmse", "cov3_ptau")

cov2_Acc2 = c("sdt_score1", "sdt_score2", "cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh", "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns",
              "cov2_dep", "cov2_mmse", "cov3_ptau")

# ---------------------------------------------------------------------------- #
# model APOE 
cov3_acc0 = "cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
 cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns + cov4_apoe"

cov3_acc1 = "sdt_score1 + cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
 cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns+ cov4_apoe"

cov3_acc2 = "sdt_score1 + sdt_score2 + cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
 cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns+ cov4_apoe"

cov3_Acc0 = c("cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh",  "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns", "cov4_apoe")

cov3_Acc1 = c("sdt_score1", "cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh",  "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns", "cov4_apoe")

cov3_Acc2 = c("sdt_score1", "sdt_score2", "cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh",  "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns", "cov4_apoe")
# ---------------------------------------------------------------------------- #



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


mod_acc0_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc0)), data = X)
mod_acc1_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc1)), data = X)
mod_acc2_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov0_acc2)), data = X)

mod_acc0_cov1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov1_acc0)), data = X)
mod_acc1_cov1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov1_acc1)), data = X)
mod_acc2_cov1 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov1_acc2)), data = X)

mod_acc0_cov2 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov2_acc0)), data = X)
mod_acc1_cov2 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov2_acc1)), data = X)
mod_acc2_cov2 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov2_acc2)), data = X)

mod_acc0_cov3 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov3_acc0)), data = X)
mod_acc1_cov3 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov3_acc1)), data = X)
mod_acc2_cov3 = coxph(as.formula(paste0("Surv(y_time, y) ~ ", cov3_acc2)), data = X)

# ---------------------------------------------------------------------------- #
# Model summary

H0 = compare_HR_1_12(mod1=mod_acc1_cov0, mod2=mod_acc2_cov0, alpha=0.05, round=3)
H1 = compare_HR_1_12(mod1=mod_acc1_cov1, mod2=mod_acc2_cov1, alpha=0.05, round=3)
H2 = compare_HR_1_12(mod1=mod_acc1_cov2, mod2=mod_acc2_cov2, alpha=0.05, round=3)
H3 = compare_HR_1_12(mod1=mod_acc1_cov3, mod2=mod_acc2_cov3, alpha=0.05, round=3)

mod_acc2_cov0

# H0 same covariates as UK Biobank
H0
age0 = age_at_event(X$cov1_age, X$y_time, X$y, X$cov1_bmi)
  

# # M0 + GHQ ADL MMSE
H1
age1 = age_at_event(X$cov1_age, X$y_time, X$y, cbind(X$cov2_mmse,X$cov2_dep))
# M1 + tau
H2
age2 = age_at_event(X$cov1_age, X$y_time, X$y, cbind(X$cov2_mmse,X$cov3_ptau))
# M0 + Apoe 
H3 
age3 = age_at_event(X$cov1_age, X$y_time, X$y, cbind(X$cov4_apoe))


# ---------------------------------------------------------------------------- #
# display Hazard ratio 

# M1 same covariates as UK Biobank
round(H0, 2)
mod_acc1_cov0$n
mod_acc1_cov0$nevent
age0
# M1 + APOE
round(H3, 2)
mod_acc1_cov3$n
mod_acc1_cov3$nevent
age3
# M2 = M1 +  GHQ ADL MMSE
round(H1, 2)
mod_acc1_cov1$n
mod_acc1_cov1$nevent
age1
# M2 + ptau
round(H2, 2)
mod_acc1_cov2$n
mod_acc1_cov2$nevent
age2
# ---------------------------------------------------------------------------- #
# Statistics 
# M0 Performance statistics of M0 model with basic covariates (same as UK Biobank) 
M0 = compare_models_123(data=X, y_str="y", time_str="y_time", cov_str1=cov0_acc0, cov_str2=cov0_acc1, cov_str3=cov0_acc2, 
                         cov_list1=cov0_Acc0, cov_list2=cov0_Acc1, cov_list3=cov0_Acc2, 
                         folloup=quantile(X$y_time,probs=0.9), precision = 0.0001, round=5, do_sens=0.75)
# M1 Performance statistics of M1 model with basic covariates + GHQ(depression) ADL MMSE
M1 = compare_models_123(data=X, y_str="y", time_str="y_time", cov_str1=cov1_acc0, cov_str2=cov1_acc1, cov_str3=cov1_acc2, 
                        cov_list1=cov1_Acc0, cov_list2=cov1_Acc1, cov_list3=cov1_Acc2, 
                        folloup=quantile(X$y_time,probs=0.9), precision = 0.0001, round=5, do_sens=0.75)
# M2 Performance statistics of M2 model with basic covariates + GHQ(depression) ADL MMSE + ptau217
M2 = compare_models_123(data=X, y_str="y", time_str="y_time", cov_str1=cov2_acc0, cov_str2=cov2_acc1, cov_str3=cov2_acc2, 
                        cov_list1=cov2_Acc0, cov_list2=cov2_Acc1, cov_list3=cov2_Acc2, 
                        folloup=quantile(X$y_time,probs=0.9), precision = 0.0001, round=5, do_sens=0.75)
# M3 Performance statistics of M3 model with basic covariates + APOE (same as UK Biobank)
M3 = compare_models_123(data=X, y_str="y", time_str="y_time", cov_str1=cov3_acc0, cov_str2=cov3_acc1, cov_str3=cov3_acc2, 
                        cov_list1=cov3_Acc0, cov_list2=cov3_Acc1, cov_list3=cov3_Acc2, 
                        folloup=quantile(X$y_time,probs=0.9), precision = 0.0001, round=5, do_sens=0.75)

# M0 Performance statistics of M0 model with basic covariates (same as UK Biobank) 
# M1 Performance statistics of M1 model with basic covariates + GHQ(depression) ADL MMSE
# M2 Performance statistics of M2 model with basic covariates + GHQ(depression) ADL MMSE + ptau217
# M3 Performance statistics of M3 model with basic covariates + APOE (same as UK Biobank)
round(M0, 3)
round(M3, 3)
round(M1, 3)
round(M2, 3)

setwd(dir = dir_res)
write.csv(M0, file = "m0_wii_v2.csv", row.names = TRUE)
write.csv(M1, file = "m1_wii_v2.csv", row.names = TRUE)
write.csv(M2, file = "m2_wii_v2.csv", row.names = TRUE)
write.csv(M3, file = "m3_wii_v2.csv", row.names = TRUE)

write.csv(H0, file = "h0_wii.csv", row.names = TRUE)
write.csv(H1, file = "h1_wii.csv", row.names = TRUE)
write.csv(H2, file = "h2_wii.csv", row.names = TRUE)
write.csv(H3, file = "h3_wii.csv", row.names = TRUE)

# ---------------------------------------------------------------------------- #

