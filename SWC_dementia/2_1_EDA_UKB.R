# ---------------------------------------------------------------------------- #
# Script : 2_1_EDA_UKB.R
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
library(arsenal) 

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
dat0 = read.csv2("data_UKB_clean.csv", header = TRUE, sep=",", dec=".", quote = "\"")
dat0$cov4_apoe
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
X$y_time = X$y_time/365.25
X$cov4_apoec = as.factor(ifelse(X$cov4_apoe>0,1,0))

# ---------------------------------------------------------------------------- #
# average and SD
round(table(X$cov1_sex)/(24459 +28989),3)
mean(X$cov1_age)
sd(X$cov1_age)
round(mean(X$y_time),2)
round(sd(X$y_time),2)

# ---------------------------------------------------------------------------- #
# Model 1  
control_ttest <- tableby.control(
  total = FALSE,
  digits = 6, 
  digits.p = 3,
  numeric.simplify = TRUE,
  cat.simplify = TRUE,
  numeric.stats = c("Nmiss", "meansd"),
  test = TRUE,
  test.numeric = "t.test" 
)

control_Wilcoxon <- tableby.control(
  total = FALSE,
  digits = 3, 
  digits.p = 3,
  numeric.simplify = TRUE,
  cat.simplify = TRUE,
  numeric.stats = c("Nmiss", "medianq1q3", "iqr"),
  test = TRUE,
  test.numeric = "wilcoxon"  
)

table0 = tableby(y ~ y_time, 
                 data=X, control=tableby.control(total=FALSE,
                                                 digits=1, digits.p = 3,
                                                 numeric.simplify = TRUE,
                                                 cat.simplify = TRUE,
                                                 numeric.stats = c("Nmiss", "medianq1q3")))

table1 = tableby(y ~ y_time + cov1_age + cov1_sex +  cov1_edu + cov1_marcoh + cov1_work + 
                   cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmi + cov1_hypt +
                   cov1_diab + cov1_hypl + cov1_Nchronic + cov1_cns, 
                 data=X, control=control_ttest)

table3 = tableby(y ~ cov4_apoec, 
                 data=X, control=control_ttest)

table4 = tableby(y ~ rar_ra + rar_r2 + rar_mesor + rar_iv + rar_is + rar_amp + 
                   spt_TP_ws + spt_TP_sw + spt_l5value + spt_eff + spt_waso + 
                   spt_mdb_wake + spt_mdb_sleep + spt_nb_sleep + spt_dur_sleep + spt_acc_night +   
                   mb_TP_ra + mb_TP_ar + mb_m10value + mb_ig_slope + mb_ig_inter +
                   mb_dur_sb + mb_dur_lipa + mb_dur_mvpa +
                   mb_nb_sb + mb_nb_lipa + mb_nb_mvpa +
                   mb_mdb_sb + mb_mdb_lipa + mb_mdb_mvpa + mb_acc_day + 
                   ct_wakeup + ct_onset + ct_m10time + ct_l5time + ct_acrotime, 
                   data=X, control=control_ttest)

table_0 = tableby(y ~ y_time, 
                 data=X, control=control_Wilcoxon)

table_1 = tableby(y ~ y_time + cov1_age + cov1_sex +  cov1_edu + cov1_marcoh + cov1_work + 
                   cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmi + cov1_hypt +
                   cov1_diab + cov1_hypl + cov1_Nchronic + cov1_cns, 
                 data=X, control=control_Wilcoxon)

table_3 = tableby(y ~ cov4_apoec, 
                 data=X, control=control_Wilcoxon)

table_4 = tableby(y ~ rar_ra + rar_r2 + rar_mesor + rar_iv + rar_is + rar_amp + 
                   spt_TP_ws + spt_TP_sw + spt_l5value + spt_eff + spt_waso + 
                   spt_mdb_wake + spt_mdb_sleep + spt_nb_sleep + spt_dur_sleep + spt_acc_night +   
                   mb_TP_ra + mb_TP_ar + mb_m10value + mb_ig_slope + mb_ig_inter +
                   mb_dur_sb + mb_dur_lipa + mb_dur_mvpa +
                   mb_nb_sb + mb_nb_lipa + mb_nb_mvpa +
                   mb_mdb_sb + mb_mdb_lipa + mb_mdb_mvpa + mb_acc_day + 
                   ct_wakeup + ct_onset + ct_m10time + ct_l5time + ct_acrotime, 
                 data=X, control=control_Wilcoxon)

labels_cov0 = c(y_time = "Follow-up (years), median (IQR)")

labels_cov1 = c(y_time = "Follow-up (years), mean (SD)",
                cov1_age = "Age (years), mean (SD)", 
                cov1_sex = "Sex (women)", 
                cov1_edu  = "Education",
                cov1_marcoh = "Cohabitation status(married/cohab.)", 
                cov1_work = "Work status (working)", 
                cov1_smoke = "Smoking status", 
                cov1_veg = "Fruit and vegetable intake", 
                cov1_alcohol = "Alcohol intake",  
                cov1_bmic = "Body mass index (BMI)", 
                cov1_hypt = "Prevalent hypertension",  
                cov1_diab = "Prevalent diabetes",
                cov1_hypl = "Prevalent hyperlipidemia",
                cov1_ncdc = "Number of chronic diseases, mean (SD)",
                cov1_cns= "CNS drug use") 


labels_cov3 = c( cov4_apoec = "APOE E4")
# ---------------------------------------------------------------------------- #
# Tables 
summary(table0, text = TRUE, pfootnote = TRUE, 
        title = "Covariates by dementia status in WII accelerometer sub-study", labelTranslations = labels_cov0)

summary(table1, text = TRUE, pfootnote = TRUE, 
        title = "Covariates by dementia status in WII accelerometer sub-study", labelTranslations = labels_cov1)


summary(table3, text = TRUE, pfootnote = TRUE, 
        title = "Covariates by dementia status in WII accelerometer sub-study", labelTranslations = labels_cov3)

summary(table4, text = TRUE, pfootnote = TRUE, 
        title = "Features by dementia status in WII accelerometer sub-study")


# ---------------------------------------------------------------------------- #
# Tables media IQR Wilcoxon
summary(table_0, text = TRUE, pfootnote = TRUE, 
        title = "Covariates by dementia status in WII accelerometer sub-study", labelTranslations = labels_cov0)

summary(table_1, text = TRUE, pfootnote = TRUE, 
        title = "Covariates by dementia status in WII accelerometer sub-study", labelTranslations = labels_cov1)


summary(table_3, text = TRUE, pfootnote = TRUE, 
        title = "Covariates by dementia status in WII accelerometer sub-study", labelTranslations = labels_cov3)

summary(table_4, text = TRUE, pfootnote = TRUE, 
        title = "Features by dementia status in WII accelerometer sub-study")

# ---------------------------------------------------------------------------- #
# follow up
quantile(X$y_time, 0.90)
quantile(X$y_time, 0.75)
quantile(X$y_time, 0.5)

time_case = ifelse(X$y == 1, X$y_time, NA)
quantile(time_case, 0.90, na.rm=TRUE)
quantile(time_case, 0.75, na.rm=TRUE)
quantile(time_case, 0.5, na.rm=TRUE)
# ---------------------------------------------------------------------------- #
# End 

