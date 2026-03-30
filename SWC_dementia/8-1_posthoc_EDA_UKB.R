# ---------------------------------------------------------------------------- #
# Script : 8-1_posthoc_EDA_UKB.R
# Author : I M Danilevicz 
# Remark : Part of the work available in:
#          C Cavailles, IM Danilevicz, S Vidil et al (2026). 
#          "Contribution of Digital Sleep-Wake Cycle Metrics to Dementia Prediction in Older Adults", (under review)
# ---------------------------------------------------------------------------- #

# Set local language as english to be able to read dates properly
# Clear the Environment 
rm(list=ls())
Sys.setlocale("LC_ALL","English")

# ---------------------------------------------------------------------------- #
# Packages 
library(arsenal) 
library(haven)
library(tidyverse)
# ---------------------------------------------------------------------------- #
# Folders

dir_load = "D:/..."
dir_fun  = "D:/..."
dir_cv   = "D:/..."
dir_ian  = "D:/..."
dir_p2  = "D:/..."

# ---------------------------------------------------------------------------- #
# to load data

setwd(dir_ian)
data0 = read_dta("Final dataset_251601.dta") 
data1 = read_dta("Data_ukb_cov1_270424.dta")
data2 = read_dta("Data_ukb_cov2_270424.dta")
data3 = read_dta("Data_ukb_cov3_270424.dta")
data4 = read_dta("Sam_ukb_dataset3.dta")
data5 = read_dta("Sam_ukb_dataset4.dta")
data6 = read_dta("cns_meds.dta")
data7 = read_dta("apoe.dta")
data8 = read_dta("dataset_diseasesanddementianew.dta")

#setwd(dir_list)
#list = read.csv("milestones_list.csv")


# ---------------------------------------------------------------------------- #
# Data
setwd(dir_load)
dat0 = read.csv2("data_UKB_clean.csv", header = TRUE, sep=",", dec=".", quote = "\"")

list = dat0 %>% select(stno)
list$included = 1 
# Between February 2013 and December 2015, participants who had provided a
# valid email address were sent an email invitation to wear an accelerometer for seven days.
# Doherty (2017) Plos one doi:10.1371/journal.pone.0169649
# midpoint 15 July 2014

midpoint_date = as.Date("15/06/2014", format = "%d/%m/%Y")

# ---------------------------------------------------------------------------- #
# Data GGIR part 2 
# to extract start_time

setwd(dir_p2)
p2 = read.csv2("part2_summary.csv", header = TRUE)
p2$doacc = as.Date(p2$start_time, format = "%Y-%m-%dT%H:%M:%S%z")
P2 = p2 %>% select(ID, doacc)


# ---------------------------------------------------------------------------- #
# Data variable lot 1
Data1                = merge(data1, list, by.x = "n_eid", by.y = "stno" , all = TRUE)
Data1                = merge(Data1, P2, by.x = "n_eid", by.y = "ID" , all = TRUE)
Data1$included       = ifelse(is.na(Data1$included), 0, 1)

Data1$doacc          = replace(Data1$doacc, is.na(Data1$doacc), midpoint_date)
Data1$age_acc        = (Data1$doacc - Data1$dob)/365.25 
Data1$age_acc_60plus = ifelse(Data1$age_acc >= 60, 1, 0)

Data1$sex            = factor(Data1$sex, levels = c(1,2), labels = c("Male","Female"))
Data1$edu            = factor(Data1$edu, levels = c(0,1,2), labels = c("Lower","Secondary","Higher"))
Data1$age_acc_60plus = factor(Data1$age_acc_60plus, levels = c(0,1), labels = c("<60","60+"))
Data1 = Data1 %>% select(n_eid, age_acc, age_acc_60plus, sex, edu, included, doacc)

# ---------------------------------------------------------------------------- #
# Data variable lot 2
# smoking
# fruits and vegetables
# alcohol consumption 
# marital status and cohabitation
# N chronic diseases: all except mental disorder
Data2          = merge(data2, Data1, by = "n_eid", all = TRUE)
Data2$doacc    = replace(Data2$doacc, is.na(Data2$doacc), midpoint_date)
Data2$included = ifelse(is.na(Data2$included), 0, Data2$included)

Data2$smoking = factor(Data2$smoking, levels = c(0,1,2), labels = c("Never","Ex-smokers","Smokers"))
Data2$fruitveg = factor(Data2$fruitveg, levels = c(0,1,2), labels = c("Less","Daily","More"))
Data2$alcohol = factor(Data2$alcohol_3cat, levels = c(0,1,2), labels = c("None","1-14 units","More"))
Data2$marcoh = ifelse(Data2$livingalone == 0, 1, 0)
Data2$marcoh = factor(Data2$marcoh, levels = c(0,1), labels = c("Alone","Married/co"))
Data2$HYPTEN_p = ifelse(Data2$hypertension_out_sro == 1 & Data2$hypertension_out_sro_date <= Data2$doacc, 1, 0)
Data2$HYPTEN_p = ifelse(is.na(Data2$HYPTEN_p), 0, Data2$HYPTEN_p)
Data2$HYPTEN_p = factor(Data2$HYPTEN_p, levels = c(0,1), labels = c("No","Hypertension"))
Data2$DIABETES_p = ifelse(Data2$diabetes_out_sro == 1 & Data2$diabetes_out_sro_date <= Data2$doacc, 1, 0)
Data2$DIABETES_p = ifelse(is.na(Data2$DIABETES_p), 0, Data2$DIABETES_p)
Data2$DIABETES_p = factor(Data2$DIABETES_p, levels = c(0,1), labels = c("No","Diabetes"))

# N chronic diseases
Data2$CHD_p = ifelse(Data2$chd_out_sro == 1 & Data2$chd_out_sro_date <= Data2$doacc, 1, 0)
Data2$CHD_p = ifelse(is.na(Data2$CHD_p), 0, Data2$CHD_p)
Data2$STROKE_p = ifelse(Data2$stroke_out_sro == 1 & Data2$stroke_out_sro_date <= Data2$doacc, 1, 0)
Data2$STROKE_p = ifelse(is.na(Data2$STROKE_p), 0, Data2$STROKE_p) 
Data2$HF_p = ifelse(Data2$hfailure_out_sro == 1 & Data2$hfailure_out_sro_date <= Data2$doacc, 1, 0)
Data2$HF_p = ifelse(is.na(Data2$HF_p), 0, Data2$HF_p)
Data2$ARTH1_p = ifelse(Data2$osteoarthritis_out_sro == 1 & Data2$osteoarthritis_out_sro_date <= Data2$doacc, 1, 0)
Data2$ARTH1_p = ifelse(is.na(Data2$ARTH1_p), 0, Data2$ARTH1_p)
Data2$ARTH2_p = ifelse(Data2$rheumarthritis_out_sro == 1 & Data2$rheumarthritis_out_sro_date <= Data2$doacc, 1, 0)
Data2$ARTH2_p = ifelse(is.na(Data2$ARTH2_p), 0, Data2$ARTH2_p)
Data2$CANCER_p = ifelse(Data2$cancer_out_sro == 1 & Data2$cancer_out_sro_date <= Data2$doacc, 1, 0)
Data2$CANCER_p = ifelse(is.na(Data2$CANCER_p), 0, Data2$CANCER_p)
Data2$DEPR_p = ifelse(Data2$depression_out_sro == 1 & Data2$depression_out_sro_date <= Data2$doacc, 1, 0)
Data2$DEPR_p = ifelse(is.na(Data2$DEPR_p), 0, Data2$DEPR_p)
Data2$PARK_p = ifelse(Data2$parkinson_out_sro == 1 & Data2$parkinson_out_sro_date <= Data2$doacc, 1, 0)
Data2$PARK_p = ifelse(is.na(Data2$PARK_p), 0, Data2$PARK_p)
Data2$COPD_p = ifelse(Data2$copd_out_sro == 1 & Data2$copd_out_sro_date <= Data2$doacc, 1, 0)
Data2$COPD_p = ifelse(is.na(Data2$COPD_p), 0, Data2$COPD_p)
Data2$LIVER_p = ifelse(Data2$liver_out_sro == 1& Data2$liver_out_sro_date <= Data2$doacc, 1, 0)
Data2$LIVER_p = ifelse(is.na(Data2$LIVER_p), 0, Data2$LIVER_p)
Data2 = Data2 %>% select(n_eid, age_acc, age_acc_60plus, sex, edu, included,
                         smoking, bmi, fruitveg, alcohol, marcoh, HYPTEN_p, 
                         DIABETES_p, CHD_p, STROKE_p, HF_p, ARTH1_p, ARTH2_p,
                         CANCER_p, DEPR_p, PARK_p, COPD_p, LIVER_p, age_acc_60plus)

# ---------------------------------------------------------------------------- #
# Data variable lot 3
# N chronic diseases: mental disorder
Data3 =merge(data3, Data1, by = "n_eid", all = TRUE)
Data3$doacc    = replace(Data3$doacc, is.na(Data3$doacc), midpoint_date)
Data3$included = ifelse(is.na(Data3$included), 0, Data3$included)

Data3$MD_p = ifelse(Data3$mentaldisorders_out_sro == 1 & Data3$mentaldisorders_out_sro_date <= Data3$doacc, 1, 0)
Data3$MD_p = ifelse(is.na(Data3$MD_p), 0, Data3$MD_p)
Data3 = Data3 %>% select(n_eid, MD_p) 

# ---------------------------------------------------------------------------- #
# Data variable lot 4
# Cholesterol 
Data4 =merge(data4, Data1, by = "n_eid", all = TRUE)
Data4$LDL = ifelse(is.na(Data4$n_30780_0_0), Data4$n_30780_1_0, Data4$n_30780_0_0)
Data4 = Data4 %>% select(n_eid,  LDL)

# ---------------------------------------------------------------------------- #
# Data variable lot 5
# Cholesterol drugs 
Data5 =merge(data5, Data4, by = "n_eid", all = TRUE)
Data5$LDLdrug = ifelse(is.na(Data5$n_6153_0_0), 0, ifelse(Data5$n_6153_0_0 == 1, 1, 0))
Data5$HYPLIP = ifelse(Data5$LDLdrug == 1, 1, 
                      ifelse(is.na(Data5$LDL), 0, 
                             ifelse(Data5$LDL > 4.1, 1, 0)))
Data5$HYPLIP = factor(Data5$HYPLIP, levels = c(0,1), labels = c("No","Hyperlipidemia"))
Data5 = Data5 %>% select(n_eid, HYPLIP) 

# ---------------------------------------------------------------------------- #
# Data variable lot 6
# CNS (central nervous system) drugs 

Data6 =merge(data6, Data1, by = "n_eid", all = TRUE)
Data6$cns1 = ifelse(is.na(Data6$antidep_0_0),0,1)
Data6$cns2 = ifelse(is.na(Data6$anxio_0_0),0,1)
Data6$cns3 = ifelse(is.na(Data6$antipsych_0_0),0,1)
Data6$cns4 = ifelse(is.na(Data6$sed_hypn_0_0),0,1)
Data6$CNS = Data6$cns1 + Data6$cns2 + Data6$cns3 + Data6$cns4
Data6$CNS = ifelse(Data6$CNS >= 1, 1, 0)
Data6$CNS = factor(Data6$CNS, levels = c(0,1), labels = c("No","CNS"))
Data6 = Data6 %>% select(n_eid, CNS) 

# ---------------------------------------------------------------------------- #
# Data variable lot 7
# Apoe
Data7 =merge(data7, Data1, by = "n_eid", all = TRUE)
Data7$apoe4_1a = ifelse(Data7$apoe4 > 0, 1, 0)
Data7$apoe4_1a = factor(Data7$apoe4_1a, levels = c(0,1), labels = c("No",">=1 allele"))
Data7 = Data7 %>% select(n_eid, apoe4_1a) 

# ---------------------------------------------------------------------------- #
# Data variable lot 8
# Dementia all cases: Incident 
Data8 =merge(data8, Data1, by = "n_eid", all = TRUE)

Data8$doacc    = replace(Data8$doacc, is.na(Data8$doacc), midpoint_date)
Data8$included = ifelse(is.na(Data8$included), 0, Data8$included)

Data8$DEM = ifelse(is.na(Data8$dementia),0 , Data8$dementia)
Data8$DEM_i = ifelse(Data8$DEM == 1 & Data8$dementia_date > Data8$doacc, 1, 0)  
Data8$DEM_p = ifelse(Data8$DEM == 1 & Data8$dementia_date <= Data8$doacc, 1, 0) 
Data8$DEM_e = ifelse(Data8$DEM == 1 & Data8$age_acc <= 65, 1, 0) # early dementia, <65 years
Data8$DEM_i = factor(Data8$DEM_i, levels = c(0,1), labels = c("No","Incident dementia"))
Data8 = Data8 %>% select(n_eid, DEM_i, DEM_p, DEM_e) 



# ---------------------------------------------------------------------------- #
# Merge data

Data9 = merge(Data2, Data3, by = "n_eid", all = TRUE)
Data9 = merge(Data9, Data4, by = "n_eid", all = TRUE)
Data9 = merge(Data9, Data5, by = "n_eid", all = TRUE)
Data9 = merge(Data9, Data6, by = "n_eid", all = TRUE)
Data9 = merge(Data9, Data7, by = "n_eid", all = TRUE)
Data9 = merge(Data9, Data8, by = "n_eid", all = TRUE)
Data9$ARTH_p = ifelse(Data9$ARTH1_p == 1, 1, ifelse(Data9$ARTH2_p == 1, 1, 0))
Data9$Nchronic = Data9$CHD_p + Data9$STROKE_p + Data9$HF_p + Data9$ARTH_p +
                  Data9$CANCER_p + Data9$DEPR_p + Data9$PARK_p + Data9$COPD_p + Data9$LIVER_p +Data9$MD


# ---------------------------------------------------------------------------- #
# filtering

Data9 = Data9 %>% filter(included == 1 | age_acc >= 60)
Data9 = Data9 %>% filter(DEM_p == 0)
Data9 = Data9 %>% filter(included == 1 |DEM_e == 0)

# ---------------------------------------------------------------------------- #
# make table 
control_Wilcoxon = tableby.control(
  total = FALSE,
  digits = 3, 
  digits.p = 3,
  numeric.simplify = TRUE,
  cat.simplify = TRUE,
  numeric.stats = c("Nmiss", "medianq1q3", "iqr"),
  test = TRUE,
  test.numeric = "wilcoxon"  
)

control_ttest = tableby.control(
  total = FALSE,
  digits = 1, 
  digits.p = 3,
  numeric.simplify = TRUE,
  cat.simplify = TRUE,
  numeric.stats = c("Nmiss", "meansd"),
  test = TRUE,
  indent = FALSE, 
  test.numeric = "t.test" 
)

table1 = tableby(included ~ age_acc + sex + edu + marcoh + smoking + fruitveg + alcohol + bmi + HYPTEN_p + 
                  DIABETES_p + HYPLIP + Nchronic + CNS, 
                 data=Data9, control=control_ttest)
table2 = tableby(included ~ apoe4_1a, 
                 data=Data9, control=control_ttest)
table3 = tableby(included ~ DEM_i, 
                 data=Data9, control=control_ttest)

summary(table1)
summary(table2)
summary(table3)

# ---------------------------------------------------------------------------- #
# End 
