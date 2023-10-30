# ========================================================= #
# Script: 
# Author: Vincent T. van Hees, Accelting
#         Mathilde Chen, INSERM
#         Ian M. Danilevicz, INSERM
# Date: 2023
# R script to process accelerometer data from UK biobank
# ========================================================= #

remotes::install_github("wadpac/GGIR")
library(GGIRread)
library(GGIR)
print(packageVersion("GGIR"))      # Used version 3.0.0
print(packageVersion("GGIRread"))  # Used version 0.3.1

datadir="/home1/USERS/DATA_UKBB/TEST5"
outputdir="/home1/USERS/DATA_UKBB/RESULTS"

source("verisense_count_steps.R")
myfun = list(FUN = verisense_count_steps,
             parameters = c(4, 4, 20, -1.0, 4, 4, 0.01, 1.25), # updated based on Rowlands et al Stepping up with GGIR 2022
             expected_sample_rate = 15,
             expected_unit = "g",
             colnames = c("step_count"),
             outputres = 1,
             minlength = 1,
             outputtype = "numeric",
             aggfunction = sum,
             timestamp = F,
             reporttype = "event")

g.shell.GGIR(#-------------------------------
             # General parameters
             #-------------------------------
             myfun = myfun,
             mode = 1:5,
             datadir = datadir,
             outputdir = outputdir,
             studyname = studyname,
             do.report = c(2, 4, 5),
             f0 = 1,
             f1 = 0,
             overwrite = TRUE,
             do.parallel = TRUE,
             maxNcores = 8,
             idloc = idloc,
             print.filename = TRUE,
             storefolderstructure = TRUE,
             data_cleaning_file = data_cleaning_file,
             desiredtz = desiredtz, 
             #-------------------------------
             # Part 1 parameters:
             #-------------------------------
             windowsizes = c(5, 900, 3600),
             do.enmo = TRUE,
             do.anglez = TRUE,
             chunksize = 1,
             printsummary = TRUE,
             #-------------------------------
             # Part 2 parameters:
             #-------------------------------
             strategy = strategy,
             maxdur = maxdur,
             winhr = c(5, 10),
             ilevels = c(seq(0, 400, by = 50), 8000),
             iglevels = TRUE,
             mvpathreshold = c(100, 120),
             IVIS_windowsize_minutes = 60,
             IVIS.activity.metric = 2,
             #-------------------------------
             # Part 3 parameters:
             #-------------------------------
             do.part3.pdf = TRUE, 
             #-------------------------------
             # Part 4 parameters:
             #-------------------------------
             excludefirstlast = excludefirstlast, # Exclude first and last night for sleep analysis?
             def.noc.sleep = 1,
             loglocation = loglocation,
             outliers.only = TRUE,
             criterror = 4,
             colid = 1,
             coln1 = 2,
             #-------------------------------
             # Part 5 parameters:
             #-------------------------------
             # Threshold for intensity levels
             threshold.lig = c(40, 45),
             threshold.mod = c(100, 110),
             threshold.vig = c(400),
             boutcriter = 0.8,
             boutcriter.in = 1,
             boutcriter.lig = 1,
             boutcriter.mvpa = 1,
             # duration of bouts to be calculated
             boutdur.in = c(10, 30), 
             boutdur.lig = c(10),
             boutdur.mvpa = c(10),
             timewindow = timewindow,
             save_ms5rawlevels = TRUE,
             includedaycrit.part5 = 2 / 3,
             minimum_MM_length.part5 = 23,
             frag.metrics = "all",
             LUX_cal_constant = LUX_cal_constant,
             LUX_cal_exponent = LUX_cal_exponent,
             LUX_day_segments = c(4, 8, 12, 16, 20, 24),
             part5_agg2_60seconds = TRUE,
             cosinor = TRUE,
             #-----------------------------------
             # pdf report generation
             visualreport = FALSE)


