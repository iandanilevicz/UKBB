# ========================================================= #
# Extracting UK biobank files
# Author: Vincent T. van Hees, Accelting
#         Ian M. Danilevicz, INSERM
# Date: 2023
# R script to process accelerometer data from UK biobank
# remotes::install_github("wadpac/GGIR")
# ========================================================= #
library(GGIRread)
library(GGIR)
print(packageVersion("GGIR"))      # Used version 3.0.0
print(packageVersion("GGIRread"))  # Used version 0.3.1

user = "Ian"
study  = "UKBiobank"

# Parameters that are user specific:
if (user == "Vincent") {
  local_datadir = "D:/whitehall"
  local_outputdir = "D:/Dropbox/Work/sharedfolder/projects/ERC_Paris/Whitehall"
  verisensedir = "D:/Code/erc-accelerometry/verisense_count_steps.R"
  # verisensedir = "verisense_count_steps.R"
  mode = 6
  do.parallel = TRUE
  f0 = 1
  f1 = 100
  do.report = c(5, 6) #c() #c(2, 4, 5, 6)
  maxNcores = 12
  fns = dir("D:/Code/GGIR/R", full.names = TRUE) # creating list of filenames of scriptfiles to load
  for (i in fns) source(i)
  
} else if (user == "Ian") {
  local_datadir = "/home1/USERS/DATA_UKBB/TEST5"
  local_outputdir = "/home1/USERS/DATA_UKBB/RESULTS"
  verisensedir = "/home1/USERS/meneghel/verisense_count_steps.R"
  mode = 1:2
  do.parallel = TRUE
  f0 = 1
  f1 = 0
  do.report = 2
  maxNcores = 36 # this is for a server, standard PC has only 8
  library(GGIR)
}

# Parameters that are study specific
if (study == "UKBiobank") {
  timewindow = "OO"
  strategy = 1
  maxdur = 7
  datadir = local_datadir
  outputdir = local_outputdir
  studyname = "binfiles"
  loglocation = NULL
  data_cleaning_file = NULL
  idloc = 2
  excludefirstlast = FALSE
  LUX_cal_constant = NULL
  LUX_cal_exponent = NULL
  desiredtz = "Europe/London"
  part6Window = c("O1", "O-1")
} else if (study == "Whitehall") {
  timewindow = "WW"
  strategy = 2
  maxdur = 9
  datadir = local_datadir
  outputdir = local_outputdir
  studyname = "whitehall"
  loglocation = paste0(outputdir, "/output_whitehall/sleeplog_corrected_v9.csv")
  data_cleaning_file = paste0(outputdir, "/output_whitehall/data_cleaning_file_v7.csv")
  idloc = 2
  excludefirstlast = TRUE
  LUX_cal_constant = 1.173
  LUX_cal_exponent = 0.0193
  part6Window = c("W1", "W-1")
  desiredtz = "Europe/London"
} else if (study == "Clinic") {
  timewindow = "WW"
  strategy = 2
  maxdur = 9
  datadir = "D:/binfiles"
  outputdir = "D:/analysis"
  studyname = "binfiles"
  loglocation = ".../sleeplog_corrected_v9.csv"
  data_cleaning_file = ".../data_cleaning_file_v7.csv"
  idloc = 2
  excludefirstlast = TRUE
  LUX_cal_constant = NULL
  LUX_cal_exponent = NULL
  part6Window = c("start", "end")
  desiredtz = "Europe/Paris"
}

# Load step detection function:
source(verisensedir)
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

GGIR(#-------------------------------
     # General parameters
     #-------------------------------
     myfun = myfun,
     mode = mode,
     datadir = datadir,
     outputdir = outputdir,
     studyname = studyname,
     do.report = do.report,
     f0 = f0,
     f1 = f1,
     overwrite = TRUE,
     do.parallel = do.parallel,
     maxNcores = maxNcores,
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
     do.part2.pdf = FALSE,
     #-------------------------------
     # Part 3 parameters:
     #-------------------------------
     do.part3.pdf = FALSE, 
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
     # saving output
     save_ms5rawlevels = TRUE,
     save_ms5raw_format = "RData",
     includedaycrit.part5 = 2 / 3,
     minimum_MM_length.part5 = 23,
     frag.metrics = "all",
     LUX_cal_constant = LUX_cal_constant,
     LUX_cal_exponent = LUX_cal_exponent,
     LUX_day_segments = c(4, 8, 12, 16, 20, 24),
     part5_agg2_60seconds = TRUE,
     cosinor = TRUE,
     #-------------------------------
     # Part 6 parameters:
     #-------------------------------
     part6_threshold_combi = "40_100_400",
     part6CR = TRUE,
     part6Window = part6Window,
     #-----------------------------------
     # pdf report generation
     visualreport = FALSE)

