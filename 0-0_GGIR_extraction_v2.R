# ========================================================= #
# Generic script for using GGIR in the ERC project
# Author: Vincent T. van Hees, Accelting
#         Ian M. Danilevicz, INSERM
# Date: 2023
# Updated May 2024
# remotes::install_github("wadpac/GGIR", ref = "issue839_add_DFA_to_part6")
# ========================================================= #

# ========================================================= #
# Recordings:  
#   UKBB part 1 used GGIR 3.0.0
#   UKBB part 1 used GGIRread 0.3.1
#   UKBB part 2-6 used GGIR 3.1.0
#   UKBB part 2-6 used GGIRread 1.0.0
# ========================================================= #

# ========================================================= #
# GGIR release
library(GGIRread)
library(GGIR)
print(packageVersion("GGIR"))      # Used version 3.0.0 -> 3.1.0 
print(packageVersion("GGIRread"))  # Used version 0.3.1 -> 1.0.0
# ========================================================= #

# ========================================================= #
# User's edition 
user = "Ian"
study  = "UKBiobank"
batch = 7
# ========================================================= #

# ========================================================= #
# Parameters that are user specific:
if (user == "Vincent") {
  # study  = "UKBiobank"
  # study  = "Clinic"
  study  = "Whitehall"
  if (study == "Whitehall") {
    local_datadir = "D:/Dropbox/Work/sharedfolder/projects/ERC_Paris/Whitehall/evaluationApril2024/test_binfiles_evaluation2024"
    local_outputdir = "D:/Dropbox/Work/sharedfolder/projects/ERC_Paris/Whitehall/evaluationApril2024"
    loglocation = "D:/Dropbox/Work/sharedfolder/projects/ERC_Paris/Whitehall/output_whitehall/sleeplog_corrected_v9.csv"
    data_cleaning_file = "D:/Dropbox/Work/sharedfolder/projects/ERC_Paris/Whitehall/output_whitehall/data_cleaning_file_v7.csv"
  } else if (study == "Clinic") {
    local_datadir = "D:/Dropbox/Work/sharedfolder/DATA/Sabia_Pilot_Summer2022/pilot2022" #"D:/pilot2022"
    local_outputdir = "D:/Dropbox/Work/sharedfolder/projects/ERC_Paris/Clinic"
  } else if (study == "UKBiobank") {
    # local_datadir = "D:/Dropbox/Work/sharedfolder/DATA/Axivity_TestData"
    local_datadir = "D:/Dropbox/Work/sharedfolder/projects/ERC_Paris/UKBiobank/sampleIan"
    local_outputdir = "D:/Dropbox/Work/sharedfolder/projects/ERC_Paris/UKBiobank"
  }
  verisensedir = "D:/Code/erc-accelerometry/verisense_count_steps.R"
  mode = 4:6
  do.parallel = TRUE
  overwrite = TRUE
  do.part2.pdf = TRUE
  part6DFA = FALSE
  f0 = 1
  f1 = 0
  do.report = c(4, 5, 6)
  maxNcores = 12
  fns = dir("D:/Code/GGIR/R", full.names = TRUE) # creating list of filenames of scriptfiles to load
  fns = fns[grep(pattern = "sysdata", x = fns, invert = TRUE)]
  for (i in fns) source(i)
} else if (user == "Ian") {
  if (study == "UKBiobank") {
    if (batch == 0) {
      local_datadir = "/\\172.27.137.244\\UK_BB_run$\\TEST\\Data"
      local_outputdir = "/\\172.27.137.244\\UK_BB_run$\\TEST"
    } else if (batch == 1) {
      local_datadir = "/\\172.27.137.244\\UK_BB_run$\\Batch1"
      local_outputdir = "/\\172.27.137.244\\UK_BB_run$"
    } else if (batch == 2) {
      local_datadir = "/\\172.27.137.244\\UK_BB_run$\\Batch2"
      local_outputdir = "/\\172.27.137.244\\UK_BB_run$"
    } else if (batch == 3) {
      local_datadir = "/\\172.27.137.244\\UK_BB_run$\\Batch3"
      local_outputdir = "/\\172.27.137.244\\UK_BB_run$"
    } else if (batch == 4) {
      local_datadir = "/\\172.27.137.244\\UK_BB_run$\\Batch4"
      local_outputdir = "/\\172.27.137.244\\UK_BB_run$"
    } else if (batch == 5) {
      local_datadir = "/\\172.27.137.244\\UK_BB_run$\\Batch5"
      local_outputdir = "/\\172.27.137.244\\UK_BB_run$"
    } else if (batch == 6) {
      local_datadir = "/\\172.27.137.244\\UK_BB_run$\\Batch6"
      local_outputdir = "/\\172.27.137.244\\UK_BB_run$"
    } else if (batch == 7) {
      local_datadir = "/\\172.27.137.244\\UK_BB_run$\\Batch7"
      local_outputdir = "/\\172.27.137.244\\UK_BB_run$"
    } 
  }
  verisensedir = "/\\172.27.137.244\\UK_BB_run$\\Codes\\verisense_count_steps.R"
  mode = 2:6
  do.parallel = TRUE
  overwrite =  TRUE
  f0 = 1
  f1 = 0
  do.part2.pdf = FALSE
  do.report = c(2,4:6)
  #verisensedir = "verisense_count_steps.R"
  part6DFA = TRUE
  maxNcores = 20 
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
  coln1 = NULL
  visualreport = FALSE
  do.part3.pdf = FALSE
  sleepwindowType = "SPT"
  do.sibreport = FALSE
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
  loglocation = loglocation
  data_cleaning_file = data_cleaning_file
  idloc = 2
  coln1 = 2
  visualreport = FALSE
  do.part3.pdf = TRUE
  sleepwindowType = "SPT"
  do.sibreport = TRUE
  excludefirstlast = TRUE
  LUX_cal_constant = 1.173
  LUX_cal_exponent = 0.0193
  part6Window = c("W1", "end")
  desiredtz = "Europe/London"
} else if (study == "Clinic") {
  timewindow = "WW"
  strategy = 2
  maxdur = 9
  datadir = local_datadir
  outputdir = local_outputdir
  studyname = "pilot2022"
  loglocation =  paste0(outputdir, "/SleepLog_160922_corr_advanced.csv")
  data_cleaning_file = NULL
  idloc = 2
  coln1 = 3
  visualreport = FALSE
  do.part3.pdf = FALSE
  sleepwindowType = "TimeInBed"
  do.sibreport = TRUE
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
     overwrite = overwrite,
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
     do.part2.pdf = do.part2.pdf,
     qwindow = c(),
     #-------------------------------
     # Part 3 parameters:
     #-------------------------------
     do.part3.pdf = do.part3.pdf, 
     #-------------------------------
     # Part 4 parameters:
     #-------------------------------
     excludefirstlast = excludefirstlast, # Exclude first and last night for sleep analysis?
     def.noc.sleep = 1,
     loglocation = loglocation,
     outliers.only = TRUE,
     criterror = 4,
     sleepwindowType = sleepwindowType,
     colid = 1,
     coln1 = coln1,
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
     do.sibreport = do.sibreport,
     save_ms5rawlevels = TRUE,
     save_ms5raw_format = "RData",
     save_ms5raw_without_invalid = FALSE, # this needs to be FALSE, because we want the full time series to be exported by part 5, see Vincent's email 17/05/2024
     includedaycrit.part5 = 2/3,
     includenightcrit.part5 = 2/3,
     minimum_MM_length.part5 = 23,
     frag.metrics = "all",
     LUX_cal_constant = LUX_cal_constant,
     LUX_cal_exponent = LUX_cal_exponent,
     LUX_day_segments = c(4, 8, 12, 16, 20, 24),
     part5_agg2_60seconds = TRUE,
     cosinor = TRUE,
     possible_nap_window = c(0, 24),
     possible_nap_dur = c(15, 240),
     #-------------------------------
     # Part 6 parameters:
     #-------------------------------
     part6_threshold_combi = "40_100_400",
     part6CR = TRUE,
     includecrit.part6 = c(2/3, 2/3),
     part6DFA = part6DFA,
     part6Window = part6Window,
     #-----------------------------------
     # pdf report generation
     dofirstpage = FALSE,
     visualreport = visualreport,
     visualreport_without_invalid = FALSE)
# ========================================================= #
