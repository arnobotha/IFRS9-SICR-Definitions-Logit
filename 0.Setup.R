# =================================== SETUP ====================================
# Preparing runtime environment and setting parameters
# ------------------------------------------------------------------------------
# PROJECT TITLE: Dynamic SICR-research
# SCRIPT AUTHOR(S): Esmerelda Oberholzer, Dr Arno Botha

# DESCRIPTION:
# This script installs and loads various libraries and packages, compiles all
# custom functions, and set requisite parameters.
# The versions of the packages installed are provided in the README-file.
# ------------------------------------------------------------------------------
# -- Inputs:
#   - 0a.Custom_Functions.R | Custom function definitions and helper functions
#   - 0b.findOptimalCutoff.R | Custom functions to aid threshold-selection for logit-models
#   - DelinqM.R | Delinquency measures and related functions
# ==============================================================================



# --------------------------------- PACKAGES -----------------------------------

# ------ Install and load packages
# - data access and big data management
require(haven) # for SAS imports
require(ETLUtils)
require(ffbase)
require(ff)
require(writexl)
#require(arrow)
tempPath <- "C:/TempData"; options("fftempdir"=tempPath)

# - for data wrangling
require(tidyr)
require(dplyr)
require(data.table)
require(lubridate)
require(readr)
require(bit64) # for very big numeric values
require(stringr) # common string operations, e.g., str_pad

# - for analyses
require(Hmisc) # for describe()
require(moments) # for using skewness() function

#for plots
require(ggplot2)
require(scales)
require(ggthemes)
require(extrafont) #remotes::install_version("Rttf2pt1", version = "1.3.8"); Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.55.0/bin/gswin32c.exe"); font_import(); loadfonts(); loadfonts(device="win")
require(RColorBrewer)
require(gridExtra)

#for modelling
require(car)
#require(prediction)
require(MASS) # for stepAIC() for stepwise regression
require(e1071) # for SVM-technique
require(mlr) # for parallelized SMV-technique and associated tuning tools
require(parallelMap); require(parallel) # for multithreaded SVM-tuning using the mlr-package
require(pROC); require(ROCR) # both for conducting ROC-analyses
require(OptimalCutpoints)

# for explainability measures
require(DALEX)
require(ranger)
require(fastshap)

# --------------------------------- GENERAL ------------------------------------

# ------ Parametrisation

# - general R options
options(scipen=999) # Suppress showing scientific notation

# - Parameters used in calculating delinquency measures
sc.Thres <- 0.9; # repayment ratio - g1
d <- 3 # default threshold for g0/g1-measures of delinquency (payments in arrears)
k <- 6 # Probation period


# -- Path variables | General

# - Common path for saving big data objects
genPath <- "C:/Data/Dynamic-SICR_Data/"

# - Common path for importing raw data
genRawPath <- "C:/Data/"


# -- Path variables | User-dependent

if (Sys.getenv("USERNAME") == "WRQ") {
  # - Custom path where R-scripts are saved
  path_cust <- "C:/Users/WRQ/OneDrive - FRG/Analytix/Research/Dynamic-SICR/IFRS9-SICR-Definitions-Logit/"
  
  # - Common path for storing important R-objects as back-up
  genObjPath <- "C:/Users/WRQ/OneDrive - FRG/Analytix/Research/Dynamic-SICR/Objects/"
  
  # - Common path for saving important analytics (e.g., sampling)
  genFigPath <- "C:/Users/WRQ/OneDrive - FRG/Analytix/Research/Dynamic-SICR/IFRS9-SICR-Definitions-Logit/Figures/"
  
} else if (Sys.getenv("USERNAME") == "Arno Botha") {
  # - Custom path where R-scripts are saved
  
  path_cust <- "E:/WorkLife/Analytix/Research/Dynamic-SICR/IFRS9-SICR-Definitions-Logit/"
  
  # - Common path for storing important R-objects as back-up
  genObjPath <- "E:/WorkLife/Analytix/Research/Dynamic-SICR/Objects/"
  
  # - Common path for saving important analytics (e.g., sampling)
  genFigPath <- "E:/WorkLife/Analytix/Research/Dynamic-SICR/IFRS9-SICR-Definitions-Logit/Figures/"
  
  # - Common path for saving big data objects
  genPath <- "E:/DataDump/FNB SLC/Dynamic-SICR_Data/"
  
  # - Common path for importing raw data
  genRawPath <- "E:/DataDump/FNB SLC/"
  
} else {
  stop("User-specific paths not set for current user: ", Sys.getenv("USERNAME"), ". Please fix in Setup script (0.Setup.R) before continuing")
}



# ----------------------------- CUSTOM FUNCTIONS -------------------------------

# ------ Custom function definitions
# - Load all custom functions defined in a separate R-script
source(paste0(path_cust,"0a.Custom_Functions.R"))

# - Compile threshold-selection function for transforming a probabilistic logit-model
# into a discrete classifier
source(paste0(path_cust,"0b.findOptimalCutoff.R"))

# - Compile Delinquency Calculation Functions (CD, MD/DoD)
source(paste0(path_cust,'DelinqM.R'))
