# =================================== BACKSTOP =================================
# Creating the actual backstop rate I(g_0>=1) in examining the classical 
# PD-comparison approach to rendering SICR-decisions.
# ------------------------------------------------------------------------------
# PROJECT TITLE: Dynamic SICR-research
# SCRIPT AUTHOR(S): Esmerelda Oberholzer, Dr Arno Botha
# ------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2b.Data_Preparation_Credit.R
#   - 2c.Data_Enrich.R
#   - 2d.Data_Fusion.R
#   - 3a.PD_logit.R | Basic PD-model, from which PD-ratio is obtained for use in SICR-models

# -- Inputs:
#   - creditdata_final4c | enriched credit dataset (script 3a)

# -- Outputs:
#   - dat_Backstop_smp | subsampled data with actual backstop rates
# ==============================================================================




# ------ 0. Setup/parameter definition


# -- Parameters used in the SICR-definition
# k: - outcome period
# s: - number of consecutive payments (stickiness)
# d: - delinquency threshold

# - Define the parameters
p.k <- 1 # for creating a 1-month transition rate from g_0(t)=0 to g_0(t+1)=1, i.e., the "backstop" from IFRS 9
p.s <- 1 # Stickiness is irrelevant for the PD-comparison approach
p.d <- 1 # 30 days past due, i.e., the "backstop" from IFRS 9

# - Field names
stratifiers <- c("Backstop_target_event", "Date") 
targetVar <- "Backstop_target_event"
timeVar <- "Date"

# - Subsampling & resampling parameters
smp_size <- 250000 # fixed size of downsampled set




# ------ 1. Define the target event 

# - Confirm prepared data after exclusions is loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4c"), tempPath)

# - Create the backstop definition based on the parameters
datCredit_real[, Backstop_def := SICR_flag(g0_Delinq, d=p.d, s=p.s), by=list(LoanID)]
describe(datCredit_real$Backstop_def) #ensure there are no missing values and only two distinct values (binary) - success

# - Look ahead (over k periods) and assign the backstop event appropriately for each record
datCredit_real[, Backstop_target_event := shift(Backstop_def, type='lead', n=p.k), by=list(LoanID)]
# check whether k-periods have NA for each account
datCredit_real[, check_backstop_periods := ifelse(is.na(Backstop_target_event), 1, 0), ]
# check the number of observations impacted
(exclusions_missing_periods <- datCredit_real[(check_backstop_periods == 1), .N] / datCredit_real[, .N] * 100)
# Number of impacted observations: 1.5% missing outcomes, considered safe

# - Check the event rate 
# RECORD-LEVEL
table(datCredit_real$Backstop_target_event) %>% prop.table()
# RESULT: 6.19% (vs 6.17% when setting k=0)




# ------ 2. Apply resampling scheme and save the data

# - Firstly, resample observations of the data - two-way stratified dataset by SICR-event and date
smp_perc <- smp_size / ( datCredit_real[complete.cases(mget(stratifiers)), mget(stratifiers)][,.N] ) # Implied sampling fraction for downsampling step

# - Downsample data into a set with a fixed size (using stratified sampling) before implementing resampling scheme
set.seed(1)
dat_Backstop <- datCredit_real %>% drop_na(all_of(stratifiers)) %>% group_by(across(all_of(stratifiers))) %>% slice_sample(prop=smp_perc) %>% as.data.table()
cat( (dat_Backstop[is.na(get(targetVar)), .N] == 0) %?% 'SAFE: No missingness in target variable.\n' %:% 
       'WARNING: Missingness detected in target variable.\n')
# safe, not missings

# - Check the event rate 
# RECORD-LEVEL
table(dat_Backstop$Backstop_target_event) %>% prop.table()
# RESULT: 6.16%, very close to the rate on the full dataset

# - Filter for the variables to keep
varKeep <- c("LoanID", "Date", "Backstop_target_event", "Backstop_def", "PD_ratio")
dat_Backstop_smp <- subset(dat_Backstop, select=varKeep)

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "data_backstop_target"), dat_Backstop_smp)

# - Clenaup
rm(datCredit_real, dat_Backstop, dat_Backstop_smp); gc()
