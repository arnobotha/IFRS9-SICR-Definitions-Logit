# ============================== MEASURE ANALYSIS ===============================
# Examining performance measures for a given SICR-definition and resulting model
# by swapping in different datasets: full set, subsampled set, validation set.
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Dynamic SICR-research
# SCRIPT AUTHOR(S): Dr Arno Botha
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2b.Data_Preparation_Credit.R
#   - 2c.Data_Enrich.R
#   - 2d.Data_Fusion.R 
#   - 3a.PD_logit.R | Basic PD-model, from which PD-ratio is obtained for use in SICR-models
#   - 3b.SICR_def_<>_logit.R | the 3b-series of scripts for definitions 1a-2c, for (i)-(iv)

# -- Inputs:
#   - datSICR_smp_<> | specific SICR-sample upon which resampling scheme is applied (3b)
#   - datSICR_<> | Specific SICR-dataset (unsampled) for ancillary analysis (3b)

# -- Outputs:
#   - <analytics>
# =======================================================================================




# ------ 1. Setup and data preparation

# - Load the full and subsampled SICR-datasets (and associated objects) for a given SICR-definition
SICR_label <- "1a(ii)"
perfObjName <- paste0('performance_measures_', substring(SICR_label, 1, 2),"_", str_match(SICR_label, "\\(([^)]+)\\)")[,2])
if (!exists('datSICR')) unpack.ffdf(paste0(genPath,"datSICR_", SICR_label), tempPath)
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
if (!exists('datSICR_valid')) unpack.ffdf(paste0(genPath,"datSICR_valid_", SICR_label), tempPath)
if(!exists('logit_model_chosen')) unpack.ffdf(paste0(genPath, "logit_model_", SICR_label), tempPath)
if (!exists(perfObjName)) {
  unpack.ffdf(paste0(genObjPath,"performance_measures_", SICR_label), tempPath)
  assign("perfMeasures", get(perfObjName ) )
  rm(list=perfObjName)
} 

# - Score full dataset with the underlying SICR-model
# Join the macroeconomic variables
datSICR <- merge_macro_info(input_dat = datSICR); gc()
# Score full set with SICR-model
datSICR[, ExpProb := predict(logit_model_chosen, newdata = datSICR, type="response")]

# - Calculate account-level standard deviance of probability scores over loan life
datSICR[, SICR_predict_variance := sd(ExpProb), by=list(LoanID)]
datSICR_smp[, SICR_predict_variance := sd(ExpProb), by=list(LoanID)]
datSICR_valid[, SICR_predict_variance := sd(Prob_chosen_1a_ii), by=list(LoanID)]

# - Create SICR-rate | Full set
SICR_StartDte <- min(datSICR$Date, na.rm=T)
SICR_EndDte <- max(datSICR$Date, na.rm=T)
port.aggr_full <- datSICR[SICR_def==0, list(EventRate = sum(as.numeric(levels(SICR_target))[SICR_target], na.rm=T)/.N, AtRisk = .N),
                          by=list(Date)][Date >= SICR_StartDte & Date <= SICR_EndDte,] %>% setkey(Date)

# - Create SICR-rate | Subsampled set
SICR_StartDte <- min(datSICR_smp$Date, na.rm=T)
SICR_EndDte <- max(datSICR_smp$Date, na.rm=T)
port.aggr_sub <- datSICR_smp[SICR_def==0, list(EventRate = sum(as.numeric(levels(SICR_target))[SICR_target], na.rm=T)/.N, AtRisk = .N),
                             by=list(Date)][Date >= SICR_StartDte & Date <= SICR_EndDte,] %>% setkey(Date)

# - Create SICR-rate | Validation set
SICR_StartDte <- min(datSICR_valid$Date, na.rm=T)
SICR_EndDte <- max(datSICR_valid$Date, na.rm=T)
port.aggr_valid <- datSICR_valid[SICR_def==0, list(EventRate = sum(as.numeric(levels(SICR_target))[SICR_target], na.rm=T)/.N, AtRisk = .N),
                             by=list(Date)][Date >= SICR_StartDte & Date <= SICR_EndDte,] %>% setkey(Date)




# ------ 2. Full dataset analysis D

# - General statistics
comma(datSICR[, .N])
comma(length(unique(datSICR$LoanID)))
### RESULTS: ~521k unique loans, though we have 34m observations
describe(datSICR[,list(Periods=.N), by=list(LoanID)]$Periods)
### RESULTS: mean number of SICR-observations of 65.64 (median: 56) per account, spanning [1,152].

# - Prevalence \phi_{dsk}
datSICR[, list(SICR_Obs = sum(as.numeric(levels(SICR_target))[SICR_target], na.rm=T), k = mean(k),
               Prevalence = mean(as.numeric(levels(SICR_target))[SICR_target], na.rm=T))]
### RESULTS: ~2.1m SICR-events, prevalence of 6.15%

# - Prediction dynamicity \omega_{dsk}
# NOTE: corresponds to perfMeasures$std_dev
round(mean(datSICR$SICR_predict_variance, na.rm=T)*100, digits=1)
### RESULTS: 5.4% standard deviation

# - Instability \sigma_{dsk}
# NOTE: corresponds to perfMeasures$std_dev_SICR_rate_Act
percent(sd(port.aggr_full$EventRate, na.rm=T), accuracy=0.01)
### RESULTS: 1.36%




# ------ 3. Subsampled set analysis D_S

# - General statistics
comma(datSICR_smp[, .N])
comma(length(unique(datSICR_smp$LoanID)))
### RESULTS: ~179k unique loans, though we have ~250k observations in this sample (as intended)
describe(datSICR_smp[,list(Periods=.N), by=list(LoanID)]$Periods)
### RESULTS: mean number of SICR-observations of 1.394 per account, spanning [1,9].
# This is a great reduction compared to the full set, simply due to subsampling

# - Prevalence
datSICR_smp[, list(SICR_Obs = sum(as.numeric(levels(SICR_target))[SICR_target], na.rm=T), k = mean(k),
                   Prevalence = mean(as.numeric(levels(SICR_target))[SICR_target], na.rm=T))]
### RESULTS: ~15k SICR-events, prevalence of 6.13%. Practically unchanged from full sample and deemed safe.

# - Prediction dynamicity \omega_{dsk}
# NOTE: corresponds to perfMeasures$std_dev
round(mean(datSICR_smp$SICR_predict_variance, na.rm=T)*100, digits=1)
### RESULTS: 3.8% standard deviation, reasonably close to the validation sample's estimate, though
# still very different (smaller) than that of the full dataset. This is sensible given the far fewer
# SICR-observations in either the subsample or validation set, compared to those available in the full dataset

# - Instability \sigma_{dsk}
# NOTE: corresponds to perfMeasures$std_dev_SICR_rate_Act
percent(sd(port.aggr_sub$EventRate, na.rm=T), accuracy=0.01)
round(sd(port.aggr_sub$EventRate, na.rm=T), digits=4) == perfMeasures$std_dev_SICR_rate_Act # TRUE
### RESULTS: 1.43%, reasonably close to that in the full dataset



# ------ 4. Validation set analysis D_V

# - General statistics
comma(datSICR_valid[, .N])
comma(length(unique(datSICR_valid$LoanID)))
### RESULTS: ~67k unique loans, though we have ~75k observations in this sample (as intended)
describe(datSICR_valid[,list(Periods=.N), by=list(LoanID)]$Periods)
### RESULTS: mean number of SICR-observations of 1.115 per account, spanning [1,4].
# This is a great reduction compared to the full set, simply due to subsampling

# - Prevalence
datSICR_valid[, list(SICR_Obs = sum(as.numeric(levels(SICR_target))[SICR_target], na.rm=T), k = mean(k),
                   Prevalence = mean(as.numeric(levels(SICR_target))[SICR_target], na.rm=T))]
### RESULTS: ~4.6k SICR-events, prevalence of 6.2%, noticeably larger than previous sets

# - Prediction dynamicity \omega_{dsk}
# NOTE: corresponds to perfMeasures$std_dev
round(mean(datSICR_valid$SICR_predict_variance, na.rm=T)*100, digits=1)
round(mean(datSICR_valid$SICR_predict_variance, na.rm=T)*100, digits=1) == perfMeasures$std_dev # TRUE
### RESULTS: 3.6% standard deviation. Note that the above relation may no longer hold if we change the set to D_S

# - Instability \sigma_{dsk}
# NOTE: corresponds to perfMeasures$std_dev_SICR_rate_Act
percent(sd(port.aggr_valid$EventRate, na.rm=T), accuracy=0.01)
### RESULTS: 1.51%, reasonably close to that in the full dataset




# ------- Cleanup
rm(datSICR, datSICR_smp, datSICR_valid, logit_model_chosen, perfMeasures,
   port.aggr_full, port.aggr_sub, port.aggr_valid); gc()
