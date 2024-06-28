# ============================== SICR-DEFINITION ANALYSIS ===============================
# Script for computing Shapley-values across SICR-definition class 1a
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Dynamic SICR-research
# SCRIPT AUTHOR(S): Esmerelda Oberholzer,
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2b.Data_Preparation_Credit.R
#   - 2c.Data_Enrich.R
#   - 2d.Data_Fusion.R 
#   - 3a.SICR_def_<>_logit.R | the 3a-series of scripts for definitions 1a-2c, for (i)-(iv)

# -- Inputs:
#   - logit_model_chosen_<> | final fitted logit-model for given SICR-definition (3a)
#   - datSICR_smp_<> | specific SICR-sample upon which resampling scheme is applied (3a)

# -- Outputs:
#   - <analytics>
# =======================================================================================




# ------ 0. Setup/load models

# --- a) Logit models

SICR_label <- "1a(i)"
if(!exists('logit_model_chosen')) unpack.ffdf(paste0(genPath, "logit_model_", SICR_label), tempPath)
logit_model_1a_i <- logit_model_chosen; rm(logit_model_chosen)
SICR_label <- "1a(ii)"
if(!exists('logit_model_chosen')) unpack.ffdf(paste0(genPath, "logit_model_", SICR_label), tempPath)
logit_model_1a_ii <- logit_model_chosen; rm(logit_model_chosen)
SICR_label <- "1a(iii)"
if(!exists('logit_model_chosen')) unpack.ffdf(paste0(genPath, "logit_model_", SICR_label), tempPath)
logit_model_1a_iii <- logit_model_chosen; rm(logit_model_chosen)
SICR_label <- "1a(iv)"
if(!exists('logit_model_chosen')) unpack.ffdf(paste0(genPath, "logit_model_", SICR_label), tempPath)
logit_model_1a_iv <- logit_model_chosen; rm(logit_model_chosen)

# --- b) Datasets

SICR_label <- "1a(i)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
dat_SICR_1a_i <- copy(datSICR_smp); rm(datSICR_smp)
SICR_label <- "1a(ii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
dat_SICR_1a_ii <- copy(datSICR_smp); rm(datSICR_smp)
SICR_label <- "1a(iii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
dat_SICR_1a_iii <- copy(datSICR_smp); rm(datSICR_smp)
SICR_label <- "1a(iv)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
dat_SICR_1a_iv <- copy(datSICR_smp); rm(datSICR_smp)




# ------ 1. Calculate Shapley values using the fastshap package


# --- 1.1 Preparation
# - Subset the data to only include the input variables
varKeep <- c(# Delinquency-theme inputs
             "g0_Delinq", "PerfSpell_Num", "TimeInPerfSpell", "slc_acct_roll_ever_24_imputed", "slc_acct_arr_dir_3",
             # Credit-themed inputs
             "BalanceLog", "Term", "InterestRate_Margin", "pmnt_method_grp", "slc_acct_pre_lim_perc_imputed", 
             # Macroeconomic-themed inputs
             "M_Repo_Rate", "M_Inflation_Growth", "M_DTI_Growth", "M_DTI_Growth_12", "M_RealGDP_Growth"
)

# 1a(i)
inputs_SICR_1a_i <- subset(dat_SICR_1a_i, select=varKeep)

# 1a(ii)
inputs_SICR_1a_ii <- subset(dat_SICR_1a_ii, select=varKeep)

# 1a(iii)
inputs_SICR_1a_iii <- subset(dat_SICR_1a_iii, select=varKeep)

# 1a(iv)
inputs_SICR_1a_iv <- subset(dat_SICR_1a_iv, select=varKeep)

# - Create explainer objects and plots

# graphing parameters
chosenFont <- "Cambria"
dpi <- 250


# --- 1.2 FASTSHAP

# - Set the seed to ensure reproducability 
set.seed(1051)

# 1a(i)
ptm <- proc.time()
((exp_all_ia_i <- explain(logit_model_1a_i, X = inputs_SICR_1a_i, nsim = 10, adjust = TRUE,  pred_wrapper = predict)))
autoplot(exp_all_ia_i)

# 1a(ii)
ptm <- proc.time()
((exp_all_ia_ii <- explain(logit_model_1a_ii, X = inputs_SICR_1a_ii, nsim = 10, adjust = TRUE,  pred_wrapper = predict)))
autoplot(exp_all_ia_ii)

# 1a(iii)
ptm <- proc.time()
((exp_all_ia_iii <- explain(logit_model_1a_iii, X = inputs_SICR_1a_iii, nsim = 10, adjust = TRUE,  pred_wrapper = predict)))
autoplot(exp_all_ia_iii)

# 1a(iv)
ptm <- proc.time()
((exp_all_ia_iv <- explain(logit_model_1a_iv, X = inputs_SICR_1a_iv, nsim = 10, adjust = TRUE,  pred_wrapper = predict)))
autoplot(exp_all_ia_iv)



