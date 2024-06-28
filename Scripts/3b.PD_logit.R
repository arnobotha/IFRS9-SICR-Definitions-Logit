# ============================== PD-MODELLIING ===============================
# SICR-model for definition 1a(i) using Logistic Regression (LR)
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

# -- Inputs:
#   - datCredit_allInputs | enriched credit dataset (script 2d)

# -- Outputs:
#   - inputs_chosen, logit_model_chosen | Chosen input variables + trained model
#   - logistic_cutoff | cut-off scalar (chosen using Youden's cost-sensitive Index)
#   - performance_measures | various performance measures for trained model
#   - <analytics>
# ==============================================================================




# ------ 0. Setup/parameter definition


# - Graphing parameters
chosenFont <- "Cambria"
dpi <- 250




# ------- 1. Remove irrelevant variables

# - Confirm prepared data after exclusions is loaded into memory
if (!exists('datCredit_allInputs')) unpack.ffdf(paste0(genPath,"creditdata_allinputs"), tempPath)

# - Identify the columns in the dataset
colnames(datCredit_allInputs)

# - Remove variables that will not be used in model building
suppressWarnings( datCredit_allInputs[, `:=`(slc_past_due_amt = NULL, DefaultStatus1 = NULL, WOff_Ind = NULL, 
                                             EarlySettle_Ind = NULL, ExclusionID = NULL, FurtherLoan_AmtLog = NULL,
                                             Redrawn_AmtLog = NULL)])






# --- 5. Feature Engineering: ratio-type variables (Period-level)

# - Loan age to loan term
datCredit_smp[, AgeToTerm := Age_Adj/Term] # where the loan is in its lifetime
# [SANITY CHECK] Check new feature for illogical values
cat( ( datCredit_smp[is.na(AgeToTerm), .N] == 0) %?% 
       'SAFE: New feature [AgeToTerm] has logical values.\n' %:% 
       'WARNING: New feature [AgeToTerm] has illogical values \n' )
(var_Info_Num$AgeToTerm <- describe(datCredit_smp$AgeToTerm)); hist(datCredit_smp[AgeToTerm<2, AgeToTerm], breaks='FD')
### RESULTS: Highly right-skewed distribution as expected, with mean of 0.37 vs median of 0.29,
# bounded by [0.02917, 0.9] for 5%-95% percentiles; some large outliers (max: 198)


# - InterestRate_Margin (incorporating risk-based pricing info)
var_Info_Num$InterestRate_Margin; hist(datCredit_smp$InterestRate_Margin, breaks="FD")
datCredit_smp[is.na(InterestRate_Margin), .N] / datCredit_smp[,.N] * 100
### RESULTS: Highly right-skewed distribution (as expected), with mean of -0.007 vs median of -0.008, 
# bounded by [-0.02, 0.01] for 5%-95% percentiles; some negative outliers distort shape of distribution
# Use median imputation, given 0.46% missingness degree
datCredit_smp[, InterestRate_Margin_imputed_mean := 
                ifelse(is.na(InterestRate_Margin) | InterestRate_Margin == "", 
                       median(InterestRate_Margin, na.rm=TRUE), InterestRate_Margin)]
# [SANITY CHECK] Confirm treatment success
cat( ( datCredit_smp[is.na(InterestRate_Margin_imputed_mean), .N] == 0) %?% 
       'SAFE: Treatment successful for [InterestRate_Margin_imputed_mean].\n' %:% 
       'ERROR: Treatment failed for [InterestRate_Margin_imputed_mean] \n' )
(var_Info_Num$InterestRate_Margin_imputed_mean <- describe(datCredit_smp$InterestRate_Margin_imputed_mean)); hist(datCredit_smp$InterestRate_Margin_imputed_mean, breaks="FD")
### RESULTS: Imputation successful, with mean of -0.007 vs median of -0.008,
# bounded by [-0.02, 0.0125] for 5%-95% percentiles; no extreme outliers


# ------ 2. Define the target event and conduct some data prep


# ------- 3. Feature Engineering that needs entire loan- and spell histories

# --- Create preliminary target/outcome variables for stated modelling objective
# NOTE: This particular field is instrumental to designing the subsampling & resampling scheme,
# as well as in tracking the impact of exclusions

# - Creating 12-month default indicators using the worst-ever approach
# NOTE: This step deliberately spans both performing and default spells
# NOTE: Need to specify a (k+1)-window for the "frollapply()" function, e.g., a 12-month outcome implies 13 elements
# Uses the custom function "imputLastKnown" defined in script 0
maxDate <- def_EndDte - years(1)
datCredit_real[, DefaultStatus1_lead_12_max := imputeLastKnown(frollapply(x=DefaultStatus1, n=13, align="left", FUN=max)), by=list(LoanID)]
datCredit_real$DefaultStatus1_lead_12_max %>% table() %>% prop.table() 
### RESULTS: 91.93% of observations have not defaulted in the next 12 months from reporting date, whilst 8.07% of accounts have.

inputs_ali_fin <-  DefaultStatus1_lead_12_max ~ AgeToTerm + Term + Balance + Principal + InterestRate_Margin_imputed_mean

# - Full logit model with all combined thematically selected variables
logitMod_full1 <- glm(inputs_ali_fin, data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_full1)


### AB: Goal: work towards a decent but basic PD-model, complete with relevant diagnostics, then ensure we have new field with PD-score for all loans in full dataset