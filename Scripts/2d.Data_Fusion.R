# ==================================== DATA FUSION ======================================
# Fuse the prepared and previously enriched credit dataset with a separate input space
# dataset that contains several fields that may prove predictive, as well as with
# prepared macroeconomic data
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Dynamic SICR-research
# SCRIPT AUTHOR(S): Dr Arno Botha, Esmerelda oberholzer
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2b.Data_Preparation_Credit.R
#   - 2c.Data_Enrich.R

# -- Inputs:
#   - datInput.raw | raw input space imported in script 1
#   - datCredit_real | prepared credit data from script 2c
#   - various parameters set in the setup script 0
#   - dat_SICR_MVs | prepared feature engineered macroeconomic data from script 2a
#
# -- Outputs:
#   - datCredit_real | enriched credit dataset, fused with various input fields
# ---------------------------------------------------------------------------------------
# NOTE: This script predominantly comes from another project (Kasmeer).
# =======================================================================================




# ------- 1. Remove some unnecessary variables to increase available memory

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final3"), tempPath)

# - Remove some unnecessary variable that will not be used in the model as well as remove
# data beyond COVID-19's start. Memory optimisation
datCredit_real <- subset(datCredit_real, Date < '2020-03-31',
                         select = -c(Age, New_Ind, Max_Counter, Date_Origination,
                                     Instalment, Arrears, AccountStatus, g1_Delinq, 
                                     DelinqState_g1, DefaultStatus2, DefSpell_Num, TimeInDefSpell,
                                     DefSpell_LeftTrunc, DefSpell_Event, DefSpell_Censored,
                                     DefSpellResol_TimeEnd, DefSpell_Age, DefSpellResol_Type_Hist, 
                                     HasLeftTruncPerfSpell, DefSpell_LastStart, ReceiptPV, LossRate_Real,
                                     PerfSpell_LeftTrunc, PerfSpell_Event, PerfSpell_Censored,
                                     PerfSpell_TimeEnd, PerfSpellResol_Type_Hist, PerfSpell_Age,
                                     HasLeftTruncDefSpell, Event_Time, Event_Type, Account_Censored,
                                     HasTrailingZeroBalances, ZeroBal_Start, NCA_CODE, STAT_CDE,
                                     DefSpell_Key, DelinqState_g0, DefSpell_Counter, PerfSpell_Counter,
                                     HasWOff, WriteOff_Amt, HasSettle, EarlySettle_Amt, HasFurtherLoan, HasRedraw,
                                     HasClosure, CLS_STAMP, TreatmentID, Curing_Ind, BOND_IND, Undrawn_Amt,
                                     slc_past_due_amt, HasRepaid, Repaid_Ind)); gc()




# ------- 2. Create features that require full history and apply exclusions thereafter on the credit dataset to increase memory

# - Creating 12-month default indicators using the worst-ever approach
# NOTE: This step deliberately spans both performing and default spells
# NOTE: Need to specify a (k+1)-window for the "frollapply()" function, e.g., a 12-month outcome implies 13 elements
# Uses the custom function "imputLastKnown" defined in script 0
maxDate <- as.Date(max(datCredit_real[,Date], na.rm=T)) %m-% months(12*1)
datCredit_real[, DefaultStatus1_lead_12_max := ifelse(Date<=maxDate,imputeLastKnown(frollapply(x=DefaultStatus1, n=13, align="left", FUN=max)),NA), by=list(LoanID)]
datCredit_real$DefaultStatus1_lead_12_max %>% table() %>% prop.table() 
### RESULTS: 91.28% of observations have not defaulted in the next 12 months from reporting date, whilst 8.72% of accounts have.

# - Relocate variable next to current default status variable
datCredit_real <- datCredit_real %>% relocate(DefaultStatus1_lead_12_max, .after=DefaultStatus1)

# --- Create a portfolio level input variable, i.e., default incidence rate
# - Aggregate to monthly level and observe up to given point
port.aggr <- datCredit_real[, list(DefaultStatus1_Aggr_Prop = sum(DefaultStatus1, na.rm=T)/.N),
                            by=list(Date)]

# - Merge default rate to credit dataset by date
datCredit_real <- merge(datCredit_real, port.aggr, by="Date", all.x=T)
# [Sanity Check] Check for any missingness in the DefaultStatus1_Aggr_Prop variable
cat(anyNA(datCredit_real[,DefaultStatus1_Aggr_Prop]) %?% "Missingness detected in the DefaultStatus1_Aggr_Prop variable. \n" %:% "No Missingness detected in the DefaultStatus1_Aggr_Prop variable. \n")
# clean-up
rm(port.aggr)


# - Proportion of new loans vs existing portfolio over time
# NOTE: we therefore measure credit demand within market, underlying market conditions, and the implicit effect of bank policies
# Create an aggregated dataset
dat_NewLoans_Aggr <- datCredit_real[, list(NewLoans_Aggr_Prop = sum(Age_Adj==1, na.rm=T)/.N), by=list(Date)]

# Merging the credit dataset with the aggregated dataset
datCredit_real <- merge(datCredit_real, dat_NewLoans_Aggr, by="Date", all.x=T)
# Validate merging success by checking for missingness (should be zero)
list_merge_variables <- list(colnames(dat_NewLoans_Aggr))
results_missingness <- list()
for (i in 1:length(list_merge_variables)){
  output <- sum(is.na(datCredit_real$list_merge_variables[i]))
  results_missingness[[i]] <- output
}
cat((length(which(results_missingness > 0)) == 0) %?% "SAFE: No missingness, fusion with aggregated data is successful.\n" %:%
       "WARNING: Missingness in certain aggregated fields detected, fusion compromised.\n")
describe(datCredit_real$NewLoans_Aggr_Prop);
### RESULTS: Variable has mean of 0.0071 vs median of 0.007

# clean-up
rm(dat_NewLoans_Aggr, list_merge_variables, results_missingness)

# - Check the impact of the exclusions from script 2b | RECORD-LEVEL
(exclusions_credit <- datCredit_real[ExclusionID != 0, .N] / datCredit_real[, .N] * 100)
# Exclusion's impact: ~6.5%

# - Check the impact of excluding the defaulted, write-off, and early settlements (dwes) exposures | RECORD-LEVEL
(exclusions_dwes <- datCredit_real[(WOff_Ind == 1 | EarlySettle_Ind == 1 | DefaultStatus1 == 1), .N] / datCredit_real[, .N] * 100)
# Exclusion's impact: ~9.7%

# - Check the combined impact for possible overlaps | RECORD-LEVEL
(exclusions_all <- datCredit_real[(ExclusionID != 0 | WOff_Ind == 1 | EarlySettle_Ind == 1 | DefaultStatus1 == 1), .N] / datCredit_real[, .N] * 100)
# Total exclusions' impact: ~12%

# - Now apply the exclusions
datCredit_real <- subset(datCredit_real, ExclusionID == 0 & !(DefaultStatus1 == 1 | WOff_Ind == 1 | EarlySettle_Ind == 1)); gc()

# - Checks
sum(datCredit_real$ExclusionID > 0) == 0 # check - success
sum(datCredit_real$DefaultStatus1 == 1 | datCredit_real$WOff_Ind == 1 | datCredit_real$EarlySettle_Ind == 1) == 0; gc() # check - success

# - Remove the columns that were only used for exclusion-purposes
datCredit_real <- subset(datCredit_real, select = -c(WOff_Ind, EarlySettle_Ind, ExclusionID)); gc()

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "creditdata_final4a"), datCredit_real)




# ------- 3. Fusing credit with macroeconomic information (only repo rate and inflation)

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4a"), tempPath)
if (!exists('dat_SICR_MVs')) unpack.ffdf(paste0(genPath,"datSICR_MVs"), tempPath)

# - Find intersection between fields in the credit dataset and the macroeconomic dataset
(overlap_flds <- intersect(colnames(datCredit_real), colnames(dat_SICR_MVs))) # no overlapping fields except Date

# - Filter for repo rate and inflation for feature engineering
dat_SICR_MVs <- subset(dat_SICR_MVs, select = c(Date, M_Repo_Rate, M_Inflation_Growth))

# - Merge on Date by performing a left-join
datCredit_real <- merge(datCredit_real, dat_SICR_MVs, by="Date", all.x=T); gc()

# - Validate merging success by checking for missingness (should be zero)
list_merge_variables <- list(colnames(dat_SICR_MVs))
results_missingness <- list()
for (i in 1:length(list_merge_variables)){
  output <- sum(is.na(datCredit_real$list_merge_variables[i]))
  results_missingness[[i]] <- output
}
cat((length(which(results_missingness > 0)) == 0) %?% "SAFE: No missingness, fusion with macroeconomic data is successful.\n" %:%
      "WARNING: Missingness in certain macroecnomic fields detected, fusion compromised.\n")
# confirmed, no missing values

# - Clean-up
# NOTE: MV-dataset is retained for feature engineering later
rm(list_merge_variables, results_missingness); gc()




# ------- 4. Fusing credit dataset with additional input fields

# - Confirm if the input space data is loaded into memory
if (!exists('datInput.raw')) unpack.ffdf(paste0(genPath,"creditdata_input1"), tempPath)

# - Find intersection between fields in input space and those perhaps already in the main credit dataset
(overlap_flds <- intersect(colnames(datCredit_real), colnames(datInput.raw))) # no overlapping fields

# - Remove any additional variables that are not going to be used
suppressWarnings( datInput.raw[, `:=`(slc_status_final_pred7 = NULL, slc_status_final = NULL, slc_curing_ind = NULL, 
                                      slc_past_due_amt = NULL, datex = NULL)])

# - Format the date in the correct format for merging
datInput.raw[, date := as.Date(date, format="%Y-%m-%d")]

# - Rename the datasets for merging
colnames(datInput.raw)[colnames(datInput.raw) %in% c("date", "acct_no")] <- c("Date", "LoanID")

# - Check the data grain
data_grain_check <- datInput.raw[, list(Freq = .N), by=list(LoanID, Date)][Freq>1,]
sum(is.na(data_grain_check$LoanID))
# the data grain is broken in the cases where a Loan_ID does not exist - we are not interested in these accounts in any case

# - Merge on LoanID and Date by performing a left-join
datCredit_real <- merge(datCredit_real, datInput.raw, by=c("Date", "LoanID"), all.x=T); gc()

# - Check the data grain
NROW(data_grain_check_merge <- datCredit_real[, list(Freq = .N), by=list(LoanID, Date)][Freq>1,])==0
# success, the data grain check is passed

# - Clean-up
rm(datInput.raw, data_grain_check, data_grain_check_merge); gc()




# ------- 5. Feature engineering for modelling purposes

# --- Missing value indicators for the input space variables
# NOTE: There are a lot of missing values for these variables because of system changes etc.
datCredit_real[, value_ind_slc_pmnt_method := ifelse(is.na(slc_pmnt_method) | slc_pmnt_method == "", 0, 1)]
datCredit_real[, value_ind_slc_days_excess := ifelse(is.na(slc_days_excess) | slc_days_excess == "", 0, 1)]
datCredit_real[, value_ind_slc_acct_pre_lim_perc := ifelse(is.na(slc_acct_pre_lim_perc) | slc_acct_pre_lim_perc == "", 0, 1)]
datCredit_real[, value_ind_slc_acct_roll_ever_24 := ifelse(is.na(slc_acct_roll_ever_24) | slc_acct_roll_ever_24 == "", 0, 1)]
datCredit_real[, value_ind_slc_acct_arr_dir_3 := ifelse(is.na(slc_acct_arr_dir_3) | slc_acct_arr_dir_3 == "", 0, 1)]
datCredit_real[, value_ind_slc_acct_prepaid_perc_dir_12 := ifelse(is.na(slc_acct_prepaid_perc_dir_12) | slc_acct_prepaid_perc_dir_12 == "", 0, 1)]
datCredit_real[, value_ind_ccm_ute_lvl_40_cnt_24m := ifelse(is.na(ccm_ute_lvl_40_cnt_24m) | ccm_ute_lvl_40_cnt_24m == "", 0, 1)]
datCredit_real[, value_ind_ccm_worst_arrears_6m := ifelse(is.na(ccm_worst_arrears_6m) | ccm_worst_arrears_6m == "", 0, 1)]
datCredit_real[, value_ind_ccm_worst_arrears_24m := ifelse(is.na(ccm_worst_arrears_24m) | ccm_worst_arrears_24m == "", 0, 1)]

# --- Check the missingness of the variables
# If they are more than 50% missing - remove
table(datCredit_real$value_ind_slc_pmnt_method) %>% prop.table()              # missingness: 10.37% - keep the variable (categorical)
table(datCredit_real$value_ind_slc_days_excess) %>% prop.table()              # missingness: 69.74% - discard the variable
table(datCredit_real$value_ind_slc_acct_pre_lim_perc) %>% prop.table()        # missingness: 10.37% - keep the variable (numeric) 
table(datCredit_real$value_ind_slc_acct_roll_ever_24) %>% prop.table()        # missingness: 10.38% - keep the variable (numeric + delinquency theme)     
table(datCredit_real$value_ind_slc_acct_arr_dir_3) %>% prop.table()           # missingness: 10.37% - keep the variable (categorical + delinquency theme)        
table(datCredit_real$value_ind_slc_acct_prepaid_perc_dir_12) %>% prop.table() # missingness: 10.37% - keep the variable (numeric)
table(datCredit_real$value_ind_ccm_ute_lvl_40_cnt_24m) %>% prop.table()       # missingness: 84.78% - discard the variable   
table(datCredit_real$value_ind_ccm_worst_arrears_6m) %>% prop.table()         # missingness: 84.88% - discard the variable    
table(datCredit_real$value_ind_ccm_worst_arrears_24m) %>% prop.table()        # missingness: 63.77% - discard the variable

# - Remove the variables that have missingness > 50% 
suppressWarnings( datCredit_real[, `:=`(value_ind_slc_days_excess = NULL, slc_days_excess = NULL, 
                                        value_ind_ccm_ute_lvl_40_cnt_24m = NULL, ccm_ute_lvl_40_cnt_24m = NULL,
                                        value_ind_ccm_worst_arrears_6m = NULL, ccm_worst_arrears_6m = NULL,
                                        value_ind_ccm_worst_arrears_24m = NULL, ccm_worst_arrears_24m = NULL)]); gc()


# --- Missing value treatment (categorical variables)
# Treatment: create "missing"-bin for all N/A values

# - Payment method
# Merge with existing "Unknown" bin or empty values
datCredit_real[, slc_pmnt_method := 
                 ifelse(is.na(slc_pmnt_method) | slc_pmnt_method == "" | slc_pmnt_method == "Unknown",
                        "MISSING_DATA", slc_pmnt_method)]
# [SANITY CHECK] Confirm treatment success
cat((sum(datCredit_real$slc_pmnt_method == "" | is.na(datCredit_real$slc_pmnt_method) | 
           datCredit_real$slc_pmnt_method == "Unknown") == 0) %?% 
       'SAFE: Treatment successful for [slc_pmnt_method].\n' %:% 'ERROR: Treatment failed for [slc_pmnt_method] \n' )
### RESULTS: Treatment for missingness was successful


# - Account-level arrears direction vs three months ago
# Merge with existing "N/A" bin or empty values
datCredit_real[, slc_acct_arr_dir_3 := 
                 ifelse(is.na(slc_acct_arr_dir_3) | slc_acct_arr_dir_3 == "" | slc_acct_arr_dir_3 == "N/A", 
                        "MISSING_DATA", slc_acct_arr_dir_3)]
# [SANITY CHECK] Confirm treatment success
cat(( sum(datCredit_real$slc_acct_arr_dir_3 == "" | is.na(datCredit_real$slc_acct_arr_dir_3) |
            datCredit_real$slc_acct_arr_dir_3 == "N/A") == 0) %?% 
       'SAFE: Treatment successful for [slc_acct_arr_dir_3].\n' %:% 'ERROR: Treatment failed for [slc_acct_arr_dir_3] \n' )
### RESULTS: Treatment for missingness was successful



# --- Missing value treatment (numeric variables)
# Analyse whether to use mean or median value imputation

# - Prepaid/available funds to limit
describe(datCredit_real$slc_acct_pre_lim_perc) 
# the 50th percentile is 0, the 75th percentile is 0.02122, whereas the mean is 0.0959. Treat missing values with the median
datCredit_real[, slc_acct_pre_lim_perc_imputed := 
                 ifelse(is.na(slc_acct_pre_lim_perc) | slc_acct_pre_lim_perc == "", 
                        median(slc_acct_pre_lim_perc, na.rm=TRUE), slc_acct_pre_lim_perc)]
# [SANITY CHECK] Confirm treatment success
cat(( datCredit_real[is.na(slc_acct_pre_lim_perc_imputed), .N ] == 0) %?% 
       'SAFE: Treatment successful for [slc_acct_pre_lim_perc_imputed].\n' %:% 
       'ERROR: Treatment failed for [slc_acct_pre_lim_perc_imputed] \n' )
### RESULTS: Treatment for imputation was successful


# - Number of times an account was in arrears over last 24 months
describe(datCredit_real$slc_acct_roll_ever_24)
# more than 80% of the data has a value of 0, the mean is 0.3079. Treat missing values with the mean
datCredit_real[, slc_acct_roll_ever_24_imputed := 
                 ifelse(is.na(slc_acct_roll_ever_24) | slc_acct_roll_ever_24 == "", 
                        mean(slc_acct_roll_ever_24, na.rm=TRUE), slc_acct_roll_ever_24)]
# [SANITY CHECK] Confirm treatment success
cat(( datCredit_real[is.na(slc_acct_roll_ever_24_imputed), .N ] == 0) %?% 
      'SAFE: Treatment successful for [slc_acct_roll_ever_24_imputed].\n' %:% 
      'ERROR: Treatment failed for [slc_acct_roll_ever_24_imputed] \n' )
### RESULTS: Treatment for imputation was successful


# - Percentage-valued direction of prepaid/available funds - current compared to 12 months ago
describe(datCredit_real$slc_acct_prepaid_perc_dir_12)
# the 50th percentile is 0 and the 75th percentile is 0.2069, whereas the mean is very high at 19.6m (due to outliers). Replace with the median
datCredit_real[, slc_acct_prepaid_perc_dir_12_imputed := 
                 ifelse(is.na(slc_acct_prepaid_perc_dir_12) | slc_acct_prepaid_perc_dir_12 == "", 
                        median(slc_acct_prepaid_perc_dir_12, na.rm=TRUE), slc_acct_prepaid_perc_dir_12)]
# [SANITY CHECK] Confirm treatment success
cat(( datCredit_real[is.na(slc_acct_prepaid_perc_dir_12_imputed), .N ] == 0) %?% 
      'SAFE: Treatment successful for [slc_acct_prepaid_perc_dir_12_imputed].\n' %:% 
      'ERROR: Treatment failed for [slc_acct_prepaid_perc_dir_12_imputed] \n' )
### RESULTS: Treatment for imputation was successful



# --- Exploring transformations

# - Log-transformed balance, then treat troublesome values
datCredit_real[, BalanceLog := log(Balance)]
datCredit_real[, BalanceLog := ifelse(is.na(BalanceLog) | is.infinite(BalanceLog), 0, BalanceLog)]
# [SANITY CHECK] Check new feature for illogical values
cat(( datCredit_real[is.na(BalanceLog) | is.infinite(BalanceLog), .N] == 0) %?% 
      'SAFE: New feature [BalanceLog] has logical values.\n' %:% 
      'WARNING: New feature [BalanceLog] has illogical values \n' )
# distributional analysis
describe(datCredit_real$BalanceLog); hist(datCredit_real$BalanceLog, breaks="FD")
### RESULTS: Large spike at zero values with some negative outliers, which skews mean to 11.9 (median: 12.7).
# However, majority of distribution's bulk is left-skewed between 5 and 15


# - Log-transformed receipt, then treat troublesome values
datCredit_real[, Receipt_InfLog := log(Receipt_Inf)]
datCredit_real[, Receipt_InfLog := ifelse(is.na(Receipt_InfLog) | is.infinite(Receipt_InfLog), 0, Receipt_InfLog)]
# [SANITY CHECK] Check new feature for illogical values
cat(( datCredit_real[is.na(Receipt_InfLog) | is.infinite(Receipt_InfLog), .N] == 0) %?% 
      'SAFE: New feature [Receipt_InfLog] has logical values.\n' %:% 
      'WARNING: New feature [Receipt_InfLog] has illogical values \n' )
# distributional analysis
describe(datCredit_real$Receipt_InfLog); hist(datCredit_real$Receipt_InfLog, breaks="FD")
### RESULTS: Despite large spike at zero and some negative outliers, mean of 7.4 (median: 8.3) with a 
# seemingly normal distribution in shape, between 5 and 11


# - Log-transformed further loan amount, then treat troublesome values
datCredit_real[, FurtherLoan_AmtLog := log(FurtherLoan_Amt)]
datCredit_real[, FurtherLoan_AmtLog := ifelse(is.na(FurtherLoan_AmtLog) | is.infinite(FurtherLoan_AmtLog), 0, FurtherLoan_AmtLog)]
# [SANITY CHECK] Check new feature for illogical values
cat(( datCredit_real[is.na(FurtherLoan_AmtLog) | is.infinite(FurtherLoan_AmtLog), .N] == 0) %?% 
      'SAFE: New feature [FurtherLoan_AmtLog] has logical values.\n' %:% 
      'WARNING: New feature [FurtherLoan_AmtLog] has illogical values \n' )
# distributional analysis
describe(datCredit_real$FurtherLoan_AmtLog); hist(datCredit_real$FurtherLoan_AmtLog, breaks="FD")
### RESULTS: Distribution is overwhelmed by zero-values, which would likely not be useful in predictive setting.


# - Log-transformed redraw amount, then treat troublesome values
datCredit_real[, Redrawn_AmtLog := log(Redrawn_Amt)]
datCredit_real[, Redrawn_AmtLog := ifelse(is.na(Redrawn_AmtLog) | is.infinite(Redrawn_AmtLog), 0, Redrawn_AmtLog)]
# [SANITY CHECK] Check new feature for illogical values
cat(( datCredit_real[is.na(Redrawn_AmtLog) | is.infinite(Redrawn_AmtLog), .N] == 0) %?% 
      'SAFE: New feature [Redrawn_AmtLog] has logical values.\n' %:% 
      'WARNING: New feature [Redrawn_AmtLog] has illogical values \n' )
# distributional analysis
describe(datCredit_real$Redrawn_AmtLog); hist(datCredit_real$Redrawn_AmtLog, breaks="FD")
### RESULTS: Distribution is overwhelmed by zero-values, which would likely not be useful in predictive setting.



# --- Feature Engineering: ratio-type variables

# - Loan age to loan term
datCredit_real[, AgeToTerm := Age_Adj/Term] # where the loan is in its lifetime
# [SANITY CHECK] Check new feature for illogical values
cat(( datCredit_real[is.na(AgeToTerm), .N] == 0) %?% 
       'SAFE: New feature [AgeToTerm] has logical values.\n' %:% 
       'WARNING: New feature [AgeToTerm] has illogical values \n' )
### RESULTS: Success, the value has logical values
# distributional analysis
describe(datCredit_real$AgeToTerm); hist(datCredit_real[AgeToTerm<2, AgeToTerm], breaks="FD")
### RESULTS: Right-skewed distribution as expected, shape skewed by some extreme positive values.
# But most of the distribution seems usable up to 2 (shown in histogram)


# - Balance to loan principal | how much is still outstanding compared to Principal/Limit
datCredit_real[, BalanceToPrincipal := Balance/Principal]
# [SANITY CHECK] Check new feature for illogical values
cat(( datCredit_real[is.na(BalanceToPrincipal), .N] == 0) %?% 
       'SAFE: New feature [BalanceToPrincipal] has logical values.\n' %:% 
       'WARNING: New feature [BalanceToPrincipal] has illogical values \n' )
### RESULTS: Success, the value has logical values
# distributional analysis
describe(datCredit_real$BalanceToPrincipal); hist(datCredit_real[BalanceToPrincipal<1, BalanceToPrincipal], breaks="FD")
### RESULTS: Highly left-skewed distribution as expected


# - Balance to loan term | how much is still outstanding compared to its lifetime
datCredit_real[, BalanceToTerm := Balance/Term]
# [SANITY CHECK] Check new feature for illogical values
cat(( datCredit_real[is.na(BalanceToTerm), .N] == 0) %?% 
       'SAFE: New feature [BalanceToTerm] has logical values.\n' %:% 
       'WARNING: New feature [BalanceToTerm] has illogical values \n' )
### RESULTS: Success, the value has logical values
# distributional analysis
describe(datCredit_real$BalanceToTerm); hist(datCredit_real[BalanceToTerm<10000, BalanceToTerm], breaks="FD")
### RESULTS: Right-skewed distribution as expected, shape skewed by some extreme positive values.
# Large influx at zero, with some negative outliers, likely due to negative-valued balances. Safe.
# Given the large scale, a log-transform may further enhance its eventual predictive value.



# --- Featuring Engineering: Delinquency-themed

# - Embed previous defaults into a new Boolean-valued input variable
datCredit_real[, PrevDefaults := ifelse(all(is.na(PerfSpell_Num)), F, max(PerfSpell_Num,na.rm = T) > 1), by=list(LoanID)]
cat((datCredit_real[is.na(PrevDefaults), .N] == 0) %?% "SAFE: No missingness, [PrevDefaults] created successfully.\n" %:%
       "WARNING: Missingness detected, [PrevDefaults] compromised.\n")
describe(datCredit_real$PrevDefaults)
describe(datCredit_real[Counter==1, PrevDefaults])
### RESULTS: 6.5% of records had previous defaults


# - Spell-level indicator for when a shift occurs in the state of g0_Delinq (target event)
# NOTE: This is an intermediary field used in the creation of subsequent fields
datCredit_real[, g0_Delinq_Shift := ifelse(lag(g0_Delinq, n=1)==g0_Delinq,0,1), by=list(LoanID)]
datCredit_real[is.na(g0_Delinq_Shift), g0_Delinq_Shift := 0] # All first observations have g0_Delinq_Shift = NA; set these values to zero.
cat((datCredit_real[is.na(g0_Delinq_Shift), .N] == 0) %?% "SAFE: No missingness, [g0_Delinq_Shift] created successfully.\n" %:%
       "WARNING: Missingness detected, [g0_Delinq_Shift] compromised.\n")
datCredit_real$g0_Delinq_Shift %>% table() %>% prop.table()
### RESULT: 96.715% of the records had no change in their delinquency level from their associated previous record.


# - Delinquency state number, where each change in g_0 denotes such a "state" that may span several periods
datCredit_real[, g0_Delinq_Num := cumsum(g0_Delinq_Shift) + 1, by=list(LoanID)] # Assign state numbers over the entire loan history (add one to ensure that there are no delinquency spell numbers equal to zero)
cat((datCredit_real[is.na(g0_Delinq_Num), .N] == 0) %?% "SAFE: No missingness, [g0_Delinq_Num] created successfully.\n" %:%
       "WARNING: Missingness detected, [g0_Delinq_Num] compromised.\n")
describe(datCredit_real$g0_Delinq_Num)
### RESULT: Mean state number of 2.547 across all rows; median: 1; max of 75 
# This high max suggests outlier-accounts with rapid and frequent changes in g0


# - Time in delinquency state
# NOTE: This variable is conceptually different to [TimeInPerfSpell].
# A performance spell starts when a loan is not in default and ends when it is in default.
# A delinquency spell starts when a loan "shifts" to a new delinquency level and ends the immediate period preceding the next shift to a different delinquency level.
datCredit_real[, TimeInDelinqState := 1:.N, by=list(LoanID, g0_Delinq_Num)]
cat((datCredit_real[is.na(TimeInDelinqState), .N] == 0) %?% "SAFE: No missingness detected, [TimeInDelinqState] created successfully.\n" %:%
       "WARNING: Missingness detected, [TimeInDelinqState] compromised.\n")



# --- Delinquency-themed variables on a performance spell-level
# - Delinquency state number, where each change in g_0 denotes such a "state" that may span several periods during a performance spell
datCredit_real[!is.na(PerfSpell_Key), PerfSpell_g0_Delinq_Num := cumsum(g0_Delinq_Shift) + 1, by=list(PerfSpell_Key)] # Assign state numbers over each performance spell
# [SANITY CHECK] Check new feature for illogical values
cat(( datCredit_real[is.na(PerfSpell_g0_Delinq_Num),.N]==datCredit_real[is.na(PerfSpell_Key),.N]) %?% 
       'SAFE: New feature [PerfSpell_g0_Delinq_Num] has logical values.\n' %:% 
       'WARNING: New feature [PerfSpell_g0_Delinq_Num] has illogical values \n' )


# - Calculate standard deviation on the performance spell level
datCredit_real[!is.na(PerfSpell_Key), PerfSpell_g0_Delinq_SD := sd(g0_Delinq), by=list(PerfSpell_Key)]
datCredit_real[!is.na(PerfSpell_Key) & is.na(PerfSpell_g0_Delinq_SD), PerfSpell_g0_Delinq_SD := 0] # Assigning an standard deviation of zero to those performance spells that have an single observation
# [SANITY CHECK] Check new feature for illogical values
cat( ( datCredit_real[is.na(PerfSpell_g0_Delinq_SD),.N]==datCredit_real[is.na(PerfSpell_Key),.N]) %?% 
       'SAFE: New feature [PerfSpell_g0_Delinq_SD] has logical values.\n' %:% 
       'WARNING: New feature [PerfSpell_g0_Delinq_SD] has illogical values \n' )



# --- Featuring Engineering: Behavioural-themed

# - Condense the payment group
datCredit_real[, pmnt_method_grp := 
                case_when(slc_pmnt_method == "Debit Order FNB account" | slc_pmnt_method == "Debit Order other bank" ~ "Debit Order",
                          slc_pmnt_method == "Salary" | slc_pmnt_method == "Suspense" ~ "Salary/Suspense",
                          TRUE ~ slc_pmnt_method)]
# [SANITY CHECK] Check new feature for illogical values
cat((datCredit_real[is.na(pmnt_method_grp), .N] == 0) %?% 
      'SAFE: New feature [pmnt_method_grp] has logical values.\n' %:% 
      'WARNING: New feature [pmnt_method_grp] has illogical values \n' )
### RESULT: Successful binning performed



# --- Featuring Engineering: Risk-based pricing

# - Create Interest Rate margin using the repo rate + 3.5% (Prime Rate's definition in South Africa)
datCredit_real <- datCredit_real %>% mutate(InterestRate_Margin = round(InterestRate_Nom - (M_Repo_Rate+0.035), digits=4)) %>%
  relocate(InterestRate_Margin, .after=InterestRate_Nom)
cat((datCredit_real[is.na(InterestRate_Margin), .N] == 0) %?% "SAFE: No missingness, [InterestRate_Margin] created successfully.\n" %:%
      "WARNING: Missingness detected, [InterestRate_Margin] compromised.\n")
# safe, no missingness detected



# --- Feature Engineering: Inflating time-sensitive monetary variables to the latest date

# - Getting a range of inflation factors for each date in the sampling window
date_range <- ceiling_date(unique(datCredit_real$Date), unit="month")-days(1)
datInflation <- data.table(Date=date_range)
datInflation[,Inf_Factor:=adjInflation_MV(datMacro=dat_SICR_MVs, time="Date", Inflation_Growth="M_Inflation_Growth", g_start=Date, g_stop = date_range[length(date_range)]), by=Date]
datCredit_real <- merge(datCredit_real, datInflation, all.x=T, by="Date")

# - [SANITY CHECK]
cat((anyNA(datCredit_real$Inf_Factor)) %?% paste0('WARNING: Inflation factor(s) is(are) missing for ', unique(datCredit_real[is.na(Inf_Factor),Date]), '. \n') %:%
      'SAFE: Inflation factors created successfully. \n')
### RESULTS: [Inf_Factor] variables created successfully without any missingness

# - Deflate the relevant variables
datCredit_real[, Principal_Real := Principal*Inf_Factor]
datCredit_real[, Balance_Real := Balance*Inf_Factor]

# - [SANITY CHECK]
cat( (all(anyNA(datCredit_real$Principal_Real), anyNA(datCredit_real$Balance_Real)))
    %?% paste0('WARNING: Some values of [Principal_Real], [Balance_Real] not created successfully. \n') %:%
      'SAFE: Variables inflated successfully. \n')
### RESULTS: Variables created successfully without any missingness

# - Clean up, drop intermediate variables
datCredit_real[, `:=`(M_Repo_Rate = NULL, M_Inflation_Growth = NULL, g0_Delinq_Shift=NULL)]
rm(dat_SICR_MVs, date_range, datInflation); gc()




# ------- 6. Pack objects to disk

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "creditdata_final4b"), datCredit_real); gc()
