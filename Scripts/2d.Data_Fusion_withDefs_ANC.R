# ==================================== DATA FUSION ======================================
# Fuse the prepared and previously enriched credit dataset with a separate input space
# dataset that contains several fields that may prove predictive, as well as with
# prepared macroeconomic data. Same as script 2d, but with defaults
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Dynamic SICR-research
# SCRIPT AUTHOR(S): Dr Arno Botha
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
#   - datCredit_allInputs_withDefs | enriched credit dataset, fused with various input fields
# ---------------------------------------------------------------------------------------
# NOTE: This script predominantly comes from another project (Kasmeer).
# =======================================================================================




# ------- 1. Remove some unnecessary variables to increase memory
# Same as script 2d, except without the default-exclusion

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final3"), tempPath)
if (!exists('dat_SICR_MVs')) unpack.ffdf(paste0(genPath,"datSICR_MVs"), tempPath)

# - Remove some unnecessary variable that will not be used in the model as well as remove
# data beyond COVID-19's start. Memory optimisation
datCredit_real <- subset(datCredit_real, Date < '2020-03-31',
                         select = -c(PerfSpell_Key, Age, New_Ind, Max_Counter, Date_Origination, Principal,
                                     Instalment, HasWOff, Arrears, AccountStatus, g1_Delinq, 
                                     DelinqState_g1, DefaultStatus2, DefSpell_Num, TimeInDefSpell,
                                     DefSpell_LeftTrunc, DefSpell_Event, DefSpell_Censored,
                                     DefSpellResol_TimeEnd, DefSpell_Age, DefSpellResol_Type_Hist, 
                                     HasLeftTruncPerfSpell, DefSpell_LastStart, ReceiptPV, LossRate_Real,
                                     PerfSpell_LeftTrunc, PerfSpell_Event, PerfSpell_Censored,
                                     PerfSpell_TimeEnd, PerfSpellResol_Type_Hist,
                                     HasLeftTruncDefSpell, Event_Time, Event_Type, Account_Censored,
                                     HasTrailingZeroBalances, ZeroBal_Start, NCA_CODE, STAT_CDE,
                                     DefSpell_Key, DelinqState_g0, DefSpell_Counter, PerfSpell_Counter,
                                     WriteOff_Amt, HasSettle, EarlySettle_Amt, HasFurtherLoan, HasRedraw,
                                     HasClosure, CLS_STAMP, TreatmentID, Curing_Ind, BOND_IND, Undrawn_Amt,
                                     slc_past_due_amt)); gc()




# ------- 2. Apply exclusions on the credit dataset to increase memory

# - Check the impact of the exclusions from 2b. Data_Prepare_Credit | RECORD-LEVEL
(exclusions_credit <- datCredit_real[ExclusionID != 0, .N] / datCredit_real[, .N] * 100)
# Exclusion's impact: ~6.5%

# - Check the impact of the write-off, and early settlements (wes) exclusions | RECORD-LEVEL
(exclusions_wes <- datCredit_real[(WOff_Ind == 1 | EarlySettle_Ind == 1), .N] / datCredit_real[, .N] * 100)
# Exclusion's impact: ~9.7% (old) vs ~4.8% (without default-exclusion)

# - Check the combined impact for possible overlaps | RECORD-LEVEL
(exclusions_all <- datCredit_real[(ExclusionID != 0 | WOff_Ind == 1 | EarlySettle_Ind == 1), .N] / datCredit_real[, .N] * 100)
# Total exclusions' impact: ~12% (old) vs ~7.2% (without default-exclusion)

# - Now apply the exclusions
datCredit_real <- subset(datCredit_real, ExclusionID == 0 & !(WOff_Ind == 1 | EarlySettle_Ind == 1)); gc()




# ------- 3. Fusing credit with macroeconomic information

# - Find intersection between fields in the credit dataset and the macroeconomic dataset
(overlap_flds <- intersect(colnames(datCredit_real), colnames(dat_SICR_MVs))) # no overlapping fields except Date

# - Merge on Date by performing a left-join
datCredit_real <- merge(datCredit_real, dat_SICR_MVs, by="Date", all.x=T); gc()

# - Create Interest Rate margin using the repo rate + 3.5% (Prime Rate's definition in South Africa)
datCredit_real <- datCredit_real %>% mutate(InterestRate_Margin = round(InterestRate_Nom - (M_Repo_Rate+0.035), digits=4)) %>%
  relocate(InterestRate_Margin, .after=InterestRate_Nom)

# - Validate merging success by checking for missingness (should be zero)
list_merge_variables <- list(colnames(dat_SICR_MVs))
results_missingness <- list()
for (i in 1:length(list_merge_variables)){
  output <- sum(is.na(datCredit_real$list_merge_variables[i]))
  results_missingness[[i]] <- output
}
length(which(results_missingness > 0)) == 0 # confirmed, no missing values

# - Clean-up
rm(dat_SICR_MVs, list_merge_variables, results_missingness); gc()





# ------- 4. Fusing credit dataset with additional input fields

# - Confirm if the input space data is loaded into memory
if (!exists('datInput.raw')) unpack.ffdf(paste0(genPath,"creditdata_input1"), tempPath)

# - Find intersection between fields in input space and those perhaps already in the main credit dataset
(overlap_flds <- intersect(colnames(datCredit_real), colnames(datInput.raw))) # no overlapping fields

# - Remove any additional variables that are not going to be used
suppressWarnings( datInput.raw[, `:=`(slc_status_final_pred7 = NULL, slc_status_final = NULL, 
                                      slc_curing_ind = NULL, datex = NULL)])

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
table(datCredit_real$value_ind_slc_pmnt_method) %>% prop.table()              # missingness: 10.36% - keep the variable (categorical)
table(datCredit_real$value_ind_slc_days_excess) %>% prop.table()              # missingness: 69.72% - discard the variable
table(datCredit_real$value_ind_slc_acct_pre_lim_perc) %>% prop.table()        # missingness: 10.36% - keep the variable (numeric) 
table(datCredit_real$value_ind_slc_acct_roll_ever_24) %>% prop.table()        # missingness: 10.37% - keep the variable (numeric + delinquency theme)     
table(datCredit_real$value_ind_slc_acct_arr_dir_3) %>% prop.table()           # missingness: 10.36% - keep the variable (categorical + delinquency theme)        
table(datCredit_real$value_ind_slc_acct_prepaid_perc_dir_12) %>% prop.table() # missingness: 10.36% - keep the variable (numeric)
table(datCredit_real$value_ind_ccm_ute_lvl_40_cnt_24m) %>% prop.table()       # missingness: 84.77% - discard the variable   
table(datCredit_real$value_ind_ccm_worst_arrears_6m) %>% prop.table()         # missingness: 84.87% - discard the variable    
table(datCredit_real$value_ind_ccm_worst_arrears_24m) %>% prop.table()        # missingness: 63.75% - discard the variable

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
sum(datCredit_real$slc_pmnt_method == "" | is.na(datCredit_real$slc_pmnt_method) | 
      datCredit_real$slc_pmnt_method == "Unknown")==0 # check - success
describe(datCredit_real$slc_pmnt_method)


# - Account-level arrears direction vs three months ago
# Merge with existing "N/A" bin or empty values
datCredit_real[, slc_acct_arr_dir_3 := 
                 ifelse(is.na(slc_acct_arr_dir_3) | slc_acct_arr_dir_3 == "" | slc_acct_arr_dir_3 == "N/A", 
                        "MISSING_DATA", slc_acct_arr_dir_3)]
sum(datCredit_real$slc_acct_arr_dir_3 == "" | is.na(datCredit_real$slc_acct_arr_dir_3) |
      datCredit_real$slc_acct_arr_dir_3 == "N/A")==0 # check - success
describe(datCredit_real$slc_acct_arr_dir_3)



# --- Missing value treatment (numeric variables)
# When imputing, ensure that we impute from the non-default population, given that models will be trained from performing population
# as well as eventually applied on performing population.
# High-level analysis on whether to use mean or median value imputation

# - Prepaid/available funds to limit
describe(datCredit_real[DefaultStatus1==0, slc_acct_pre_lim_perc]) 
# the 50th percentile is 0, the 75th percentile is 0.02122, whereas the mean is 0.0959. Replace with the median
getAggr <- median(datCredit_real[DefaultStatus1==0, slc_acct_pre_lim_perc], na.rm=T)
datCredit_real[, slc_acct_pre_lim_perc_imputed := 
                 ifelse(is.na(slc_acct_pre_lim_perc) | slc_acct_pre_lim_perc == "", 
                        getAggr, slc_acct_pre_lim_perc)]
sum(datCredit_real[DefaultStatus1==0, slc_acct_pre_lim_perc_imputed] == "" | is.na(datCredit_real[DefaultStatus1==0, slc_acct_pre_lim_perc_imputed]))==0 # check - success


# - Number of times an account was in arrears over last 24 months
describe(datCredit_real[DefaultStatus1==0, slc_acct_roll_ever_24])
# more than 80% of the data has a value of 0, the mean is 0.3079. Replace with the mean
getAggr <- mean(datCredit_real[DefaultStatus1==0, slc_acct_roll_ever_24], na.rm=T)
datCredit_real[, slc_acct_roll_ever_24_imputed := 
                 ifelse(is.na(slc_acct_roll_ever_24) | slc_acct_roll_ever_24 == "", 
                        getAggr, slc_acct_roll_ever_24)]
sum(datCredit_real[DefaultStatus1==0, slc_acct_roll_ever_24_imputed] == "" | is.na(datCredit_real[DefaultStatus1==0, slc_acct_roll_ever_24_imputed]))==0 # check - success


# - Percentage-valued direction of prepaid/available funds - current compared to 12 months ago
describe(datCredit_real[DefaultStatus1==0, slc_acct_prepaid_perc_dir_12])
# the 50th percentile is 0 and the 75th percentile is 0.2104, whereas the mean is 19671691. Replace with the median
getAggr <- median(datCredit_real[DefaultStatus1==0, slc_acct_prepaid_perc_dir_12], na.rm=T)
datCredit_real[, slc_acct_prepaid_perc_dir_12_imputed := 
                 ifelse(is.na(slc_acct_prepaid_perc_dir_12) | slc_acct_prepaid_perc_dir_12 == "", 
                        getAggr, slc_acct_prepaid_perc_dir_12)]
sum(datCredit_real[DefaultStatus1==0, slc_acct_prepaid_perc_dir_12_imputed] == "" | is.na(datCredit_real[DefaultStatus1==0, slc_acct_prepaid_perc_dir_12_imputed]))==0 # check - success



# --- Exploring transformations

# - Log-transformed balance
datCredit_real[, BalanceLog := log(Balance)]
check_na_balance <- subset(datCredit_real, is.na(BalanceLog))
# missing log_balances produced by negative balance amounts
# fix by assigning zero's
datCredit_real[, BalanceLog := ifelse(is.na(BalanceLog), 0, BalanceLog)]
(sum(is.na(datCredit_real$BalanceLog)))==0 # check - success
# infinite values caused by log(0) - assign zero's
datCredit_real[, BalanceLog := ifelse(is.infinite(BalanceLog), 0, BalanceLog)]
sum(is.infinite(datCredit_real$BalanceLog))==0 # check - success
# distributional analysis
describe(datCredit_real$BalanceLog); hist(datCredit_real$BalanceLog, breaks="FD")
### RESULTS: Large spike at zero values with some negative outliers, which skews mean to 11.9 (median: 12.7).
# However, majority of distribution's bulk is left-skewed between 5 and 15


# - Log-transformed receipt
datCredit_real[, Receipt_InfLog := log(Receipt_Inf)]
# check for missings or infinite values
sum(is.na(datCredit_real$Receipt_InfLog))
sum(is.infinite(datCredit_real$Receipt_InfLog))
# there are infinite values assigned - fix by assigning zero's to them
datCredit_real[, Receipt_InfLog := ifelse(is.infinite(Receipt_InfLog), 0, Receipt_InfLog)]
sum(is.infinite(datCredit_real$Receipt_InfLog))==0 # check again - success
# distributional analysis
describe(datCredit_real$Receipt_InfLog); hist(datCredit_real$Receipt_InfLog, breaks="FD")
### RESULTS: Despite large spike at zero and some negative outliers, mean of 7.4 (median: 8.3) with a 
# seemingly normal distribution in shape, between 5 and 11


# - Log-transformed further loan amount
datCredit_real[, FurtherLoan_AmtLog := log(FurtherLoan_Amt)]
# check for missings or infinite values
sum(is.na(datCredit_real$FurtherLoan_AmtLog))
sum(is.infinite(datCredit_real$FurtherLoan_AmtLog))
# there are infinite values assigned - fix by assigning zero's to them
datCredit_real[, FurtherLoan_AmtLog := ifelse(is.infinite(FurtherLoan_AmtLog), 0, FurtherLoan_AmtLog)]
sum(is.infinite(datCredit_real$FurtherLoan_AmtLog))==0 # check again - success
# distributional analysis
describe(datCredit_real$FurtherLoan_AmtLog); hist(datCredit_real$FurtherLoan_AmtLog, breaks="FD")
### RESULTS: Distribution is overwhelmed by zero-values, which would likely not be useful in predictive setting.


# - Log-transformed redraw amount
datCredit_real[, Redrawn_AmtLog := log(Redrawn_Amt)]
# check for missings or infinite values
sum(is.na(datCredit_real$Redrawn_AmtLog))
sum(is.infinite(datCredit_real$Redrawn_AmtLog))
# there are infinite values assigned - fix by assigning zero's to them
datCredit_real[, Redrawn_AmtLog := ifelse(is.infinite(Redrawn_AmtLog), 0, Redrawn_AmtLog)]
sum(is.infinite(datCredit_real$Redrawn_AmtLog))==0 # check again - success
# distributional analysis
describe(datCredit_real$Redrawn_AmtLog); hist(datCredit_real$Redrawn_AmtLog, breaks="FD")
### RESULTS: Distribution is overwhelmed by zero-values, which would likely not be useful in predictive setting.



# --- Feature Engineering: ratio-type variables

# - Loan age to loan term
datCredit_real[, AgeToTerm := Age_Adj/Term] # where the loan is in its lifetime
# distributional analysis
describe(datCredit_real$AgeToTerm); hist(datCredit_real[AgeToTerm<2, AgeToTerm], breaks="FD")
### RESULTS: Right-skewed distribution as expected, shape skewed by some extreme positive values.
# But most of the distribution seems usable up to 2 (shown in histogram)


# - Balance to loan term | how much is still outstanding compared to its lifetime
datCredit_real[, BalanceToTerm := Balance/Term]
# distributional analysis
describe(datCredit_real$BalanceToTerm); hist(datCredit_real[BalanceToTerm<10000, BalanceToTerm], breaks="FD")
### RESULTS: Right-skewed distribution as expected, shape skewed by some extreme positive values.
# Large influx at zero, with some negative outliers, likely due to negative-valued balances. Safe.
# Given the large scale, a log-transform may further enhance its eventual predictive value.


# - Check for missing values in the newly-created variables after applying the exclusions
sum(is.na(datCredit_real$AgeToTerm)) == 0
sum(is.na(datCredit_real$BalanceToTerm)) == 0
# success, no missing values

# - Cleanup
rm(check_na_balance); gc()


# --- Featuring Engineering: Delinquency-themed

# - Embed previous defaults into a new Boolean-valued input variable
datCredit_real[, PrevDefaults := PerfSpell_Num > 1, by=list(LoanID)]


# --- Featuring Engineering: Behavioural-themed

# - Condense the payment group
datCredit_real[, pmnt_method_grp := 
                 ifelse(slc_pmnt_method == "Debit Order FNB account" | slc_pmnt_method == "Debit Order other bank", 
                        "Debit Order", slc_pmnt_method)]
datCredit_real[, pmnt_method_grp := 
                 ifelse(slc_pmnt_method == "Salary" | slc_pmnt_method == "Suspense", 
                        "Salary/Suspense", pmnt_method_grp)]
describe(datCredit_real$pmnt_method_grp) # success




# ------- 6. Pack objects to disk

# - Save to disk (zip) for quick disk-based retrieval later
datCredit_allInputs_withDefs <- datCredit_real; rm(datCredit_real); gc() # rename object to preserve parity with modelling scripts
pack.ffdf(paste0(genPath, "creditdata_allinputs_withDefs"), datCredit_allInputs_withDefs)
gc()
