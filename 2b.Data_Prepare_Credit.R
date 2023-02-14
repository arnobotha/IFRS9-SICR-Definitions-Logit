# ================================== DATA PREPARATION ===================================
# Perform various data checks, conduct data cleaning, and apply various data treatments
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Dynamic SICR-research
# SCRIPT AUTHOR(S): Dr Arno Botha

# DESCRIPTION:
# This script prepares raw imported credit data into a much more meaningful form to 
# facilitate modelling. This preparation includes the following high-level steps:
#   1)  Perform high-level structural checks on raw data
#   2)  Conduct high-level basic cleaning
#   3a) Rename and reorder certain credit fields to align with broader platform
#   3b) Create basic features in checking for certain phenomenon, i.e., account-level 
#       write-offs. 
#   3c) Conduct basic data cleaning and perform some more checks on terminal events
#   4)  Apply exclusions based on insight gleaned from Data Experiments (script 2)
#   5)  Execute basic missing value treatments, supported by Data Experiments (script 2)
#   6)  Conduct various Advanced Data Treatments in rigorously cleaning and treating 
#       various oddities, all supported by extensive experimentation (script 2). This 
#       includes advanced feature engineering (e.g., [Receipt_Inf], [New_Ind]), as well 
#       as implicit additional exclusions, and fixing broken mechanics in certain 
#       fields (e.g., [WOff_Ind])
#   7)  Re-apply previous exclusions due to previous treatments having unveiled additional cases
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R

# -- Inputs:
#   - dat.raw | raw monthly loan performance data imported in script 1
#   - various parameters set in the setup script 0
#
# -- Outputs:
#   - datCredit_real | Enhanced version of input
# ---------------------------------------------------------------------------------------
# NOTE: This script predominantly comes from another project (Kasmeer). As such, all 
# references to associated Data Experiments reside within that project and will be taken 
# as established 'facts' within this project.
# =======================================================================================




# ------- 1. High-level data checks
ptm <- proc.time() # for runtime calculations (ignore)

# --- 1. Confirm raw data is loaded into memory
if (!exists('dat.raw')) unpack.ffdf(paste0(genPath,"creditdata_final1"), tempPath)

# --- 2. Confirm data grain and non-missingness
dat.raw[, list(Freq = .N), by=list(ACCT_NO, DATEX)][Freq>1,]
dat.raw[is.na(ACCT_NO), .N] / dat.raw[,.N] * 100 == 0 # Should be true
dat.raw[is.na(DATEX), .N] / dat.raw[,.N] * 100 == 0 # Should be true
### RESULTS: Confirmed with no missingness

# --- 3. Frequency analysis on DATE (period) in checking consistency across platforms (SAS + R)
test <- dat.raw[order(DATE), list(Freq=.N),by=list(DATE)]
# [SANITY CHECK] Are there any missing periods in aggregate?
test[, Prev_DATE := shift(DATE, n=1, type="lag")]
test[!is.na(Prev_DATE), Diff_Months := interval(Prev_DATE, DATE) %/% months(1)]
check_cred0 <- test[Diff_Months > 1, .N] == 0 # If TRUE then no missing periods.
cat( check_cred0 %?% 'SAFE: No missing periods found in aggregate.\n' %:% 'WARNING: Missing periods found in aggregate.\n')
rm(test)

# --- 4. Some parameter observations
(maxDate_observed <- max(dat.raw$DATE, na.rm=T) )
(prevMaxDate_observed <- rollback(max(dat.raw$DATE, na.rm=T), roll_to_first = T) - days(1))
(minDate_observed <- rollback(min(dat.raw$DATE, na.rm=T), roll_to_first = T) )





# ------- 2. Necessary Data Cleaning on Raw Data
# - remove redundant fields for greater memory efficiency
suppressWarnings(dat.raw[, `:=`(SUB_STAT_CDE = NULL,
                                # Fields that were investigated and deemed irrelevant | See Data Experiment 10
                                PRE_D7_ACC_STATUS = NULL, slc_status_final_pred7 = NULL, slc_status_final = NULL,
                                # Fields yet to be created 
                                LoanAge = NULL, Max_Counter = NULL)] )

# - engineer fundamental loan parameter fields
dat.raw[, LoanAge := interval(Date_Origination, DATE)]
dat.raw[, LoanAge := LoanAge %/% months(1)]
dat.raw[, Max_Counter := max(Counter, na.rm=T), by=list(ACCT_NO)]
gc() # Memory optimization





# ------- 3. Data mapping & Basic Data Preparation (Cleaning & Feature Engineering)

# --- 1. Data mapping
# Map field names (and reorder them) to align with rest of the platform, and drop unnecessary ones
datCredit_real <- dat.raw %>% 
  rename(LoanID = ACCT_NO, Date = DATE, Age = LoanAge, AccountStatus = POST_D7_ACC_STATUS, 
         Curing_Ind = slc_curing_ind) %>% 
  as.data.table(key=c("LoanID", "Counter")) %>%
  relocate(LoanID, Date, Age, Counter, Max_Counter, Date_Origination, Principal, 
           Term, InterestRate_Nom, Instalment, Arrears, Balance, AccountStatus, 
           Curing_Ind, WOff_Ind, WriteOff_Amt, EarlySettle_Ind, EarlySettle_Amt, 
           FurtherLoan_Ind, FurtherLoan_Amt, Redraw_Ind, Redrawn_Amt, WOFF_DATE, CLS_STAMP,
           .before = LoanID) %>% 
  dplyr::select(-c(PIP_DATE, WOT_IND))


# --- 2. Basic Feature Engineering

# - Create Default Indicator based on given delinquency status field 
# NOTE: We will recreate this and related fields later
datCredit_real <- datCredit_real %>% mutate(DefaultStatus1 = 
                                              ifelse(AccountStatus == "05. NPL", 1, 0)) %>%
  relocate(DefaultStatus1, .after=AccountStatus)
datCredit_real <- datCredit_real %>% mutate(g0_Delinq = as.numeric(substr(AccountStatus,1,2))) %>%
  mutate(g0_Delinq = case_when(g0_Delinq==1 ~ 0, g0_Delinq <= 3 ~ 1, 
                               g0_Delinq == 4 ~ 2, g0_Delinq == 5 ~ 3)) %>%
  relocate(g0_Delinq, .after = AccountStatus)

# - Creating account-level aggregates of events to facilitate the creation of other features
datCredit_real[, HasWOff := max(WOff_Ind, na.rm=T), by=list(LoanID)]
datCredit_real[, HasSettle := max(EarlySettle_Ind, na.rm=T), by=list(LoanID)]
datCredit_real[, HasClosure := ifelse(any(!is.na(CLS_STAMP)), 1, 0) ,by=list(LoanID)]
datCredit_real[, HasFurtherLoan := max(FurtherLoan_Ind, na.rm=T), by=list(LoanID)]
datCredit_real[, HasRedraw := max(Redraw_Ind, na.rm=T), by=list(LoanID)]

# - Create repaid flag for unaccounted account closures | See Data Experiment 2
datCredit_real[, Repaid_Ind :=
                 ifelse(Counter==Max_Counter & HasClosure==1 & HasSettle==0 & HasWOff == 0 &
                          ( (Balance <= 250 & is.na(RMNG_TERM)) | 
                              (!is.na(RMNG_TERM) & RMNG_TERM <= 1 & Date < maxDate_observed)), 1,0),
               by=list(LoanID)]
datCredit_real[, HasRepaid := max(Repaid_Ind, na.rm=T), by=list(LoanID)]


# --- 3. Basic Data Cleaning
datCredit_real[is.na(Arrears), Arrears := 0]
datCredit_real[WOff_Ind == 0, WriteOff_Amt := 0] # See Data Experiment 2
datCredit_real[EarlySettle_Ind == 0, EarlySettle_Amt := 0] # See Data Experiment 2


# --- 4. Basic checks: Terminal event amounts (Write-off, Early Settlement) | See Data Experiment 2

# [SANITY CHECK] How often is the write-off amount non-zero (when it should be zero)?
(diag.real2b <- datCredit_real[WOff_Ind == 0 & WriteOff_Amt != 0, .N] / datCredit_real[WOff_Ind == 0, .N] * 100)
check_cred1 <- diag.real2b == 0 # Should yield TRUE for test to pass
cat( check_cred1 %?% 'SAFE: Write-off amounts are zero wherever write-off has not occurred.\n' %:% 'WARNING: Write-off amounts are non-zero in cases when they should be zero.\n')

# [SANITY CHECK] How often is the settlement amount non-zero (when it should be zero)?
(diag.real2f <- datCredit_real[EarlySettle_Ind == 0 & EarlySettle_Amt != 0, .N] / datCredit_real[EarlySettle_Ind == 0, .N] * 100)
check_cred2 <- diag.real2f == 0 # Should be TRUE
cat( check_cred2 %?% 'SAFE: Early settlement amounts are zero wherever settlement has not occurred.\n' %:% 'WARNING: Early settlement amounts are non-zero in cases when they should be zero.\n')





# ------ 4. Exclusions

# --- 0. Preliminaries
# Create a field that will be 0 for unaffected observations but >= 1 for all cases to be excluded subsequently,
# such that the value of this field denotes the Exclusion ID, itself tracked in a common table that measures the 
# sample impact per exclusion.
suppressWarnings({ datCredit_real[, ExclusionID := NULL]; rm(datExclusions) })
datCredit_real[, ExclusionID := 0]


# --- 1. Old single-record cases | See Data Experiment 4

# - maxDate set to be the first of the last month in which loans were last disbursed
maxDate <- rollback(max(datCredit_real$Date_Origination, na.rm=T), roll_to_first = T)

# [DIAGNOSTIC] Account-level and dataset-wide impacts of exclusion
diag.real4b <-  datCredit_real[ExclusionID == 0 & Counter == 1 & Max_Counter == 1 & 
                                 (Date_Origination < maxDate | is.na(Date_Origination) ), .N] / 
  datCredit_real[ExclusionID == 0 & Counter == 1, .N] * 100 
diag.real4b_abs <- datCredit_real[ExclusionID == 0 & Max_Counter == 1 & 
                                    (Date_Origination < maxDate | is.na(Date_Origination)) , .N]
diag.real4b_rec <-  diag.real4b_abs / datCredit_real[ExclusionID == 0, .N] * 100 

# - Conditional exclusion
if (diag.real4b > 0) {
  
  cat("EXCLUSION: Old single-record cases. Prevalence: ", round(diag.real4b,digits=1), "% of accounts (",
      round(diag.real4b_rec,digits=1), "% of records).\n")
  
  # - Mark affected records for exclusion later with an ID-value
  LoanIDs <- unique(subset(datCredit_real, ExclusionID == 0 & Counter == 1 & Max_Counter == 1 & 
                             (Date_Origination < maxDate | is.na(Date_Origination)), select="LoanID"))
  datCredit_real[LoanID %in% LoanIDs$LoanID, ExclusionID := 1]
  
  # [SANITY CHECK] Treatment success?
  check_excl1 <- datCredit_real[ExclusionID == 1 & Counter == 1, .N] / 
    datCredit_real[ExclusionID %in% c(0,1) & Counter == 1, .N] * 100 == diag.real4b
  cat( check_excl1 %?% 'SAFE: Exclusion 1 successfully applied.\n' %:% 
         'WARNING: Applying Exclusion 1 failed.\n')
  
  # - Create and add exclusion impact to a common table
  datExcl <- data.table("Excl_ID"=1, "Reason"="Old single-record cases",
                        "Impact_Account" = diag.real4b, "Impact_Dataset" = diag.real4b_rec,
                        "Impact_records" = diag.real4b_abs)
  if (exists('datExclusions')) datExclusions <- rbind(datExclusions, datExcl) else datExclusions <- datExcl
}


# --- 2. Zero-balance credit histories | See Data Experiment 6

# - Preliminaries
datCredit_real[ExclusionID==0, HasZeroBalances_all := all(ifelse(Balance <= 0, TRUE, FALSE)), by=list(LoanID)]

# [DIAGNOSTIC] What proportion of account's credit histories have zero balances throughout?
diag.real6a <- datCredit_real[ExclusionID == 0 & Counter == 1 & HasZeroBalances_all == TRUE, .N] /
  datCredit_real[ExclusionID == 0 & Counter == 1, .N] * 100
diag.real6a_abs <- datCredit_real[ExclusionID == 0 & HasZeroBalances_all == TRUE, .N]
diag.real6a_rec <- diag.real6a_abs  / datCredit_real[ExclusionID == 0, .N] * 100

# - Conditional exclusion
if (diag.real6a > 0) {
  
  cat("EXCLUSION: Zero-balance credit histories. Prevalence: ", round(diag.real6a,digits=1), "% of accounts (",
      round(diag.real6a_rec,digits=1), "% of records).\n")
  
  # - Mark affected records for exclusion later with an ID-value
  LoanIDs <- unique(subset(datCredit_real, ExclusionID == 0 & 
                             Counter == 1 & HasZeroBalances_all == TRUE, select="LoanID"))
  datCredit_real[LoanID %in% LoanIDs$LoanID, ExclusionID := 2]
  
  # [SANITY CHECK] Treatment success?
  check_excl2 <- datCredit_real[ExclusionID == 2 & Counter == 1, .N] / 
    datCredit_real[ExclusionID %in% c(0,2) & Counter == 1, .N] * 100 == diag.real6a
  cat( check_excl2 %?% 'SAFE: Exclusion 2 successfully applied.\n' %:% 
         'WARNING: Applying Exclusion 2 failed.\n')
  
  # - Create and add exclusion impact to a common table
  datExcl <- data.table("Excl_ID"=2, "Reason"="Zero-balance credit histories",
                        "Impact_Account" = diag.real6a, "Impact_Dataset" = diag.real6a_rec,
                        "Impact_records" = diag.real6a_abs)
  if (exists('datExclusions')) datExclusions <- rbind(datExclusions, datExcl) else datExclusions <- datExcl
}


# --- 3. Zero-Principal credit histories | See Data Experiment 5

# - Preliminaries
datCredit_real[ExclusionID==0, HasZeroPrincipal_all := all(Principal==0), by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of zero-valued Principals throughout the entire account history?
diag.real5_1d <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroPrincipal_all==T, .N] /
  datCredit_real[ExclusionID==0 & Counter==1, .N] * 100 
diag.real5_1d_abs <- datCredit_real[ExclusionID==0 & HasZeroPrincipal_all==T, .N]
diag.real5_1d_rec <- diag.real5_1d_abs / datCredit_real[ExclusionID==0, .N] * 100 

# - Conditional treatment
if (diag.real5_1d > 0) {
  
  cat("EXCLUSION: Zero-principal credit histories. Prevalence: ", round(diag.real5_1d,digits=3), "% of accounts (",
      round(diag.real5_1d_rec,digits=4), "% of records).\n")
  
  # - Mark affected records for exclusion later with an ID-value
  LoanIDs <- unique(subset(datCredit_real, ExclusionID==0 & Counter==1 & 
                             HasZeroPrincipal_all==T, select="LoanID"))
  datCredit_real[LoanID %in% LoanIDs$LoanID, ExclusionID := 3]
  
  # [SANITY CHECK] Treatment success?
  check_excl3 <- datCredit_real[ExclusionID == 3 & Counter == 1, .N] / 
    datCredit_real[ExclusionID %in% c(0,3) & Counter == 1, .N] * 100 == diag.real5_1d
  cat( check_excl3 %?% 'SAFE: Exclusion 3 successfully applied.\n' %:% 
         'WARNING: Applying Exclusion 3 failed.\n')
  
  # - Create and add exclusion impact to a common table
  datExcl <- data.table("Excl_ID"=3, "Reason"="Zero-principal credit histories",
                        "Impact_Account" = diag.real5_1d, "Impact_Dataset" = diag.real5_1d_rec,
                        "Impact_records" = diag.real5_1d_abs)
  if (exists('datExclusions')) datExclusions <- rbind(datExclusions, datExcl) else datExclusions <- datExcl
}


# --- 4. Zero-Instalment & Zero-Arrears credit histories | See Data Experiment 8

# - Preliminaries
datCredit_real[ExclusionID==0, HasZeroIns_All := ifelse(all(Instalment==0),T,F),by=list(LoanID)]
datCredit_real[ExclusionID==0, HasZeroArrears_All := ifelse(all(Arrears==0),T,F),by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of account-level zero-valued instalments
diag.real8_1a <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_All==T, .N] / 
  datCredit_real[ExclusionID==0 & Counter==1, .N] * 100
diag.real8_1a_abs <- datCredit_real[ExclusionID==0 & HasZeroIns_All==T, .N]
diag.real8_1a_rec <- diag.real8_1a_abs / datCredit_real[ExclusionID==0, .N] * 100

# [DIAGNOSTIC] Prevalence of zero-valued arrears amongst zero-valued instalments?
diag.real8_1c <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_All==T & HasZeroArrears_All==T, .N] /
  datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_All==T, .N] * 100

# - Conditional treatment
if (diag.real8_1a > 0 & diag.real8_1c >= 0.9) {
  
  cat("EXCLUSION: Zero-instalment credit histories. Prevalence: ", round(diag.real8_1a,digits=3), "% of accounts (",
      round(diag.real8_1a_rec,digits=4), "% of records), \n\tof which ", round(diag.real8_1c,digits=3),
      "% also have zero-valued arrears throughout.\n")
  
  # - Mark affected records for exclusion later with an ID-value
  LoanIDs <- unique(subset(datCredit_real, ExclusionID==0 & Counter==1 & 
                             HasZeroIns_All==T & HasZeroArrears_All==T, select="LoanID"))
  datCredit_real[LoanID %in% LoanIDs$LoanID, ExclusionID := 4]
  
  # [SANITY CHECK] Treatment success?
  check_excl4 <- datCredit_real[ExclusionID == 4 & Counter == 1, .N] / 
    datCredit_real[ExclusionID %in% c(0,4) & Counter == 1, .N] * 100 == diag.real8_1a
  cat( check_excl4 %?% 'SAFE: Exclusion 4 successfully applied.\n' %:% 
         'WARNING: Applying Exclusion 4 failed.\n')
  
  # - Create and add exclusion impact to a common table
  datExcl <- data.table("Excl_ID"=4, "Reason"="Zero-instalment & zero-arrears credit histories",
                        "Impact_Account" = diag.real8_1a, "Impact_Dataset" = diag.real8_1a_rec,
                        "Impact_records" = diag.real8_1a_abs)
  if (exists('datExclusions')) datExclusions <- rbind(datExclusions, datExcl) else datExclusions <- datExcl  
}


# --- Clean up | Intermediary treatment-related fields no longer necessary
suppressWarnings( datCredit_real[,`:=`(HasZeroPrincipal_all = NULL, HasZeroBalances_all = NULL,
                                       HasZeroIns_All = NULL, HasZeroArrears_All = NULL)] )




# ------ 5. Missing value treatments | Sensitive to Exclusions

# --- 1. Treating [Term] | # See Data Experiment 5

# - Preliminaries
datCredit_real[, HasMissingTerm := any(is.na(Term)), by=list(LoanID)]
datCredit_real[, HasMissingTerm_all := all(is.na(Term)), by=list(LoanID)]

# [DIAGNOSTIC] How prevalent is missingness in parts of TERM within the wider dataset
diag.real5_2b <- datCredit_real[Counter == 1 & HasMissingTerm==T & ExclusionID==0, .N] / 
  datCredit_real[Counter == 1 & ExclusionID==0, .N] * 100  # account-level
diag.real5_2b_rec <- datCredit_real[ is.na(Term) & ExclusionID==0, .N] /
  datCredit_real[ExclusionID==0, .N] * 100  # dataset-level

# - Conditional treatment of missingness using the mode
if (diag.real5_2b > 0 | diag.real5_2b_rec > 0) {
  
  cat("DETECTED: Missingness in [Term]. Prevalence:", round(diag.real5_2b,digits=1), "% of accounts (",
      round(diag.real5_2b_rec,digits=1), "% of records).\n\tTreating by assiging the account-level mode ...\n")
  
  # [TREATMENT] Assign mode of account-level Term-vectors to its missing parts, 
  #   using custom "getmode()" function defined in script 0
  datCredit_real[HasMissingTerm == T & HasMissingTerm_all == F & ExclusionID==0, 
                 Term := ifelse(is.na(Term), getmode(Term), Term), by=list(LoanID)]
  
  # [TREATMENT] For those cases with universal missing terms, use mode imputation
  term_mode <- getmode(datCredit_real[Counter == 1 & HasMissingTerm_all == F, Term])
  datCredit_real[HasMissingTerm_all == T, Term := term_mode]
  
  # - Recalculate affected aggregates
  datCredit_real[, HasMissingTerm := any(is.na(Term)), by=list(LoanID)]
  datCredit_real[, HasMissingTerm_all := all(is.na(Term)), by=list(LoanID)]
  
  # [SANITY CHECK] Confirm successful treatment, considering previous sanity check
  check_treat1 <- datCredit_real[Counter == 1 & HasMissingTerm==T & ExclusionID==0, .N] / 
    datCredit_real[Counter == 1 & ExclusionID==0, .N] * 100 == 0 # account-level
  cat( check_treat1 %?% 'SAFE: Missing value treatment on [Term] succeeded.\n' %:% 
         'WARNING: Missing value treatment on [Term] failed.\n')
}


# --- 2. Treating [Date_Origination] | # See Data Experiment 3

# - Preliminaries
datCredit_real[, HasMissingOpenDate_all := all(ifelse(is.na(Date_Origination), TRUE, FALSE)), by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of missingness in [Date_Origination]
diag.real3_1e <- datCredit_real[ExclusionID==0 & Counter==1 & HasMissingOpenDate_all==T, .N] / 
  datCredit_real[ExclusionID==0 & Counter==1, .N] * 100

# - Conditional treatment
if (diag.real3_1e > 0) {
  
  cat("DETECTED: Missingness in [Date_Origination]. Prevalence:", round(diag.real3_1e,digits=3),
      "% of accounts.\n\tTreating by assigning the first-observed [Date]-value ...\n")
  
  # [TREATMENT] Set first observed date as the "open date"
  datCredit_real[HasMissingOpenDate_all==T, 
                 Date_Origination := rollback(Date[1], roll_to_first = T)-days(1), by=list(LoanID)]
  
  # - Recalculate derived fields that depend on this treated field
  datCredit_real[HasMissingOpenDate_all==T, Age := interval(Date_Origination, Date) %/% months(1)]
  
  # - Recalculate affected aggregates 
  datCredit_real[, HasMissingOpenDate_all := all(ifelse(is.na(Date_Origination), TRUE, FALSE)), by=list(LoanID)]
  
  # [SANITY CHECK] Confirm successful treatment
  check_treat2 <- datCredit_real[Counter==1 & ExclusionID==0 & HasMissingOpenDate_all==T, .N] / 
    datCredit_real[Counter==1 & ExclusionID==0, .N] * 100 == 0 # account-level
  cat( check_treat2 %?% 'SAFE: Missing value treatment on [Date_Origination] succeeded.\n' %:% 
         'WARNING: Missing value treatment on [Date_Origination] failed.\n')
}

# --- Clean up | Intermediary treatment-related fields no longer necessary
suppressWarnings(
  datCredit_real[, `:=`(HasMissingTerm = NULL, HasMissingTerm_all = NULL, HasMissingOpenDate_all = NULL,
                        HasMissingOpenDate_any = NULL)]  
)




# ------ 6. Advanced Data Treatments

# --- 0. Preliminaries
# - Ensure any engineered features during treatments do not exist before starting advanced treatment
suppressWarnings( datCredit_real[, `:=`(TreatmentID = NULL, HasZeroBalance_Start = NULL, Counter_TruStart1 = NULL,
                                        Principal_LastNonzeroPos = NULL, Principal_FirstNonzeroPos = NULL,
                                        Principal_LastNonzero = NULL, Principal_FirstNonzero = NULL,
                                        ZeroBal_Start = NULL, HasTrailingZeroBalances = NULL,
                                        Var_Term = NULL, Term_Mode = NULL, HasZeroPrincipal = NULL, Receipt_Inf = NULL)])

# - Similar to [ExclusionID], create an account-level field that will be flagged by the ID-value
# of the corresponding Advanced Data Treatment if affected.
datCredit_real[, TreatmentID := ""]; gc()


# --- 1. Advanced Data Treatment 1: Starting zero-balances | See Data Experiment 3

# - Preliminaries
datCredit_real[, HasZeroBalance_Start := max(ifelse(Counter == 1 & Balance == 0, 1, 0)), 
               by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of accounts that start with a zero-valued balance?
diag.real3_1a <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroBalance_Start==1, .N] / 
  datCredit_real[ExclusionID==0 & Counter==1, .N] * 100 

# - Conditional treatment
if (diag.real3_1a > 0) {
  
  # Preparing treatment
  datCredit_real[HasZeroBalance_Start==1, Counter_TruStart1 := 
                   max(Counter[1], which(Balance > 0)[1]),
                 by=list(LoanID)]
  
  # [SANITY CHECK] Is the prepared treatment fit for purpose? Does it cover the affected data space?
  diag.real3_1d <- datCredit_real[ExclusionID==0 & Counter==1 & Counter_TruStart1 != Counter, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] * 100 
  diag.real3_1d_abs <- datCredit_real[ExclusionID==0 & Counter<Counter_TruStart1, .N]
  diag.real3_1d_rec <- diag.real3_1d_abs / datCredit_real[ExclusionID==0, .N] * 100
  check_advTreat2a <- diag.real3_1d == diag.real3_1a # Should be TRUE
  cat( paste0("DETECTED: Zero-valued starting balances found in ", round(diag.real3_1a, digits=1),
              "% of accounts (", round(diag.real3_1d_rec,digits=1), "% of records). Preparing treatment ... \n",
              check_advTreat2a %?% 'SAFE: Treatment correctly prepared. Appplying treatment ...\n' %:% 
                'WARNING: Prepared treatment does not correspond 100% to the affected data space.\n') )
  
  # [TREATMENT] Mark the affected histories as an Exclusion
  datCredit_real[ExclusionID == 0 & HasZeroBalance_Start==1 & Counter<Counter_TruStart1, ExclusionID := 5]
  
  # [TREATMENT] Re-assign the affected counter fields accordingly using the true starting point
  datCredit_real[ExclusionID==0 & HasZeroBalance_Start==1, Max_Counter:= .N, by=list(LoanID)]
  datCredit_real[ExclusionID==0 & HasZeroBalance_Start==1, Counter:= 1:.N, by=list(LoanID)]
  
  # - Re-assign [Date_Origination] to be the newly-observed first record, purely
  # to facilitate the recalculation of loan [Age] accordingly. Although this
  # contaminates the previously observed field somewhat, information of this
  # event is recorded in the [TreatmentID] field.
  datCredit_real[ExclusionID==0 & HasZeroBalance_Start==1, 
                 Date_Origination := rollback(Date[1], roll_to_first = T)-days(1), by=list(LoanID)]
  datCredit_real[ExclusionID==0 & HasZeroBalance_Start==1, 
                 Age := interval(Date_Origination, Date) %/% months(1)]
  
  # - Mark the affected accounts with current treatment ID
  LoanIDs <- unique(datCredit_real[ExclusionID==0 & HasZeroBalance_Start==1, LoanID])
  datCredit_real[LoanID %in% LoanIDs, TreatmentID := paste0(TreatmentID, ";1")]
  
  # - Create and add exclusion impact to a common table
  datExcl <- data.table("Excl_ID"=5, "Reason"="Starting Zero-balances; partially excluded, the rest treated",
                        "Impact_Account" = diag.real3_1d, "Impact_Dataset" = diag.real3_1d_rec,
                        "Impact_records"=diag.real3_1d_abs) 
  if (exists('datExclusions')) datExclusions <- rbind(datExclusions, datExcl) else datExclusions <- datExcl
  
  # - Recalculate affected aggregates
  datCredit_real[ExclusionID==0, HasZeroBalance_Start := max(ifelse(Counter == 1 & Balance == 0, 1, 0)), 
                 by=list(LoanID)]
  
  # [SANITY CHECK] Account-level prevalence of accounts that start with a zero-valued balance?
  check_advTreat2b <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroBalance_Start == 1, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] *100 == 0 # Should be true
  cat( check_advTreat2b %?% paste0('SAFE: Zero-valued starting balances succesfully treated by adjusting ',
                                   '\n\tthe true starting point, having excluded the zero-balance history at the start (', 
                                   round(diag.real3_1d_rec,digits=2), '% of records).\n') %:% 
         'WARNING: Failed to treat zero-valued starting balances.\n')
}

# - Cleanup
suppressWarnings(datCredit_real[, `:=`(HasZeroBalance_Start = NULL, Counter_TruStart1 = NULL)])



# --- 2. Advanced Data Treatment 2: Unflagged account closures | See Data Experiment 2

# [DIAGNOSTIC] Account-level prevalence of closures missed by [EarlySettle_Ind] or [WOff_Ind] or [Repaid_Ind] flags?
diag.real2_2h <- datCredit_real[ExclusionID==0 & Counter==1 & HasClosure==1 & HasSettle==0 & HasWOff==0 & HasRepaid==0, .N] / 
  datCredit_real[ExclusionID==0 & Counter==1, .N] *100 
diag.real2_2h_closures <- datCredit_real[ExclusionID==0 & Counter==1 & HasClosure==1 & HasSettle==0 & HasWOff==0 & HasRepaid==0, .N] / 
  datCredit_real[ExclusionID==0 & Counter==1 & (HasClosure==1 | HasSettle==1 | HasWOff==1 | HasRepaid==1), .N] *100 
diag.real2_2h_rec <- datCredit_real[ExclusionID==0 & HasClosure==1 & HasSettle==0 & HasWOff==0 & HasRepaid==0, .N] / 
  datCredit_real[ExclusionID==0, .N] *100 

# - Conditional treatment
if (diag.real2_2h > 0) {
  
  cat("DETECTED: Account closures unflagged by [EarlySettle_Ind], [WOff_Ind], or [Repaid_Ind].",
      "\n\tPrevalence: ", round(diag.real2_2h,digits=3), "% of accounts, ", round(diag.real2_2h_closures,digits=2), 
      "% of terminating accounts (those with a [CLS_STAMP]-value).\n\t", round(diag.real2_2h_rec,digits=2),
      "% of records affected.\n\tTreating affected accounts as early settlements ..\n")
  
  # [TREATMENT] Mark unaccounted closures as early settlements while redistributing any non-zero balances
  # NOTE: The associated [EarlySettle_Amt] and other fields will be curated later
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasClosure==1 & HasSettle==0 & 
                   HasWOff==0 & HasRepaid==0, EarlySettle_Ind := 1]
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasClosure==1 & HasSettle==0 & 
                   HasWOff==0 & HasRepaid==0, EarlySettle_Amt := EarlySettle_Amt + Balance]
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasClosure==1 & HasSettle==0 & 
                   HasWOff==0 & HasRepaid==0, Balance := 0]
  
  # - Mark affected accounts with current treatment ID
  LoanIDs <- unique(datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasClosure==1 & 
                                     HasSettle==0 & HasWOff == 0 & HasRepaid==0, LoanID])
  datCredit_real[LoanID %in% LoanIDs, TreatmentID := paste0(TreatmentID, ";2")]
  
  # - Recalculate affected aggregates
  datCredit_real[, HasSettle := max(EarlySettle_Ind, na.rm=T), by=list(LoanID)]
  
  # [SANITY CHECK] Account-level prevalence of closures unaccounted for by either [EarlySettle_Ind] or [WOff_Ind] flags?
  check_advTreat1 <- datCredit_real[ExclusionID==0 & Counter==1 & HasClosure==1 & HasSettle==0 & HasWOff==0 & HasRepaid==0, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] *100 == 0 # Should be true
  cat( check_advTreat1 %?% 'SAFE: Unaccounted closures succesfully treated as early settlements.\n' %:% 
         'WARNING: Failed to treat unaccounted closures as early settlements.\n')
}



# --- 3. Advanced Data Treatment 3: Partially zero-valued [Principal] and missing [InterestRate_Nom]-cases
# See Data Experiment 5

# - Preliminaries
datCredit_real[ExclusionID==0, HasZeroPrincipal := any(Principal==0), by=list(LoanID)]
datCredit_real[ExclusionID==0, HasMissingInterest := any(is.na(InterestRate_Nom)), by=list(LoanID)]

# [DIAGNOSTIC] How prevalent are zero-valued Principals in parts of [Principal] within the wider dataset
diag.real5_1c <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroPrincipal==T, .N] / 
  datCredit_real[ExclusionID==0 & Counter==1 , .N] * 100  # account-level
diag.real5_1c_rec <- datCredit_real[HasZeroPrincipal==T, .N] / datCredit_real[, .N] * 100  # dataset-level

# - Conditional treatment | [Principal]
if (diag.real5_1c > 0) {
  
  cat(paste0("DETECTED: Partially zero-valued [Principal]-cases in ", round(diag.real5_1c,digits=2), 
             "% of accounts (", round(diag.real5_1c_rec,digits=2), " of records).",
             "\n\tTreating by assigning the last-known non-zero account-level [Principal]-value ...\n"))
  
  # - Find the account-level position of the last known non-zero Principal-value
  datCredit_real[ExclusionID==0 & HasZeroPrincipal==T, 
                 Principal_LastNonzeroPos := rev(which(Principal > 0))[1], 
                 by=list(LoanID)]
  
  # - Find the corresponding account-level last non-zero Principal-value itself
  datCredit_real[ExclusionID==0 & HasZeroPrincipal==T, 
                 Principal_LastNonzero := Principal[Principal_LastNonzeroPos], 
                 by=list(LoanID)]
  
  # [TREATMENT] Assign last known non-zero Principal-value to trailing zero-valued [Principal]-cases
  datCredit_real[ExclusionID==0 & HasZeroPrincipal==T & Counter>Principal_LastNonzeroPos,
                 Principal := Principal_LastNonzero]
  
  # - Find the account-level position of the first known non-zero Principal-value
  datCredit_real[ExclusionID==0 & HasZeroPrincipal==T, 
                 Principal_FirstNonzeroPos := which(Principal > 0)[1], 
                 by=list(LoanID)]
  
  # - Find the corresponding account-level first non-zero Principal-value itself
  datCredit_real[ExclusionID==0 & HasZeroPrincipal==T, 
                 Principal_FirstNonzero := Principal[Principal_FirstNonzeroPos], 
                 by=list(LoanID)]
  
  # [TREATMENT] Assign first known non-zero Principal-value to starting zero-valued [Principal]-cases
  datCredit_real[ExclusionID==0 & HasZeroPrincipal==T & Counter<Principal_FirstNonzeroPos,
                 Principal := Principal_FirstNonzero]
  
  # [TREATMENT] Assign the account-level mode of [Principal] to those remaining zero-valued cases
  datCredit_real[ExclusionID==0 & HasZeroPrincipal==T, 
                 Principal := ifelse(Principal > 0, Principal,
                                     getmode( .SD[Principal > 0, Principal]) # filter out zero-valued cases when getting mode
                 ), by=list(LoanID)]
  
  # - Mark the affected accounts with current treatment ID
  LoanIDs <- unique(datCredit_real[ExclusionID==0 & HasZeroPrincipal==T, LoanID])
  datCredit_real[LoanID %in% LoanIDs, TreatmentID := paste0(TreatmentID, ";3a")]
  
  # - Recalculate affected account-level aggregates
  datCredit_real[ExclusionID==0, HasZeroPrincipal := any(Principal==0), by=list(LoanID)]
  
  # [SANITY CHECK] Account-level prevalence of accounts that start with a zero-valued balance?
  check_advTreat3a <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroPrincipal==T, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] *100 == 0 # Should be true
  cat( check_advTreat3a %?% 'SAFE: Zero-valued [Principal]-values succesfully treated by 
    interleaving the first and last known non-zero values accordingly, along with treating 
    the remaining cases with the account-level mode.\n' %:% 
         'WARNING: Failed to treat zero-valued [Principal]-values.\n')
}

# [DIAGNOSTIC] How prevalent is missingness in parts of [InterestRate_Nom] within the wider dataset
diag.real5_3b <- datCredit_real[Counter == 1 & HasMissingInterest==T, .N] / 
  datCredit_real[Counter == 1, .N] * 100  # account-level
diag.real5_3b_rec <- datCredit_real[ HasMissingInterest==T, .N] / datCredit_real[, .N] * 100  # dataset-level

# - Conditional treatment | [InterestRate_Nom]
if (diag.real5_3b > 0) {
  
  # - Conditional treatment
  cat(paste0("DETECTED: Partially missing [InterestRate_Nom]-cases in ", round(diag.real5_3b,digits=3), 
             "% of accounts (", round(diag.real5_3b_rec,digits=3), " of records).",
             "\n\tTreating by assigning the first/last-known non-zero account-level [InterestRate_Nom]-value ...\n"))  
  
  # - Mark the affected accounts with current treatment ID
  datCredit_real[ExclusionID==0 & is.na(InterestRate_Nom), TreatmentID := paste0(TreatmentID, ";3b")]
  
  # [TREATMENT] Back-fill starting missingness with the first non-missing element (if available),
  # then treat remaining partial missingness by imputing with the last-known non-missing element 
  datCredit_real[ExclusionID==0 & HasMissingInterest==T, InterestRate_Nom := 
                   imputeLastKnown(imputeFirstKnown(InterestRate_Nom)), by=list(LoanID)]
  
  # - Recalculate affected account-level aggregates
  datCredit_real[ExclusionID==0, HasMissingInterest := any(is.na(InterestRate_Nom)), by=list(LoanID)]
  
  # [SANITY CHECK] Account-level prevalence of accounts that start with a zero-valued balance?
  check_advTreat3b <- datCredit_real[ExclusionID==0 & Counter==1 & HasMissingInterest==T, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] *100 == 0 # Should be true
  cat( check_advTreat3b %?% 'SAFE: Missing [InterestRate_Nom]-values succesfully treated by 
    interleaving the first and last known non-missing values accordingly.\n' %:% 
         'WARNING: Failed to treat missing [InterestRate_Nom]-values.\n')
}

# - Cleanup
suppressWarnings( datCredit_real[, `:=`(Principal_LastNonzeroPos = NULL, Principal_FirstNonzeroPos = NULL,
                                        Principal_LastNonzero = NULL, Principal_FirstNonzero = NULL,
                                        HasZeroPrincipal = NULL, HasMissingInterest = NULL)]  )



# --- 4. Advanced Data Treatment 4: Identifying truly 'new' accounts | See Data Experiment 7

# - Set parameters for New-indicator based on Balance-to-Principal and Age-to-Term
thresh_TermRatio <- 0.15
thresh_PrincipalRatio <- 0.9

# - Sample data for treatment purposes without contaminating the original dataset
treat_samp1 <- subset(datCredit_real, ExclusionID==0, 
                      select=c("LoanID", "Counter", "Age", "Date_Origination",
                               "Term", "Principal", "Balance", "ExclusionID", "TreatmentID"))

# - Basic feature engineering & account-level aggregates for analyses & treatments.
treat_samp1[Principal > 0, Principal_Ratio := Balance / Principal]
treat_samp1[, Term_Ratio := Age / Term]
treat_samp1[, First_Age := Age[1], by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of older ages for seemingly new accounts that were
# disbursed during the sampling period?
diag.real7e <- treat_samp1[ExclusionID==0 & Counter==1 & Date_Origination >= minDate_observed & Age>0,.N] / 
  treat_samp1[ExclusionID==0 & Counter==1 & Date_Origination >= minDate_observed, .N] * 100 
diag.real7e_rec <- treat_samp1[ExclusionID==0 & Date_Origination >= minDate_observed & First_Age>0,.N] / 
  treat_samp1[ExclusionID==0 & Date_Origination >= minDate_observed, .N] * 100 

cat( paste0("Creating [New_Ind] to isolate truly 'new' accounts from data errors during the sampling period ..
            Thresholds selected: Age-to-Term <= ", round(thresh_TermRatio*100,digits=0), 
            "%; Balance-to-Principal >=", round(thresh_PrincipalRatio*100,digits=0), "%.\n"))

# [TREATMENT] Create an account-level new/old indicator based on derived decision rule
treat_samp2 <- treat_samp1 %>% filter(Counter==1) %>%
  mutate(New_Ind = ifelse(Date_Origination >= minDate_observed & 
                            (Age == 0 | (Term_Ratio <= thresh_TermRatio & Principal_Ratio >= thresh_PrincipalRatio)),
                          1, 0))

# [DIAGNOSTIC] Prevalence rate of new loans: a logic comparison of 'new'
diag.real7f_a <- treat_samp2[New_Ind == 1, .N] / treat_samp2[,.N] * 100 
diag.real7f_b <- treat_samp1[ExclusionID==0 & Counter==1 & Age==0,.N] / 
  treat_samp1[ExclusionID==0 & Counter==1, .N] * 100 

# [DIAGNOSTIC] Prevalence rate of new loans disbursed during sampling period?
diag.real7g <- treat_samp2[New_Ind == 1 & Date_Origination >= minDate_observed, .N] / 
  treat_samp2[Date_Origination >= minDate_observed,.N] * 100

# - Conditional Treatment
if (diag.real7e > 0 | diag.real7f_a != diag.real7f_b) {
  
  cat( paste0("DETECTED: Mis-aging of accounts disbursed during the sampling period with starting Age > 0.
              Prevalence: ", round(diag.real7e,digits=1), "% of new accounts.\n"))
  
  cat( paste0("DETECTED: Difference in prevalence rates of supposedly 'new' accounts in total sample.
              New accounts (old): ", round(diag.real7f_b,digits=1), "%. New accounts (using [New_Ind]): ",
              round(diag.real7f_a,digits=1), "%.\n") )
  
  # - Fuse account-level indicator [New_Ind] back into main longitudinal dataset
  datCredit_real <- merge(datCredit_real, treat_samp2[,list(LoanID, New_Ind)], by=c("LoanID"), all.x=T)
  
  # [SANITY CHECK] Treatment success?
  check_advTreat4a <- datCredit_real[ExclusionID==0 & Counter==1 & New_Ind==1, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] * 100 == diag.real7f_a
  cat( check_advTreat4a %?% paste0('SAFE: Accounts that were disbursed during sampling period are successfully isolated as "new", \n',
                                   '\tdespite non-zero starting [Age]-values. This peculiarity is presumably ascribed to lagged disbursals, e.g., \n',
                                   '\tdue to lengthy bond registration processes.\n\tPrevalence of truly "new" accounts observed during sampling period: ', round(diag.real7g,digits=1), '%\n') %:% 
         'WARNING: Failed to isolate truly "new" accounts that were disbursed during sampling period.\n')
  
  # - Recalculate a new [Age] field, adjusted to start at 1 for survival modelling.
  datCredit_real[ExclusionID==0, Age_Adj := ifelse(New_Ind==0, Age, 1:.N),
                 by=list(LoanID)]
  
  # - Mark affected accounts with the current treatment ID
  LoanIDs <- subset(datCredit_real, ExclusionID==0 & Counter==1 & New_Ind==1 & 
                      Age != (Age_Adj), select="LoanID")
  datCredit_real[LoanID %in% LoanIDs$LoanID & ExclusionID==0, TreatmentID := paste0(TreatmentID, ";4")]
  
  # [SANITY CHECK] Are newly-disbursed accounts now correctly aged?
  diag.real7h <- datCredit_real[ExclusionID==0 & Counter==1 & Date_Origination >= minDate_observed & Age_Adj>1, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1 & Date_Origination >= minDate_observed, .N] * 100
  check_advTreat4b <-  diag.real7h < diag.real7e/10 # Should be true
  cat( check_advTreat4b %?% 
         paste0('SAFE: Newly disbursed accounts (during sampling period) are now ', 
                'correctly aged for survival modelling.\n\tTreatment reduced the ',
                'original problem by at least 1000% (', round(diag.real7e / diag.real7h, digits=0),  
                ' times reduction in prevalence).\n') %:% 
         'WARNING: Failed to age newly disbursed accounts (during sampling period) for survival modelling.\n') 
  
  # - Reorder new fields
  datCredit_real <- datCredit_real %>% relocate(Age_Adj, New_Ind, .after=Age)
  
}

# - Cleanup
rm(treat_samp1, treat_samp2)



# --- 5. Advanced Data Treatment 5: Treating trailing zero-valued balances | See Data Experiment 1

# - ZAR-valued threshold for [Balance]
currThresh <- 250
cat(paste0("Detecting so-called Trailing Zero-valued Balances (TZB) cases with ZAR <= ",
           currThresh, " ..\n"))

# - Sample data for testing purposes without contaminating the original dataset
test <- subset(datCredit_real, ExclusionID==0, 
               select=c("LoanID", "Date", "Counter", "Max_Counter", "Balance",
                        "HasWOff", "HasSettle", "HasRepaid", "ExclusionID", "TreatmentID"))

# - Create lead and last-observed balance fields, whilst imputing missing-elements
# with the last known nonmissing value, using the custom function "imputeLastKnown()"
# as defined in script 0
test[, Balance_lead1 := 
       imputeLastKnown(shift(Balance,type="lead",n=1)), by=list(LoanID)]
test[, Balance_last := Balance[.N], by=list(LoanID)]
test[, Balance_secondlast := Balance[.N-1], by=list(LoanID)]
test[, LastDate := Date[.N], by=list(LoanID)]

# - Indicate persisting zero balances across credit histories | absolute basis
test[, ZeroBal_Ind := 
       ifelse(Balance <= currThresh & Balance_lead1 <= currThresh & 
                Balance_secondlast <= currThresh &
                Balance_last <= currThresh, 1,0), by=list(LoanID)]

# - Lag the future values of given vector by n periods | n = 1
# This will help in finding definitive 'regimes' of when a non-zero balance switches to
# a zero-valued balance, as controlled by this iteration's thresholds for a 'zero-valued' balance.
test[, ZeroBal_Ind_lag := 
       shift(ZeroBal_Ind, type="lag", n=1), by=list(LoanID)]

# - Find the starting point of where [ZeroBal_Ind] became (and stayed) 1, 
# The logic "ZeroBal_Ind_lag==0 & ZeroBal_Ind==1" achieves this idea, particularly by
# taking the index of where this logic is first true. However, there can be multiple 
# such 'regimes'. Therefore, reverse the which(..), and take the index of the first TRUE-element,
# which should correspond to the last such regime. This regime should then be truly trailing 
# zero-valued balances.
test[, ZeroBal_Start := 
       ifelse(all(!is.na(ZeroBal_Ind)),
              min(coalesce( rev(which(ZeroBal_Ind_lag==0 & ZeroBal_Ind==1))[1] + 1 , 0),
                  Max_Counter), 0), by=list(LoanID)]

# - Indicate affected accounts for easier traversal
test[, HasTrailingZeroBalances := 
       ifelse(ZeroBal_Start > 0, TRUE, FALSE),
     by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of accounts with trailing zero-valued balances
# that suffered a terminal event later in loan life?
diag.real1a <- test[Counter==1 & HasTrailingZeroBalances==T & (HasWOff==1 | HasSettle==1), .N] / 
  test[Counter == 1, .N] * 100
diag.real1a_abs <- test[HasTrailingZeroBalances==T & (HasWOff==1 | HasSettle==1) & 
                          Counter>=ZeroBal_Start, .N]
diag.real1a_rec <-  diag.real1a_abs / test[, .N] * 100

# - Conditional treatment
if (diag.real1a > 0) {
  
  # - Calculate mean balance from the true ending point up to the available ending point
  test[HasTrailingZeroBalances==T & Counter>=ZeroBal_Start, 
       Balance_Mean := mean(Balance,na.rm=T), by=list(LoanID)]
  
  # - Calculate length of trailing zero-valued balances
  test[HasTrailingZeroBalances==T & Counter>=ZeroBal_Start, 
       ZeroBal_Length := Max_Counter - ZeroBal_Start + 1, by=list(LoanID)]
  
  # - Sample at the start of trailing zeros or end of the account
  test_samp <- subset(test, Counter==ZeroBal_Start & HasTrailingZeroBalances==T)
  
  # [DIAGNOSTIC] Prevalence of terminal events for these accounts with TZBs
  diag.real1_2a <- test_samp[HasWOff==1, .N] / test_samp[,.N] * 100 
  diag.real1_2b <- test_samp[HasSettle==1, .N] / test_samp[,.N] * 100 
  diag.real1_2 <- diag.real1_2a + diag.real1_2b
  
  # [DIAGNOSTIC] Right-censorship of those TZB-cases without a terminal event
  diag.real1_2c <- test_samp[HasWOff==0 & HasSettle==0 & HasRepaid==0 & LastDate == maxDate_observed, .N] / 
    test_samp[HasWOff==0 & HasSettle==0 & HasRepaid==0, .N] * 100 
  
  # [DIAGNOSTIC] Mean of [Balance_Mean], should be very low
  diag.real1_3a <- mean(test_samp$Balance_Mean, na.rm=T)
  diag.real1_3b <- median(test_samp$Balance_Mean, na.rm=T)
  
  # [DIAGNOSTIC] Mean of [ZeroBal_Length]
  diag.real1_4a <- mean(test_samp$ZeroBal_Length, na.rm=T)
  diag.real1_4b <- median(test_samp$ZeroBal_Length, na.rm=T)
  
  cat("DETECTED: TZB-cases occurred in ", round(diag.real1a,digits=2), 
      "% of accounts, of which ", round(diag.real1_2,digits=1), "% suffered a terminal event.",
      "\n\tOf those accounts that did not terminate, ", round(diag.real1_2c,digits=1), 
      "% are right-censored at the study-end [", format(maxDate_observed, "%d-%b-%Y"), "].", 
      "\n\tThe grand mean balance across TZB-histories is ZAR", round(diag.real1_3a,digits=0), "(median:",
      round(diag.real1_3b,digits=0), ") and the mean length of TZB-histories ",
      "\n\tis", round(diag.real1_4a,digits=0),"months (median:", round(diag.real1_4b,digits=0), 
      "). These excessive records will be removed during treatment, after having timed ",
      "\n\tterminal events correctly to the point preceding the isolated TZB-histories.",
      "\n\tTreating ...\n")
  
  # - Cleanup in optimising memory use
  rm(test_samp)
  
  # - Fuse account-level indicator back into main longitudinal dataset
  datCredit_real <- merge(datCredit_real, test[,list(LoanID, Counter, 
                                                     HasTrailingZeroBalances, ZeroBal_Start)], 
                          by=c("LoanID", "Counter"), all.x=T)
  
  # [TREATMENT] Move terminal event incidence earlier | Write-off
  datCredit_real[ExclusionID==0 & HasTrailingZeroBalances==T & HasWOff==1, 
                 WOff_Ind := ifelse(Counter < (ZeroBal_Start-1), WOff_Ind, WOff_Ind[.N]), by=list(LoanID)]
  datCredit_real[ExclusionID==0 & HasTrailingZeroBalances==T & HasWOff==1, 
                 WriteOff_Amt := ifelse(Counter < (ZeroBal_Start-1), WriteOff_Amt, WriteOff_Amt[.N]), 
                 by=list(LoanID)]
  
  # [TREATMENT] Move terminal event incidence earlier | Early Settlement / Closure
  datCredit_real[ExclusionID==0 & HasTrailingZeroBalances==T & HasSettle==1, 
                 EarlySettle_Ind := ifelse(Counter < (ZeroBal_Start-1), EarlySettle_Ind, EarlySettle_Ind[.N]),
                 by=list(LoanID)]
  datCredit_real[ExclusionID==0 & HasTrailingZeroBalances==T & HasSettle==1, 
                 EarlySettle_Amt := ifelse(Counter < (ZeroBal_Start-1), EarlySettle_Amt, EarlySettle_Amt[.N]), 
                 by=list(LoanID)]
  
  # [TREATMENT] Re-calculate affected aggregates
  datCredit_real[ExclusionID==0 & HasTrailingZeroBalances==T & (HasWOff==1 | HasSettle==1) & 
                   Counter < ZeroBal_Start, Max_Counter := .N, by=list(LoanID)]
  
  # - Mark affected accounts with current treatment ID
  datCredit_real[ExclusionID==0 & HasTrailingZeroBalances==T & (HasWOff==1 | HasSettle==1), 
                 TreatmentID := paste0(TreatmentID, ";5")]
  
  # [TREATMENT] Mark the affected histories as an Exclusion
  datCredit_real[ExclusionID==0 & HasTrailingZeroBalances==T & (HasWOff==1 | HasSettle==1) & 
                   Counter>=ZeroBal_Start, ExclusionID := 6]
  
  # - Create and add exclusion impact to a common table
  datExcl <- data.table("Excl_ID"=6, "Reason"=paste0("Trailing zero-valued balances [", currThresh, "]"),
                        "Impact_Account" = diag.real1a, "Impact_Dataset" = diag.real1a_rec, 
                        "Impact_records" = diag.real1a_abs) 
  if (exists('datExclusions')) datExclusions <- rbind(datExclusions, datExcl) else datExclusions <- datExcl
  
  check_advTreat5 <- datCredit_real[ExclusionID==0 & HasTrailingZeroBalances==T & 
                                      (HasWOff==1 | HasSettle==1) & Counter==Max_Counter, .N] / 
    datCredit_real[ExclusionID==0 & Counter == 1, .N] * 100 == diag.real1a
  cat( check_advTreat5 %?% paste0('SAFE: Accounts with trailing zero-valued balances (<= ZAR', currThresh, 
                                  ') and a terminal event were successfully treated.\n') %:% 
         paste0('SAFE: Failed to treat accounts with trailing zero-valued balances (<= ZAR', currThresh, 
                ') and a terminal event.\n'))
}

# - Cleanup
rm(test)



# --- 6. Advanced Data Treatment 6: Treating varying [Term]-values | See Data Experiment 5

# - Preliminaries
datCredit_real[ExclusionID==0, Var_Term := var(Term, na.rm=T), by=list(LoanID)]

# [DIAGNOSTIC] How often is the variance non-zero for [Term] where the account 
#   has neither redraw nor further loan across its history?
diag.real5_2a <- datCredit_real[ExclusionID==0 & Counter==1 & Var_Term > 0 & HasRedraw == 0 & HasFurtherLoan == 0, .N] / 
  datCredit_real[ExclusionID==0 & Counter==1, .N] * 100  # account-level
diag.real5_2a_rec <- datCredit_real[ExclusionID==0 & Var_Term > 0 & HasRedraw == 0 & HasFurtherLoan == 0, .N] /
  datCredit_real[ExclusionID==0, .N] * 100  # dataset-level

# - Conditional Treatment | Varying Term
if (diag.real5_2a > 0) {
  
  cat(paste0("DETECTED: Varying [Term]-values unaccounted for by Redraws or Further Loans found in ",
             round(diag.real5_2a,digits=4), "% of accounts. \n\tTreating by assigning the account-level mode ..\n"))
  
  # - Calculate account-level mode of [Term]
  datCredit_real[ExclusionID==0 & Var_Term>0 & HasRedraw==0 & HasFurtherLoan==0,
                 Term_Mode := getmode(Term), by=list(LoanID)]
  
  # [TREATMENT] Assign the account-level mode to the non-equal parts
  datCredit_real[ExclusionID==0 & Term!=Term_Mode & HasRedraw==0 & HasFurtherLoan==0,
                 Term := Term_Mode[1], by=list(LoanID)]
  
  # - Mark affected accounts with current treatment ID
  datCredit_real[ExclusionID==0 & Term!=Term_Mode & HasRedraw==0 & HasFurtherLoan==0, 
                 TreatmentID := paste0(TreatmentID, ";6")]
  
  # - Recalculate affected aggregates
  datCredit_real[ExclusionID==0, Var_Term := var(Term, na.rm=T), by=list(LoanID)]
  
  # [SANITY CHECK] Account-level prevalence of accounts that varying [Term]-values?
  check_advTreat6 <- datCredit_real[ExclusionID==0 & Counter==1 & Var_Term > 0 & HasRedraw == 0 & 
                                      HasFurtherLoan == 0, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] * 100 == 0 # Should be true
  cat( check_advTreat6 %?% 
         'SAFE: Cases with varying [Term]-values successfully treated with the account-level mode.\n' %:% 
         'WARNING: Failed to treat cases with varying [Term]-values.\n') 
}

# - Cleanup
suppressWarnings( datCredit_real[, `:=`(Var_Term = NULL, Term_Mode = NULL)] )



# --- 7. Advanced Data Treatment 7: Inferring net cash inflow (Receipts) | See Data Experiment 9

cat("Inferring [Receipt_Inf] field from first principles, using balance-differences\n\t",
    "between two consecutive points in time ..\n")

# - Create lagged fields | Intermediary fields
# Balance | Note: Assign first element to Principal if a new loan
datCredit_real[ExclusionID==0, Balance_prev := shift(Balance), by=list(LoanID)]
datCredit_real[ExclusionID==0 & Counter==1, Balance_prev := ifelse(New_Ind, Principal[1], Balance[1]), by=list(LoanID)]
# Interest Rate
datCredit_real[ExclusionID==0, IntRate_prev := shift(InterestRate_Nom), by=list(LoanID)]
datCredit_real[ExclusionID==0 & Counter==1, IntRate_prev := InterestRate_Nom[1], by=list(LoanID)]
# Arrears
datCredit_real[ExclusionID==0, Arrears_prev := shift(Arrears), by=list(LoanID)]
datCredit_real[ExclusionID==0 & Counter==1, Arrears_prev := 0, by=list(LoanID)]

# - Calculate net cash flow (Balance difference) between two successive points in time
datCredit_real[ExclusionID==0, CashFlow := (1+IntRate_prev/12)*Balance_prev - Balance, by=list(LoanID)]

# - Arrears difference between two successive points in time
datCredit_real[ExclusionID==0, Arrears_Diff := (1+IntRate_prev/12)*Arrears_prev - Arrears, by=list(LoanID)]

# - Infer the Receipt R(t) given the following logic with balance B(t), interest i(t):
# 1) For terminal events (settlement, write-off) with event amount E at last time t': 
#     R(t') = (1+i(t-1)/12)*B(t-1) - B(t) + E_s, where R(t) >= 0 and E_s is the settlement amount
#     R(t') = (1+i(t-1)/12)*B(t-1) - B(t) - E_w, where R(t) >= 0 and E_w is the write-off amount
#     This should also redistribute any non-zero balance B(t) (which should rightfully be zero; treated in 
#     Advanced Data Treatment 8) to the receipt amount itself.
# 2) For behavioural events (redraw, further loan) with instalment I(t):
#     R(t) = I(t), where R(t) >= 0. 
#     In order for a redraw/further loan to occur, we must reasonably assume the account to be in good standing,
#     otherwise, why 'disburse' even more funds? As such, we can assume that at least one instalment to be paid.
# 3) For all other events, with calculated cash flow C(t) and Arrears balance A(t):
#     R(t) = C(t) + A(t), where R(t) >= 0
#     Any accrued arrears are offset against the presumably positive cash flow,
#     itself signifying a reducing balance, thereby a payment of sorts. Otherwise, if negative, then no payment.
datCredit_real[ExclusionID==0, Receipt_Inf := round(case_when(
  EarlySettle_Ind==1 ~ pmax((1+IntRate_prev/12)*Balance_prev - Balance + EarlySettle_Amt, 0),
  WOff_Ind==1 ~ pmax((1+IntRate_prev/12)*Balance_prev - Balance - WriteOff_Amt, 0),
  (Redraw_Ind == 1 | FurtherLoan_Ind == 1) ~ pmax(Instalment, 0),
  TRUE ~ pmax(CashFlow + Arrears_Diff, 0)
), digits=2), by=list(LoanID)]
datCredit_real <- datCredit_real %>% relocate(Receipt_Inf, .after=Instalment)

cat("[Receipt_Inf] inferred.\n")

# - Cleanup
suppressWarnings( datCredit_real[, `:=`(Balance_prev = NULL, IntRate_prev = NULL, Arrears_prev = NULL,
                                        CashFlow = NULL, Arrears_Diff = NULL, Receipt_Diff = NULL)] )



# --- 8. Advanced Data Treatment 8: Conforming event amounts to zero-valued balance at termination 
# See Data Experiment 2

# - Isolate affected cases
treat_samp1 <- subset(datCredit_real, ExclusionID==0 & Counter==Max_Counter & 
                        (HasSettle == 1 | HasWOff == 1 | HasRepaid == 1))[,
                                                                          list(LoanID, Counter, Max_Counter, ExclusionID, TreatmentID, 
                                                                               Receipt_Inf, Balance, HasSettle, EarlySettle_Amt,
                                                                               HasWOff, WriteOff_Amt, HasRepaid)]

# - For early settlements & repaids, redistribute any remaining balance to [Receipt_Inf]
treat_samp1[HasSettle==1 | HasRepaid==1, Receipt_Adj := Receipt_Inf + Balance]

# - For write-offs, redistribute any remaining balance to the Write-off Amount
treat_samp1[HasWOff==1, WriteOff_Amt_Adj := WriteOff_Amt + Balance]

# [DIAGNOSTIC] Prevalence of mismatched early settlement amounts observed
treat_samp1[HasSettle==1 & Counter==Max_Counter, Settle_Diff := EarlySettle_Amt - Receipt_Adj]
diag.real2_3a <- treat_samp1[ExclusionID==0 & HasSettle==1 & Counter==Max_Counter & EarlySettle_Amt != Receipt_Adj, .N] / 
  treat_samp1[ExclusionID==0 & HasSettle==1 & Counter==Max_Counter, .N] * 100 
diag.real2_3b <- mean(treat_samp1[HasSettle==1, Settle_Diff], na.rm=T)
diag.real2_3c <- median(treat_samp1[HasSettle==1, Settle_Diff], na.rm=T)

# [DIAGNOSTIC] Prevalence of mismatched write-off amounts observed
treat_samp1[HasWOff==1 & Counter==Max_Counter, WOff_Diff := WriteOff_Amt - WriteOff_Amt_Adj]
diag.real2_3d <- treat_samp1[ExclusionID==0 & HasWOff==1 & Counter==Max_Counter & WriteOff_Amt != WriteOff_Amt_Adj, .N] / 
  treat_samp1[ExclusionID==0 & HasWOff==1 & Counter==Max_Counter, .N] * 100 
diag.real2_3e <- mean(treat_samp1[HasWOff==1, WOff_Diff], na.rm=T)
diag.real2_3f <- median(treat_samp1[HasWOff==1, WOff_Diff], na.rm=T)

# [DIAGNOSTIC] Prevalence of mismatched repaid amounts observed
treat_samp1[HasRepaid==1 & Counter==Max_Counter, Repaid_Diff := Receipt_Inf - Receipt_Adj]
diag.real2_3g <- treat_samp1[ExclusionID==0 & HasRepaid==1 & Counter==Max_Counter & Receipt_Inf != Receipt_Adj, .N] / 
  treat_samp1[ExclusionID==0 & HasRepaid==1 & Counter==Max_Counter, .N] * 100
diag.real2_3h <- mean(treat_samp1[HasRepaid==1, Repaid_Diff], na.rm=T)
diag.real2_3i <- median(treat_samp1[HasRepaid==1, Repaid_Diff], na.rm=T)

# - Conditional Treatment
if (diag.real2_3a > 0 | diag.real2_3d > 0 | diag.real2_3g > 0) {
  
  cat(paste0("DETECTED: Of early settlements / closures, ", round(diag.real2_3a,digits=1), 
             "% of accounts had event amounts that did not agree with calculated 
             event amounts such that [Balance]=0. Mean difference of ", comma(round(diag.real2_3b,digits=2)), 
             " (median: ", comma(round(diag.real2_3c,digits=2)), "). \n\tCorrecting by redistributing non-zero ",
             "balance to [Receipt_Inf] ..\n"))
  
  cat(paste0("DETECTED: Of repaid accounts, ", round(diag.real2_3g,digits=1), 
             "% of accounts did not have [Balance]=0 at loan-end. Mean difference of ", comma(round(diag.real2_3h,digits=2)), 
             " (median: ", comma(round(diag.real2_3i,digits=2)), "). \n\tCorrecting by redistributing non-zero ",
             " balance to [Receipt_Inf] ..\n"))
  
  cat(paste0("DETECTED: Of write-offs, ", round(diag.real2_3d,digits=1), 
             "% of accounts had event amounts E_w that did not agree with calculated 
             event amounts such that 
                  B(t_w)' = B(t_w-1)*(1+i(t-1)/12) - R(t_w) - E_w  :=  0 where
             B(t) is the observed balance at time t, B(t)' is the calculated balance at time t, 
             t_w is the write-off point, i(t) is the nominal interest rate at time t, 
             R(t) is the inferred receipt at time t, E_w is the write-off event amount.
             Note: R(t) = B(t_w-1)*(1+i(t-1)/12)- B(t) - E_w by definition.
             
             Proposed correction: adjusting the write-off amount by E_w' = ( E_w + B(t_w) ), followed
                by setting B(t_w) = 0.
             
             Mean difference between E_w and E_w' of ZAR ", 
             comma(round(diag.real2_3e,digits=2)), " (median: ", comma(round(diag.real2_3f,digits=2)), 
             "). \n\tCorrecting ..\n"))
  
  # - Fuse treated accounts' event amounts with main dataset
  # To facilitate treatment success measurement later
  datCredit_real <- merge(datCredit_real, treat_samp1[,list(LoanID, WriteOff_Amt_Adj, Receipt_Adj)], 
                          by=c("LoanID"), all.x=T)
  
  # [TREATMENT] For early settlements, redistribute the 
  # remaining balance to [Receipt_Inf] and correct Early Settlement Amount
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasSettle==1 & 
                   EarlySettle_Amt != Receipt_Adj, Receipt_Inf := Receipt_Inf + Balance]
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasSettle==1 & 
                   EarlySettle_Amt != Receipt_Adj, EarlySettle_Amt := Receipt_Inf] 
  
  # [TREATMENT] For repaids, redistribute the remaining balance to [Receipt].
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasRepaid==1 & Receipt_Inf != Receipt_Adj, 
                 Receipt_Inf := Receipt_Inf + Balance]
  
  # [TREATMENT] For write-offs, redistribute the remaining
  # balance to the Write-off Amount
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasWOff==1 & WriteOff_Amt != WriteOff_Amt_Adj, 
                 WriteOff_Amt := WriteOff_Amt + Balance]
  
  # [TREATMENT] Set the balance explicitly to zero in either terminal case
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & (HasSettle==1 | HasWOff==1 | HasRepaid==1),
                 Balance := 0]
  
  # - Mark the affected accounts with current treatment ID
  LoanIDs <- unique(treat_samp1[ExclusionID==0 & (WriteOff_Amt != WriteOff_Amt_Adj | 
                                                    EarlySettle_Amt != Receipt_Adj | 
                                                    Receipt_Inf != Receipt_Adj), LoanID])
  datCredit_real[LoanID %in% LoanIDs & Counter==Max_Counter, TreatmentID := paste0(TreatmentID, ";8")]
  
  # [SANITY CHECK] Are terminal cases behaving as expected?
  check_advTreat8a <- datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasSettle==1 &
                                       Receipt_Inf != Receipt_Adj, .N] / 
    datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasSettle==1, .N] *100 == 0 # Should be true
  check_advTreat8b <- datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasWOff==1 &
                                       WriteOff_Amt != WriteOff_Amt_Adj, .N] / 
    datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasWOff==1, .N] *100 == 0 # Should be true
  check_advTreat8c <- datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasRepaid==1 &
                                       Receipt_Inf != Receipt_Adj, .N] / 
    datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasRepaid==1, .N] *100 == 0
  cat( (check_advTreat8a & check_advTreat8b & check_advTreat8c)  %?% 
         'SAFE: Terminal event amounts successfully corrected where necessary.\n' %:% 
         'WARNING: Failed to correct all broken terminal event amounts.\n')
  
  # - Cleanup
  suppressWarnings( datCredit_real[, `:=`(Receipt_Adj = NULL, WriteOff_Amt_Adj = NULL)])
}

# - Cleanup
rm(treat_samp1)



# --- 9. Advanced Data Treatment 9: Treating zero-valued instalments at end-of-loan-life | See Data Experiment 8

# - Preliminaries
datCredit_real[ExclusionID == 0, HasZeroIns_Last := ifelse(Instalment[.N] == 0, T, F), by=list(LoanID)]
datCredit_real[ExclusionID == 0, HasZeroIns_SecondLast := ifelse(Instalment[.N-1] == 0, T, F), by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of zero-valued instalments at the last record
diag.real8_3a <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_Last == T & HasZeroIns_SecondLast == F &
                                  (HasWOff==1 | HasSettle==1 | HasRepaid==1), .N] / 
  datCredit_real[ExclusionID==0 & Counter==1, .N] * 100 
diag.real8_3a_abs <- datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasZeroIns_Last == T & 
                                      HasZeroIns_SecondLast == F & (HasWOff==1 | HasSettle==1 | HasRepaid==1), .N]
diag.real8_3a_rec <- diag.real8_3a_abs / datCredit_real[ExclusionID==0 & Counter==Max_Counter, .N] * 100

# - Conditional treatment
if (diag.real8_3a > 0) {
  
  cat("DETECTED: Zero-valued instalments at end-of-loan-life with terminal events.\n\tPrevalence: ",
      round(diag.real8_3a,digits=2), "% of accounts (", round(diag.real8_3a_rec,digits=2), 
      "% of records as well).\n")
  
  # [TREATMENT] Copy previous non-zero instalment to last record
  datCredit_real[ExclusionID==0 & HasZeroIns_Last == T & HasZeroIns_SecondLast == F & 
                   (HasWOff==1 | HasSettle==1 | HasRepaid==1), 
                 Instalment := ifelse(Counter<Max_Counter, Instalment, Instalment[.N-1]),
                 by=list(LoanID)]
  
  # - Mark affected accounts with current treatment ID
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasZeroIns_Last == T & HasZeroIns_SecondLast == F 
                 & (HasWOff==1 | HasSettle==1 | HasRepaid==1), TreatmentID := paste0(TreatmentID, ";9")]
  
  # - Recalculate affected aggregates
  datCredit_real[ExclusionID == 0, HasZeroIns_Last := ifelse(Instalment[.N] == 0, T, F), by=list(LoanID)]
  
  # [SANITY CHECK] Are terminal cases behaving as expected?
  check_advTreat9 <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_Last == T & 
                                      HasZeroIns_SecondLast == F & (HasWOff==1 | HasSettle==1 | HasRepaid==1), .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] *100 == 0 # Should be true
  cat( check_advTreat9  %?% 
         'SAFE: Terminated accounts with zero-valued instalments at the end were successfully treated.\n' %:% 
         'WARNING: Failed to treat terminated accounts with zero-valued instalments at their end.\n')
}

# - Cleanup
suppressWarnings( datCredit_real[, `:=`(HasZeroIns_Last = NULL, HasZeroIns_SecondLast = NULL )])



# --- 10. Advanced Data Treatment 10: Treating zero-valued instalment-regimes at start | See Data Experiment 8

# - Preliminaries
datCredit_real[ExclusionID==0, Instalment_NonZero_Pos := which(Instalment>0)[1], by=list(LoanID)]
datCredit_real[ExclusionID==0, Arrears_Diff := c(0,diff(Arrears)), by=list(LoanID)]
datCredit_real[ExclusionID==0 & Instalment_NonZero_Pos > 1, ZeroIns_Length2 := Instalment_NonZero_Pos-1,
               by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of accounts with starting zero-valued instalment-regimes
diag.real8_4a <- datCredit_real[ExclusionID==0 & Counter == 1 & Instalment_NonZero_Pos > 1, .N] / 
  datCredit_real[ExclusionID==0 & Counter==1, .N] * 100
diag.real8_4a_abs <- datCredit_real[ExclusionID==0 & Counter < Instalment_NonZero_Pos & Instalment_NonZero_Pos > 1, .N]
diag.real8_4a_rec <- diag.real8_4a_abs / datCredit_real[ExclusionID==0,.N] * 100
diag.real8_4d <- mean(datCredit_real[ExclusionID==0 & Counter == 1 & Instalment_NonZero_Pos > 1, ZeroIns_Length2])
diag.real8_4e <- median(datCredit_real[ExclusionID==0 & Counter == 1 & Instalment_NonZero_Pos > 1, ZeroIns_Length2])

# - Conditional treatment
if (diag.real8_4a > 0) {
  
  cat("DETECTED: Zero-valued instalment-regimes found at the start of credit histories.\n\tPrevalence:",
      round(diag.real8_4a,digits=2), "% of accounts (", round(diag.real8_4a_rec,digits=3), 
      "% of records).\n\tMean regime length of",
      round(diag.real8_4d, digits=1), "months (median: ", round(diag.real8_4e,digits=1), 
      ").\n\tTreating by [Instalment] = [Receipt_Inf] where [Receipt_Inf] > 0,",
      "\n\totherwise assign the first non-zero [Instalment].\n")
  
  # [TREATMENT] Assign [Receipt_Inf] selectively, otherwise assign first non-zero [Instalment]
  # Note: The filter "Counter <= Instalment_NonZero_Pos, Instalment" is necessary to 
  # lookup the first non-zero [Instalment], which "Counter < Instalment_NonZero_Pos, Instalment" will
  # not achieve. Effective contamination is zero since the value at Counter == Instalment_NonZero_Pos
  # is 'overwritten' by itself.
  datCredit_real[ExclusionID==0 & Instalment_NonZero_Pos > 1 & Counter <= Instalment_NonZero_Pos, Instalment := 
                   (Arrears_Diff>=0 & Receipt_Inf>0)*Receipt_Inf + 
                   (Arrears_Diff>=0 & Receipt_Inf==0)*Instalment[Instalment_NonZero_Pos[1]]
                 ,by=list(LoanID)]
  
  # - Mark affected accounts with current treatment ID
  datCredit_real[ExclusionID==0 & Instalment_NonZero_Pos > 1 & Counter < Instalment_NonZero_Pos, 
                 TreatmentID := paste0(TreatmentID, ";10")]
  
  # - Recalculate affected aggregates
  datCredit_real[ExclusionID==0, Instalment_NonZero_Pos := which(Instalment>0)[1], by=list(LoanID)]
  
  # [SANITY CHECK] Did the treatment succeed?
  check_advTreat10 <- datCredit_real[ExclusionID==0 & Counter==1 & Instalment_NonZero_Pos > 1, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] *100 == 0 # Should be true
  cat( check_advTreat10  %?% 
         'SAFE: Accounts with zero-valued instalment-regimes at the start were successfully treated.\n' %:% 
         'WARNING: Failed to treat accounts with zero-valued instalment-regim
       es at the start.\n')
}

# - Cleanup
suppressWarnings( datCredit_real[,`:=`(Instalment_NonZero_Pos = NULL, Arrears_Diff = NULL, ZeroIns_Length2 = NULL)] )




# ------ 7. Re-applying Exclusions
# Previous treatments may have availed additional records for exclusion using
# the same logic, albeit applied as a single exclusion this time

# --- 1. Old single-record cases | See Data Experiment 4

# [DIAGNOSTIC] Account-level and dataset-wide impacts of exclusion
diag.real4b_2 <-  datCredit_real[ExclusionID == 0 & Counter == 1 & Max_Counter == 1 & 
                                   (Date_Origination < maxDate | is.na(Date_Origination) ), .N] / 
  datCredit_real[ExclusionID == 0 & Counter == 1, .N] * 100 
diag.real4b_2_abs <- datCredit_real[ExclusionID == 0 & Max_Counter == 1 & 
                                      (Date_Origination < maxDate | is.na(Date_Origination)) , .N]
diag.real4b_2_rec <-  diag.real4b_2_abs / datCredit_real[ExclusionID == 0, .N] * 100 

# - Conditional exclusion
if (diag.real4b_2 > 0) {
  
  cat("RE-EXCLUSION: Newly-revealed old single-record cases. Prevalence: ", round(diag.real4b_2,digits=3), 
      "% of accounts (", round(diag.real4b_2_rec,digits=5), "% of records).\n")
  
  # - Mark affected records for exclusion later with an ID-value
  LoanIDs <- unique(subset(datCredit_real, ExclusionID == 0 & Counter == 1 & Max_Counter == 1 & 
                             (Date_Origination < maxDate | is.na(Date_Origination)), select="LoanID"))
  datCredit_real[LoanID %in% LoanIDs$LoanID & ExclusionID == 0, ExclusionID := 7]
  
  # [SANITY CHECK] Treatment success?
  check_excl1_2 <- datCredit_real[ExclusionID == 0 & Counter == 1 & Max_Counter == 1 & 
                                    (Date_Origination < maxDate | is.na(Date_Origination) ), .N]  / 
    datCredit_real[ExclusionID==0 & Counter == 1, .N] * 100 == 0
  cat( check_excl1_2 %?% 'SAFE: Exclusion 1 successfully re-applied.\n' %:% 
         'WARNING: Re-applying Exclusion 1 failed.\n')
}


# --- 2. Zero-balance credit histories | See Data Experiment 6

# - Preliminaries
datCredit_real[ExclusionID==0, HasZeroBalances_all := all(ifelse(Balance <= 0, TRUE, FALSE)), by=list(LoanID)]

# [DIAGNOSTIC] What proportion of account's credit histories have zero balances throughout?
diag.real6a_2 <- datCredit_real[ExclusionID == 0 & Counter == 1 & HasZeroBalances_all == TRUE, .N] /
  datCredit_real[ExclusionID == 0 & Counter == 1, .N] * 100
diag.real6a_2_abs <- datCredit_real[ExclusionID == 0 & HasZeroBalances_all == TRUE, .N]
diag.real6a_2_rec <- diag.real6a_2_abs / datCredit_real[ExclusionID == 0, .N] * 100

# - Conditional exclusion
if (diag.real6a_2 > 0) {
  
  cat("RE-EXCLUSION: Newly-revealed zero-balance credit histories. Prevalence: ", round(diag.real6a_2,digits=3),
      "% of accounts (", round(diag.real6a_2_rec,digits=4), "% of records).\n")
  
  # - Mark affected records for exclusion later with an ID-value
  LoanIDs <- unique(subset(datCredit_real, ExclusionID == 0 & 
                             Counter == 1 & HasZeroBalances_all == TRUE, select="LoanID"))
  datCredit_real[LoanID %in% LoanIDs$LoanID & ExclusionID == 0, ExclusionID := 7]
  
  # - Recalculate affected aggregates
  datCredit_real[ExclusionID==0, HasZeroBalances_all := all(ifelse(Balance <= 0, TRUE, FALSE)), by=list(LoanID)]
  
  # [SANITY CHECK] Treatment success?
  check_excl2_2 <-  datCredit_real[ExclusionID == 0 & Counter == 1 & HasZeroBalances_all == TRUE, .N] / 
    datCredit_real[ExclusionID==0 & Counter == 1, .N] * 100 == 0
  cat( check_excl2_2 %?% 'SAFE: Exclusion 2 successfully re-applied.\n' %:% 
         'WARNING: Re-applying Exclusion 2 failed.\n')
}


# --- 3. Zero-Principal credit histories | See Data Experiment 5

# - Preliminaries
datCredit_real[ExclusionID==0, HasZeroPrincipal_all := all(Principal==0), by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of zero-valued Principals throughout the entire account history?
diag.real5_1d_2 <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroPrincipal_all==T, .N] /
  datCredit_real[ExclusionID==0 & Counter==1, .N] * 100 
diag.real5_1d_2_abs <- datCredit_real[ExclusionID==0 & HasZeroPrincipal_all==T, .N]
diag.real5_1d_2_rec <- diag.real5_1d_2_abs / datCredit_real[ExclusionID==0, .N] * 100 

# - Conditional treatment
if (diag.real5_1d_2 > 0) {
  
  cat("RE-EXCLUSION: Newly-revealed zero-principal credit histories. Prevalence: ", 
      round(diag.real5_1d_2,digits=3), "% of accounts (",
      round(diag.real5_1d_2_rec,digits=4), "% of records).\n")
  
  # - Mark affected records for exclusion later with an ID-value
  LoanIDs <- unique(subset(datCredit_real, ExclusionID==0 & Counter==1 & 
                             HasZeroPrincipal_all==T, select="LoanID"))
  datCredit_real[LoanID %in% LoanIDs$LoanID & ExclusionID == 0, ExclusionID := 7]
  
  # - Recalculate affected aggregates
  datCredit_real[ExclusionID==0, HasZeroPrincipal_all := all(Principal==0), by=list(LoanID)]
  
  # [SANITY CHECK] Treatment success?
  check_excl3_2 <-  datCredit_real[ExclusionID==0 & Counter==1 & HasZeroPrincipal_all==T, .N] / 
    datCredit_real[ExclusionID==0 & Counter == 1, .N] * 100 == 0
  cat( check_excl3_2 %?% 'SAFE: Exclusion 3 successfully re-applied.\n' %:% 
         'WARNING: Re-applying Exclusion 3 failed.\n')
}


# --- 4. Zero-Instalment credit histories | See Data Experiment 8

# - Preliminaries
datCredit_real[ExclusionID==0, HasZeroIns_All := ifelse(all(Instalment==0),T,F),by=list(LoanID)]
datCredit_real[ExclusionID==0, HasZeroArrears_All := ifelse(all(Arrears==0),T,F),by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of account-level zero-valued instalments
diag.real8_1a_2 <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_All==T, .N] / 
  datCredit_real[ExclusionID==0 & Counter==1, .N] * 100
diag.real8_1a_2_abs <- datCredit_real[ExclusionID==0 & HasZeroIns_All==T, .N]
diag.real8_1a_2_rec <- diag.real8_1a_2_abs / datCredit_real[ExclusionID==0, .N] * 100

# [DIAGNOSTIC] Prevalence of zero-valued arrears amongst zero-valued instalments?
diag.real8_1c_2 <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_All==T & HasZeroArrears_All==T, .N] /
  datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_All==T, .N] * 100

# - Conditional treatment
if (diag.real8_1a_2 > 0 & diag.real8_1c_2 >= 0.9) {
  
  cat("RE-EXCLUSION: Newly-revealed zero-instalment credit histories. Prevalence: ", 
      round(diag.real8_1a_2,digits=3), "% of accounts (",
      round(diag.real8_1a_2_rec,digits=4), "% of records), \n\tof which ", round(diag.real8_1c_2,digits=3),
      "% also have zero-valued arrears throughout.\n")
  
  # - Mark affected records for exclusion later with an ID-value
  LoanIDs <- unique(subset(datCredit_real, ExclusionID==0 & Counter==1 & 
                             HasZeroIns_All==T & HasZeroArrears_All==T, select="LoanID"))
  datCredit_real[LoanID %in% LoanIDs$LoanID & ExclusionID == 0, ExclusionID := 7]
  
  # - Recalculate affected aggregates
  datCredit_real[ExclusionID==0, HasZeroIns_All := ifelse(all(Instalment==0),T,F),by=list(LoanID)]
  
  # [SANITY CHECK] Treatment success?
  check_excl4_2 <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_All==T, .N] / 
    datCredit_real[ExclusionID==0 & Counter == 1, .N] * 100 == 0
  cat( check_excl4_2 %?% 'SAFE: Exclusion 4 successfully re-applied.\n' %:% 
         'WARNING: Re-applying Exclusion 4 failed.\n')
}


# --- Create and add one big exclusion impact to a common table
datExcl <- data.table("Excl_ID"=7, "Reason"="Rerunning previous exclusions, given recent data treatments",
                      "Impact_Account" = diag.real4b_2 + diag.real6a_2 + diag.real5_1d_2 + diag.real8_1a_2,
                      "Impact_Dataset" = diag.real4b_2_rec + diag.real6a_2_rec + diag.real5_1d_2_rec + diag.real8_1a_2_rec,
                      "Impact_records" = diag.real4b_2_abs + diag.real6a_2_abs + diag.real5_1d_2_abs + diag.real8_1a_2_abs)
if (exists('datExclusions')) datExclusions <- rbind(datExclusions, datExcl) else datExclusions <- datExcl

# --- Clean up | Intermediary treatment-related fields no longer necessary
suppressWarnings( datCredit_real[,`:=`(HasZeroPrincipal_all = NULL, HasZeroBalances_all = NULL,
                                       HasZeroIns_All = NULL, HasZeroArrears_All = NULL)] )
gc()

# - Store experimental objects | Memory optimisation
pack.ffdf(paste0(genObjPath,"Exclusions"), datExclusions);





# ------ 8. General Cleanup | Remove fields that will not likely be used
suppressWarnings( datCredit_real[, `:=`(WOFF_DATE = NULL, RMNG_TERM = NULL, BOND_AMT = NULL, 
                                        FACILITY_SIZE = NULL, DATEX = NULL)] )
### NOTES: See Data Experiment 2 on WOFF_DATE's removal

# [SANITY CHECK] Confirm dataset's grain
check_cred3a <- datCredit_real[,list(Freqs = .N), by=list(LoanID, Date)][Freqs > 1,.N]
cat( (check_cred3a == 0) %?% cat('SAFE: Grain of {datCredit_real} confirmed.\n') %:% 
       cat(paste0('ERROR: Grain broken in {datCredit_real} for ', check_cred3b, " cases.\n")) )

# - Save to disk (zip) for quick disk-based retrieval later
rm(dat.raw, LoanIDs, datExcl); gc() # memory optimization; no need for raw data anymore at this point
pack.ffdf(paste0(genPath,"creditdata_final2"), datCredit_real)

proc.time() - ptm # IGNORE: elapsed runtime
