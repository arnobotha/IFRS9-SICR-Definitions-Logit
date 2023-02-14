# ==================================== DATA ENRICHMENT ==================================
# Engineer more advanced fields related to performance and default spells and fuse with
# previously prepared credit data towards further enrichment
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Dynamic SICR-research
# SCRIPT AUTHOR(S): Dr Arno Botha
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2b.Data_Preparation_Credit.R
#
# -- Inputs:
#   - datCredit_real | Prepared credit data from script 2b
#   - various parameters set in the setup script 0
#
# -- Outputs:
#   - datCredit_real | Enriched credit dataset
#   - matState_real | Delinquency state matrix (g1-measure), columns = loans, rows = time (t>=0)
#   - matState_g0 | Delinquency state matrix (g0-measure), columns = loans, rows = time (t>=0)
# ---------------------------------------------------------------------------------------
# NOTE: This script predominantly comes from another project (Kasmeer), but with following changes:
#   - removed creation of [Partition]-field
# =======================================================================================





# ------- 1. Platform-aligned Feature Engineering & Enrichment | Basic-level
ptm <- proc.time() # for runtime calculations (ignore)

# - Confirm prepared credit data is loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final2"), tempPath)

# --- 1.1. Abstract terminal event types & times at the account-level

# - Ensure variables are not present in dataset before creation (useful during debugging)
suppressWarnings( datCredit_real[, `:=`(Event_Type = NULL, Event_Time = NULL, Account_Censored = NULL)])

# - Establish event outcome based on terminal event vectors or censoring information
datCredit_real[ExclusionID==0, Event_Type := case_when(
  WOff_Ind[Max_Counter[1]] == 1 ~ "WOFF",
  EarlySettle_Ind[Max_Counter[1]] == 1 ~ "SETTLE",
  Date[Max_Counter[1]] == maxDate_observed ~ "ACTIVE",
  TRUE ~ "COMPLETED"
), by=list(LoanID)]

# - Find final age at last record
datCredit_real[ExclusionID==0, Event_Time := Age_Adj[Max_Counter[1]], by=list(LoanID)]

# - Right-censored (active) indicator wrt study-end period (ignore competing risks)
# An incomplete loan observed up to the current date will a 1-value at its last period
# Terminal events (Write-off & Settlement) occurring at study are excluded from this logic
datCredit_real[ExclusionID==0, Account_Censored := ifelse(Date >= maxDate_observed & Event_Type == "ACTIVE", 1, 0),
               by=list(LoanID)]




# --- 1.2. Data abstractions into simpler more manageable objects for delinquency measurement

# - Scalars
(n_observed <- length(unique(datCredit_real[ExclusionID == 0, LoanID]) ) )
(period_observed <- max(datCredit_real[ExclusionID == 0 & Counter==1, Max_Counter]))

# - Vectors
# LoanIDs | Useful for fusing abstracted matrices and vectors back to the dataset later
LoanIDs_real <- datCredit_real[ExclusionID == 0 & Counter==1 & order(LoanID), LoanID]
# Max Counter as an adjusted term vector (behavioural)
advance_adjterm_v_real <- copy(datCredit_real[ExclusionID == 0 & Counter==1 & order(LoanID), Max_Counter])
names(advance_adjterm_v_real) <- LoanIDs_real

# - Matrices: rows are periods (up to maximum observed period), columns are loan accounts
# Receipt matrix | Inferred (later to be changed to properly-calculated [Receipt])
matReceipt_real <- as.matrix(pivot_wider(data=datCredit_real[ExclusionID == 0 & order(LoanID),list(LoanID,Counter,Receipt_Inf)], 
                                         id_cols=LoanID:Counter, names_from=LoanID, values_from=Receipt_Inf))[,-1]
# Instalment matrix
matInstal_real <- as.matrix(pivot_wider(data=datCredit_real[ExclusionID == 0 & order(LoanID),list(LoanID,Counter,Instalment)], 
                                        id_cols=LoanID:Counter, names_from=LoanID, values_from=Instalment))[,-1]

# g0-Delinquency matrix | Observed
# Note: Append a zero-valued row at the start to represent t=0 | Purely as an expedient to align 
#   with pre-existing functions that use delinquency matrices
matg0 <- rbind(rep(0, n_observed),
               as.matrix(pivot_wider(data=datCredit_real[ExclusionID == 0 & order(LoanID),list(LoanID,Counter,g0_Delinq)], 
                                     id_cols=LoanID:Counter, names_from=LoanID, values_from=g0_Delinq))[,-1] )



# --- 1.3. Calculate Delinquency Measures for available loan performance history

# - Calculate CD (g_1: Contractual Delinquency)
matCD_real <- calculate.CD.forData(matInstal_real, matReceipt_real, sc.Thres, period_observed, n_observed, method="base")
colnames(matCD_real) <- LoanIDs_real

# - Transform delinquency matrix into a dataset with the same structure as that of the main dataset
# Note: Discard delinquencies at time t=0 for now since main dataset does not have records at t=0
datTemp <- pivot_longer(as.data.frame(matCD_real), cols=everything(), names_to="LoanID", values_to="g1_Delinq") %>% as.data.table()
datTemp[, LoanID := as.numeric(LoanID)]; datTemp[, Counter := (1:.N)-1,by=list(LoanID)]

# - Ensure variables are not present in dataset before fusion (useful during debugging)
suppressWarnings( datCredit_real[, `:=`(g1_Delinq = NULL)])

# - Merge delinquency measurements back to the base dataset, reorder new fields, and reset key
datCredit_real <- merge(datCredit_real, datTemp, by=c("LoanID","Counter"), all.x=T)  %>% 
  relocate(g1_Delinq, .after = DefaultStatus1) %>% setkey(LoanID, Counter)

# [SANITY CHECK] Missingness?
check_fuse1 <- datCredit_real[ExclusionID==0 & is.na(g1_Delinq), .N] == 0
cat( check_fuse1 %?% 'SAFE: No missingness in newly-created g1-delinquency measure [g1_Delinq].\n' %:% 
       'WARNING: Missingness detected in newly-created g1-delinquency measure [g1_Delinq].\n')

# - cleanup
rm(datTemp); gc()



# --- 1.4. Create Default Status field, given (d,k)-thresholds
# d: default threshold for g0/g1 measures; k; probation period for leaving k-curable state back into performing state


# -- g1-Delinquency | Calculated/implied using [Receipt]
# - Impose specific k-value and return corresponding delinquency state matrix (S_D, S_C, S_P), given preset d-value
# Uses custom function "DelinqCure_m()" in DelinqM.R
lstCurables <- DelinqCure_m(d=d, k=k, matCD.Use=matCD_real, n.given=n_observed, advance_adjterm_v=advance_adjterm_v_real)
matState_real <- lstCurables[['State']] # columns denote loans, rows denote time (t>=0)
colnames(matState_real) <- LoanIDs_real

# - Transform delinquency matrix into a dataset with the same structure as that of the main dataset
# Note: Discard delinquencies at time t=0 for now since main dataset does not have records at t=0
datTemp <- pivot_longer(as.data.frame(matState_real), cols=everything(), names_to="LoanID", values_to="Value") %>% as.data.table()
datTemp[, LoanID := as.numeric(LoanID)]; datTemp[, Counter := (1:.N)-1,by=list(LoanID)]

# - Re-code numeric values in state matrix to specific strings:
#   1) S_D (Default); 2) S_C (K-curable); 3) S_P (Performing);
datTemp[, DelinqState_g1 := case_when(Value==3 ~ "S_P", Value==1 ~ "S_D", Value==2 ~ "S_C")]

# - Assign Default Indicator Accordingly
datTemp[, DefaultStatus2 := ifelse(Value==3,0,1)]


# -- g0-Delinquency | "Observed"
# - Impose specific k-value and return corresponding delinquency state matrix (S_D, S_C, S_P), given preset d-value
# Uses custom function "DelinqCure_m()" in DelinqM.R
lstCurables <- DelinqCure_m(d=d, k=k, matCD.Use=matg0, n.given=n_observed, advance_adjterm_v=advance_adjterm_v_real)
matState_g0 <- lstCurables[['State']] # columns denote loans, rows denote time (t>=0)
colnames(matState_g0) <- LoanIDs_real

# - Transform delinquency matrix into a dataset with the same structure as that of the main dataset
# Note: Discard delinquencies at time t=0 for now since main dataset does not have records at t=0
datTemp2 <- pivot_longer(as.data.frame(matState_g0), cols=everything(), names_to="LoanID", values_to="Value") %>% as.data.table()
datTemp2[, LoanID := as.numeric(LoanID)]; datTemp2[, Counter := (1:.N)-1,by=list(LoanID)]

# - Re-code numeric values in state matrix to specific strings:
#   1) S_D (Default); 2) S_C (K-curable); 3) S_P (Performing);
datTemp2[, DelinqState_g0 := case_when(Value==3 ~ "S_P", Value==1 ~ "S_D", Value==2 ~ "S_C")]

# - Assign Default Indicator Accordingly
datTemp2[, DefaultStatus1 := ifelse(Value==3,0,1)]


# -- Preparing for data fusion
# - Ensure variables are not present in dataset before fusion (useful during debugging)
suppressWarnings({
  datCredit_real[, `:=`(DefaultStatus2 = NULL, DelinqState_g1 = NULL, 
                        DefaultStatus1 = NULL, DelinqState_g0 = NULL)]   
})

# - Merge extra information together, to be fused with main dataset later
datTemp_Final <- merge(datTemp[, list(LoanID, Counter, DelinqState_g1, DefaultStatus2)], 
                       datTemp2[, list(LoanID, Counter, DelinqState_g0, DefaultStatus1)], by=c("LoanID", "Counter"), all.x=T)

# - Intermediate cleanup | Memory optimisation
rm(datTemp, datTemp2, lstCurables, matInstal_real, matReceipt_real, matCD_real, matg0); gc()

# - Merge delinquency state information back to the base dataset, reorder new fields, and reset key
datCredit_real <- merge(datCredit_real, 
                        datTemp_Final[, list(LoanID, Counter, DelinqState_g1, DefaultStatus2, DelinqState_g0, DefaultStatus1)],
                        by=c("LoanID", "Counter"), all.x=T) %>% relocate(DelinqState_g1, DefaultStatus2, .after=g1_Delinq) %>%
  relocate(DelinqState_g0, DefaultStatus1, .after=g0_Delinq) %>% setkey(LoanID, Counter)

# [SANITY CHECK] Missingness?
check_fuse2a <- datCredit_real[ExclusionID==0 & is.na(DelinqState_g1), .N] == 0
cat( check_fuse2a %?% 'SAFE: No missingness in newly-created 3-state delinquency state vector [DelinqState_g1].\n' %:% 
       'WARNING: Missingness detected in newly-created 3-state delinquency state vector[DelinqState].\n')
check_fuse2b <- datCredit_real[ExclusionID==0 & is.na(DefaultStatus2), .N] == 0
cat( check_fuse2b %?% 'SAFE: No missingness in newly-created default indicator [DefaultStatus2].\n' %:% 
       'WARNING: Missingness detected in newly-created default indicator [DefaultStatus2].\n')
check_fuse2c <- datCredit_real[ExclusionID==0 & is.na(DelinqState_g0), .N] == 0
cat( check_fuse2c %?% 'SAFE: No missingness in newly-created 3-state delinquency state vector [DelinqState_g0].\n' %:% 
       'WARNING: Missingness detected in newly-created 3-state delinquency state vector[DelinqState_g0].\n')
check_fuse2c <- datCredit_real[ExclusionID==0 & is.na(DefaultStatus1), .N] == 0
cat( check_fuse2c %?% 'SAFE: No missingness in amended default indicator [DefaultStatus1].\n' %:% 
       'WARNING: Missingness detected in amended default indicator [DefaultStatus1].\n')

# - Cleanup
rm(datTemp_Final); gc()



# --- 1.5. Reorder new features created thus far
datCredit_real <- datCredit_real %>% 
  # Re-order and group together fields related to terminal events or behavioural dynamics
  relocate(Event_Time, Event_Type, Account_Censored, HasWOff, WOff_Ind, WriteOff_Amt, 
           HasSettle, EarlySettle_Ind, EarlySettle_Amt, HasFurtherLoan, FurtherLoan_Ind, FurtherLoan_Amt, 
           HasRedraw, Redraw_Ind, Redrawn_Amt, HasClosure, CLS_STAMP, .after=DefaultStatus2) %>%
  # Re-order and group together other fields
  relocate(ExclusionID, TreatmentID, HasTrailingZeroBalances, ZeroBal_Start, .after=CLS_STAMP)





# ------- 2. Platform-aligned Advanced Feature engineering & Enrichment | Default Spells
# NOTE: [DefaultStatus1] will be used for now given the deficiencies in the underlying [Receipt_Inf]
# that corrupts [DefaultStatus2].

# --- 2.0 Preliminaries & intermediary fields

# - Delete variables from dataset if they already exist (useful during debugging)
suppressWarnings({
  datCredit_real[, `:=`(DefaultStatus_Next = NULL, DefaultStatus_Prev = NULL, Time_EnterDefault = NULL, 
                        LastTime_Default = NULL, DefSpell_Num = NULL, DefSpell_Key = NULL, DefSpell_Counter = NULL, 
                        TimeInDefSpell = NULL, DefSpell_LeftTrunc = NULL, DefSpellResol_Type_Hist = NULL, 
                        DefSpellResol_TimeEnd = NULL, DefSpell_Age = NULL, DefSpell_Censored = NULL, DefSpell_Event = NULL, 
                        HasLeftTruncDefSpell=NULL)]   
})

# - Create lagging/leading default status fields to help select periods of entering/leaving default
datCredit_real[ExclusionID==0, DefaultStatus_Next := shift(DefaultStatus1, n=1, type="lead"),by=list(LoanID)]
datCredit_real[ExclusionID==0, DefaultStatus_Prev := shift(DefaultStatus1, n=1, type="lag"),by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of performing statuses at the write-off point
diag.real11_1a <- datCredit_real[ExclusionID==0 & Event_Type == "WOFF" & Counter==Max_Counter & 
                                   DefaultStatus1==0, .N] / 
  datCredit_real[ExclusionID==0 & Event_Type == "WOFF" & Counter==Max_Counter, .N] * 100
# - Conditional reporting
if (diag.real11_1a > 0) {
  cat(paste0("NOTE: Write-off without a default status [DefaultStatus1] occured in ",
             round(diag.real11_1a, digits=2), "% of write-offs.\n")) 
}

# [DIAGNOSTIC] Prevalence of performing statuses at the write-off point, 
# given a default status the preceding period
diag.real11_1b <- datCredit_real[ExclusionID==0 & Event_Type == "WOFF" & Counter==Max_Counter & 
                                   DefaultStatus1==0 & DefaultStatus_Prev == 1, .N] / 
  datCredit_real[ExclusionID==0 & Event_Type == "WOFF" & Counter==Max_Counter, .N] * 100

# - Conditional treatment | Advanced Data Treatment 11
if (diag.real11_1b > 0) {
  
  cat(paste0("DETECTED: A default status at the point preceding write-off no longer holds at write-off itself in ",
             round(diag.real11_1b, digits=3), "% of write-offs.\n\tCorrecting by ",
             "setting [DefaultStatus1]=1 explicitly .. \n"))
  
  # - Mark affected accounts with current treatment ID
  datCredit_real[ExclusionID==0 & Event_Type == "WOFF" & Counter==Max_Counter & 
                   DefaultStatus1==0 & DefaultStatus_Prev == 1, TreatmentID := paste0(TreatmentID, ";11")]
  
  # [TREATMENT] Change delinquency state and default indicator accordingly 
  datCredit_real[ExclusionID==0 & Event_Type == "WOFF" & Counter==Max_Counter & 
                   DefaultStatus1==0 & DefaultStatus_Prev == 1, DelinqState_g0 := "S_D"]
  datCredit_real[ExclusionID==0 & Event_Type == "WOFF" & Counter==Max_Counter & 
                   DefaultStatus1==0 & DefaultStatus_Prev == 1, DefaultStatus1 := 1]
  
  # - Recreate affected fields
  datCredit_real[ExclusionID==0, DefaultStatus_Next := shift(DefaultStatus1, n=1, type="lead"),by=list(LoanID)]
  datCredit_real[ExclusionID==0, DefaultStatus_Prev := shift(DefaultStatus1, n=1, type="lag"),by=list(LoanID)]
  
  # [SANITY CHECK] Did the treatment succeed?
  check_advTreat11 <- datCredit_real[ExclusionID==0 & Event_Type == "WOFF" & Counter==Max_Counter & 
                                       DefaultStatus1==0 & DefaultStatus_Prev == 1, .N] *100 == 0 # Should be true
  cat( check_advTreat11  %?% 
         'SAFE: Accounts with performing statuses at the write-off point, given default statuses at the previous point,
            were successfully treated.\n' %:% 
         'WARNING: Failed to treat accounts Accounts with performing statuses at the write-off point, 
            given default statuses at the previous point.\n')
}



# --- 2.1 Default episodes: Starting/ending times, spell number, duration counter
# These features are useful in reporting on various aspects of LGD-modelling as well as 
# properly assigning zero loss later to cured cases for both incomplete and completed portfolios (the current case)

# - Obtain start and ending times of each default episode accordingly
# These variables will only have a value at the moment the default episode begins or at
# its last period, respectively for [Time_EnterDefault] and [LastTime_Default]
datCredit_real[ExclusionID==0, 
               Time_EnterDefault := ifelse(DefaultStatus1 == 1 & (DefaultStatus_Prev == 0 | is.na(DefaultStatus_Prev)), 
                                           Age_Adj, as.integer(NA)), by=list(LoanID)]
datCredit_real[ExclusionID==0, 
               LastTime_Default := ifelse(DefaultStatus1 == 1 & (is.na(DefaultStatus_Next) | DefaultStatus_Next == 0), 
                                          Age_Adj, as.integer(NA)), by=list(LoanID)]

# - Calculate the index of each default episode, starting with 0 for the performing parts 
# Naturally, whenever [Time_EnterDefault] has a value, a new default episode has begun
# This field is later used as a grouping variable
datCredit_real[ExclusionID==0, DefSpell_Num := cumsum(ifelse(is.na(Time_EnterDefault), 0, 1)), by=list(LoanID)]

# - Create composite key between LoanID & default spell to facilitate survival modelling
datCredit_real[ExclusionID==0 & DefaultStatus1 == 1, DefSpell_Key := paste0(LoanID, "_", DefSpell_Num)]

# - Create spell-specific row counter variable to ease navigation
datCredit_real[ExclusionID == 0 & DefaultStatus1==1, DefSpell_Counter := 1:.N, by=list(LoanID, DefSpell_Num)]

# - Create a time variable specific to track duration/time spent in default spell
datCredit_real[ExclusionID==0 & DefaultStatus1==1, TimeInDefSpell := 1:.N, by=list(LoanID, DefSpell_Num)]
# Now, amend previous counter for left-truncated accounts observed mid-life
datCredit_real[ExclusionID==0 & DefaultStatus1==1 & DefSpell_Num == 1, 
               TimeInDefSpell := TimeInDefSpell + (Age_Adj[1] - 1)*ifelse(Counter[1]==1,1,0), 
               by=list(LoanID, DefSpell_Num)]

# - Create a default spell-level left-truncated indicator to facilitate possible exclusion during modelling
datCredit_real[ExclusionID == 0 & DefaultStatus1==1, DefSpell_LeftTrunc := 
                 ifelse(TimeInDefSpell[1] > 1, 1,0), by=list(LoanID, DefSpell_Num)]

# - Create an account-level left-truncated indicator to facilitate possible exclusion during modelling
datCredit_real[ExclusionID == 0, HasLeftTruncDefSpell := 
                 ifelse(DefaultStatus1[1] == 1 & Age_Adj[1] > 1, 1,0), by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of accounts with left-truncated default spells, i.e., older loans
# that were observed mid-life
diag.real11_1c <- datCredit_real[ExclusionID == 0 & Counter==1 & HasLeftTruncDefSpell==1, .N] / 
  datCredit_real[ExclusionID == 0 & Counter==1, .N] * 100
if (diag.real11_1c > 0) { cat("NOTE:", round(diag.real11_1c,digits=3), "% of accounts are left-truncated wrt default spells, i.e., 
    observation started after the default spell's true start.\n") }

# - Copy starting/ending times of a default episode across all periods within a default episode
# Exclude all histories that have not yet experienced a default event.
# In creating [Time_EnterDefault], mean() is used merely to extract a singular value amidst NA-values, per default episode
# In creating [Time_EnterDefault], the second logic gate in the IF is necessary to cater for the last default episode prior write-off,
# since it won't have a value for [LastTime_Default] (hasn't cured).
# In creating [LastTime_Default], mean() is used as a mechanism to spread the extracted singular value 
# across all elements within a vector that otherwise contains NA-values.
datCredit_real[ExclusionID==0 & DefSpell_Num>0, 
               Time_EnterDefault := ifelse( Age_Adj <= mean(LastTime_Default,na.rm=T) | 
                                              all(is.na(LastTime_Default)), 
                                            Time_EnterDefault[1], as.integer(NA)), by=list(LoanID, DefSpell_Num)]
datCredit_real[ExclusionID==0 & DefSpell_Num>0, 
               LastTime_Default := ifelse( Age_Adj >= mean(Time_EnterDefault,na.rm=T) & 
                                             Age_Adj <= mean(LastTime_Default,na.rm=T), 
                                           mean(LastTime_Default,na.rm=T), as.integer(NA)), by=list(LoanID, DefSpell_Num)]

# - Various sanity checks and conditional corrections on starting/ending times of a default episode
# These checks are very important since the respective fields inform much of the LGD-modelling's structure
# [SANITY CHECK] Is Time_EnterDefault reliably populated?
check1_real <- datCredit_real[ExclusionID==0 & is.na(Time_EnterDefault) & DefaultStatus1 == 1,.N] == 0
# [SANITY CHECK] Is LastTime_Default reliably populated?
check2_real <- datCredit_real[ExclusionID==0 & is.na(LastTime_Default) & DefaultStatus1 == 1,.N] == 0
# [SANITY CHECK] Is Time_EnterDefault/LastTime_Default reliably populated, even for single-period write-offs?
brokenCases_real <- datCredit_real[ExclusionID==0 & WOff_Ind == 1 & is.na(Time_EnterDefault),.N]
# - Conditional correction of starting/ending times at write-off point coinciding at t=1
if (brokenCases_real > 0) {
  datCredit_real[ExclusionID==0 & WOff_Ind == 1 & is.na(Time_EnterDefault), 
                 Time_EnterDefault := Age_Adj[1], by=list(LoanID)]
  datCredit_real[ExclusionID==0 & WOff_Ind == 1 & is.na(LastTime_Default), 
                 LastTime_Default := Age_Adj[.N], by=list(LoanID)]  
}
# [SANITY CHECK] Re-check success of previous conditional treatment
check3_real <- datCredit_real[ExclusionID==0 & WOff_Ind == 1 & is.na(LastTime_Default),.N] == 0
# - Messaging on sanity checks
cat( check1_real %?% 
       'SAFE: [Time_EnterDefault] correctly constructed such that it has non-missing values during default spells.\n' %:%
       'WARNING: Missingness detected in [Time_EnterDefault] during default spells.\n')
cat( (check2_real & check3_real) %?% 
       'SAFE: [LastTime_Default] correctly constructed such that it has non-missing values during default spells, 
          including at write-off point.\n' %:%
       'WARNING: Missingness detected in [LastTime_Default] during default spells, including at the write-off point.\n')

# - Housekeeping
suppressWarnings({
  datCredit_real[, `:=`(DefaultStatus_Next = NULL, DefaultStatus_Prev = NULL)]   
}); gc()



# --- 2.2 Default spell: Write-off event & Censoring fields

# - Censoring indicator per default spell (PIT, can vary over the duration of the spell)
datCredit_real[ExclusionID==0 & DefaultStatus1==1, DefSpell_Censored :=
                 ifelse(Account_Censored == 1 | # time-censoring
                          ( TimeInDefSpell == .N + DefSpell_LeftTrunc[1]*(Age_Adj[1] - 1) & # Last record in spell AND
                              LastTime_Default != Event_Time) | # curing-event
                          (Age_Adj == Event_Time[1] & Event_Type != "WOFF"), # competing risk terminal event
                        1, 0), by=list(LoanID, DefSpell_Num)]

# - Main event indicator per default spell (PIT, can vary over the duration of the spell)
# This is created simply by checking whether the default spell-specific time counter equals that spell's 
# overall duration (.N). Given the construction of [DefSpell_Num], the last entry per default spell MUST
# be the moment of default, hence the event indicator equaling 1, but only in the absence of a censoring event
datCredit_real[ExclusionID==0 & DefaultStatus1==1, DefSpell_Event := 
                 ifelse(DefSpell_Censored == 0 & Event_Type == "WOFF" & 
                          TimeInDefSpell == .N + DefSpell_LeftTrunc[1]*(Age_Adj[1] - 1) & # Last record in spell
                          LastTime_Default == Event_Time, 1, 0), by=list(LoanID, DefSpell_Num)]

# [SANITY CHECK] does censoring and the main event coincide? (Should not)
check4_real <- datCredit_real[ExclusionID==0 & DefSpell_Censored == 1 & DefSpell_Event == 1, .N] == 0
cat( check4_real %?% 
       'SAFE: [DefSpell_Censored] and [DefSpell_Event] correctly constructed such that 1-values do not coincide.\n' %:%
       'WARNING: Structural problems detected in the construction of [DefSpell_Censored] and [DefSpell_Event] with coinciding 1-values.\n')



# --- 2.3 Default spell resolution types (incomplete + completed portfolios)

# - Designate the type of default resolution (cure vs write-off) for completed portfolios
# NOTE: [DefSpellResol_Type] from simulation script 2a(i) is not (yet) relevant in the current 2b script series.

# - Designate the type of default resolution (cure vs write-off) for incomplete portfolios
datCredit_real[ExclusionID==0 & DefaultStatus1==1, DefSpellResol_Type_Hist := 
                 ifelse(Event_Type == "WOFF" & LastTime_Default == Event_Time & Date[.N] <= maxDate_observed, "WOFF", 
                        ifelse( (!is.na(LastTime_Default) & Date[.N] < maxDate_observed ) | # normal cures
                                  (Event_Type == "SETTLE" & LastTime_Default == Event_Time & Date[.N] <= maxDate_observed), # settlement-related cures
                                "Cured", "Censored")), by=list(LoanID, DefSpell_Num)]

# [SANITY CHECK] Prevalence of unjustified missingness in newly-created field?
check4b_real <- datCredit_real[ExclusionID==0 & DefaultStatus1==1 & is.na(DefSpellResol_Type_Hist), .N] / 
  datCredit_real[ExclusionID==0 & DefaultStatus1==1, .N] * 100 
cat ( (check4b_real == 0) %?% "SAFE: No unjustified missingness in [DefSpellResol_Type_Hist].\n" %:%
        paste0("WARNING: Prevalence of unjustified missingness in [DefSpellResol_Type_Hist] of ", 
               round(check4b_real,digits=2), "% of records during default spells.\n"))



# --- 2.4 Default spell resolution times (incomplete portfolio only)

# - Create event time field (not time-to-event) for modelling write-off/cure survival
# Future default episodes not yet commenced are set to 0
# But in-progress default events are set to the age at the last known date (controlled via date.monthstart.long)
datCredit_real[ExclusionID==0 & DefaultStatus1 == 1, DefSpellResol_TimeEnd := 
                 ifelse(DefSpellResol_Type_Hist != "Censored", LastTime_Default,
                        ifelse(Date[1] <= maxDate_observed, 
                               .SD[Date == maxDate_observed, Age_Adj], # Return age at last observed 'current' date
                               0 # future default episode that has not yet commenced
                        )), by=list(LoanID, DefSpell_Num)]

# [SANITY CHECK] Prevalence of unexpected values in newly-created field?
check4c_real <- datCredit_real[ExclusionID==0 & DefaultStatus1==1 & DefSpellResol_TimeEnd == 0, .N] / 
  datCredit_real[ExclusionID==0 & DefaultStatus1==1, .N] * 100 
cat ( (check4c_real == 0) %?% "SAFE: No unexpected zero-values in [DefSpellResol_TimeEnd].\n" %:%
        paste0("WARNING: Prevalence of unexpected zero-values in [DefSpellResol_TimeEnd] of ", 
               round(check4c_real,digits=2), "% of records during default spells.\n"))

# - Create time-to-event field for modelling write-off/cure survival
# Only for uncensored default episodes
datCredit_real[ExclusionID==0 & DefaultStatus1 == 1 & DefSpellResol_TimeEnd > 0, 
               DefSpell_Age := DefSpellResol_TimeEnd - (1-DefSpell_LeftTrunc[1])*(Age_Adj[1] - 1), 
               by=list(LoanID, DefSpell_Num)]

# - Housekeeping
suppressWarnings({
  datCredit_real[, `:=`(Time_EnterDefault = NULL, LastTime_Default = NULL)]   
})


# --- 2.5. Reorder new features created thus far
datCredit_real <- datCredit_real %>% 
  # Re-order and group together fields related to terminal events or behavioural dynamics
  relocate(DefSpell_Num, DefSpell_Counter, TimeInDefSpell, DefSpell_LeftTrunc, DefSpell_Event, DefSpell_Censored, 
           DefSpellResol_TimeEnd, DefSpell_Age, DefSpellResol_Type_Hist, 
           HasLeftTruncDefSpell, .after=DefaultStatus2) %>%
  relocate(DefSpell_Key, .after=LoanID)





# ------- 3. Platform-aligned Advanced Feature engineering & Enrichment | Realised loss severity (LGW)

# --- 3.1 Find the starting point of the last default spell prior write-off

# - get the episode counts for the default state in vector form (each element = 1 loan)
# Using a custom function "episodeCount()" defined in DelinqM.R
epiCount_DC_real <- sapply(1:n_observed, episodeCount, state_m=matState_g0, state="S_D:S_C")
names(epiCount_DC_real) <- LoanIDs_real

# - get the relevant start time of the last default episode in vector form (each element = 1 loan)
# Using a custom function "startTime()" defined in DelinqM.R
lastDefaultTimes <- sapply(1:n_observed, function(i,c,mat) {
  # - Testing conditions
  # i <- 85; c <- epiCount_DC_real; mat <- matState_g0
  if (c[i] > 0) {
    
    # get start times of all default episodes, take the last element and 
    # minus 1 to account for t>=0 structure instead of t>=1
    start_d <- rev(startTime(i=i, state_m=mat, state="S_D:S_C"))[1] - 1
    return (start_d)
    
  } else {
    return (-1)
  }
}, c=epiCount_DC_real, mat=matState_g0)

# - Transform into a data.table for efficient merging
datTemp <- data.table(LoanID = LoanIDs_real, DefSpell_LastStart = lastDefaultTimes); 

# - Delete underlying objects for memory efficiency
rm(epiCount_DC_real, LoanIDs_real); gc()

# - Delete variables from dataset if they already exist (useful during debugging)
suppressWarnings({datCredit_real[, DefSpell_LastStart := NULL]})

# - Merge episode counts back to the base dataset, reorder new fields, and reset key
datCredit_real <- merge(datCredit_real, datTemp, by="LoanID", all.x=T) %>% 
  relocate(DefSpell_LastStart, .after = HasLeftTruncDefSpell) %>% setkey(LoanID, Counter)

# - Lookup the corresponding [Age_Adj]-value, given that the new field only lists the position currently
datCredit_real[ExclusionID == 0, DefSpell_LastStart := 
                 ifelse(DefSpell_LastStart[1] > 0, Age_Adj[DefSpell_LastStart[1]], -1), by=list(LoanID)]

# [SANITY CHECK] Missingness?
check_fuse3 <- datCredit_real[ExclusionID==0 & is.na(DefSpell_LastStart), .N] == 0
cat( check_fuse3 %?% 'SAFE: No missingness in newly-created starting time of last default spell [DefSpell_LastStart].\n' %:% 
       'WARNING: Missingness detected in newly-created starting time of last default spell [DefSpell_LastStart].\n')

# - cleanup
rm(lastDefaultTimes, datTemp); gc()



# --- 3.2 Sampling write-offs from which realised losses will be calculated

# - Preliminary write-off analysis
samp1 <- subset(datCredit_real, ExclusionID==0 & Event_Type == "WOFF" & DefSpell_LeftTrunc == 0)
# [DIAGNOSTIC] How many of these write-offs have entered the default state prior write-off
diag.real_fuse_1a <- sum(samp1[Age_Adj == (Event_Time-1), DefaultStatus1]) / samp1[Age_Adj == (Event_Time), .N] * 100
### RESULT: Should be 100%, but 99.96% observed, likely due to qualitative write-off triggers not
# necessarily captured by default definition. Or very old loans

# - Sample those accounts that were in the default state prior write-off
datLGD_L <- subset(datCredit_real, ExclusionID==0 & DefSpellResol_Type_Hist == "WOFF" & 
                     Age_Adj >= DefSpell_LastStart & DefSpell_LeftTrunc == 0)

# [DIAGNOSTIC] Checking data grain
n_woffs_observed <- datLGD_L[TimeInDefSpell==1, .N]
check_fuse4a <- n_woffs_observed == length(unique(datLGD_L$LoanID))
cat( check_fuse4a %?% 'SAFE: Data grain in loss severity dataset confirmed using [LoanID] and [TimeInDefSpell].\n' %:% 
       'WARNING: Broken data grain detected in loss severity dataset using [LoanID] and [TimeInDefSpell].\n.')

# [DIAGNOSTIC] Checking for consistency with previous sampling analysis
check_fuse4b <- abs( (n_woffs_observed) / samp1[Age_Adj == (Event_Time), .N] * 100 - 
                       diag.real_fuse_1a) < 1
cat( check_fuse4b %?% 'SAFE: Write-offs correctly isolated using [Event_Type] with requisite fields 
          [DefSpellResol_Type_Hist] and [DefSpell_LastStart].\n' %:% 
       'WARNING: Failed to isolate write-offfs using [Event_Type] with requisite fields 
          [DefSpellResol_Type_Hist] and [DefSpell_LastStart].\n')

# [DIAGNOSTIC] Checking data grain differently
check_fuse4c <- datLGD_L[TimeInDefSpell==1, list(Freq=.N), by=list(LoanID, TimeInDefSpell)][Freq>1,.N] == 0
cat( check_fuse4c %?% 'SAFE: Last Default spells per account prior write-off were correctly extracted; grain confirmed.\n' %:%
       'WARNING: Broken grain detected in extracting last default spells per account prior write-off.\n')

# [DIAGNOSTIC] Prevalence of write-offs
diag.real_fuse_1b <- n_woffs_observed / n_observed * 100 # account-level impact
diag.real_fuse_1b_rec <- datLGD_L[, .N] / datCredit_real[ExclusionID==0, .N] * 100 # dataset-level impact
cat("NOTE: Write-off occurred in", round(diag.real_fuse_1b,digits=2),"% of accounts (", 
    round(diag.real_fuse_1b_rec,digits=2), "% of records)\n")

# - Cleanup
rm(samp1)



# --- 3.3 Calculate net recoveries and discount to start of default episode, then fuse with main dataset

# - Calculate sum of discounted net cash flows as at time of default
datLGD_L[, ReceiptPV := sum(Receipt_Inf * (1+InterestRate_Nom/12)^(-1*TimeInDefSpell)), by=list(LoanID)]

# - Calculate loss severity as the ratio between discounted unrecovered versus EAD
# Note: This definition only holds for default spells with duration greater than 1 period
datLGD_L[DefSpell_Age > 1, LossRate_Real := (Balance[1] - ReceiptPV[1]) / Balance[1], by=list(LoanID)]
# Note: The following is more suitable for single-period default spells prior write-off
datLGD_L[DefSpell_Age == 1, LossRate_Real := (WriteOff_Amt) / (Receipt_Inf + WriteOff_Amt), by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of infinite amounts in calculated loss rate?
diag.real_fuse_2a <- datLGD_L[TimeInDefSpell==1 & (is.na(LossRate_Real) | abs(LossRate_Real) == Inf), .N] / 
  datLGD_L[,.N] * 100

# - Conditional treatment | Basic
if (diag.real_fuse_2a > 0) {
  cat(paste0("DETECTED: Infinite loss rates [LossRate_Real], likely due to single-period write-offs with a zero-valued balance.\n\t",
             "Prevalence: ", round(diag.real_fuse_2a,digits=4), "% of write-offs. ",
             "Correcting by setting [LossRate_Real] to 100% .. \n"))
  
  # [TREATMENT]
  datLGD_L[is.na(LossRate_Real), LossRate_Real := 1] 
}

# - Delete variables from dataset if they already exist (useful during debugging)
suppressWarnings( datCredit_real[, `:=`(ReceiptPV = NULL, LossRate_Real = NULL)] )

# - Fuse data and reorder
datCredit_real <- merge(datCredit_real, datLGD_L[,list(LoanID, Counter, ReceiptPV, LossRate_Real)], 
                        by=c("LoanID","Counter"), all.x=T) %>% relocate(ReceiptPV, LossRate_Real, .after=DefSpell_LastStart)

# [SANITY CHECK] Missingness?
check_fuse4d <- datCredit_real[ExclusionID==0 & DefSpellResol_Type_Hist == "WOFF" & DefSpellResol_TimeEnd == Event_Time & 
                                 Age_Adj >= DefSpell_LastStart & DefSpell_LeftTrunc == 0 & is.na(ReceiptPV), .N] == 0
cat( check_fuse4d %?% 'SAFE: No missingness in newly-created PV of receipts [ReceiptPV].\n' %:% 
       'WARNING: Missingness detected in newly-created PV of receipts [ReceiptPV].\n')
check_fuse4e <- datCredit_real[ExclusionID==0 & DefSpellResol_Type_Hist == "WOFF" & DefSpellResol_TimeEnd == Event_Time & 
                                 Age_Adj >= DefSpell_LastStart & DefSpell_LeftTrunc == 0 & is.na(LossRate_Real), .N] == 0
cat( check_fuse4e %?% 'SAFE: No missingness in newly-created realised loss rate [LossRate_Real].\n' %:% 
       'WARNING: Missingness detected in newly-created realised loss rate[LossRate_Real].\n')



# --- 3.4 Zero-loss on cure assumption

# - Assign zero loss to cures (resolved defaults that were not written-off)
# NOTE: all defaulted but5 resolved cases with missing values in [LossRate_Real] must be cures
datCredit_real[ExclusionID==0 & is.na(LossRate_Real) & DefSpellResol_Type_Hist != "Censored", LossRate_Real := 0]

# - Accordingly, set ReceiptPV to default balance so that a zero loss rate is plausible
# This becomes useful for subsequent analytics
datCredit_real[ExclusionID==0 & LossRate_Real == 0 & DefSpellResol_Type_Hist != "Censored", ReceiptPV := Balance[1], 
               by=list(LoanID, DefSpell_Num)]

# - Sample for checking success of previous treatments
test <- subset(datCredit_real, ExclusionID==0 & LossRate_Real == 0 & DefSpellResol_Type_Hist != "Censored" & 
                 TimeInDefSpell == 1)

# - Re-calculate loss severity as the ratio between discounted unrecovered versus EAD
# NOTE: Coalesce() function is used to handle default spells that 1) start with zero-valued balances; 2) are single-period spells
# For these few minority cases, division-by-zero errors are handled by setting the loss rate to 0 explicitly
test[, LossRate_Real2 := coalesce( (Balance - ReceiptPV) / Balance, 0)]  # already unique at loanID-default_episode-level

# [SANITY CHECK] Will these treatments yield a zero-valued loss rate?
check_5_real <- all(test$LossRate_Real2 == 0)
cat( check_5_real %?% "SAFE: Zero-loss-on-cure assumption successfully imposed such that zero-valued loss rates exist accordingly.
                    Affected fields: [LossRate_Real], [ReceiptPV].\n" %:%
       "WARNING: Failed to impose zero-loss-on-cure assumption such that zero-valued loss rates exist accordingly.\n")

# - Cleanup
rm(datLGD_L, test); gc()





# ------- 4. Platform-aligned Advanced Feature engineering & Enrichment | Performing spells

# --- 4.0 Preliminaries & intermediary fields

# - Delete variables from dataset if they already exist (useful during debugging)
suppressWarnings({
  datCredit_real[, `:=`(PerfSpell_Num = NULL, PerfSpell_Key = NULL, PerfSpell_Counter = NULL, TimeInPerfSpell = NULL, 
                        PerfSpell_LeftTrunc = NULL, PerfSpell_Censored = NULL, PerfSpell_Event = NULL, PerfSpell_TimeEnd = NULL, 
                        PerfSpell_Age = NULL, PerfSpell_Desc = NULL, PerfSpellResol_Type_Hist = NULL, 
                        HasLeftTruncPerfSpell = NULL)]   
})



# --- 4.1 Performance spells: Spell number & duration counter

# - Performing spell counter
# Each moment of entering default is also the terminal event for a performing spell/duration
# Therefore, lag the default-episode number (>=0) by 1 period backwards. Wherever this lagged counter changes,
# we know a new default episode has started, thereby ending the previous performing spell.
# Then, add 1 to the counter so that it starts at 1 (DefSpell_Num starts at 0)
datCredit_real[ExclusionID==0, PerfSpell_Num := coalesce(shift(DefSpell_Num, n=1, type="lag", fill=0),0) + 1, by=list(LoanID)]
# Now, void the performing spell counter whenever the account is in actual default, except for time-in-default = 1,
# unless it's the first record of a left-truncated account that is currently in default at the start of observation. 
# Otherwise we'll accidentally override our own mechanism for indicating end-of-time for the current performing spell
# This operation should also cater for episodic delinquency (re-defaults) as well
datCredit_real[ExclusionID==0 & DefaultStatus1==1 & (TimeInDefSpell > 1 | Counter==1), PerfSpell_Num := NA]

# - Create composite key between LoanID & performance spell to facilitate survival modelling
datCredit_real[ExclusionID==0 & !is.na(PerfSpell_Num), PerfSpell_Key := paste0(LoanID, "_", PerfSpell_Num)]

# - Create spell-specific row counter variable to ease navigation
datCredit_real[ExclusionID == 0 & !is.na(PerfSpell_Num), PerfSpell_Counter := 1:.N, by=list(LoanID,PerfSpell_Num)]

# - Create a time variable specific to track duration/time spent in performing spell
datCredit_real[ExclusionID==0 & !is.na(PerfSpell_Num), TimeInPerfSpell := 1:.N, 
               by=list(LoanID, PerfSpell_Num)]
# Now, amend previous counter for left-truncated accounts observed mid-life
datCredit_real[ExclusionID==0 & !is.na(PerfSpell_Num) & PerfSpell_Num == 1, 
               TimeInPerfSpell := TimeInPerfSpell + (Age_Adj[1] - 1)*ifelse(Counter[1]==1,1,0), 
               by=list(LoanID, PerfSpell_Num)]

# - Create a performing spell-level left-truncated indicator to facilitate possible exclusion during modelling
datCredit_real[ExclusionID == 0 & !is.na(PerfSpell_Num), PerfSpell_LeftTrunc := 
                 ifelse(TimeInPerfSpell[1] > 1, 1,0), by=list(LoanID, PerfSpell_Num)]

# - Create an account-level left-truncated indicator to facilitate possible exclusion during modelling
datCredit_real[ExclusionID == 0, HasLeftTruncPerfSpell := 
                 ifelse(DefaultStatus1[1] == 0 & Age_Adj[1]>1, 1, 0), by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of accounts with left-truncated performing spells, i.e., older loans
# that were observed mid-life
diag.real11_1d <- datCredit_real[ExclusionID == 0 & Counter==1 & HasLeftTruncPerfSpell==1, .N] / 
  datCredit_real[ExclusionID == 0 & Counter==1, .N] * 100
diag.real11_1d2 <- datCredit_real[ExclusionID == 0 & PerfSpell_Num==1 & PerfSpell_Counter==1 & HasLeftTruncPerfSpell==1, .N] / 
  datCredit_real[ExclusionID == 0 & PerfSpell_Num==1 & PerfSpell_Counter==1, .N] * 100
if (diag.real11_1d > 0) { cat("NOTE:", round(diag.real11_1d,digits=2), "% of accounts are left-truncated wrt performing spells, i.e., 
    observation started after the performing spell's true start. Similarly, ", round(diag.real11_1d2, digits=2), 
                              "% of performing spells are left-truncated.\n") }



# --- 4.2 Performance spells: Default-event & Censoring fields

# - Censoring indicator per performing spell (PIT, can vary over the duration of the spell)
datCredit_real[ExclusionID==0 & !is.na(PerfSpell_Num), PerfSpell_Censored := 
                 ifelse((Account_Censored == 1 & DefaultStatus1==0) | (Age_Adj == Event_Time[1] & DefaultStatus1==0), 1, 0), 
               by=list(LoanID, PerfSpell_Num)]

# - Main event indicator per performing spell (PIT, can vary over the duration of the spell)
datCredit_real[ExclusionID==0 & !is.na(PerfSpell_Num), PerfSpell_Event := 
                 ifelse(DefaultStatus1==1 & PerfSpell_Censored == 0, 1, 0), by=list(LoanID, PerfSpell_Num)]

# [SANITY CHECK] Does censoring and the main event coincide? (Should not)
check_6_real <- datCredit_real[ExclusionID==0 & PerfSpell_Event == 1 & PerfSpell_Censored == 1, .N] == 0
cat( check_6_real %?% 
       'SAFE: [PerfSpell_Censored] and [PerfSpell_Event] correctly constructed such that 1-values do not coincide.\n' %:%
       'WARNING: Structural problems detected in the construction of [PerfSpell_Censored] and [PerfSpell_Event] with 
          coinciding 1-values.\n')



# --- 4.3 Performance spell resolution times  (incomplete portfolio only)

# - Create event time field (not time-to-event) for modelling default survival
# Should the spell's end be earlier than the current/censoring date, then it is merely a matter of
# observing the minimum start times between a historical right-censoring event's time and the main event's time itself at the end of the spell.
# In-progress performance spells (relative to current date) are set to the age at the last known date (controlled via date.monthstart.long)
# Future performance spells not yet commenced are set to 0
datCredit_real[ExclusionID==0 & !is.na(PerfSpell_Num), PerfSpell_TimeEnd := 
                 ifelse(Date[.N] < maxDate_observed, 
                        min(.SD[PerfSpell_Censored==1 & order(Age_Adj), Age_Adj][1], Age_Adj[.N], na.rm=T),
                        ifelse(Date[1] <= maxDate_observed, 
                               .SD[Date == maxDate_observed, Age_Adj], # Return age at current date
                               0 # future performance spell that has not yet commenced
                        )), by=list(LoanID, PerfSpell_Num)]

# [SANITY CHECK] Prevalence of unexpected values in newly-created field?
check6b_real <- datCredit_real[ExclusionID==0 & !is.na(PerfSpell_Num) & PerfSpell_TimeEnd == 0, .N] / 
  datCredit_real[ExclusionID==0 & !is.na(PerfSpell_Num), .N] * 100 
cat ( (check6b_real == 0) %?% "SAFE: No unexpected zero-values in [PerfSpell_TimeEnd].\n" %:%
        paste0("WARNING: Prevalence of unexpected zero-values in [PerfSpell_TimeEnd] of ", 
               round(check6b_real,digits=2), "% of records during performing spells.\n"))

# - Create time-to-event field for modelling default survival
# NOTE: left-truncated performing spells are adjusted accordingly
datCredit_real[ExclusionID==0 & !is.na(PerfSpell_Num) & PerfSpell_TimeEnd > 0, 
               PerfSpell_Age := PerfSpell_TimeEnd - (1-PerfSpell_LeftTrunc[1])*(Age_Adj[1] - 1), 
               by=list(LoanID, PerfSpell_Num)]



# --- 4.4 Performance spell resolution types (incomplete portfolio only)

# - Create an intermediate field containing the appropriate index of the last record per spell
datCredit_real[ExclusionID==0 & !is.na(PerfSpell_Num), PerfSpell_LastRec := 
                 which(TimeInPerfSpell== PerfSpell_Age), by=list(LoanID,PerfSpell_Num)]

# - Designate the type of performance resolution for right-censored portfolios
# Select end-of-spell record for decision-making, using previous field
datCredit_real[ExclusionID==0 & !is.na(PerfSpell_Num), PerfSpellResol_Type_Hist := case_when(
  DefaultStatus1[PerfSpell_LastRec[1]] == 1 ~ "Defaulted",
  Event_Time == Age_Adj[PerfSpell_LastRec[1]] & Event_Type[.N] == "WOFF" & Account_Censored[PerfSpell_LastRec[1]] == 0 ~ "Written-off", # shouldn't happen without entering a default spell though (but just in case)
  Event_Time == Age_Adj[PerfSpell_LastRec[1]] & Event_Type[.N] == "SETTLE" & Account_Censored[PerfSpell_LastRec[1]] == 0 ~ "Settled",
  Event_Time == Age_Adj[PerfSpell_LastRec[1]] & Event_Type[.N] == "COMPLETED" & Account_Censored[PerfSpell_LastRec[1]] == 0 ~ "Paid-up",
  Account_Censored[PerfSpell_LastRec[1]] == 1 ~ "Censored"), by=list(LoanID, PerfSpell_Num)]

# [SANITY CHECK] Prevalence of unexpected values in newly-created field?
check6c_real <- datCredit_real[ExclusionID==0 & !is.na(PerfSpell_Num) & 
                                 is.na(PerfSpellResol_Type_Hist), .N] / 
  datCredit_real[ExclusionID==0 & !is.na(PerfSpell_Num), .N] * 100 
cat ( (check6c_real == 0) %?% "SAFE: No unexpected missingness in [PerfSpellResol_Type_Hist].\n" %:%
        paste0("WARNING: Prevalence of unexpected missingness in [PerfSpellResol_Type_Hist] of ", 
               round(check6c_real,digits=2), "% of records during performing spells.\n"))

# - Cleanup
datCredit_real[, PerfSpell_LastRec := NULL]



# --- 4.5. Reorder new features created thus far
datCredit_real <- datCredit_real %>% 
  # Re-order and group together fields related to terminal events or behavioural dynamics
  relocate(PerfSpell_Num, PerfSpell_Counter, TimeInPerfSpell, PerfSpell_LeftTrunc, PerfSpell_Event, PerfSpell_Censored, 
           PerfSpell_TimeEnd, PerfSpell_Age, PerfSpellResol_Type_Hist, 
           HasLeftTruncPerfSpell, .after=LossRate_Real) %>%
  relocate(PerfSpell_Key, .after=LoanID)



# ------ 5. General cleanup & checks

# [SANITY CHECK] Confirm dataset's grain
check_cred3b <- datCredit_real[,list(Freqs = .N), by=list(LoanID, Date)][Freqs > 1,.N]
cat( (check_cred3b == 0) %?% cat('SAFE: Grain of {datCredit_real} confirmed.\n') %:% 
       cat(paste0('ERROR: Grain broken in {datCredit_real} for ', check_cred3b, " cases.\n")) )

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath,"creditdata_final3"), datCredit_real)
pack.ffdf(paste0(genObjPath,"matState_g0"), matState_g0);
pack.ffdf(paste0(genObjPath,"matState_g1"), matState_real);

# - Cleanup
rm(matState_g0, matState_real, advance_adjterm_v_real); gc()

proc.time() - ptm # IGNORE: elapsed runtime



# [LOOKUP] Specific case with g1-delinquency not matching with observed [AccountStatus], ending in settlement
# Lookup <- subset(datCredit_real, LoanID == 155721) #  Treatments 1,3,7 would not have affected the instalment
# [LOOKUP] Specific left-truncated write-off case that was performing prior write-off
# Lookup <- subset(datCredit_real, ExclusionID==0 & LoanID == 134247)
# [Lookup] Specific write-off case starting in performing
# Lookup <- subset(datCredit_real, ExclusionID==0 & LoanID == 69708)
# [LOOKUP] Specific written-off new loan with multiple default spells (also PerfSpell_TimeEnd != PerfSpell_Age)
# Lookup <- subset(datCredit_real, ExclusionID==0 & LoanID == 3000012424328)
# [LOOKUP] Specific left-truncated/old-loan case that settled early
# Lookup <- subset(datCredit_real, ExclusionID == 0 & LoanID ==  13342)
# [LOOKUP] Specific old/left-truncated case starting in performing with multiple default spells
# Lookup <- subset(datCredit_real, ExclusionID == 0 & LoanID ==  145862)
# [LOOKUP] Newly disbursed loan that was subsequently written off
# Lookup <- subset(datCredit_real, ExclusionID==0 & LoanID == 
#                 unique(datCredit_real[ExclusionID == 0 & New_Ind==1 & HasWOff==T, LoanID])[1])
# [LOOKUP] Newly disbursed loan that was time-censored (active) case
# Lookup <- subset(datCredit_real, ExclusionID==0 & LoanID == 
#                   unique(datCredit_real[ExclusionID == 0 & New_Ind==1 & Account_Censored==1, LoanID])[1])
# [LOOKUP] Newly disbursed case that was settled early with multiple default spells, ending in settlement
# Lookup <- subset(datCredit_real, ExclusionID==0 & LoanID == 
#                 unique(datCredit_real[ExclusionID == 0 & New_Ind==1 & HasSettle==1 & DefSpell_Num>1, LoanID])[1])
# [LOOKUP] Case that started off in default
#Lookup <- subset(datCredit_real, ExclusionID==0 & LoanID == 
#                  unique(datCredit_real[ExclusionID == 0 & Counter==1 & DefaultStatus1==1, LoanID])[1])
# [LOOKUP] Old left-truncated loan starting in default with multiple default spells
# Lookup <- subset(datCredit_real, ExclusionID==0 & LoanID == 
#                 unique(datCredit_real[ExclusionID == 0 & New_Ind==0 & DefSpell_Num > 1 &
#                                        HasLeftCensoredDefSpell == 1,  LoanID])[1])
# [LOOKUP] Old left-truncated loan starting in performing with multiple default spells
# Lookup <- subset(datCredit_real, ExclusionID==0 & LoanID == 
#                  unique(datCredit_real[ExclusionID == 0 & New_Ind==0 & DefSpell_Num > 1 &
#                                        HasLeftCensoredDefSpell == 0,  LoanID])[1])
# [LOOKUP] Specific left-truncated case that settled at the study-end out of default
# Lookup <- subset(datCredit_real, ExclusionID==0 & LoanID == 3000001183014)
# [LOOKUP] Specific left-truncated case that defaulted at study-end
# Lookup <- subset(datCredit_real, LoanID == 3000000154495 & ExclusionID == 0)
# [LOOKUP] Specific left-truncated case that was written-off at study-end
# Lookup <- subset(datCredit_real, ExclusionID == 0 & LoanID == 3000001565979)
# [LOOKUP] Specific left-truncated case that was in performing at study-end after a default spell the previous period
#Lookup <- subset(datCredit_real, ExclusionID == 0 & LoanID == 3000001377773)
# [LOOKUP] Specific write-off case with redraws upon which the [Receipt_Inf] field was refined
# Lookup <- subset(datCredit_real, ExclusionID==0 & LoanID == 3000000184289)

# - Write a Lookup-case to Excel:
# write_xlsx(Lookup, "ExampleReal_WOff_MultipleSpells.xlsx")
