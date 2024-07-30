# ============================== SICR-MODELLIING ===============================
# SICR-model for definition 2a(iii) using Logistic Regression (LR)
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
#   - datCredit_real | enriched credit dataset (script 2d)

# -- Outputs:
#   - inputs_chosen, logit_model_chosen | Chosen input variables + trained model
#   - logistic_cutoff | cut-off scalar (chosen using Youden's cost-sensitive Index)
#   - performance_measures | various performance measures for trained model
#   - <analytics>
# ==============================================================================




# ------ 0. Setup/parameter definition

# -- Parameters used in the SICR-definition
# k: - outcome period
# s: - number of consecutive payments (stickiness)
# d: - delinquency threshold

# - Define the parameters
p.k <- 9
p.s <- 1
p.d <- 2

# - Define SICR-definition label
SICR_label <- "2a(iii)" 

# - Graphing parameters
chosenFont <- "Cambria"
dpi <- 250

# - Field names
stratifiers <- c("SICR_target_event", "Date") # Must at least include target variable used in graphing event rate
targetVar <- "SICR_target_event"
timeVar <- "Date"

# - Subsampling & resampling parameters
smp_size <- 1000000 # fixed size of downsampled set
train_prop <- 0.7 # sampling fraction for resampling scheme




# ------- 1. Remove irrelevant variables

# - Confirm prepared data after exclusions is loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4c"), tempPath)

# - Identify the columns in the dataset
colnames(datCredit_real)

# - Remove variables that will not be used in model building
suppressWarnings( datCredit_real[, `:=`(slc_past_due_amt = NULL, DefaultStatus1 = NULL, WOff_Ind = NULL, 
                                        EarlySettle_Ind = NULL, ExclusionID = NULL, FurtherLoan_AmtLog = NULL,
                                        Redrawn_AmtLog = NULL)])




# ------ 2. Define the target event and conduct some data prep

# - Create the SICR-definition based on the parameters
datCredit_real[, SICR_def := SICR_flag(g0_Delinq, d=p.d, s=p.s), by=list(LoanID)]
describe(datCredit_real$SICR_def) #ensure there are no missing values and only two distinct values (binary) - success

# - Look ahead (over k periods) and assign the SICR-event appropriately for each record
datCredit_real[, SICR_target_event := shift(SICR_def, type='lead', n=p.k), by=list(LoanID)]
# check whether k-periods have NA for each account
datCredit_real[, check_SICR_periods := ifelse(is.na(SICR_target_event), 1, 0), ]
# check the number of observations impacted
(exclusions_missing_periods <- datCredit_real[(check_SICR_periods == 1), .N] / datCredit_real[, .N] * 100)
# Number of impacted observations: 12.92%

# further checks
datCredit_real[, sum_SICR_periods := sum(check_SICR_periods), by=list(LoanID)]
datCredit_real[, flag_SICR_periods := ifelse(sum_SICR_periods < p.k, 1, 0), by=list(LoanID)]
sum(datCredit_real$flag_SICR_periods == 1)
# study an account to check what is causing the problem
check_SICR_periods <- subset(datCredit_real, LoanID == unique(datCredit_real[flag_SICR_periods == 1, LoanID])[1])
# just seem to be accounts that do not have sufficient history - will be excluded in the next step

# - Discard observations where target has NA, implying insufficient history
datCredit_real <- subset(datCredit_real, !is.na(SICR_target_event))
describe(datCredit_real$SICR_target_event)
# checked for missing target events as well as two unique binary events - success

# - Create a copy of the dataset that will be further referred to as dat_SICR_def
dat_SICR_def <- copy(datCredit_real)
rm(datCredit_real); gc()

# - Check the event rate of each class
# RECORD-LEVEL
table(dat_SICR_def$SICR_target_event) %>% prop.table()

# SICR-events:      0.66% 
# Non SICR-events:  99.34%

# ACCOUNT-LEVEL
dat_SICR_def[, HasSICR := max(SICR_target_event, na.rm=T), by=list(LoanID)]
(sicr_events_account_level <- dat_SICR_def[Counter == 1 & HasSICR == 1, .N] / dat_SICR_def[Counter == 1, .N] * 100)
(non_sicr_events_account_level <- dat_SICR_def[Counter == 1 & HasSICR == 0, .N] / dat_SICR_def[Counter == 1, .N] * 100)

# SICR-events:      15.75%
# Non SICR-events:  84.25%

# - Convert the target variable to a categorical variable for modelling
dat_SICR_def[, SICR_target_event := factor(SICR_target_event)]

# - Remove variables that will not be used in model building
colnames(dat_SICR_def)
suppressWarnings( dat_SICR_def[, `:=`(SICR_def = NULL, check_SICR_periods = NULL, sum_SICR_periods = NULL, 
                                       flag_SICR_periods = NULL, HasSICR = NULL)])

# - Clean-up
rm(check_SICR_periods); gc()




# ------- 3. Train and test datasets

# Split the data into training and validation
# The split is based on 70% training data and 30% validation data
# Need to ensure we have a balanced dataset over time to limit sampling bias
# Since this is an extremely large dataset, memory is a problem
# Therefore, subsample the train and test datasets to have 1 million observations in total

# - Firstly, resample 1000000 observations of the data - two-way stratified dataset by SICR-event and date
smp_perc <- smp_size / ( dat_SICR_def[complete.cases(mget(stratifiers)), mget(stratifiers)][,.N] ) # Implied sampling fraction for downsampling step

# - Downsample data into a set with a fixed size (using stratified sampling) before implementing resampling scheme
set.seed(1)
dat_SICR_def_resample <- dat_SICR_def %>% drop_na(all_of(stratifiers)) %>% group_by(across(all_of(stratifiers))) %>% slice_sample(prop=smp_perc) %>% as.data.table()
cat( (dat_SICR_def_resample[is.na(get(targetVar)), .N] == 0) %?% 'SAFE: No missingness in target variable.\n' %:% 
       'WARNING: Missingness detected in target variable.\n')
# safe, not missings

# - Merge the macros on
dat_SICR_def_resample <- merge_macro_info(input_dat = dat_SICR_def_resample)

# - Apply basic cross-validation resampling scheme with 2-way stratified sampling
dat_SICR_def_resample[, Ind := 1:.N] # prepare for resampling scheme

# - Implement resampling scheme using given main sampling fraction
set.seed(1)
dat_SICR_def_train_s <- dat_SICR_def_resample %>% group_by(across(all_of(stratifiers))) %>% slice_sample(prop=train_prop) %>% as.data.table()
dat_SICR_def_valid_s <- subset(dat_SICR_def_resample, !(Ind %in% dat_SICR_def_train_s$Ind)) %>% as.data.table()

# - Clean-up
dat_SICR_def_resample[, Ind := NULL]
dat_SICR_def_train_s[, Ind := NULL]
dat_SICR_def_valid_s[, Ind := NULL]

# - Check the event rate of the training and validation data sets to ensure the SICR-events are balanced
table(dat_SICR_def_train_s$SICR_target_event) %>% prop.table()
table(dat_SICR_def_valid_s$SICR_target_event) %>% prop.table()
# success - the event rates are the same

# Calculate the SICR-incidence rates for the training and validation datasets and construct a graph
# This is done to check whether we have sampling bias
# In other words, check the SICR-incidence rates for the train and test datasets across time

# - Calculate the total number of SICR-events per month
# training data
SICR_count_train <- dat_SICR_def_train_s[SICR_target_event == 1, .N, by=.(year(Date), month(Date))]
names(SICR_count_train)[names(SICR_count_train)=="N"] <- "SICR_obs_train"

all_obs_train <- dat_SICR_def_train_s[, .N, by=.(year(Date), month(Date))]
names(all_obs_train)[names(all_obs_train)=="N"] <- "all_obs_train"

# merge to calculate the proportions
SICR_rates_train <- merge(all_obs_train, SICR_count_train, by=c("year", "month"), all.x=T)
SICR_rates_train[, SICR_prop_train := SICR_obs_train/all_obs_train]

# validation data
SICR_count_valid <- dat_SICR_def_valid_s[SICR_target_event == 1, .N, by=.(year(Date), month(Date))]
names(SICR_count_valid)[names(SICR_count_valid)=="N"] <- "SICR_obs_valid"

all_obs_valid <- dat_SICR_def_valid_s[, .N, by=.(year(Date), month(Date))]
names(all_obs_valid)[names(all_obs_valid)=="N"] <- "all_obs_valid"

# merge to calculate the proportions
SICR_rates_valid <- merge(all_obs_valid, SICR_count_valid, by=c("year", "month"), all.x=T)
SICR_rates_valid[, SICR_prop_valid := SICR_obs_valid/all_obs_valid]

# - Merge all the SICR-rate data sets into one to construct a graph to check whether the SICR-incidence rates align
SICR_rates_all <- merge(SICR_rates_train, SICR_rates_valid, by=c("year", "month"), all.x=T)

# define a date variable to use in the plot
SICR_rates_all[, Date := as.Date(paste(year, month,"01",sep="-"))]

# clean-up
rm(all_obs_train, all_obs_valid, SICR_count_train, SICR_count_valid, SICR_rates_train, SICR_rates_valid); gc()

# - Now plot the proportions
# note - change the font and y-axis to percentage
plot.data_SICR_rates <- as.data.table(gather(SICR_rates_all[, list(Date, a=SICR_prop_train, b=SICR_prop_valid)
                                                            ], key="Prop", value = "Proportion", -Date))

col.v <- brewer.pal(3, "Set2")
label.vec <- c("Stratified training data set", "Stratified validation data set")
shape.v <- c(15,16)
chosenFont <- "Cambria"

ggplot(plot.data_SICR_rates, aes(x=Date, y=Proportion, colour=Prop)) + 
  theme_minimal() + 
  geom_line(aes(x=Date, y=Proportion, colour=Prop), size=0.5) + 
  geom_point(aes(x=Date, y=Proportion, colour=Prop, shape=Prop), size=2) + 
  theme(legend.position = "bottom", text=element_text(family=chosenFont)) + 
  labs(y="SICR-incidence rates for definition with parameters k=9, d=2, and s=1", x= "Time") + 
  scale_colour_manual(name="Data sets", values=col.v, labels=label.vec) + 
  scale_shape_manual(name="Data sets", values=shape.v, labels=label.vec) + 
  scale_y_continuous(breaks=pretty_breaks(), labels = percent) + 
  scale_x_date(date_breaks = "2 year", date_labels = "%b %Y") +
  ggtitle("Line graphs of SICR-incidence representativeness across different data sets") +
  theme(plot.title = element_text(hjust = 0.5))




# ------- 4. Model building for feature selection


# --- 4.1 Account-level information
# Analyse the significance of the account-level information

# - "Raw" variables
inputs_account1 <- SICR_target_event ~ Age_Adj + Term + Receipt_Inf + Balance + Redraw_Ind + Redrawn_Amt + InterestRate_Margin + 
                                       FurtherLoan_Ind + FurtherLoan_Amt + LN_TPE + AgeToTerm + BalanceToTerm

logit_model_account1 <- glm(inputs_account1, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_account1)
# The variables that are not significant are:
# FurtherLoan_Ind, FurtherLoan_Amt, AgeToTerm and BalanceToTerm

# - Log-transform variables
inputs_account2 <- SICR_target_event ~ Age_Adj + Term + Receipt_InfLog + BalanceLog + LN_TPE + AgeToTerm + BalanceToTerm + InterestRate_Margin

logit_model_account2 <- glm(inputs_account2, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_account2)
# AgeToTerm is insignificant

# - Remove the insignificant variables from the "raw" variable theme and analyse again
inputs_account3 <- SICR_target_event ~ Age_Adj + Term + Receipt_Inf + Balance + Redraw_Ind + Redrawn_Amt + InterestRate_Margin + LN_TPE

logit_model_account3 <- glm(inputs_account3, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_account3)
# All the variables are significant

# - Remove the insignificant variables from the log-transform variable theme and analyse again
inputs_account4 <- SICR_target_event ~ Age_Adj + Term + Receipt_InfLog + BalanceLog + LN_TPE + BalanceToTerm + InterestRate_Margin

logit_model_account4 <- glm(inputs_account4, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_account4)
# All the variables are significant

# - Calculate the AUC to make a decision
dat_SICR_def_train_s[, Prob_account_3 := predict(logit_model_account3, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_account_3 := predict(logit_model_account3, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_account_3) # 60.20%
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_account_3) # 60.64%

dat_SICR_def_train_s[, Prob_account_4 := predict(logit_model_account4, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_account_4 := predict(logit_model_account4, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_account_4) # 62.35%
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_account_4) # 62.48%

# Conclusion: The difference in AUC-values indiciate that the log-transform variables perform better - keep


# --- 4.2 Delinquency/performance-data
# Analyse the significance of the variables related to the delinquency/performance of the account

# - Imputed variables
inputs_delinq1 <- SICR_target_event ~ TimeInPerfSpell + PerfSpell_Num + g0_Delinq + PrevDefaults + slc_acct_arr_dir_3 + 
                                      slc_acct_roll_ever_24_imputed 

logit_model_delinq1 <- glm(inputs_delinq1, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_delinq1)
# PerfSpell_Num is insignificant, but keep to analyse with the other variables, and PrevDefaults (remove)

# - Value indicator variable
inputs_delinq2 <- SICR_target_event ~ TimeInPerfSpell + PerfSpell_Num + g0_Delinq + slc_acct_arr_dir_3 + PrevDefaults + 
                                      slc_acct_roll_ever_24_imputed:value_ind_slc_acct_roll_ever_24
                                      
logit_model_delinq2 <- glm(inputs_delinq2, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_delinq2)
# PerfSpell_Num is insignificant, but keep to analyse with the other variables, and PrevDefaults (remove)

inputs_delinq3 <- SICR_target_event ~ TimeInPerfSpell + PerfSpell_Num + g0_Delinq + slc_acct_arr_dir_3 + slc_acct_roll_ever_24_imputed 

logit_model_delinq3 <- glm(inputs_delinq3, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_delinq3)
# All the variables are significant

inputs_delinq4 <- SICR_target_event ~ TimeInPerfSpell + PerfSpell_Num + g0_Delinq + slc_acct_arr_dir_3 + 
                                      slc_acct_roll_ever_24_imputed:value_ind_slc_acct_roll_ever_24

logit_model_delinq4 <- glm(inputs_delinq4, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_delinq4)
# All the variables are significant

# - Compute the AUC to make a decision
dat_SICR_def_train_s[, Prob_delinq_3 := predict(logit_model_delinq3, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_delinq_3 := predict(logit_model_delinq3, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_delinq_3) # 80.97%
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_delinq_3) # 82.24%

dat_SICR_def_train_s[, Prob_delinq_4 := predict(logit_model_delinq4, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_delinq_4 := predict(logit_model_delinq4, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_delinq_4) # 80.96%
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_delinq_4) # 82.23%

# - Slightly higher AUC on the training dataset using the imputed variables. Look at the p-values again
summary(logit_model_delinq1)
summary(logit_model_delinq2)
# Keep the imputed variables


# --- 4.3 Behavioral variables
# Analyse the significance of the variables related to the behavior of the account

# - Imputed variables
inputs_behav1 <- SICR_target_event ~ slc_pmnt_method + slc_acct_pre_lim_perc_imputed + slc_acct_prepaid_perc_dir_12_imputed

logit_model_behav1 <- glm(inputs_behav1, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_behav1)
# slc_acct_prepaid_perc_dir_12_imputed is not significant - remove

# - Value indicator variables
inputs_behav2 <- SICR_target_event ~ slc_pmnt_method + slc_acct_pre_lim_perc_imputed:value_ind_slc_acct_pre_lim_perc + 
                                     slc_acct_prepaid_perc_dir_12_imputed:value_ind_slc_acct_prepaid_perc_dir_12

logit_model_behav2 <- glm(inputs_behav2, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_behav2)
# slc_acct_prepaid_perc_dir_12_imputed is not significant - remove

inputs_behav3 <- SICR_target_event ~ slc_pmnt_method + slc_acct_pre_lim_perc_imputed

logit_model_behav3 <- glm(inputs_behav3, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_behav3)
# All variables are significant

# - Value indicator variables
inputs_behav4 <- SICR_target_event ~ slc_pmnt_method + slc_acct_pre_lim_perc_imputed:value_ind_slc_acct_pre_lim_perc

logit_model_behav4 <- glm(inputs_behav4, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_behav4)
# All variables are significant

# - Compute the AUC to make a decision
dat_SICR_def_train_s[, Prob_behav_3 := predict(logit_model_behav3, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_behav_3 := predict(logit_model_behav3, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_behav_3) # 73.06% 
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_behav_3) # 73.97%

dat_SICR_def_train_s[, Prob_behav_4 := predict(logit_model_behav4, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_behav_4 := predict(logit_model_behav4, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_behav_4) # 73.06% 
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_behav_4) # 73.97%

# - The AUC-values are the same. Look at the p-values and standard errors
summary(logit_model_behav3)
summary(logit_model_behav4)
# p-values and standard errors are equal
# use the imputed variables to be consistent with the choice from the delinquency theme


# --- 4.4 Macroeconomic information
# - Analyse the difference in AUC using the variable itself as well as together with the volatility version
# Repo rate
inputs_macro1 <- SICR_target_event ~ M_Repo_Rate + M_Repo_Rate_1 + M_Repo_Rate_2 + M_Repo_Rate_3 + M_Repo_Rate_4 + 
                                     M_Repo_Rate_5 + M_Repo_Rate_6 + M_Repo_Rate_7 + M_Repo_Rate_8 + M_Repo_Rate_9 + 
                                     M_Repo_Rate_10 + M_Repo_Rate_11 + M_Repo_Rate_12 + M_Repo_Rate_Vol_2 + 
                                     M_Repo_Rate_Vol_3 + M_Repo_Rate_Vol_4 + M_Repo_Rate_Vol_5 + M_Repo_Rate_Vol_6 + 
                                     M_Repo_Rate_Vol_7 + M_Repo_Rate_Vol_8 + M_Repo_Rate_Vol_9 + M_Repo_Rate_Vol_10 + 
                                     M_Repo_Rate_Vol_11 + M_Repo_Rate_Vol_12

logit_model_macro1 <- glm(inputs_macro1, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_macro1)
# M_Repo_Rate_2 is significant

dat_SICR_def_train_s[, Prob_macro_1 := predict(logit_model_macro1, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_macro_1 := predict(logit_model_macro1, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_macro_1) # 57.25% 
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_macro_1) # 57.31%

inputs_macro2 <- SICR_target_event ~ M_Repo_Rate + M_Repo_Rate_1 + M_Repo_Rate_2 + M_Repo_Rate_3 + M_Repo_Rate_4 + 
                                     M_Repo_Rate_5 + M_Repo_Rate_6 + M_Repo_Rate_7 + M_Repo_Rate_8 + M_Repo_Rate_9 + 
                                     M_Repo_Rate_10 + M_Repo_Rate_11 + M_Repo_Rate_12 

logit_model_macro2 <- glm(inputs_macro2, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_macro2)
# M_Repo_Rate_2 is significant

dat_SICR_def_train_s[, Prob_macro_2 := predict(logit_model_macro2, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_macro_2 := predict(logit_model_macro2, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_macro_2) # 56.41% 
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_macro_2) # 56.36%

# The reduction in AUC is not significant but the standard errors are big for the volatility variables

# Inflation growth rate
inputs_macro3 <- SICR_target_event ~ M_Inflation_Growth + M_Inflation_Growth_1 + M_Inflation_Growth_2 + 
                                     M_Inflation_Growth_3 + M_Inflation_Growth_4 + M_Inflation_Growth_5 + 
                                     M_Inflation_Growth_6 + M_Inflation_Growth_7 + M_Inflation_Growth_8 +
                                     M_Inflation_Growth_9 + M_Inflation_Growth_10 + M_Inflation_Growth_11 + 
                                     M_Inflation_Growth_12 + M_Inflation_Growth_Vol_2 + M_Inflation_Growth_Vol_3 + 
                                     M_Inflation_Growth_Vol_4 + M_Inflation_Growth_Vol_5 + M_Inflation_Growth_Vol_6 + 
                                     M_Inflation_Growth_Vol_7 + M_Inflation_Growth_Vol_8 + M_Inflation_Growth_Vol_9 +
                                     M_Inflation_Growth_Vol_10 + M_Inflation_Growth_Vol_11 + M_Inflation_Growth_Vol_12

logit_model_macro3 <- glm(inputs_macro3, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_macro3)
# M_Inflation_Growth is significant

dat_SICR_def_train_s[, Prob_macro_3 := predict(logit_model_macro3, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_macro_3 := predict(logit_model_macro3, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_macro_3) # 57.69% 
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_macro_3) # 57.74%

inputs_macro4 <- SICR_target_event ~ M_Inflation_Growth + M_Inflation_Growth_1 + M_Inflation_Growth_2 + 
                                     M_Inflation_Growth_3 + M_Inflation_Growth_4 + M_Inflation_Growth_5 + 
                                     M_Inflation_Growth_6 + M_Inflation_Growth_7 + M_Inflation_Growth_8 +
                                     M_Inflation_Growth_9 + M_Inflation_Growth_10 + M_Inflation_Growth_11 + 
                                     M_Inflation_Growth_12 

logit_model_macro4 <- glm(inputs_macro4, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_macro4)
# M_Inflation_Growth is significant

dat_SICR_def_train_s[, Prob_macro_4 := predict(logit_model_macro4, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_macro_4 := predict(logit_model_macro4, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_macro_4) # 57.49% 
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_macro_4) # 57.54%

# The reduction in AUC is not significant but the standard errors are big for the volatility variables

# DTI growth rate
inputs_macro5 <- SICR_target_event ~ M_DTI_Growth + M_DTI_Growth_1 + M_DTI_Growth_2 + M_DTI_Growth_3 + M_DTI_Growth_4 +
                                     M_DTI_Growth_5 + M_DTI_Growth_6 + M_DTI_Growth_7 + M_DTI_Growth_8 + M_DTI_Growth_9 + M_DTI_Growth_10 +
                                     M_DTI_Growth_11 + M_DTI_Growth_12 + M_DTI_Growth_Vol_2 + M_DTI_Growth_Vol_3 + M_DTI_Growth_Vol_4 +
                                     M_DTI_Growth_Vol_5 + M_DTI_Growth_Vol_6 + M_DTI_Growth_Vol_7 + M_DTI_Growth_Vol_8 + M_DTI_Growth_Vol_9 + 
                                     M_DTI_Growth_Vol_10 + M_DTI_Growth_Vol_11 + M_DTI_Growth_Vol_12

logit_model_macro5 <- glm(inputs_macro5, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_macro5)
# M_DTI_Growth and M_DTI_Growth_12 is the most significant at 20.40%

dat_SICR_def_train_s[, Prob_macro_5 := predict(logit_model_macro5, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_macro_5 := predict(logit_model_macro5, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_macro_5) # 59.36% 
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_macro_5) # 59.46%

inputs_macro6 <- SICR_target_event ~ M_DTI_Growth + M_DTI_Growth_1 + M_DTI_Growth_2 + M_DTI_Growth_3 + M_DTI_Growth_4 +
                                     M_DTI_Growth_5 + M_DTI_Growth_6 + M_DTI_Growth_7 + M_DTI_Growth_8 + M_DTI_Growth_9 + M_DTI_Growth_10 +
                                     M_DTI_Growth_11 + M_DTI_Growth_12

logit_model_macro6 <- glm(inputs_macro6, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_macro6)
# M_DTI_Growth_12 is significant

dat_SICR_def_train_s[, Prob_macro_6 := predict(logit_model_macro6, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_macro_6 := predict(logit_model_macro6, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_macro_6) # 59.34% 
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_macro_6) # 59.42%

# The reduction in AUC is not significant but the standard errors are big for the volatility variables

# Employment growth rate 
inputs_macro7 <- SICR_target_event ~ M_Emp_Growth + M_Emp_Growth_1 + M_Emp_Growth_2 + M_Emp_Growth_3 + M_Emp_Growth_4 + 
                                     M_Emp_Growth_5 + M_Emp_Growth_6 + M_Emp_Growth_7 + M_Emp_Growth_8 + M_Emp_Growth_9 + M_Emp_Growth_10 + 
                                     M_Emp_Growth_11 + M_Emp_Growth_12 + M_Emp_Growth_Vol_2 + M_Emp_Growth_Vol_3 + M_Emp_Growth_Vol_4 + 
                                     M_Emp_Growth_Vol_5 + M_Emp_Growth_Vol_6 + M_Emp_Growth_Vol_7 + M_Emp_Growth_Vol_8 + M_Emp_Growth_Vol_9 + 
                                     M_Emp_Growth_Vol_10 + M_Emp_Growth_Vol_11 + M_Emp_Growth_Vol_12

logit_model_macro7 <- glm(inputs_macro7, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_macro7)
# M_Emp_Growth and M_Emp_Growth_12 is significant

dat_SICR_def_train_s[, Prob_macro_7 := predict(logit_model_macro7, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_macro_7 := predict(logit_model_macro7, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_macro_7) # 58.61% 
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_macro_7) # 58.66%

inputs_macro8 <- SICR_target_event ~ M_Emp_Growth + M_Emp_Growth_1 + M_Emp_Growth_2 + M_Emp_Growth_3 + M_Emp_Growth_4 + 
                                     M_Emp_Growth_5 + M_Emp_Growth_6 + M_Emp_Growth_7 + M_Emp_Growth_8 + M_Emp_Growth_9 + M_Emp_Growth_10 + 
                                     M_Emp_Growth_11 + M_Emp_Growth_12

logit_model_macro8 <- glm(inputs_macro8, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_macro8)
# M_Emp_Growth_12 is significant

dat_SICR_def_train_s[, Prob_macro_8 := predict(logit_model_macro8, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_macro_8 := predict(logit_model_macro8, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_macro_8) # 57.55% 
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_macro_8) # 57.58%

# The reduction in AUC is not significant but the standard errors are big for the volatility variables

# Real GDP growth rate
inputs_macro9 <- SICR_target_event ~ M_RealGDP_Growth + M_RealGDP_Growth_1 + M_RealGDP_Growth_2 + M_RealGDP_Growth_3 + M_RealGDP_Growth_4 + 
                                     M_RealGDP_Growth_5 + M_RealGDP_Growth_6 + M_RealGDP_Growth_7 + M_RealGDP_Growth_8 + M_RealGDP_Growth_9 + 
                                     M_RealGDP_Growth_10 + M_RealGDP_Growth_11 + M_RealGDP_Growth_12 + M_RealGDP_Growth_Vol_2 + 
                                     M_RealGDP_Growth_Vol_3 + M_RealGDP_Growth_Vol_4 + M_RealGDP_Growth_Vol_5 + M_RealGDP_Growth_Vol_6 + 
                                     M_RealGDP_Growth_Vol_7 + M_RealGDP_Growth_Vol_8 + M_RealGDP_Growth_Vol_9 + M_RealGDP_Growth_Vol_10 + 
                                     M_RealGDP_Growth_Vol_11 + M_RealGDP_Growth_Vol_12

logit_model_macro9 <- glm(inputs_macro9, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_macro9)
# M_RealGDP_Growth is significant

dat_SICR_def_train_s[, Prob_macro_9 := predict(logit_model_macro9, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_macro_9 := predict(logit_model_macro9, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_macro_9) # 59.21% 
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_macro_9) # 59.29%

inputs_macro10 <- SICR_target_event ~ M_RealGDP_Growth + M_RealGDP_Growth_1 + M_RealGDP_Growth_2 + M_RealGDP_Growth_3 + M_RealGDP_Growth_4 + 
                                      M_RealGDP_Growth_5 + M_RealGDP_Growth_6 + M_RealGDP_Growth_7 + M_RealGDP_Growth_8 + M_RealGDP_Growth_9 + 
                                      M_RealGDP_Growth_10 + M_RealGDP_Growth_11 + M_RealGDP_Growth_12

logit_model_macro10 <- glm(inputs_macro10, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_macro10)
# M_RealGDP_Growth and M_RealGDP_Growth_12 is significant

dat_SICR_def_train_s[, Prob_macro_10 := predict(logit_model_macro10, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_macro_10 := predict(logit_model_macro10, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_macro_10) # 58.58% 
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_macro_10) # 58.65%

# The reduction in AUC is more significant than the previous variables, but the standard errors are big for the volatility variables

# Real income growth
inputs_macro11 <- SICR_target_event ~ M_RealIncome_Growth + M_RealIncome_Growth_1 + M_RealIncome_Growth_2 + M_RealIncome_Growth_3 + 
                                      M_RealIncome_Growth_4 + M_RealIncome_Growth_5 + M_RealIncome_Growth_6 + M_RealIncome_Growth_7 + 
                                      M_RealIncome_Growth_8 + M_RealIncome_Growth_9 + M_RealIncome_Growth_10 + M_RealIncome_Growth_11 + 
                                      M_RealIncome_Growth_12 + M_RealIncome_Growth_Vol_2 + M_RealIncome_Growth_Vol_3 + M_RealIncome_Growth_Vol_4 + 
                                      M_RealIncome_Growth_Vol_5 + M_RealIncome_Growth_Vol_6 + M_RealIncome_Growth_Vol_7 + M_RealIncome_Growth_Vol_8 + 
                                      M_RealIncome_Growth_Vol_9 + M_RealIncome_Growth_Vol_10 + M_RealIncome_Growth_Vol_11 + M_RealIncome_Growth_Vol_12

logit_model_macro11 <- glm(inputs_macro11, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_macro11)
# M_RealIncome_Growth is significant

dat_SICR_def_train_s[, Prob_macro_11 := predict(logit_model_macro11, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_macro_11 := predict(logit_model_macro11, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_macro_11) # 59.08% 
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_macro_11) # 59.14%

inputs_macro12 <- SICR_target_event ~ M_RealIncome_Growth + M_RealIncome_Growth_1 + M_RealIncome_Growth_2 + M_RealIncome_Growth_3 + 
                                      M_RealIncome_Growth_4 + M_RealIncome_Growth_5 + M_RealIncome_Growth_6 + M_RealIncome_Growth_7 + 
                                      M_RealIncome_Growth_8 + M_RealIncome_Growth_9 + M_RealIncome_Growth_10 + M_RealIncome_Growth_11 + 
                                      M_RealIncome_Growth_12

logit_model_macro12 <- glm(inputs_macro12, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_macro12)
# M_RealIncome_Growth_12 is significant

dat_SICR_def_train_s[, Prob_macro_12 := predict(logit_model_macro12, newdata = dat_SICR_def_train_s, type="response")]
dat_SICR_def_valid_s[, Prob_macro_12 := predict(logit_model_macro12, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_macro_12) # 57.82% 
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_macro_12) # 57.86%

# The reduction in AUC is more significant than the previous variables, but the standard errors are big for the volatility variables

# Model with all the significant MVs
inputs_macro7 <- SICR_target_event ~ M_Repo_Rate + M_Repo_Rate_2 + M_Inflation_Growth + M_DTI_Growth + M_DTI_Growth_12 + 
                                     M_Emp_Growth + M_Emp_Growth_12 + M_RealGDP_Growth + M_RealGDP_Growth_12 +
                                     M_RealIncome_Growth + M_RealIncome_Growth_12


# --- 4.5 Combined information
# Analyse the all the significant variables from the modelling themes
inputs_chosen <- SICR_target_event ~ Term + Receipt_InfLog + BalanceToTerm + InterestRate_Margin +
                                     TimeInPerfSpell + PerfSpell_Num + g0_Delinq + slc_acct_arr_dir_3 + slc_acct_roll_ever_24_imputed +
                                     slc_pmnt_method + slc_acct_pre_lim_perc_imputed + M_Repo_Rate + M_Inflation_Growth + M_DTI_Growth + 
                                     M_DTI_Growth_12 + M_RealGDP_Growth + M_RealGDP_Growth_12 + M_RealIncome_Growth + M_RealIncome_Growth_12
                                     # Age_Adj + BalanceLog + LN_TPE + M_Repo_Rate_2 + M_Emp_Growth_12 + M_Emp_Growth

logit_model_chosen <- glm(inputs_chosen, data=dat_SICR_def_train_s, family="binomial")
summary(logit_model_chosen)

# Notes on the significance of the variables
# Iteration 1
# The insignificant variables are:
# Age_Adj, BalanceLog, LN_TPE
# Iteration 2
# The following variables are not significant:
# TimeInPerfSpell (but still quite significant with a p-value of 18.36%), M_Repo_Rate_2, M_Emp_Growth_12
# Iteration 3
# TimeInPerfSpell (but still quite significant with a p-value of 17.68%), repo rate (p-value of 16.48%), inflation growth rate
# (p-value of 14.12%), employment growth, real GDP growth (but the 12-month lag is significant, so keep), real income growth
# (but the 12-month lag is significant, so keep)
# Iteration 4
# TimeInPerfSpell (but still quite significant with a p-value of 17.83%), repo rate (p-value of 14.92%), inflation growth rate
# (p-value of 15.74%), real GDP growth (but the 12-month lag is significant, so keep), real income growth
# (but the 12-month lag is significant, so keep)

# - Compute the AUC
dat_SICR_def_train_s[, Prob_chosen_2a_iii := predict(logit_model_chosen, newdata = dat_SICR_def_train_s, type="response")] 
dat_SICR_def_valid_s[, Prob_chosen_2a_iii := predict(logit_model_chosen, newdata = dat_SICR_def_valid_s, type="response")]

auc(dat_SICR_def_train_s$SICR_target_event, dat_SICR_def_train_s$Prob_chosen_2a_iii) 
auc(dat_SICR_def_valid_s$SICR_target_event, dat_SICR_def_valid_s$Prob_chosen_2a_iii) 

# Report the AUC for the validation dataset
# Iteration 1's AUC: 85.72% | Iteration 2's AUC: 85.74% | Iteration 3's AUC: 85.75% | Iteration 4's AUC: 85.75%

# - Include only the numerical variables to analyse possible multicollinearity
inputs_all_mc1 <- SICR_target_event ~ Term + Receipt_InfLog + BalanceToTerm + InterestRate_Margin +
                                      TimeInPerfSpell + PerfSpell_Num + g0_Delinq + slc_acct_roll_ever_24_imputed + M_RealIncome_Growth_12 +
                                      slc_acct_pre_lim_perc_imputed + M_Repo_Rate + M_Inflation_Growth + M_DTI_Growth + 
                                      M_DTI_Growth_12 + M_RealGDP_Growth + M_RealGDP_Growth_12 + M_RealIncome_Growth  

logit_model_all_mc1 <- glm(inputs_all_mc1, data=dat_SICR_def_train_s, family="binomial")

# - Inspect for multicollinearity (use VIF > 10 as a threshold as suggested by de Jongh et al. (2015)
(vif_all_mc1 <- vif(logit_model_all_mc1))
# Only the MVs have VIF > 10, but this is to be expected




# ------- 5. Implement the final model and find the optimal cut-off


# --- 5.1 Final logit model with stabilized input variables across all the definitions

# - Confirm prepared data after exclusions is loaded into memory
if(!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4c"), tempPath)

# - Retain fields based on logit-model corresponding to this definition
varKeep <- c("LoanID", "Date", "Counter", 
             # Delinquency-theme inputs
             "g0_Delinq", "PerfSpell_Num", "TimeInPerfSpell", "slc_acct_roll_ever_24_imputed", "slc_acct_arr_dir_3",
             # Credit-themed inputs
             "BalanceLog", "InterestRate_Margin", "pmnt_method_grp", 
             "slc_acct_pre_lim_perc_imputed",  "PD_ratio"
)
datSICR <- subset(datCredit_real, select=varKeep)

# - Cleanup (Memory optimisation)
rm(datCredit_real); gc()

# - Create the SICR-definition based on the parameters [Time-consuming step]
datSICR[, SICR_def := SICR_flag(g0_Delinq, d=p.d, s=p.s), by=list(LoanID)]; gc()

# - Look ahead (over k periods) and assign the SICR-event appropriately for each record
datSICR[, SICR_target := shift(SICR_def, type='lead', n=p.k), by=list(LoanID)]

# - Discard observations where target has NA, implying insufficient history
datSICR <- subset(datSICR, !is.na(SICR_target))

# - Check the event rate of each class | RECORD-LEVEL
table(datSICR$SICR_target) %>% prop.table() # 99.33% Non-SICR vs 0.67% SICR

# - Convert the target variable to a categorical variable for modelling
datSICR[, SICR_target := factor(SICR_target)]; gc()

# - Prepare for resamling scheme
datSICR[, ind := 1:.N]

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "datSICR_", SICR_label), datSICR)


# -- Creating SICR-samples

# - Confirm SICR-dataset is loaded into memory (useful step during interactive execution)
if (!exists('datSICR')) unpack.ffdf(paste0(genPath,"datSICR_", SICR_label), tempPath)

# - Downsample data into a fixed subsample before implementing resampling scheme
smp_size <- 250000; smp_percentage <- smp_size/nrow(datSICR)
set.seed(1)
datSICR_smp <- datSICR %>% group_by(SICR_target, Date) %>% slice_sample(prop=smp_percentage) %>% as.data.table()

# - Join the macros on the filtered dataset to avoid memory constraints
datSICR_smp <- merge_macro_info(input_dat = datSICR_smp)

# - Retain fields based on logit-model corresponding to this definition
varKeep <- c("LoanID", "Date", "Counter", "SICR_target", "ind", "SICR_def",
             # Delinquency-theme inputs
             "g0_Delinq", "PerfSpell_Num", "TimeInPerfSpell", "slc_acct_roll_ever_24_imputed", "slc_acct_arr_dir_3",
             # Credit-themed inputs
             "BalanceLog", "InterestRate_Margin", "pmnt_method_grp", 
             "slc_acct_pre_lim_perc_imputed",  "PD_ratio",
             # Macroeconomic-themed inputs
             "M_Repo_Rate", "M_Inflation_Growth", 
             "M_DTI_Growth", "M_DTI_Growth_12"
)
datSICR_smp <- subset(datSICR_smp, select=varKeep)

# - Implement resampling scheme using 70% as sampling fraction
set.seed(1)
datSICR_train <- datSICR_smp %>% group_by(SICR_target, Date) %>% slice_sample(prop=0.7) %>% mutate(Sample="Train") %>% as.data.table()
datSICR_valid <- subset(datSICR_smp, !(ind %in% datSICR_train$ind)) %>% mutate(Sample="Validation")

# - Check representativeness | dataset-level proportions should be similar
table(datSICR_smp$SICR_target) %>% prop.table()
table(datSICR_train$SICR_target) %>% prop.table()
table(datSICR_valid$SICR_target) %>% prop.table()
### RESULTS: Conforms to original proportions, representativeness confirmed

# - cleanup
rm(datSICR); gc()

# - Define model form
inputs_chosen <- SICR_target ~ InterestRate_Margin + BalanceLog + pmnt_method_grp + slc_acct_pre_lim_perc_imputed + TimeInPerfSpell + PD_ratio +
                               PerfSpell_Num + g0_Delinq + slc_acct_arr_dir_3 + slc_acct_roll_ever_24_imputed + M_Repo_Rate +
                               M_Inflation_Growth + M_DTI_Growth + M_DTI_Growth_12
# Not all variables are statistically significant, including: the log-transformed balance, the missing and salary/suspense group of the payment method group,
# time in performing spell, the missing group of the account direction, repo rate and inflation growth rate
# After including PD ratio, the statistically insignificant variables are: the log-transformed balance, the missing and salary/suspense group of the payment method group,
# time in performing spell, the missing group of the account direction, repo rate and inflation growth rate
# Therefore, the inclusion of PD ratio did not change the significance of any variables
# [Ad hoc] PD ratio is not statistically significant (p-value of 0.426785 and standard error of 0.0024056)

# - Save model formula
pack.ffdf(paste0(genObjPath, "SICR_", SICR_label, "_formula_undummified"), inputs_chosen)

# - Fit final logit model
logit_model_chosen <- glm(inputs_chosen, data=datSICR_train, family="binomial")
summary(logit_model_chosen)

# - Score data using fitted model
datSICR_train[, Prob_chosen_2a_iii := predict(logit_model_chosen, newdata = datSICR_train, type="response")] 
datSICR_valid[, Prob_chosen_2a_iii := predict(logit_model_chosen, newdata = datSICR_valid, type="response")]
datSICR_smp[, ExpProb := predict(logit_model_chosen, newdata = datSICR_smp, type="response")]

# - Compute the AUC
auc(datSICR_train$SICR_target, datSICR_train$Prob_chosen_2a_iii) # 85.66% vs 85.67%
auc(datSICR_valid$SICR_target, datSICR_valid$Prob_chosen_2a_iii) # 84.61% vs 84.63%
auc(datSICR_smp$SICR_target, datSICR_smp$ExpProb) # 85.30% vs 85.31%


# --- 5.2 Plot the density of the class probabilities
# - Graphing parameters
labels.v <- c(bquote(italic(C)[0]), 
              bquote(italic(C)[1]))

# - Plot double density across both classes
ggplot( data=datSICR_valid, aes(x=Prob_chosen_2a_iii)) + theme_bw() + 
        geom_histogram(aes(y=after_stat(density), colour=factor(SICR_target), fill=factor(SICR_target)), alpha=0.7,
                       bins=round(2*datSICR_valid[,.N]^(1/3)), position="identity") + # using Rice's rule
        geom_density(aes(colour=factor(SICR_target), fill=factor(SICR_target)), linewidth=0.8, alpha=0.5) + 
        labs(x="Class probability", y="Density") + 
        theme(legend.position="bottom", text=element_text(family=chosenFont)) + 
        scale_color_brewer(palette="Dark2", name="Class", labels=labels.v) + 
        scale_fill_brewer(palette="Set2", name="Class", labels=labels.v) + 
        scale_x_continuous(breaks=pretty_breaks(), label=percent)


# --- 5.3 Find optimal points (c) according to several measures using the training data
# - Set misclassification costs for false positives (FP) and false negatives (FN) respectively
# These are only applicable to cost-sensitive measures later
cost_fp <- 1; cost_fn <- 6
cost_ratio <- cost_fn/cost_fp

# Experimented with [cost_fn] given the underprediction-problem outlined in section 6.3
# Candidates include 40, 10, 6, 7, 8 ; all of whom gave very high overprediction (decreasingly so for lower cost-values). 
#  AUC-values remained between 80-83% once discretised.
# Misclassification cost of 8 was propagated to definitions 1a(ii) and 1a(iii)
# Upon this, it was found that the resulting cut-off leads to severe overprediction
# Accordingly, an analysis was conducted to investigate the use of 7 or 6 as the misclassification cost
# The analysis revealed that as k increases, higher misclassification costs result in overprediction
# Since a constant misclassification cost is required over all the definitions, it was decided that 6 yields a good balance across all definitions
# Therefore, final value selected: 6

# - Find optimal cut-offs according to the Generalised Youden's Index measures
# This requires significant memory, which cannot be handled by the current size of the training dataset
optimal.cutpoint.GenYouden <- Gen_Youd_Ind(logit_model_chosen, datSICR_valid, "SICR_target", cost_ratio)

# - Set final cut-off
(logistic_cutoff <- optimal.cutpoint.GenYouden$cutoff)
datSICR_train[, Pred_chosen_2a_iii := ifelse(Prob_chosen_2a_iii >= logistic_cutoff, 1, 0)]
datSICR_valid[, Pred_chosen_2a_iii := ifelse(Prob_chosen_2a_iii >= logistic_cutoff, 1, 0)]
datSICR_smp[, ExpDisc := ifelse(ExpProb >= logistic_cutoff, 1, 0)]

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "datSICR_smp_", SICR_label), datSICR_smp)
pack.ffdf(paste0(genPath, "datSICR_valid_", SICR_label), datSICR_valid)
pack.ffdf(paste0(genPath, "datSICR_train_", SICR_label), datSICR_train)




# ------- 6. ROC-Analysis and overall model assessment

# --- 6.1 ROC-analysis using pROC-package
# See https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
# Mixed with https://cran.r-project.org/web/packages/ROCit/vignettes/my-vignette.html

# - Set confidence level for bootstrapping the uncertainty of AUC/Gini-measures
alpha <- 0.05

# - Confirm SICR-dataset is loaded into memory (useful step during interactive execution)
if (!exists('datSICR_valid')) unpack.ffdf(paste0(genPath,"datSICR_valid_", SICR_label), tempPath)

# - Create ROC-object | probabilities vs discrete labels
pROC_obj_chosena <- roc(formula= SICR_target~Pred_chosen_2a_iii, data=datSICR_valid, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
pROC_obj_chosenb <- roc(formula= SICR_target~Prob_chosen_2a_iii, data=datSICR_valid, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)


# --- 6.2 Compute other performance measures

# - Standard deviation
# used to represent the stability of the SICR-definition
datSICR_valid[, SICR_predict_variance := sd(Prob_chosen_2a_iii), by=list(LoanID)]
standard_deviation <- round(mean(datSICR_valid$SICR_predict_variance, na.rm=T)*100, digits=1)

# - Confusion matrix
conf_mat <- datSICR_valid[, list(TN=sum(ifelse(SICR_target == 0 & Pred_chosen_2a_iii == 0, 1, 0)),
                                 FP=sum(ifelse(SICR_target == 0 & Pred_chosen_2a_iii == 1, 1, 0)),
                                 TP=sum(ifelse(SICR_target == 1 & Pred_chosen_2a_iii == 1, 1, 0)),
                                 FN=sum(ifelse(SICR_target == 1 & Pred_chosen_2a_iii == 0, 1, 0)))]
conf_mat[, positives := TP + FN]
conf_mat[, negatives := TN + FP]

# - Sensitivity and specificity (true positive rate and true negative rate)
# True positive rate 
(true_positive_rate <- round(conf_mat$TP/conf_mat$positives*100, digits=1))

# True negative rate
(true_negative_rate <- round(conf_mat$TN/conf_mat$negatives*100, digits=1))


# --- 6.3 Calculate SICR-incidence

# - Confirm SICR-dataset is loaded into memory (useful step during interactive execution)
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
if (!exists('logistic_cutoff')) logistic_cutoff <- 0.867763

# A few things of concern:
# 1) Volatility in event rates due to relatively low sampling volumes in validation set
# 2) 0-counts over 20006-2007 periods using discretised output [Pred_chosen_2a_iii]
# 3) Trend of "underprediction" (expected red line consistently being underneath actual green line). "overprediction" would have been 
#   more palatable given our preference for greater sensitivity (T^+ rate) over low false positive rate under IFRS 9
#   But this largely comes down to cut-off selection when dealing with probabilistic classifiers ..
#   In fact, underprediction would have been a good argument to adjust cut-off accordingly, had we been in a Technical Committee

# As such, the following changes have been made:
# 1) Switched to subsampled dataset (250k) for reporting purposes, instead of the too-small validation set
# 2) Included the probabilistic output, which we'll know will be much closer to the green line
# 3) renamed some fields accordingly
datSICR_graph <- rbind(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target , Type="a_Actual")],
                       datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=ExpProb, Type="b_Modelled_prob")],
                       datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=ExpDisc, Type="c_Modelled_disc")])

# - Transform factor back to numeric variables for aggregation purposes
datSICR_graph[, SICR_events := as.numeric(levels(SICR_events))[SICR_events]]

# - Aggregate to monthly level and observe up to given point
SICR_StartDte <- min(datSICR_smp$Date, na.rm=T)
SICR_EndDte <- max(datSICR_smp$Date, na.rm=T)
port.aggr <- datSICR_graph[SICR_def==0, list(EventRate = sum(SICR_events, na.rm=T)/.N, AtRisk = .N),
                           by=list(Type, Date)][Date >= SICR_StartDte & Date <= SICR_EndDte,] %>% setkey(Type,Date)

# - Aesthetics engineering
port.aggr[, Facet_label := paste0("SICR-definition ", SICR_label)]

# - Calculate MAE over time by line graph type in summarising differences amongst line graphs
port.aggr2 <- port.aggr %>% pivot_wider(id_cols = c(Date), names_from = c(Type), values_from = c(EventRate))
(diag.Act_ExpProb <- mean(abs(port.aggr2$a_Actual - port.aggr2$b_Modelled_prob)) * 100)
(diag.Act_ExpDisc <- mean(abs(port.aggr2$a_Actual - port.aggr2$c_Modelled_disc)) * 100)

# - Calculate standard deviation of these processes
stdev_SICR_Act <- sd(port.aggr2$a_Actual, na.rm=T)
stdev_SICR_ExpProb <- sd(port.aggr2$b_Modelled_prob, na.rm=T)
stdev_SICR_ExpDisc <- sd(port.aggr2$c_Modelled_disc, na.rm=T)

# - Calculate so-called risk prudence degree to measure the degree to which the discrete expected SICR- rate exceeds the actual SICR-rate
overPredictDegree_prob <- sum(port.aggr2$b_Modelled_prob>=port.aggr2$a_Actual)/length(port.aggr2$b_Modelled_prob)
overPredictDegree_disc <- sum(port.aggr2$c_Modelled_disc>=port.aggr2$a_Actual)/length(port.aggr2$c_Modelled_disc)

# - Graphing parameters
col.v <- brewer.pal(5, "Dark2")
label.v <- c("a_Actual"=bquote(italic(A[t])*": Actual"),
             "b_Modelled_prob"=bquote(italic(B[t])*": Expected"),
             "c_Modelled_disc"=bquote(italic(C[t])*": Expected-discrete ("*italic(c)==.(round(logistic_cutoff*100,digits=1))*"%)"))

# - Create graph
(g <- ggplot(port.aggr, aes(x=Date, y=EventRate, group=Type)) + theme_minimal() + 
    labs(x="Reporting date (months)", y="Conditional SICR-rate (%)") + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90),
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Type, linetype=Type), linewidth=0.1) + 
    geom_point(aes(colour=Type, shape=Type), size=0.6) + 
    #annotations
    annotate(geom="text", x=as.Date("2015-12-31"), y=port.aggr[Date >= "2012-12-31" & Type=="a_Actual", mean(EventRate)]*1.9,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.2f", diag.Act_ExpProb),"%'"),
             family=chosenFont, size=3, parse=T) + 
    annotate(geom="text", x=as.Date("2015-12-31"), y=port.aggr[Date >= "2012-12-31" & Type=="a_Actual", mean(EventRate)]*1.7,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(C[t])*': ", sprintf("%.2f", diag.Act_ExpDisc),"%'"),
             family=chosenFont, size=3, parse=T) +     
    # facets & scale options
    facet_grid(Facet_label ~ .) + 
    scale_colour_manual(name="", values=col.v, labels=label.v) + 
    scale_shape_discrete(name="", labels=label.v) + scale_linetype_discrete(name="", labels=label.v) + 
    #guides(colour=guide_legend(nrow=2,byrow=T)) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))

# - Save graph
ggsave(g, file=paste0(genFigPath, "TimeGraph_SICR-Incidence_ActExp", SICR_label,".png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Cleanup
rm(datSICR_graph, port.aggr, port.aggr2); gc()




# ------- 7. Pack objects to disk


# --- 7.1 Performance measures

performance_measures_2a_iii <- data.frame(SICR_definition = paste0(SICR_label, "_logit"),
                                          d=p.d, s=p.s, k=p.k,
                                          AUC_prob = c(round(pROC_obj_chosenb$auc,digits=1)),
                                          CI_lower_prob = c(round(pROC_obj_chosenb$ci[1],digits=2)),
                                          CI_upper_prob = c(round(pROC_obj_chosenb$ci[3],digits=2)),
                                          cut_off = c(round(logistic_cutoff*100,digits=1)),
                                          cut_off_raw = logistic_cutoff,
                                          AUC_discrete = c(round(pROC_obj_chosena$auc,digits=1)),
                                          CI_lower_discrete = c(round(pROC_obj_chosena$ci[1],digits=2)),
                                          CI_upper_discrete = c(round(pROC_obj_chosena$ci[3],digits=2)),
                                          std_dev = c(standard_deviation),
                                          tpr = c(true_positive_rate),
                                          tnr = c(true_negative_rate),
                                          MAE_Act_ExpDisc = round(diag.Act_ExpDisc, digits=2),
                                          MAE_Act_ExpProb = round(diag.Act_ExpProb, digits=2),
                                          std_dev_SICR_rate_Act = round(stdev_SICR_Act, digits=4),
                                          std_dev_SICR_rate_ExpDisc = round(stdev_SICR_ExpDisc, digits=4),
                                          std_dev_SICR_rate_ExpProb = round(stdev_SICR_ExpProb, digits=4),
                                          OverPredict_ExpDisc = round(overPredictDegree_disc,digits=5),
                                          OverPredict_ExpProb = round(overPredictDegree_prob,digits=5),
                                          stringsAsFactors = FALSE)
pack.ffdf(paste0(genObjPath, "performance_measures_", SICR_label), performance_measures_2a_iii); gc()


# --- 7.2 Trained logit model
pack.ffdf(paste0(genPath, "logit_model_", SICR_label), logit_model_chosen); gc()


