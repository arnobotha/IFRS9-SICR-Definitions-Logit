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

# - Field names
stratifiers <- c("DefaultStatus1_lead_12_max", "Date") # Must at least include target variable used in graphing event rate
targetVar <- "DefaultStatus1_lead_12_max"
timeVar <- "Date"

# - Subsampling & resampling parameters
smp_size <- 1000000 # fixed size of downsampled set
train_prop <- 0.7 # sampling fraction for resampling scheme




# ------- 1. Remove irrelevant variables and ensure that 12-month forward-looking indicator exists on all data

# - Confirm prepared data after exclusions is loaded into memory
if (!exists('datCredit_allInputs')) unpack.ffdf(paste0(genPath,"creditdata_allinputs"), tempPath)

# - Remove data where 12-month forward-looking indicator could not have been created due to date restriction
datCredit_allInputs_PD <- subset(datCredit_allInputs, !is.na(DefaultStatus1_lead_12_max))
describe(datCredit_allInputs_PD$DefaultStatus1_lead_12_max)
# confirmed, no missingness




# ------ 2. Sample the data and then split between train and test

# - Preliminaries
smp_perc <- smp_size / ( datCredit_allInputs_PD[complete.cases(mget(stratifiers)), mget(stratifiers)][,.N] ) # Implied sampling fraction for downsampling step

# - Downsample data into a set with a fixed size (using stratified sampling) before implementing resampling scheme
set.seed(1)
datCredit_smp <- datCredit_allInputs_PD %>% drop_na(all_of(stratifiers)) %>% group_by(across(all_of(stratifiers))) %>% slice_sample(prop=smp_perc) %>% as.data.table()
cat( (datCredit_smp[is.na(get(targetVar)), .N] == 0) %?% 'SAFE: No missingness in target variable.\n' %:% 
       'WARNING: Missingness detected in target variable.\n')

# - Apply basic cross-validation resampling scheme with 2-way stratified sampling
datCredit_smp[, Ind := 1:.N] # prepare for resampling scheme

# - Implement resampling scheme using given main sampling fraction
set.seed(1)
datCredit_train <- datCredit_smp %>% group_by(across(all_of(stratifiers))) %>% slice_sample(prop=train_prop) %>% as.data.table()
datCredit_valid <- subset(datCredit_smp, !(Ind %in% datCredit_train$Ind)) %>% as.data.table()

# - Clean up
datCredit_train[,Ind:=NULL]; datCredit_valid[,Ind:=NULL]

# - [SANITY CHECK] Ensuring that the resampling scheme reconstitutes the full (subsampled) dataset
cat( (datCredit_smp[,.N] == datCredit_train[,.N] + datCredit_valid[,.N]) %?% "SAFE: Resampling scheme implemented successfully\n" %:%
       "WARNING: Resampling scheme not implemented successfully.\n")
# successful implementation

# - Check the event rate of the training and validation data sets to ensure the SICR-events are balanced
table(datCredit_train$DefaultStatus1_lead_12_max) %>% prop.table()
table(datCredit_valid$DefaultStatus1_lead_12_max) %>% prop.table()
# success - the event rates are the same

# - Clean-up
rm(datCredit_allInputs_PD)




# ------ 3. Develop a basic PD-model

# - Define input variables
inputs_basic_logit <-  DefaultStatus1_lead_12_max ~ AgeToTerm + Term + Principal + Balance + InterestRate_Margin +
                                                    slc_acct_pre_lim_perc_imputed + pmnt_method_grp
                                                    
# - Full logit model with all combined thematically selected variables
logitMod_basic <- glm(inputs_basic_logit, data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_basic)
# all variables are statistically significant

# - Calculate the AUC 
datCredit_train[, Prob_account := predict(logitMod_basic, newdata = datCredit_train, type="response")]
datCredit_valid[, Prob_account := predict(logitMod_basic, newdata = datCredit_valid, type="response")]

auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$Prob_account) # 73.26%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$Prob_account) # 73.57%

# - Variable importance
varImport_logit(logitMod_basic, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top three variables: Payment method, Prepaid funds, and Balance

# - Calculate the total number of default-events per month
# training data
def_count_train <- datCredit_train[DefaultStatus1_lead_12_max == 1, .N, by=.(year(Date), month(Date))]
names(def_count_train)[names(def_count_train)=="N"] <- "default_obs_train"

all_obs_train <- datCredit_train[, .N, by=.(year(Date), month(Date))]
names(all_obs_train)[names(all_obs_train)=="N"] <- "all_obs_train"

# merge to calculate the proportions
default_rates_train <- merge(all_obs_train, def_count_train, by=c("year", "month"), all.x=T)
default_rates_train[, def_prop_train := default_obs_train/all_obs_train]

# validation data
def_count_valid <- datCredit_valid[DefaultStatus1_lead_12_max == 1, .N, by=.(year(Date), month(Date))]
names(def_count_valid)[names(def_count_valid)=="N"] <- "def_obs_valid"

all_obs_valid <- datCredit_valid[, .N, by=.(year(Date), month(Date))]
names(all_obs_valid)[names(all_obs_valid)=="N"] <- "all_obs_valid"

# merge to calculate the proportions
default_rates_valid <- merge(all_obs_valid, def_count_valid, by=c("year", "month"), all.x=T)
default_rates_valid[, def_prop_valid := def_obs_valid/all_obs_valid]

# - Merge all the default-rate data sets into one to construct a graph to check whether the SICR-incidence rates align
def_rates_all <- merge(default_rates_train, default_rates_valid, by=c("year", "month"), all.x=T)

# define a date variable to use in the plot
def_rates_all[, Date := as.Date(paste(year, month,"01",sep="-"))]

# clean-up
rm(all_obs_train, all_obs_valid, def_count_train, def_count_valid, default_rates_train, default_rates_valid); gc()

# - Now plot the proportions
# note - change the font and y-axis to percentage
plot.data_def_rates <- as.data.table(gather(def_rates_all[, list(Date, a=def_prop_train, b=def_prop_valid)
], key="Prop", value = "Proportion", -Date))

col.v <- brewer.pal(3, "Set2")
label.vec <- c("Stratified training data set", "Stratified validation data set")
shape.v <- c(15,16)

ggplot(plot.data_def_rates, aes(x=Date, y=Proportion, colour=Prop)) + 
  theme_minimal() + 
  geom_line(aes(x=Date, y=Proportion, colour=Prop), size=0.5) + 
  geom_point(aes(x=Date, y=Proportion, colour=Prop, shape=Prop), size=2) + 
  theme(legend.position = "bottom", text=element_text(family=chosenFont)) + 
  labs(y="Default-incidence rates", x= "Time") + 
  scale_colour_manual(name="Data sets", values=col.v, labels=label.vec) + 
  scale_shape_manual(name="Data sets", values=shape.v, labels=label.vec) + 
  scale_y_continuous(breaks=pretty_breaks(), labels = percent) + 
  scale_x_date(date_breaks = "2 year", date_labels = "%b %Y") +
  ggtitle("Line graphs of default-incidence representativeness across different data sets") +
  theme(plot.title = element_text(hjust = 0.5))

### RESULTS: Rates are the same over time for train and test
#EO: Actuals vs expected graphing logic in 4a
# EO: house keeping, maak seker OneDrive is gesync, copy script folder en paste in my eie folder op machine




# ------ 4. Apply the basic PD-model on the full dataset to use at a later stage

# - Apply the basic PD-model on the full dataset
datCredit_allInputs[, PD_score := predict(logitMod_basic, newdata = datCredit_allInputs, type="response")]
datCredit_allInputs[, PD_margin := PD_score - PD_score[1], by=list(LoanID)]
datCredit_allInputs[, PD_ratio := PD_score / PD_score[1], by=list(LoanID)]
# basic analysis of newly-created variables
describe(datCredit_allInputs$PD_score)
describe(datCredit_allInputs$PD_margin)
describe(datCredit_allInputs$PD_ratio)




# ------- 5. Pack objects to disk

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "creditdata_allinputs"), datCredit_allInputs)
gc()



