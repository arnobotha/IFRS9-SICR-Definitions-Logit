# ============================== SICR-MODELLIING ===============================
# SICR-model for definition 1a(i) using Support Vector Machines (SVMs)
# ------------------------------------------------------------------------------
# PROJECT TITLE: Dynamic SICR-research
# SCRIPT AUTHOR(S): Dr Arno Botha
# ------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2b.Data_Preparation_Credit.R
#   - 2c.Data_Enrich.R
#   - 2d.Data_Fusion.R

# -- Inputs:
#   - datCredit_allInputs | enriched credit dataset (script 2d)
#   - inputs_chosen | Chosen input variables from corresponding script 3a

# -- Outputs:
#   - performance_measures | various performance measures for trained model
#   - <analytics>
# ==============================================================================




# ----------- A. PRELIMINARIES

# ------ 0. Setup/parameter definition

# -- Parameters used in the SICR-definition
# k: - outcome period
# s: - number of consecutive payments (stickiness)
# d: - delinquency threshold

# - Define the parameters
p.k <- 3
p.s <- 1
p.d <- 1

# - Define SICR-definition label
SICR_label <- "1a(i)"

# - Graphing parameters
chosenFont <- "Cambria"
dpi <- 170




# ------- 1. Data loading and subsetting

# - Confirm prepared data after exclusions is loaded into memory
if (!exists('datCredit_allInputs')) unpack.ffdf(paste0(genPath,"creditdata_allinputs"), tempPath)

# - Retain fields based on logit-model corresponding to this definition
varKeep <- c("LoanID", "Date", "Counter", 
             # Delinquency-theme inputs
             "g0_Delinq", "PerfSpell_Num", "TimeInPerfSpell", "slc_acct_roll_ever_24_imputed", "slc_acct_arr_dir_3", # "PrevDefaults",
             # Credit-themed inputs
             "Receipt_InfLog","BalanceLog", "Term", "InterestRate_Margin", "pmnt_method_grp", #"BalanceToTerm",
             "slc_acct_pre_lim_perc_imputed", 
             # Macroeconomic-themed inputs
             "M_Repo_Rate", "M_Inflation_Growth", #"M_Inflation_Growth_12",
             "M_DTI_Growth", "M_DTI_Growth_12", #"M_Emp_Growth", "M_Emp_Growth_12", 
             "M_RealGDP_Growth" #, "M_RealIncome_Growth"
             #"M_Repo_Rate_Vol_12", "M_Inflation_Growth_Vol_12", "M_Emp_Growth_Vol_12", "M_RealIncome_Growth_Vol_2"
             )
datSICR <- subset(datCredit_allInputs, select=varKeep)

# - Cleanup (Memory optimisation)
rm(datCredit_allInputs); gc()



# ------ 2. Implement SICR-definition given parameters

# - Create the SICR-definition based on the parameters [Time-consuming step]
datSICR[, SICR_def := SICR_flag(g0_Delinq, d=p.d, s=p.s), by=list(LoanID)]; gc()

# - Look ahead (over k periods) and assign the SICR-event appropriately for each record
datSICR[, SICR_target := shift(SICR_def, type='lead', n=p.k), by=list(LoanID)]

# - Discard observations where target has NA, implying insufficient history
datSICR <- subset(datSICR, !is.na(SICR_target))

# - Check the event rate of each class | RECORD-LEVEL
table(datSICR$SICR_target) %>% prop.table() # 93.8% Non-SICR vs 6.2% SICR

# - Convert the target variable to a categorical variable for modelling
datSICR[, SICR_target := factor(SICR_target)]; gc()




# ------ 3. Model Form & related regression analyses

# --- Define the model form, chosen using iterative logistic regressions in dependent script
#inputs_chosen <- SICR_target ~ Receipt_InfLog + BalanceLog + Term + InterestRate_Margin + slc_pmnt_method + 
  #g0_Delinq + TimeInPerfSpell + slc_acct_roll_ever_24_imputed + PrevDefaults + # PerfSpell_Num +
  #slc_acct_pre_lim_perc_imputed + slc_acct_arr_dir_3 + 
  #M_Repo_Rate + M_Inflation_Growth_7 + M_RealGDP_Growth
  # #M_Repo_Rate_Vol_12 + M_Inflation_Growth_Vol_12 #+ M_Emp_Growth_Vol_12 + M_RealIncome_Growth_Vol_2

inputs_chosen <- SICR_target ~ Term + InterestRate_Margin + BalanceLog + # BalanceToTerm + 
  TimeInPerfSpell + PerfSpell_Num + g0_Delinq + # PrevDefaults + 
  slc_acct_arr_dir_3 + slc_acct_roll_ever_24_imputed + slc_acct_pre_lim_perc_imputed +
  pmnt_method_grp + M_Repo_Rate + M_Inflation_Growth + # M_Inflation_Growth_12 + 
  M_DTI_Growth + M_DTI_Growth_12 + M_RealGDP_Growth #+ M_Emp_Growth + M_Emp_Growth_12 +  M_RealIncome_Growth

### ---- RESULTS:
# - Iteration 1: Re-estimated model exactly as-is for a baseline
#     Strangely, [M_RealIncome_Growth_Vol_2] was not significant with high standard error
#     Attained very similar AUC (slightly higher even). Good enough as a baseline
# - Iteration 2: Removed [Redrawn_AmtLog], added [PrevDefaults] and [TimeInPerfSpell]
#     Slightly lower AIC-value (good) and [M_RealIncome_Growth_Vol_2] was even less significant
#     Same AUC, but estimates for new variables have low standard errors
# - Iteration 3: Removed [M_RealIncome_Growth_Vol_2], achieved slightly lower AIC-value.
# - Iteration 4: Updated formula based on Essie's latest work [26-May-2022]. 
#     Addition of [Term], [InterestRate_Margin], [slc_pmnt_method]
#       [M_Repo_Rate], [M_Inflation_Growth_Vol_12], [M_Emp_Growth_Vol_12], [M_RealIncome_Growth_Vol_2]
#     Removal of [value_ind_slc_acct_pre_lim_perc], [M_Repo_Rate_2]
#       [M_Inflation_Growth_Vol_8], [M_DTI_Growth_Vol_2]
#     AUC-value increased even more.
# - Iteration 5: Removal of volatility-related variables, achieved more stable coefficient estimates 
#     without sacrificing predictive power
# - Iteration 6a: Newly refined input space, stabilized across Def-1a class over all outcome periods
#     But had a few superfluous inputs still (6), to be removed. AUC: 91.32%
# - Iteration 6b: Variables removed: [M_Emp_Growth], [M_Emp_Growth_12], [M_RealIncome_Growth], 
#   [M_Inflation_Growth_12], [PrevDefaults], [BalanceToTerm]. AUC: 91.31%




# ------ 4. Apply Resampling scheme
# Simple cross-validation resampling scheme using 2-way stratified sampling across target event and Date

# - Prepare for resamling scheme
datSICR[, ind := 1:.N]

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "datSICR_", SICR_label), datSICR)


# --- Creating SICR-samples

# - Confirm SICR-dataset is loaded into memory (useful step during interactive execution)
if (!exists('datSICR')) unpack.ffdf(paste0(genPath,"datSICR_", SICR_label), tempPath)

# - Downsample data into a fixed subsample before implementing resampling scheme
smp_size <- 250000; smp_percentage <- smp_size/nrow(datSICR)
set.seed(1,kind="Mersenne-Twister")
datSICR_smp <- datSICR %>% group_by(SICR_target, Date) %>% slice_sample(prop=smp_percentage) %>% as.data.table()

# - Graphing strata frequencies
lables.v <- c("0"=paste0(0, " (", round(table(datSICR_smp$SICR_target) %>% prop.table()*100,digits=2)[1], "%)"),
              "1"=paste0(1, " (", round(table(datSICR_smp$SICR_target) %>% prop.table()*100,digits=2)[2], "%)"))
(g1 <- ggplot(datSICR_smp[,list(Freq=.N), by=list(SICR_target, Date)], aes(y=Freq,x=Date)) + theme_bw() + 
  labs(x="Reporting date (ccyymm)", y="Observation frequencies") + 
  theme(text=element_text(family=chosenFont),legend.position = "bottom",
        axis.text.x=element_text(angle=90)) +
  # main line graph with overlaid points
  geom_line(aes(colour=SICR_target, linetype=SICR_target), size=0.3) + 
  geom_point(aes(colour=SICR_target, shape=SICR_target), size=1) + 
  annotate(geom="text", x= rollforward(min(datSICR_smp$Date,na.rm=T)+days(1) + months(3)), 
           y= datSICR_smp[Date==min(Date,na.rm=T), list(Freq=.N), by=list(SICR_target)][,mean(Freq)],
           label=paste0("Strata: ",datSICR_smp[,list(Freq=.N), by=list(SICR_target, Date)][,.N]), family=chosenFont, size=3 ) + 
    annotate(geom="text", x= rollforward(min(datSICR_smp$Date,na.rm=T)+days(1) + months(11)),
             y= datSICR_smp[Date==min(Date,na.rm=T), list(Freq=.N), by=list(SICR_target)][,mean(Freq)]*0.95,
             label=paste0("Observations: ",comma(datSICR_smp[,.N])), family=chosenFont, size=3 ) +
  # facets & scale options
  scale_colour_brewer(name="SICR-event", palette="Dark2", labels=lables.v) + 
  scale_shape_discrete(name="SICR-event", labels=lables.v) + scale_linetype_discrete(name="SICR-event", labels=lables.v) + 
  scale_y_continuous(breaks=pretty_breaks(), label=comma) + 
  scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))

# - Save graph
ggsave(g1, file=paste0(genFigPath, "TimeGraph_StrataFreqs_", SICR_label,".png"), width=1200/dpi, height=1000/dpi, dpi=dpi)

# - Implement resampling scheme using 70% as sampling fraction
set.seed(1)
datSICR_train <- datSICR_smp %>% group_by(SICR_target, Date) %>% slice_sample(prop=0.7) %>% mutate(Sample="b_Train") %>% as.data.table()
datSICR_valid <- subset(datSICR_smp, !(ind %in% datSICR_train$ind)) %>% mutate(Sample="c_Validation")

# - Create tuning set
tuneSize <- 90000
datSICR_rest <- subset(datSICR, !(ind %in% datSICR_smp$ind)) # %>% filter(Date <= "2017-01-31")
set.seed(2)
datSICR_tune <- datSICR_rest %>% group_by(SICR_target, Date) %>% 
  slice_sample(prop=tuneSize/nrow(datSICR_rest)) %>% mutate(Sample="d_Tune") %>% as.data.table()

# - Check representativeness | dataset-level proportions should be similar
table(datSICR_smp$SICR_target) %>% prop.table()
table(datSICR_train$SICR_target) %>% prop.table()
table(datSICR_valid$SICR_target) %>% prop.table()
table(datSICR_tune$SICR_target) %>% prop.table()
### RESULTS: Conforms to original proportions, representativeness confirmed


# --- SICR_Incidence over time across sample | Checking representativeness over time

# - High-level statistics for article-purposes
cat(SICR_label, ": Mortgages observed from ", format(min(datSICR_smp$Date, na.rm=T)), " up to ", format(max(datSICR_smp$Date, na.rm = T)),
    ", totalling ", comma(NROW(unique(datSICR_smp$LoanID))), " accounts and ", comma(datSICR_smp[,.N]), " monthly observations, split across ",
    datSICR_smp[,list(Freq=.N), by=list(SICR_target, Date)][,.N], " strata (SICR-event x Date)" )

# - Merge samples together
datSICR_graph <- rbind(datSICR[, list(LoanID, Date, SICR_def, SICR_target, Sample="a_Full")],
                       datSICR_train[,list(LoanID, Date, SICR_def, SICR_target, Sample)], 
                       datSICR_valid[,list(LoanID, Date, SICR_def, SICR_target, Sample)], 
                       datSICR_tune[,list(LoanID, Date, SICR_def, SICR_target, Sample)])

# - Transform factor back to numeric variables for aggregation purposes
datSICR_graph[, SICR_target := as.numeric(levels(SICR_target))[SICR_target]]

# - Aggregate to monthly level and observe up to given point
SICR_StartDte <- min(datSICR$Date, na.rm=T)
SICR_EndDte <- max(datSICR$Date, na.rm=T)
port.aggr <- datSICR_graph[SICR_def==0, list(EventRate = sum(SICR_target, na.rm=T)/.N, AtRisk = .N),
                     by=list(Sample, Date)][Date >= SICR_StartDte & Date <= SICR_EndDte,] %>% setkey(Sample,Date)

# - Calculate MAE over time by sample
port.aggr2 <- port.aggr %>% pivot_wider(id_cols = c(Date, Sample), names_from = c(Sample), values_from = c(EventRate))
(diag.samplingRep.train <- mean(abs(port.aggr2$a_Full - port.aggr2$b_Train)) * 100)
(diag.samplingRep.valid <- mean(abs(port.aggr2$a_Full - port.aggr2$c_Validation)) * 100)
(diag.samplingRep.tune <- mean(abs(port.aggr2$a_Full - port.aggr2$d_Tune), na.rm=T) * 100)
### RESULTS: Sample-size dependent
# 1m-sample:   Train: 0.14%, Validation: 0.22%; Tune  (9k < Dec-2017): 1.19%
# 500k-sample: Train: 0.20%; Validation: 0.31%; Tune  (9k < Dec-2017): 1.16%
# 300k-sample: Train: 0.26%; Validation: 0.40%; Tune  (9k < Dec-2017): 1.27%
# 250k-sample: Train: 0.28%; Validation: 0.43%; Tune (25k < Dec-2017): 0.69%
# 250k-sample: Train: 0.28%; Validation: 0.43%; Tune (75k < Dec-2017): 0.49%
# 250k-sample: Train: 0.28%; Validation: 0.43%; Tune (90k): 0.41%
# 250k-sample: Train: 0.28%; Validation: 0.43%; Tune (45k): 0.54%

# - Graphing parameters
col.v <- brewer.pal(9, "Set1")[c(1,5,2,4)]; size.v <- c(0.5,0.3,0.3,0.3)
label.v <- c("a_Full"=expression("Full set "*italic(D)),
             "b_Train"=bquote("Training set "*italic(D)[italic(T)]~"("*.(round(train_prop*smp_size/1000))*"k)"),
             "c_Validation"=bquote("Validation set "*italic(D)[italic(V)]~"("*.(round((1-train_prop)*smp_size/1000))*"k)"),
             "d_Tune"= bquote("Tuning set "*italic(D)[italic(H)]~"("*.(round(tuneSize/1000))*"k)" ))
port.sel <- port.aggr # port.aggr[Sample %in% c("a_Full","b_Train","c_Validation"),]
# port.sel <- port.aggr[Sample %in% c("b_Train","c_Validation", "d_Tune"),]

# - Create graph
(g2 <- ggplot(port.sel, aes(x=Date, y=EventRate, group=Sample)) + theme_minimal() + 
    labs(x="Reporting date (ccyymm)", y="Conditional SICR-incidence rate (%) given Stage 1") + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Sample, linetype=Sample, size=Sample)) + 
    geom_point(aes(colour=Sample, shape=Sample), size=1) + 
    #annotations
    annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Date <= "2008-12-31", mean(EventRate)],
             label=paste0("'MAE between '*italic(D)*' and '*italic(D)[italic(T)]*': ", sprintf("%.2f", diag.samplingRep.train),"%'"),
             family=chosenFont, size=3, parse=T) + 
    annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Date <= "2008-12-31", mean(EventRate)]*0.95,
             label=paste0("'MAE between '*italic(D)*' and '*italic(D)[italic(V)]*': ", sprintf("%.2f", diag.samplingRep.valid),"%'"),
             family=chosenFont, size=3, parse=T) +     
    annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Date <= "2008-12-31", mean(EventRate)]*0.9,
             label=paste0("'MAE between '*italic(D)*' and '*italic(D)[italic(H)]*': ", sprintf("%.2f", diag.samplingRep.tune),"%'"),
             family=chosenFont, size=3, parse=T) +      
    # facets & scale options
    facet_grid(Facet_label ~ .) + 
    scale_colour_manual(name="Sample", values=col.v, labels=label.v) + 
    scale_size_manual(name="Sample", values=size.v, labels=label.v) + 
    scale_shape_discrete(name="Sample", labels=label.v) + scale_linetype_discrete(name="Sample", labels=label.v) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))

# - Save graph
ggsave(g2, file=paste0(genFigPath, "SICR-Incidence_SampleRates_", SICR_label,"_AllSets.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")


# --- Cleanup (memory optimisation)
rm(datSICR, datSICR_rest, datSICR_graph, port.aggr, port.aggr2, datSICR_smp, port.sel); gc()

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "datSICR_", SICR_label,"_valid"), datSICR_valid)



# ------ 5. Basic Feature Engineering

# --- Convert categorical and binary fields into dummy variables in preparing for fitting SVMs
# See https://stackoverflow.com/questions/61782970/r-e1071-svm-function-is-it-necessary-to-convert-categorical-to-dummies

# - Update model form with dummified categorical/binary variables and remove undummified ones
inputs_chosen_refined <- dummify(datGiven=datSICR_train, modelForm = inputs_chosen, returnType = "modelForm")

# - Dummy-encoding Using custom function "dummify()".
datSICR_train <- dummify(datGiven=datSICR_train, modelForm = inputs_chosen)
datSICR_valid <- dummify(datGiven=datSICR_valid, modelForm = inputs_chosen)
datSICR_tune <- dummify(datGiven=datSICR_tune, modelForm = inputs_chosen)

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "datSICR_", SICR_label,"_tune-", round(tuneSize/1000), "k"), datSICR_tune)

# - Save formulas for SVM-tuning purposes
pack.ffdf(paste0(genObjPath, "SICR_", SICR_label, "_formula"), inputs_chosen_refined)
pack.ffdf(paste0(genObjPath, "SICR_", SICR_label, "_formula_undummified"), inputs_chosen)





# ----------- B. MODELLING

# ------ 0. Re-estimate logistic regression model as baseline [EXPERIMENTAL]

# - Fit logit model and analyse ANOVA
logit_model_chosen <- glm(inputs_chosen_refined, data=datSICR_train, family="binomial")
summary(logit_model_chosen) # Wald statistics (order of inputs does not matter)

# - Score data using fitted model
datSICR_valid[, Prob_Score := predict(logit_model_chosen, newdata = datSICR_valid, type="response")]

# - Conduct brief ROC-analysis on validation set
auc(datSICR_valid$SICR_target, datSICR_valid$Prob_Score)
### RESULTS: Original AUC: 90.91%, with iterations (It) thereafter (1m-sample used by default):
# It-1: 91.1% |  It-2: 91.13% | It-: 91.13% | It-4: 91.48%
# It-5: 91.47% | It-5-500k: 91.46% | IT-5-250k: 91.24%
# It-6a-250k: 91.32%; IT-6b-250k: 91.31%



### ARNO: Rewrite everything below and model on HyperParamater Tuning project's final outcome, before propagating
# Also, perhaps deprecate this script and write a "clean-up" version hereof purely for SVM-tuning and training,
# relying on data-partitioning, cleaning, etc, done in 3a-series. Validation/formulae already packed therein.

# ------ 1a. SVM with Linear kernel K(u,v) = u \dot v | Untuned hyperparameters

ptm <- proc.time() #IGNORE: for computation time calculation
svm_model1a <- svm(inputs_chosen_refined, data=datSICR_train, type="C-classification",
                  kernel = "linear", cost=1, probability=T, scale=T); gc()
summary(svm_model1a) # ~18k support vectors
round((proc.time() - ptm)[3]/60) # IGNORE: elapsed runtime (minutes)
### RESULTS: Training times for svm() with ~20 inputs: <5 minutes (100k); <64 minutes (300k-sample);
# Inf < hours (1m-sample); <883 minutes [14.7 hours] (500k-sample); < 73 minutes (250k-sample);



# ------ 1b. Tuned SVM | mlr-package
# Hyperparamter tuning process is handled elsewhere in "HyperParamater Tuning" R-project
# Corresponds to Tuner 1a (optimising mmce) using random search

# - Load tuned hyperparameters
unpack.ffdf(paste0(genObjPath,"SICR_", SICR_label, "_tunedSVM1a"), tempPath)

# - Retrain model with tuned hyperparametes
ptm <- proc.time() #IGNORE: for computation time calculation
svm_model1b <- svm(inputs_chosen_refined, data=datSICR_train, type="C-classification",
                   kernel=tunedSVM1a$kernel, cost=tunedSVM1a$cost, 
                   gamma=tunedSVM1a$gamma, coef0=tunedSVM1a$coef0,
                   degree=tunedSVM1a$degree, probability=T, scale=T); gc()
summary(svm_model1b) # ~ 13k support vectors
round((proc.time() - ptm)[3]/(60)) # IGNORE: elapsed runtime (minutes)
### RESULTS: Training time 535 < minutes [9 hours] (250k-sample)

### ARNO: Currently running ... [05-Aug-2022: 18h23]
# Needed to rerun since scale=F was incorrectly set the first time
save.image("C:/Data/Dynamic-SICR_Data/ModellingEnv.RData")


# ------ 1c. Tuned SVM | mlr-package
# Hyperparamter tuning process is handled elsewhere in "HyperParamater Tuning" R-project
# Corresponds to Tuner 1b (optimising AUC) using random search

# - Load tuned hyperparameters
unpack.ffdf(paste0(genObjPath,"SICR_", SICR_label, "_tunedSVM1b"), tempPath)

# - Retrain model with tuned hyperparametes
ptm <- proc.time() #IGNORE: for computation time calculation
svm_model1c <- svm(inputs_chosen_refined, data=datSICR_train, type="C-classification",
                   kernel=tunedSVM1b$kernel, cost=tunedSVM1b$cost, 
                   gamma=tunedSVM1b$gamma, coef0=tunedSVM1b$coef0,
                   degree=tunedSVM1b$degree, probability=T, scale=T); gc()
summary(svm_model1c) # ~ 13k support vectors
round((proc.time() - ptm)[3]/(60)) # IGNORE: elapsed runtime (minutes)
### RESULTS: Training time  < minutes [ hours] (250k-sample)




# ----------- C. Model assessment

# --- 1. SVM-scoring
# make predictions using the trained model across the resampling scheme
svm_pred1a_untuned <- predict(svm_model1a, newdata = datSICR_valid, probability = TRUE, decision.values=T)
svm_pred1b_tuned <- predict(svm_model1b, newdata = datSICR_valid, probability = TRUE, decision.values=T)

# --- 2. Conduct brief ROC-analysis on validation set
# - Discrete output
auc(datSICR_valid$SICR_target, as.numeric(levels(svm_pred1a_untuned))[svm_pred1a_untuned])
auc(datSICR_valid$SICR_target, as.numeric(levels(svm_pred1b_tuned))[svm_pred1b_tuned])
### RESULTS: AUC: 1) 79.57%; 2)

# - Probabilities (Platt-scaling)
auc(datSICR_valid$SICR_target, attr(svm_pred1a_untuned, "probabilities")[,"1"])
auc(datSICR_valid$SICR_target, attr(svm_pred1b_tuned, "probabilities")[,"1"])
### RESULTS: AUC: 1) 78.85%; 2)



# ----------- D. Finalization
# - Save R-environment specific to this SICR-definition
save.image(paste0(genObjPath,"Backup-SICR_", SICR_label, ".RData"))
