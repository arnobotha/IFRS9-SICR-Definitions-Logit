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
#   - datCredit_real | enriched credit dataset (script 2d)

# -- Outputs:
#   - <Basic PD-model> 
#   - <Actual vs Expected time graph of default rate>
#   - datCredit_real | Further enriched with probability scores of the basic PD-model
# ==============================================================================




# ------ 0. Setup/parameter definition


# - Graphing parameters
chosenFont <- "Cambria"
dpi <- 180

# - Field names
stratifiers <- c("DefaultStatus1_lead_12_max", "Date") # Must at least include target variable used in graphing event rate
targetVar <- "DefaultStatus1_lead_12_max"
timeVar <- "Date"

# - Subsampling & resampling parameters
smp_size <- 1000000 # fixed size of downsampled set
train_prop <- 0.7 # sampling fraction for resampling scheme




# ------- 1. Remove irrelevant variables and ensure that 12-month forward-looking indicator exists on all data

# - Confirm prepared data after exclusions is loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4b"), tempPath)

# - Remove data where 12-month forward-looking indicator could not have been created due to date restriction
datCredit_real_PD <- subset(datCredit_real, !is.na(DefaultStatus1_lead_12_max))
# - [SANITY CHECK]
cat( (anyNA(datCredit_real_PD$DefaultStatus1_lead_12_max))
     %?% paste0('WARNING: [DefaultStatus1_lead_12_max] still contains some missing values. \n') %:%
       'SAFE: [DefaultStatus1_lead_12_max] has no missing values. \n')
### RESULTS: no missingness




# ------ 2. Sample the data and then split between train and test

# - Preliminaries
smp_perc <- smp_size / ( datCredit_real_PD[complete.cases(mget(stratifiers)), mget(stratifiers)][,.N] ) # Implied sampling fraction for downsampling step

# - Downsample data into a set with a fixed size (using stratified sampling) before implementing resampling scheme
set.seed(1)
datCredit_smp <- datCredit_real_PD %>% drop_na(all_of(stratifiers)) %>% group_by(across(all_of(stratifiers))) %>% slice_sample(prop=smp_perc) %>% as.data.table()
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
rm(datCredit_real_PD); gc()




# ------ 3. Develop a basic PD-model

# - Define input variables
inputs_basic_logit <-  DefaultStatus1_lead_12_max ~ Age_Adj + Term + Principal_Real + Balance_Real + InterestRate_Margin +
                                                    slc_acct_pre_lim_perc_imputed + pmnt_method_grp
                                                    
# - Full logit model with all combined thematically selected variables
logitMod_basic <- glm(inputs_basic_logit, data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_basic)
# all variables are statistically significant

# - Calculate the AUC 
datCredit_train[, Prob_account := predict(logitMod_basic, newdata = datCredit_train, type="response")]
datCredit_valid[, Prob_account := predict(logitMod_basic, newdata = datCredit_valid, type="response")]

auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$Prob_account) # 73.66%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$Prob_account) # 73.94%

# - Evaluate fit using pseudo R^2-measures based on deviance vs null deviance
coefDeter_glm(logitMod_basic)
### RESULTS: McFadden 7.8%

# - Variable importance
varImport_logit(logitMod_basic, method="stdCoef_Goodman", sig_level=0.1, impPlot=T, plotVersionName = "PD_Basic")
### RESULTS: Top three variables: Payment method, Prepaid funds, and Balance

# - Residual deviance analysis
resid_deviance_glm(logitMod_basic)
### RESULTS: Model fit is strained (3 diagnostics gave warnings)




# ------ 4. Portfolio Analytics

# --- 1. Comparison of actual vs expected default rate

# - Add probability scores to the sub sampled set
datCredit_smp[, prob_basic := predict(logitMod_basic, newdata = datCredit_smp, type="response")]

# - Structure different line series together
datDefault_graph <- rbind(datCredit_smp[, list(LoanID, Date, DefaultStatus1, Default_target=DefaultStatus1_lead_12_max , Type="a_Actual")],
                          datCredit_smp[, list(LoanID, Date, DefaultStatus1, Default_target=prob_basic, Type="b_Modelled_prob")])

# - Aggregate to monthly level and observe up to given point
Def_StartDte <- min(datCredit_smp$Date, na.rm=T)
Def_EndDte <- max(datCredit_smp$Date, na.rm=T)
port.aggr <- datDefault_graph[DefaultStatus1==0,list(EventRate = sum(Default_target, na.rm=T)/.N, AtRisk = .N),
                        by=list(Type,Date)][Date >= Def_StartDte & Date <= Def_EndDte,] %>% setkey(Type,Date)

# - Aesthetics engineering
port.aggr[, Facet_label := "Worst-ever aggregation approach"]

# - Calculate MAE over time by line graph type in summarising differences amongst line graphs
port.aggr2 <- port.aggr %>% pivot_wider(id_cols = c(Date), names_from = c(Type), values_from = c(EventRate))
(diag.Default_Act_ExpProb <- mean(abs(port.aggr2$a_Actual - port.aggr2$b_Modelled_prob)) * 100)

# - Calculate standard deviation of these processes
stdev_Def_Act <- sd(port.aggr2$a_Actual, na.rm=T)
stdev_Def_ExpProb <- sd(port.aggr2$b_Modelled_prob, na.rm=T)

# - Calculate so-called risk prudence degree to measure the degree to which the expected default rate exceeds the actual default rate over time
overPredictDegree_prob <- sum(port.aggr2$b_Modelled_prob>=port.aggr2$a_Actual)/length(port.aggr2$b_Modelled_prob)

# - Graphing parameters
vCol <- brewer.pal(5, "Set1")
vLabel <- c("a_Actual"=bquote(italic(A[t])*": Actual event rate"),
             "b_Modelled_prob"=bquote(italic(B[t])*": Expected event rate"))

# - Create graph
(g <- ggplot(port.aggr, aes(x=Date, y=EventRate, group=Type)) + theme_minimal() + 
    labs(x="Reporting date (months)", y="Conditional 12-month default rate (%)") + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90),
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Type, linetype=Type), linewidth=0.4) + 
    geom_point(aes(colour=Type, shape=Type), size=1.2) + 
    #annotations
    annotate(geom="text", x=as.Date("2015-12-31"), y=port.aggr[Date >= "2012-12-31" & Type=="a_Actual", mean(EventRate)]*1.9,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.2f", diag.Default_Act_ExpProb),"%'"),
             family=chosenFont, size=3, parse=T) + 
    # facets & scale options
    facet_grid(Facet_label ~ .) + 
    scale_colour_manual(name="", values=vCol, labels=vLabel) + 
    scale_shape_discrete(name="", labels=vLabel) + scale_linetype_discrete(name="", labels=vLabel) + 
    #guides(colour=guide_legend(nrow=2,byrow=T)) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))

# - Save graph
ggsave(g, file=paste0(genFigPath, "TimeGraph_DefaultRate_ActExp.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Cleanup
rm(datDefault_graph, port.aggr, port.aggr2, g, vLabel); gc()





# ------ 5. Apply the basic PD-model on the full dataset to use at a later stage

# - Apply the basic PD-model on the full dataset
datCredit_real[, PD_score := predict(logitMod_basic, newdata = datCredit_real, type="response")]
datCredit_real[, PD_margin := PD_score - PD_score[1], by=list(LoanID)]
datCredit_real[, PD_ratio := PD_score / PD_score[1], by=list(LoanID)]
# basic analysis of newly-created variables
describe(datCredit_real$PD_score); hist(datCredit_real$PD_score, breaks="FD")
describe(datCredit_real$PD_margin)
describe(datCredit_real$PD_ratio)
### RESULTS: Distributional analysis are reasonable, though heavily right-skewed as expected.



# ------- 6. Pack objects to disk

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "creditdata_final4c"), datCredit_real)

# - Cleanup
rm(datCredit_smp, datCredit_train, datCredit_valid, logitMod_basic); gc()

