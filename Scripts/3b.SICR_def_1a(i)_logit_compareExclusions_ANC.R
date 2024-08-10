# ============================== SICR-MODELLIING ===============================
# Comparative study of default exclusion on SICR-model using definition 1a(i)
# Ancillary script.
# ------------------------------------------------------------------------------
# PROJECT TITLE: Dynamic SICR-research
# SCRIPT AUTHOR(S): Dr Arno Botha
# ------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2b.Data_Preparation_Credit.R
#   - 2c.Data_Enrich.R
#   - 2d.Data_Fusion.R | (i)
#   - 2d.Data_Fusion_withDefs.R | (ii)
#   - 3a.PD_logit.R | Basic PD-model, from which PD-ratio is obtained for use in SICR-models

# -- Inputs:
#   - datCredit_allInputs | enriched credit dataset (script 2d(i))
#   - datCredit_allInputs_withDefs | enriched credit dataset (script 2d(ii))

# -- Outputs:
#   - datSICR_withDefs | SICR dataset with defaults
#   - <analytics>
# ==============================================================================




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
dpi <- 250




# ------- 2. Implement the final model and find the optimal cut-off | with defaults


# --- 2.1 Final logit model with stabilized input variables across all the definitions

# - Confirm prepared data after exclusions is loaded into memory
if(!exists('datCredit_allInputs_withDefs')) unpack.ffdf(paste0(genPath,"creditdata_allinputs_withDefs"), tempPath)

# - Retain fields based on logit-model corresponding to this definition
varKeep <- c("LoanID", "Date", "Counter", 
             # Delinquency-theme inputs
             "g0_Delinq", "DefaultStatus1","PerfSpell_Num", "TimeInPerfSpell", "slc_acct_roll_ever_24_imputed", "slc_acct_arr_dir_3",
             # Credit-themed inputs
             "BalanceLog", "Term", "InterestRate_Margin", "pmnt_method_grp", 
             "slc_acct_pre_lim_perc_imputed", 
             # Macroeconomic-themed inputs
             "M_Repo_Rate", "M_Inflation_Growth", 
             "M_DTI_Growth", "M_DTI_Growth_12", 
             "M_RealGDP_Growth"
)
datSICR_withDefs <- subset(datCredit_allInputs_withDefs, select=varKeep)

# - Cleanup (Memory optimisation)
rm(datCredit_allInputs_withDefs); gc()

# - Create the SICR-definition based on the parameters [Time-consuming step]
datSICR_withDefs[, SICR_def := SICR_flag(g0_Delinq, d=p.d, s=p.s), by=list(LoanID)]; gc()

# - Look ahead (over k periods) and assign the SICR-event appropriately for each record
datSICR_withDefs[, SICR_target := shift(SICR_def, type='lead', n=p.k), by=list(LoanID)]

# - create lead flag for default rate over 12-months
datSICR_withDefs[, DefaultStatus1_lead12 := shift(DefaultStatus1, n=12, type="lead"), by=list(LoanID)]

# - Discard observations where target has NA, implying insufficient history
datSICR_withDefs <- subset(datSICR_withDefs, !is.na(SICR_target))

# - Assessing the need for a back-stop to cater fo illogical cases (Defaulted loans that are not SICR-flagged)
(treatment.Effect1 <- datSICR_withDefs[DefaultStatus1==1 & SICR_def==0, .N] / datSICR_withDefs[,.N]*100 )
(treatment.Effect2 <- datSICR_withDefs[DefaultStatus1==1 & SICR_target==0, .N] / datSICR_withDefs[,.N]*100 )

# - Apply default-specific backstop onto SICR-definition to cater for defaulted loans without sufficient history
# For reporting purposes only, will be excluded during model training
datSICR_withDefs[DefaultStatus1==1 & SICR_def==0, SICR_def := 1]
datSICR_withDefs[DefaultStatus1==1 & SICR_target==0, SICR_target := 1]

# - Convert the target variable to a categorical variable for modelling
datSICR_withDefs[, SICR_target := factor(SICR_target)]; gc()

# - Prepare for resampling scheme
datSICR_withDefs[, ind := 1:.N]

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "datSICR_", SICR_label, "_withDefs"), datSICR_withDefs); gc()




# --- 2.2 Analysis: SICR & Default

# - Confirm SICR-dataset is loaded into memory (useful step during interactive execution)
if (!exists('datSICR_withDefs')) unpack.ffdf(paste0(genPath,"datSICR_", SICR_label, "_withDefs"), tempPath)

# - Distribution analysis
describe(factor(datSICR_withDefs$SICR_def)) # no missings
describe(factor(datSICR_withDefs$SICR_target)) # no missings
describe(factor(datSICR_withDefs$DefaultStatus1)) # no missings

# - Crossing SICR_target with Default
table(datSICR_withDefs$SICR_target, datSICR_withDefs$DefaultStatus1, deparse.level = 2) %>% prop.table()
# Major agreement between SICR and Default when both equals 0 (87%),
#   though there are some illogical cases, e.g., SICR = 0 but Default = 1 (<0.6%)
# [POST HOC]: Fixed

# Lookup: SICR = 0 but Default = 1
test <- subset(datSICR_withDefs, LoanID == datSICR_withDefs[SICR_target==0 & DefaultStatus1==1, LoanID][1])
# Very old account that started off in default (SICR_def=1) but cured very soon, then presumably was terminated
# There is no way for our SICR-definition to have picked up this case, given the insufficient amount of history


# - Crossing SICR_def with Default
table(datSICR_withDefs$SICR_def, datSICR_withDefs$DefaultStatus1, deparse.level = 2) %>% prop.table()
# Major agreement between SICR and Default when both equals 0 (89%),
#   though there are some illogical cases, e.g., SICR = 0 but Default = 1 (<0.4%)
# [POST HOC]: Fixed

# Lookup: SICR = 0 but Default = 1
test <- subset(datSICR_withDefs, LoanID == datSICR_withDefs[SICR_def==0 & DefaultStatus1==1, LoanID][1])
# Similar case as before. Very old loan that started off in default, then cured, then accrued some delinquency.
# The SICR-status changed accordingly, even though DefaultStatus1=1 remained constant, largely because of a probation period pre-applied.
# To rectify properly, one would probably need to apply a similar probation period onto SICR. However, this is the function of the s-parameter.
# More importantly, these are presumably cases only within default, which lies outside of the ambit of SICR-modelling (pre-default)


# - Crossing SICR_def with g0_Delinq | POST HOC (after applying backstop)
table(datSICR_withDefs$SICR_def, datSICR_withDefs$g0_Delinq, deparse.level = 2) %>% prop.table()
# Major agreement between SICR and g0_Delinq when both equals 0 (89%).
# No illogical cases (e.g., g0_Delinq > 0 while SICR=0). SICR=1 coincides mostly with g0_Delinq=1 (5.7%) and g0_Delinq>=3 (3.7%).

### CONCLUSION: Perhaps apply a simple back-stop since all of this work is only for reporting-purposes and will not affect model training itself




# --- 2.3 Downsample data into a fixed subsample before implementing resampling scheme

# - Confirm SICR-dataset is loaded into memory (useful step during interactive execution)
if (!exists('datSICR_withDefs')) unpack.ffdf(paste0(genPath,"datSICR_", SICR_label, "_withDefs"), tempPath)

smp_size <- 250000; smp_percentage <- smp_size/nrow(datSICR_withDefs)
set.seed(1,kind="Mersenne-Twister")
datSICR_smp <- subset(datSICR_withDefs, DefaultStatus1 ==0) %>% group_by(SICR_target, Date) %>% slice_sample(prop=smp_percentage) %>% as.data.table()

# - Check representativeness | dataset-level proportions should be similar
table(datSICR_withDefs$SICR_target) %>% prop.table() # 11.3% SICR-prevalence
table(datSICR_withDefs[DefaultStatus1==0,SICR_target]) %>% prop.table() # 6.8% SICR-prevalence
table(datSICR_smp$SICR_target) %>% prop.table() # 6.8% SICR-prevalence
# Significant difference between full set and full set excluding defaults, as expected




# --- 2.4. SICR-incidence rate over time by sample
# - Merge samples together
datSICR_graph <- rbind(datSICR_withDefs[, list(LoanID, Date, SICR_def, SICR_target, Sample="a_Full")],
                       datSICR_withDefs[DefaultStatus1==0, list(LoanID, Date, SICR_def, SICR_target, Sample="b_WithoutDefs")],
                       datSICR_smp[,list(LoanID, Date, SICR_def, SICR_target, Sample="c_Sample")])

# - Transform factor back to numeric variables for aggregation purposes
datSICR_graph[, SICR_target := as.numeric(levels(SICR_target))[SICR_target]]

# - Aggregate to monthly level and observe up to given point
SICR_StartDte <- min(datSICR_withDefs$Date, na.rm=T)
SICR_EndDte <- max(datSICR_withDefs$Date, na.rm=T)
port.aggr <- datSICR_graph[SICR_def==0, list(EventRate = sum(SICR_target, na.rm=T)/.N, AtRisk = .N),
                           by=list(Sample, Date)][Date >= SICR_StartDte & Date <= SICR_EndDte,] %>% setkey(Sample,Date)
plot(port.aggr[Sample=="a_Full", EventRate], type="b")
plot(datSICR_withDefs[DefaultStatus1==0, list(EventRate = sum(DefaultStatus1_lead12, na.rm=T)/.N, AtRisk = .N),
              by=list(Date)][Date >= SICR_StartDte & Date <= SICR_EndDte,][, EventRate], type="b")

# - Aesthetics engineering
port.aggr[, Facet_label := paste0("SICR-definition ", SICR_label)]

# - Calculate MAE over time by sample
port.aggr2 <- port.aggr %>% pivot_wider(id_cols = c(Date, Sample), names_from = c(Sample), values_from = c(EventRate))
(diag.samplingRep.exclDef <- mean(abs(port.aggr2$a_Full - port.aggr2$b_WithoutDefs)) * 100)
(diag.samplingRep.sample <- mean(abs(port.aggr2$a_Full - port.aggr2$c_Sample)) * 100)
### RESULTS: Sample-size dependent
# 250k-sample: without Defaults: 0.13%; Train: 0.28%
# 250k-sample: without Defaults: 0%; Train: 0.25% | Post-hoc after applying back-stop

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 170
col.v <- brewer.pal(9, "Set1")[c(1,5,2,4)]; size.v <- c(0.5,0.3,0.3,0.3)
label.v <- c("a_Full"=expression("Full set "*italic(D)), "b_WithoutDefs"=expression("Excluding defaults "*italic(D)[-italic(d)]),
             "c_Sample"=bquote("Sample "*italic(D)[italic(T)]~"("*.(round(smp_size/1000))*"k)"))
port.sel <- port.aggr[Sample %in% c("a_Full", "b_WithoutDefs", "c_Sample"),]

# - Create graph
(g1 <- ggplot(port.sel, aes(x=Date, y=EventRate, group=Sample)) + theme_minimal() + 
    labs(x="Reporting date (ccyymm)", y="Conditional SICR-rate (%) given Stage 1") + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Sample, linetype=Sample, size=Sample)) + 
    geom_point(aes(colour=Sample, shape=Sample), size=1) + 
    #annotations
    annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Date <= "2008-12-31", mean(EventRate)],
             label=paste0("'MAE between '*italic(D)*' and '*italic(D)[-italic(d)]*': ", sprintf("%.2f", diag.samplingRep.exclDef),"%'"),
             family=chosenFont, size=3, parse=T) + 
    annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Date <= "2008-12-31", mean(EventRate)]*0.95,
             label=paste0("'MAE between '*italic(D)*' and '*italic(D)[italic(S)]*': ", sprintf("%.2f", diag.samplingRep.sample),"%'"),
             family=chosenFont, size=3, parse=T) +   
    # facets & scale options
    facet_grid(Facet_label ~ .) + 
    scale_colour_manual(name="Sample", values=col.v, labels=label.v) + 
    scale_size_manual(name="Sample", values=size.v, labels=label.v) + 
    scale_shape_discrete(name="Sample", labels=label.v) + scale_linetype_discrete(name="Sample", labels=label.v) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))

# - Save graph
ggsave(g1, file=paste0(genFigPath, "SICR-Incidence_SampleRates_", SICR_label,"_ExclDefs.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")




