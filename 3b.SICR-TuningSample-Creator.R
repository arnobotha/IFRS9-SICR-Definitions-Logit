# ============================== SICR-MODELLIING ===============================
# Script for testing the representativeness of a particular resampling scheme,
# using the aggregated event rate over time per sample
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
#   - 3a.SICR_def_<>_logit.R | Any script/SICR-definition within the 3a-series of scripts

# -- Inputs:
#   - datSICR | enriched credit dataset (script 3a)

# -- Outputs:
#   - <analytics>
# ==============================================================================




# ----- 0. Setup

# - Load all custom functions defined in a separate R-script
p.k <- 3
p.s <- 1
p.d <- 1

# - Define SICR-definition label
SICR_label <- "1a(i)"

# - Tuning sample size in observations
tuneSize <- 45000

# --- Confidence interval parameters
confLevel <- 0.95



# ----- 1. Sample creation from prepared SICR-dataset

# - Confirm SICR-dataset is loaded into memory (useful step during interactive execution)
if (!exists('datSICR')) unpack.ffdf(paste0(genPath,"datSICR_", SICR_label), tempPath)

# - Downsample data into a fixed subsample before implementing resampling scheme
smp_size <- 250000; smp_percentage <- smp_size/nrow(datSICR)
set.seed(1,kind="Mersenne-Twister")
datSICR_smp <- datSICR %>% group_by(SICR_target, Date) %>% slice_sample(prop=smp_percentage) %>% as.data.table()

# - Implement resampling scheme using 70% as sampling fraction
train_prop <- 0.7; set.seed(1)
datSICR_train <- datSICR_smp %>% group_by(SICR_target, Date) %>% slice_sample(prop=train_prop) %>% mutate(Sample="b_Train") %>% as.data.table()
datSICR_valid <- subset(datSICR_smp, !(ind %in% datSICR_train$ind)) %>% mutate(Sample="c_Validation")

# - Create tuning sample
datSICR_rest <- subset(datSICR, !(ind %in% datSICR_smp$ind)) #%>% filter(Date <= "2017-01-31")
set.seed(2)
datSICR_tune <- datSICR_rest %>% group_by(SICR_target, Date) %>% 
  slice_sample(prop=tuneSize/nrow(datSICR_rest)) %>% mutate(Sample="d_Tune") %>% as.data.table()



# ----- 2. SICR-incidence rate over time by sample
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

# - Aesthetics engineering
port.aggr[, Facet_label := paste0("SICR-definition ", SICR_label)]

# - calculate TTC event rate and confidence interval for one sample, dichotomous outcome (population proportion)
mean_EventRate <- port.aggr[Sample == "b_Train", mean(EventRate, na.rm=T)]
stdError_EventRate <- port.aggr[Sample == "b_Train", sd(EventRate, na.rm=T)] / port.aggr[Sample == "b_Train", .N]
margin_EventRate <- qnorm(1-(1-confLevel)/2) * stdError_EventRate
cat("\nMean event rate with 95% confidence intervals in training sample: ", sprintf("%.2f", mean_EventRate*100) , "% +-", sprintf("%.3f", margin_EventRate*100), "%")

# - Calculate MAE over time by sample
port.aggr2 <- port.aggr %>% pivot_wider(id_cols = c(Date, Sample), names_from = c(Sample), values_from = c(EventRate))
(diag.samplingRep.train <- mean(abs(port.aggr2$a_Full - port.aggr2$b_Train)) * 100)
(diag.samplingRep.valid <- mean(abs(port.aggr2$a_Full - port.aggr2$c_Validation)) * 100)
(diag.samplingRep.tune <- mean(abs(port.aggr2$a_Full - port.aggr2$d_Tune), na.rm=T) * 100)
(diag.samplingRep.trainValid <- mean(abs(port.aggr2$b_Train - port.aggr2$c_Validation)) * 100)
### RESULTS: Sample-size dependent
# 1m-sample:   Train: 0.14%, Validation: 0.22%; Tune  (9k < Dec-2017): 1.19%
# 500k-sample: Train: 0.20%; Validation: 0.31%; Tune  (9k < Dec-2017): 1.16%
# 300k-sample: Train: 0.26%; Validation: 0.40%; Tune  (9k < Dec-2017): 1.27%
# 250k-sample: Train: 0.28%; Validation: 0.43%; Tune (25k < Dec-2017): 0.69%
# 250k-sample: Train: 0.28%; Validation: 0.43%; Tune (75k < Dec-2017): 0.49%
# 250k-sample: Train: 0.28%; Validation: 0.43%; Tune (90k): 0.41%
# 250k-sample: Train: 0.28%; Validation: 0.43%; Tune (45k): 0.54%
# 250k-sample: Train: 0.28%; Validation: 0.43%; Tune (60k): 0.45%

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 170
col.v <- brewer.pal(9, "Set1")[c(1,5,2,4)]; size.v <- c(0.5,0.3,0.3,0.3)
label.v <- c("a_Full"=expression(italic(A)[t]*": Full set "*italic(D)),
             "b_Train"=bquote(italic(B)[t]*": Training set "*italic(D)[italic(T)]~"("*.(round(train_prop*smp_size/1000))*"k)"),
             "c_Validation"=bquote(italic(C)[t]*": Validation set "*italic(D)[italic(V)]~"("*.(round((1-train_prop)*smp_size/1000))*"k)"),
             "d_Tune"= bquote(italic(D)[t]*": Tuning set "*italic(D)[italic(H)]~"("*.(round(tuneSize/1000))*"k)" ))
port.sel <- port.aggr #


# - Create graph 1 (all sets)
(g2 <- ggplot(port.sel, aes(x=Date, y=EventRate, group=Sample)) + theme_minimal() + 
    labs(x="Reporting date (months)", y=bquote("Conditional SICR-rate (%) given Stage 1 and sample "*italic(bar(D)))) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Sample, linetype=Sample, size=Sample)) + 
    geom_point(aes(colour=Sample, shape=Sample), size=1) + 
    #annotations
    # annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Date <= "2008-12-31", mean(EventRate)],
    #          label=paste0("'MAE between '*italic(D)*' and '*italic(D)[italic(T)]*': ", sprintf("%.2f", diag.samplingRep.train),"%'"),
    #          family=chosenFont, size=3, parse=T) + 
    annotate("text", x=as.Date("2013-02-28"), y=port.aggr[Date <= "2008-12-31", mean(EventRate)]*1.1, size=3, family=chosenFont,
             label=paste0("'TTC-mean '*E(italic(B[t]))*': ", sprintf("%.3f", mean_EventRate*100), "% ± ", 
                          sprintf("%.3f", margin_EventRate*100),"%'"), parse=T) +     
    annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Date <= "2008-12-31", mean(EventRate)],
             label=paste0("'MAE between '*italic(B)[t]*' and '*italic(C)[t]*': ", sprintf("%.3f", diag.samplingRep.trainValid),"%'"),
             family=chosenFont, size=3, parse=T) + 
    annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Date <= "2008-12-31", mean(EventRate)]*0.95,
             label=paste0("'MAE between '*italic(B)[t]*' and '*italic(A)[t]*': ", sprintf("%.3f", diag.samplingRep.train),"%'"),
             family=chosenFont, size=3, parse=T) +     
    annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Date <= "2008-12-31", mean(EventRate)]*0.9,
             label=paste0("'MAE between '*italic(C)[t]*' and '*italic(A)[t]*': ", sprintf("%.3f", diag.samplingRep.valid),"%'"),
             family=chosenFont, size=3, parse=T) +      
    annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Date <= "2008-12-31", mean(EventRate)]*0.85,
             label=paste0("'MAE between '*italic(D)[t]*' and '*italic(A)[t]*': ", sprintf("%.3f", diag.samplingRep.tune),"%'"),
             family=chosenFont, size=3, parse=T) +      
    # facets & scale options
    facet_grid(Facet_label ~ .) + 
    scale_colour_manual(name=bquote("Sample "*italic(bar(D))), values=col.v, labels=label.v) + 
    scale_size_manual(name=bquote("Sample "*italic(bar(D))), values=size.v, labels=label.v) + 
    scale_shape_discrete(name=bquote("Sample "*italic(bar(D))), labels=label.v) + scale_linetype_discrete(name=bquote("Sample "*italic(bar(D))), labels=label.v) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))

# - Save graph
ggsave(g2, file=paste0(genFigPath, "SICR-Incidence_SampleRates_", SICR_label,"_AllSets.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")



# - Create graph 2 (excluding the tuning set)
port.sel <- port.aggr[Sample %in% c("a_Full","b_Train","c_Validation"),]

# - Create graph
(g3 <- ggplot(port.sel, aes(x=Date, y=EventRate, group=Sample)) + theme_minimal() + 
    labs(x="Reporting date (months)", y="Conditional SICR-rate (%) given Stage 1") + 
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
    # facets & scale options
    facet_grid(Facet_label ~ .) + 
    scale_colour_manual(name="Sample", values=col.v, labels=label.v) + 
    scale_size_manual(name="Sample", values=size.v, labels=label.v) + 
    scale_shape_discrete(name="Sample", labels=label.v) + scale_linetype_discrete(name="Sample", labels=label.v) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))

# - Save graph
ggsave(g3, file=paste0(genFigPath, "SICR-Incidence_SampleRates_", SICR_label,"_ExclTune.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")





# ------ 3. Dummy encoding of categorical/binary fields for SVM-fitting
# - Get SICR-model
if (!exists('inputs_chosen')) unpack.ffdf(paste0(genObjPath,"SICR_",SICR_label,"_formula_undummified"), tempPath)

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "datSICR_", SICR_label,"_tune-", round(tuneSize/1000), "k-undummified"), datSICR_tune)

# - Dummy-encoding Using custom function "dummify()".
datSICR_tune <- dummify(datGiven=datSICR_tune, modelForm = inputs_chosen)

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "datSICR_", SICR_label,"_tune-", round(tuneSize/1000), "k"), datSICR_tune)
