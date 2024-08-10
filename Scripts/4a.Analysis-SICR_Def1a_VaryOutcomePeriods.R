# ============================== SICR-DEFINITION ANALYSIS ===============================
# Comparing the SICR-models and their results across various SICR-definitions.
# In particular, we analyse definition class 1a and compare results across k-parameter
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Dynamic SICR-research
# SCRIPT AUTHOR(S): Dr Arno Botha
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2b.Data_Preparation_Credit.R
#   - 2c.Data_Enrich.R
#   - 2d.Data_Fusion.R 
#   - 3a.PD_logit.R | Basic PD-model, from which PD-ratio is obtained for use in SICR-models
#   - 3b.SICR_def_<>_logit.R | the 3b-series of scripts for definitions 1a(i)-(vii)

# -- Inputs:
#   - performance_measures_<> | aggregated performance measures for given SICR-definition (3b)
#   - datSICR_smp_<> | specific SICR-sample upon which resampling scheme is applied (3b)

# -- Outputs:
#   - <analytics>
#   - ROC-analyses
# =======================================================================================




# ------ 1. Performance measure analysis across SICR-definitions

# - Load into memory
if (!exists('performance_measures_1a_i')) unpack.ffdf(paste0(genObjPath,"performance_measures_1a(i)"), tempPath)
if (!exists('performance_measures_1a_ii')) unpack.ffdf(paste0(genObjPath,"performance_measures_1a(ii)"), tempPath)
if (!exists('performance_measures_1a_iii')) unpack.ffdf(paste0(genObjPath,"performance_measures_1a(iii)"), tempPath)
if (!exists('performance_measures_1a_iv')) unpack.ffdf(paste0(genObjPath,"performance_measures_1a(iv)"), tempPath)
if (!exists('performance_measures_1a_v')) unpack.ffdf(paste0(genObjPath,"performance_measures_1a(v)"), tempPath)
if (!exists('performance_measures_1a_vi')) unpack.ffdf(paste0(genObjPath,"performance_measures_1a(vi)"), tempPath)
if (!exists('performance_measures_1a_vii')) unpack.ffdf(paste0(genObjPath,"performance_measures_1a(vii)"), tempPath)

# - Merge datasets together
performance_measures <- data.table(rbind(performance_measures_1a_i, performance_measures_1a_ii, performance_measures_1a_iii,
                              performance_measures_1a_iv, performance_measures_1a_v, performance_measures_1a_vi,
                              performance_measures_1a_vii), key="SICR_definition")

# - Cleanup
rm(performance_measures_1a_i, performance_measures_1a_ii, performance_measures_1a_iii,
   performance_measures_1a_iv, performance_measures_1a_v, performance_measures_1a_vi, performance_measures_1a_vii); gc()

# - Analysis
performance_measures
### RESULTS: 
# 1) It seems like the model has a decreasing accuracy as k increases. This is actually consistent with both works of Harris2013
#   when he too varied the outcome period. Cross-sectional models become less accurate as the outcome window is increased.
# 2) This suffering accuracy is also reflected in the widening confidence interval for AUC as k increases, which attests to increasing 
#    "noise" or uncertainty/variance in trying to gauge the AUC even. For k=3, confidence width is 91.1-90.7 (0.4), yet for 
#     k=18, that width has increased to 0.8 % points.
# 3) Even though the cut-off is definition-specific (Youden's cost-sensitive index), the discrete variant's AUC follows a similar yet
#    more pronounced pattern in accuracy degradation.
# 4) Despite the degradation in accuracy as k increases, the stability of model output increases, as evidenced by [std_dev] or 
#     'flexibility' \omega in main text. This is in line with the bias-variance trade-off in statistical learning.

sprintf("%.2f", (performance_measures$CI_upper_prob - performance_measures$CI_lower_prob)/2)
sprintf("%.2f",(performance_measures$CI_upper_discrete - performance_measures$CI_lower_discrete)/2)
# summary plots over k
plot(performance_measures$k, performance_measures$AUC_prob, type="b")
plot(performance_measures$k, (performance_measures$AUC_prob^-1), type="b")
plot(performance_measures$k, performance_measures$cut_off_raw, type="b")
plot(performance_measures$k, performance_measures$AUC_discrete, type="b")
plot(performance_measures$k, performance_measures$std_dev, type="b") # called "flexibility \omega" in main text
plot(performance_measures$k, performance_measures$std_dev_SICR_rate_Act, type="b") # called "instability \sigma" in main text
plot(performance_measures$k, performance_measures$std_dev_SICR_rate_ExpProb, type="b") # very similar to actual-variant, can discard therefore
plot(performance_measures$k, performance_measures$MAE_Act_ExpProb, type="b")
plot(performance_measures$k, performance_measures$OverPredict_ExpDisc, type="b") # some linearity
plot(performance_measures$k, performance_measures$OverPredict_ExpProb, type="b") # no apparent pattern
# difference plots over k
plot(diff(performance_measures$AUC_prob)/performance_measures$AUC_prob[1:6] , x=performance_measures$k[2:7], xlab="SICR-Definitions 1a: k", ylab="AUC-difference (%) from previous definition [prob]", type="b")
plot(diff(performance_measures$AUC_discrete)/performance_measures$AUC_discrete[1:6], x=performance_measures$k[2:7], xlab="SICR-Definitions 1a: k", ylab="AUC-difference (%) from previous definition [discrete]", type="b")

# - Grand mean of MAE (Actual vs Expected probability)
percent(mean(performance_measures[1:6, MAE_Act_ExpProb])/100, accuracy=0.01) # 0.44%
percent(mean(performance_measures[1:4, MAE_Act_ExpProb])/100, accuracy=0.01) # 0.43% (not used)

# - Grand mean of MAE (Actual vs Expected discrete prediction)
percent(mean(performance_measures[1:7, MAE_Act_ExpDisc])/100, accuracy=0.01) # 1.11%

# - Grand mean of AUC
percent(mean(performance_measures[1:6, AUC_prob])/100, accuracy=0.1) # 85.65%

# - Store results
write_xlsx(x=performance_measures,path=paste0(genObjPath, "PerfMeasures_1a.xlsx"))
pack.ffdf(paste0(genObjPath, "PerformanceMeasures_1a"), performance_measures); gc()





# ------ 2. SICR-Incidence rates over time across SICR-definitions: (d=1,s=1, k) | Varying k-parameter

# --- 1. Load each sample into memory and bind together successively
SICR_label <- "1a(i)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR_1a <- copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target , SICR_Def=SICR_label)]); rm(datSICR_smp)
SICR_label <- "1a(ii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR_1a <- rbind(datSICR_1a, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label)])); rm(datSICR_smp)
SICR_label <- "1a(iii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR_1a <- rbind(datSICR_1a, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label)])); rm(datSICR_smp)
SICR_label <- "1a(iv)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR_1a <- rbind(datSICR_1a, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label)])); rm(datSICR_smp)
SICR_label <- "1a(v)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR_1a <- rbind(datSICR_1a, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label)])); rm(datSICR_smp)
SICR_label <- "1a(vi)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR_1a <- rbind(datSICR_1a, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label)])); rm(datSICR_smp)
SICR_label <- "1a(vii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR_1a <- rbind(datSICR_1a, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label)])); rm(datSICR_smp)



# --- 2. Graph data preparation and aggregation

# - Transform factor back to numeric variables for aggregation purposes
datSICR_1a[, SICR_events := as.numeric(levels(SICR_events))[SICR_events]]

# - Aggregate to monthly level and observe up to given point
SICR_StartDte <- min(datSICR_1a$Date, na.rm=T)
SICR_EndDte <- max(datSICR_1a$Date, na.rm=T)
port.aggr <- datSICR_1a[SICR_def==0,list(EventRate = sum(SICR_events, na.rm=T)/.N, AtRisk = .N),
                     by=list(SICR_Def, Date)][Date >= SICR_StartDte & Date <= SICR_EndDte,] %>% setkey(SICR_Def,Date)
describe(port.aggr$SICR_Def)

# - Recast/pivot data in order to create useful summaries across all time series
port.aggr2 <- port.aggr %>% pivot_wider(id_cols = c(Date), names_from = c(SICR_Def), values_from = c(EventRate)) %>% as.data.table()

# - Calculate useful summaries over rate time series
stdevs_1a_full <- c(sd(port.aggr2$`1a(i)`, na.rm=T), sd(port.aggr2$`1a(ii)`, na.rm=T), sd(port.aggr2$`1a(iii)`, na.rm=T),
  sd(port.aggr2$`1a(iv)`, na.rm=T), sd(port.aggr2$`1a(v)`, na.rm=T), sd(port.aggr2$`1a(vi)`, na.rm=T), sd(port.aggr2$`1a(vii)`, na.rm=T))
stdevs_1a_gfc <- c(port.aggr2[Date <= "2009-12-31", sd(`1a(i)`,na.rm=T)], port.aggr2[Date <= "2009-12-31", sd(`1a(ii)`,na.rm=T)], 
  port.aggr2[Date <= "2009-12-31", sd(`1a(iii)`,na.rm=T)], port.aggr2[Date <= "2009-12-31", sd(`1a(iv)`,na.rm=T)],
  port.aggr2[Date <= "2009-12-31", sd(`1a(v)`,na.rm=T)], port.aggr2[Date <= "2009-12-31", sd(`1a(vi)`,na.rm=T)],
  port.aggr2[Date <= "2009-12-31", sd(`1a(vii)`,na.rm=T)])
stdevs_1a_postgfc <- c(port.aggr2[Date > "2009-12-31", sd(`1a(i)`,na.rm=T)], port.aggr2[Date > "2009-12-31", sd(`1a(ii)`,na.rm=T)], 
                   port.aggr2[Date > "2009-12-31", sd(`1a(iii)`,na.rm=T)], port.aggr2[Date > "2009-12-31", sd(`1a(iv)`,na.rm=T)],
                   port.aggr2[Date > "2009-12-31", sd(`1a(v)`,na.rm=T)], port.aggr2[Date > "2009-12-31", sd(`1a(vi)`,na.rm=T)],
                   port.aggr2[Date > "2009-12-31", sd(`1a(vii)`,na.rm=T)])
first_1a <- c(port.aggr2[, `1a(i)`[1]], port.aggr2[, `1a(ii)`[1]], port.aggr2[, `1a(iii)`[1]],
              port.aggr2[, `1a(iv)`[1]], port.aggr2[, `1a(v)`[1]], port.aggr2[, `1a(vi)`[1]], port.aggr2[, `1a(vii)`[1]])
max_1a <- c(max(port.aggr2$`1a(i)`, na.rm=T), max(port.aggr2$`1a(ii)`, na.rm=T), max(port.aggr2$`1a(iii)`, na.rm=T),
            max(port.aggr2$`1a(iv)`, na.rm=T), max(port.aggr2$`1a(v)`, na.rm=T), max(port.aggr2$`1a(vi)`, na.rm=T), max(port.aggr2$`1a(vii)`, na.rm=T))
max_locales_1a <- c( port.aggr2[, Date[which(`1a(i)`==max(`1a(i)`,na.rm=T))]], port.aggr2[, Date[which(`1a(ii)`==max(`1a(ii)`,na.rm=T))]],
                     port.aggr2[, Date[which(`1a(iii)`==max(`1a(iii)`,na.rm=T))]], port.aggr2[, Date[which(`1a(iv)`==max(`1a(iv)`,na.rm=T))]],
                     port.aggr2[, Date[which(`1a(v)`==max(`1a(v)`,na.rm=T))]], port.aggr2[, Date[which(`1a(vi)`==max(`1a(vi)`,na.rm=T))]],
                     port.aggr2[, Date[which(`1a(vii)`==max(`1a(vii)`,na.rm=T))]])
mean_1a <- c(mean(port.aggr2$`1a(i)`, na.rm=T), mean(port.aggr2$`1a(ii)`, na.rm=T), mean(port.aggr2$`1a(iii)`, na.rm=T),
            mean(port.aggr2$`1a(iv)`, na.rm=T), mean(port.aggr2$`1a(v)`, na.rm=T), mean(port.aggr2$`1a(vi)`, na.rm=T), mean(port.aggr2$`1a(vii)`, na.rm=T))
mean_1a_postgfc <- c(port.aggr2[Date > "2009-12-31", mean(`1a(i)`,na.rm=T)], port.aggr2[Date > "2009-12-31", mean(`1a(ii)`,na.rm=T)], 
                     port.aggr2[Date > "2009-12-31", mean(`1a(iii)`,na.rm=T)], port.aggr2[Date > "2009-12-31", mean(`1a(iv)`,na.rm=T)],
                     port.aggr2[Date > "2009-12-31", mean(`1a(v)`,na.rm=T)], port.aggr2[Date > "2009-12-31", mean(`1a(vi)`,na.rm=T)],
                     port.aggr2[Date > "2009-12-31", mean(`1a(vii)`,na.rm=T)])

# - quick graphs of summaries to detect patterns
plot(stdevs_1a_full, x=c(3,6,9,12,18,24,36), xlab="k-value", ylab="Standard deviation of SICR-incidence rate", type="b")
plot(stdevs_1a_gfc, x=c(3,6,9,12,18,24,36), xlab="k-value", ylab="Standard deviation of SICR-incidence rate (2008-GFC)", type="b")
plot(stdevs_1a_postgfc, x=c(3,6,9,12,18,24,36), xlab="k-value", ylab="Standard deviation of SICR-incidence rate (after 2008-GFC)", type="b")
plot(max_1a, x=c(3,6,9,12,18,24,36), xlab="k-value", ylab="Max SICR-incidence rate", type="b")
plot(max_locales_1a, x=c(3,6,9,12,18,24,36), xlab="k-value", ylab="2007-date of Max SICR-incidence rate", type="b")
plot(mean_1a, x=c(3,6,9,12,18,24,36), xlab="k-value", ylab="Mean SICR-incidence rate", type="b")
plot(mean_1a_postgfc, x=c(3,6,9,12,18,24,36), xlab="k-value", ylab="Mean SICR-incidence rate (after 2008-GFC)", type="b")
plot(first_1a, x=c(3,6,9,12,18,24,36), xlab="k-value", ylab="First SICR-incidence rate", type="b")
plot(max_1a-first_1a, x=c(3,6,9,12,18,24,36), xlab="k-value", ylab="Differential SICR-incidence rate between max and first", type="b")
plot(max_1a-mean_1a_postgfc, x=c(3,6,9,12,18,24,36), xlab="k-value", ylab="Differential SICR-incidence rate between max and mean (post 2008-GFC)", type="b")

# - Create graphing dataset for secondary graph that shows series summaries
datGraph_1a <- data.table(k=c(3,6,9,12,18,24,36), Rate_Max=max_1a, Rate_First=first_1a, Rate_Mean=mean_1a, Rate_mean_postGFC=mean_1a_postgfc,
                          Diff_First_Max=max_1a-first_1a, Diff_Max_Mean_postGFC=max_1a-mean_1a_postgfc)

# - Create graphing-related bins for facetting
port.aggr[, SICR_Def_Facet := 
            case_when(SICR_Def %in% c("1a(i)", "1a(ii)", "1a(iii)", "1a(iv)") ~ "a_ShortTerm",
                      SICR_Def %in% c("1a(v)","1a(vi)", "1a(vii)") ~ "b_LongTerm")]
describe(port.aggr$SICR_Def_Facet)

# - Create ancillary graphing dataset to annotate certain features within eventual main graph
datAnnotate <- data.table(Date=max_locales_1a, EventRate=max_1a, SICR_Def=c("1a(i)", "1a(ii)", "1a(iii)", "1a(iv)", "1a(v)", "1a(vi)", "1a(vii)"),
                          SICR_Def_Facet=c("a_ShortTerm","a_ShortTerm","a_ShortTerm","a_ShortTerm","b_LongTerm","b_LongTerm","b_LongTerm"))



# --- 3. Graphing | Main Time Graph
# - Graphing parameters
col.v <- brewer.pal(7, "Dark2")
x.label.period <- 6
chosenFont <- "Cambria"
label.v <- c("1a(i)"=  bquote("1a(i):   "*italic(k)==3), 
             "1a(ii)"= bquote("1a(ii):  "*italic(k)==6), 
             "1a(iii)"=bquote("1a(iii): "*italic(k)==9),
             "1a(iv)"= bquote("1a(iv):  "*italic(k)==12), 
             "1a(v)"=  bquote("1a(v):   "*italic(k)==18), 
             "1a(vi)"= bquote("1a(vi):  "*italic(k)==24), 
             "1a(vii)"=bquote("1a(vii): "*italic(k)==36))
facet.v <- c("a_ShortTerm"="(a) Shorter outcome periods", "b_LongTerm"="(b) Longer outcome periods")

# - 1. Create main time series graph
(g <- ggplot(port.aggr, aes(x=Date, y=EventRate, group=SICR_Def)) + theme_minimal() + 
  labs(x="Reporting date (months)", y="Conditional SICR-rate (%) given Stage 1 [Actual]") + 
  theme(text=element_text(family=chosenFont),legend.position = "bottom",
        axis.text.x=element_text(angle=90),
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
  # overlay maxima
  geom_point(data=datAnnotate, aes(x=Date,y=EventRate,group=SICR_Def,colour=SICR_Def), size=5, shape=1, show.legend=F) +     
  # main line graph with overlaid points
  geom_line(aes(colour=SICR_Def, linetype=SICR_Def), linewidth=0.2) + 
  geom_point(aes(colour=SICR_Def, shape=SICR_Def), size=0.8) + 
  # facets & scale options
  facet_grid(SICR_Def_Facet ~., scales="free", labeller=labeller(SICR_Def_Facet=as_labeller(facet.v))) + 
  scale_colour_manual(name="SICR-Definition", values=col.v, labels=label.v) + 
  scale_shape_manual(name="SICR-Definition", values=c(16,17,15,3,7,8,16), labels=label.v) + 
  scale_linetype_discrete(name="SICR-Definition", labels=label.v) + 
  scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
  scale_x_date(date_breaks=paste0(x.label.period, " month"), date_labels = "%b %Y") )

# - Create dataset for inset graph to show summaries
datSummary <- data.table(k=c(3,6,9,12,18,24,36), SICR_Def=c("1a(i)", "1a(ii)", "1a(iii)", "1a(iv)", "1a(v)", "1a(vi)", "1a(vii)"), 
                         SICR_sd = stdevs_1a_full, SICR_mean = mean_1a)

# - 2. Create inset graph for summaries; standard deviation
(g2 <- ggplot(datSummary, aes(x=k, y=SICR_sd)) + theme_minimal() +
  labs(x=bquote(italic(k)*"-values"), y="Standard deviation (%)") + 
  theme(text=element_text(family=chosenFont),# axis.text.y=element_text(margin=margin(l=15,r=-35)),
        #axis.text.y=element_text(angle=90, hjust=0.5),
        axis.text.y=element_text(size=9),
        panel.grid.major = element_blank(),  panel.grid.minor=element_blank(), 
        panel.background=element_rect(color="black", fill="white"),
        plot.background=element_rect(color="white"),
        plot.margin=unit(c(0,0,0,0), "mm")) + 
  # main line graph with overlaid points
  geom_line(linetype="dotted", colour="gray20", linewidth=0.3) + geom_point(aes(colour=SICR_Def, shape=SICR_Def)) + 
  # facets & scale options
  scale_colour_manual(name="", values=col.v, labels=label.v, guide="none") + 
  scale_shape_manual(name="", values=c(16,17,15,3,7,8,16), labels=label.v, guide="none") + 
  scale_y_continuous(breaks=pretty_breaks(), label=percent))

# - 3. Create inset graph for summaries; standard deviation
(g3 <- ggplot(datSummary, aes(x=k, y=SICR_mean)) + theme_minimal() +
    labs(x=bquote(italic(k)*"-values"), y="Mean (%)") + 
    theme(text=element_text(family=chosenFont),# axis.text.y=element_text(margin=margin(l=15,r=-35)),
          #axis.text.y=element_text(angle=90, hjust=0.5),
          axis.text.y=element_text(size=9),
          panel.grid.major = element_blank(),  panel.grid.minor=element_blank(), 
          panel.background=element_rect(color="black", fill="white"),
          plot.background=element_rect(color="white"),
          plot.margin=unit(c(0,0,0,0), "mm")) + 
    # main line graph with overlaid points
    geom_line(linetype="dotted", colour="gray20", linewidth=0.3) + geom_point(aes(colour=SICR_Def, shape=SICR_Def)) + 
    # facets & scale options
    scale_colour_manual(name="", values=col.v, labels=label.v, guide="none") + 
    scale_shape_manual(name="", values=c(16,17,15,3,7,8,16), labels=label.v, guide="none") + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent))

# - Merge graphs using custom function annotation_custom2(), as defined in script 0
(gmain <- g + annotation_custom2(grob=ggplotGrob(g2), data=port.aggr[SICR_Def_Facet == "b_LongTerm",], 
                       xmin=as.Date("2012-06-30"), xmax=as.Date("2020-01-31"), ymin=0.046, ymax=0.104))
(gmain <- gmain + annotation_custom2(grob=ggplotGrob(g3), data=port.aggr[SICR_Def_Facet == "a_ShortTerm",], 
                                 xmin=as.Date("2012-06-30"), xmax=as.Date("2020-01-31"), ymin=0.039, ymax=0.102))

# - Save graph
dpi <- 200
ggsave(gmain, file=paste0(genFigPath, "TimeGraph_SICR-Incidence_Actual_1a.png"), width=1200/dpi, height=1500/dpi, dpi=dpi, bg="white")



# --- 4. Graphing | Secondary k-value summary graph

# - Subset and recast graphing dataset to show only certain measures of interest
datGraph_1a.aggr <- pivot_longer(datGraph_1a[,list(k, a_Rate_First=Rate_First, b_Rate_Max=Rate_Max, c_Rate_mean_postGFC=Rate_mean_postGFC, 
                                                   d_Diff_First_Max=Diff_First_Max, e_Diff_Max_Mean_postGFC=Diff_Max_Mean_postGFC)],
                                 cols=a_Rate_First:e_Diff_Max_Mean_postGFC, names_to="Aggregate_Type", values_to="Aggregate_Value") %>% as.data.table()
datGraph_1a.aggr[, Aggregate_Type2 := substr(Aggregate_Type,1,2)] # aesthetic reasons so that we can have two size scales in effect

# - Subset again for encircling certain points
datGraph_1a.aggr2 <- subset(datGraph_1a.aggr, k %in% c(6,9,12) & Aggregate_Type %in% c("d_Diff_First_Max","e_Diff_Max_Mean_postGFC"), 
       select=c("k","Aggregate_Type","Aggregate_Value"))

# - Graphing parameters
col.v <- brewer.pal(7, "Dark2")
chosenFont <- "Cambria"
label.v <- c("a_Rate_First"=bquote("Earliest SICR-rate "*italic(a(k))), 
             "b_Rate_Max"=bquote("Max SICR-rate "*italic(b(k))), 
             "c_Rate_mean_postGFC"=bquote("Post-GFC SICR-mean "*italic(c(k))), 
             "d_Diff_First_Max"=bquote("Early-warning "*italic(b(k))-italic(a(k))), 
             "e_Diff_Max_Mean_postGFC"=bquote("Recovery "*italic(b(k))-italic(c(k))))
size.v <- c("a_Rate_First"=0.4, "b_Rate_Max"=0.4, "c_Rate_mean_postGFC"=0.4,
            "d_Diff_First_Max"=0.8, "e_Diff_Max_Mean_postGFC"=0.8,
            "a_"=1.5, "b_"=1.5, "c_"=1.5, "d_"=2.5, "e_"=2.5)

# - Create corresponding graph
(g4 <- ggplot(datGraph_1a.aggr, aes(x=k, y=Aggregate_Value, group=Aggregate_Type)) + theme_minimal() + 
    labs(x=bquote(italic(k)*"-value in SICR-definition class 1a"), y="Summary statistic (%)") + 
    theme(text=element_text(family=chosenFont),legend.position = "right",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Aggregate_Type, linetype=Aggregate_Type, linewidth=Aggregate_Type)) + 
    geom_point(aes(colour=Aggregate_Type, shape=Aggregate_Type, size=Aggregate_Type2)) + 
    # Encircle certain points
    geom_point(data=datGraph_1a.aggr2, aes(x=k, y=Aggregate_Value, group=Aggregate_Type), size=7, colour="black", shape=1) + 
    # facets & scale options
    scale_colour_manual(name="Summary Type", values=col.v, labels=label.v) + 
    scale_shape_manual(name="Summary Type", values=c(7,8,15, 16, 17), labels=label.v) + 
    scale_linetype_manual(name="Summary Type", values=c("twodash", "dotdash", "dotted", "solid", "dashed"), labels=label.v) + 
    scale_size_manual(name="Summary Type", values=size.v, labels=label.v) + 
    guides(colour=guide_legend(ncol=1,byrow=T), size="none") + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_continuous(breaks=pretty_breaks(n=6)) )

# - Save graph
dpi <- 200
ggsave(g4, file=paste0(genFigPath, "Summaries_SICR-Incidence_Actual_1a.png"), width=1100/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Cleanup
rm(port.aggr, datSICR_1a, datGraph_1a.aggr, g, g2, gmain, g4, port.aggr2, datSummary, datAnnotate)





# ------ 3. ROC-curves across SICR-definitions (Probabilistic) | Varying k-parameter


# --- 1. Load each sample into memory and bind together successively
if (!exists('datSICR_valid')) unpack.ffdf(paste0(genPath,"datSICR_valid_1a(i)"), tempPath)
datSICR_valid_1a1 <- copy(datSICR_valid[, list(LoanID, Date, Outcome_Act=SICR_target, Outcome_Exp=ExpDisc, Prob_Score=ExpProb)]); rm(datSICR_valid)
if (!exists('datSICR_valid')) unpack.ffdf(paste0(genPath,"datSICR_valid_1a(ii)"), tempPath)
datSICR_valid_1a2 <- copy(datSICR_valid[, list(LoanID, Date, Outcome_Act=SICR_target, Outcome_Exp=ExpDisc, Prob_Score=ExpProb)]); rm(datSICR_valid)
if (!exists('datSICR_valid')) unpack.ffdf(paste0(genPath,"datSICR_valid_1a(iii)"), tempPath)
datSICR_valid_1a3 <- copy(datSICR_valid[, list(LoanID, Date, Outcome_Act=SICR_target, Outcome_Exp=ExpDisc, Prob_Score=ExpProb)]); rm(datSICR_valid)
if (!exists('datSICR_valid')) unpack.ffdf(paste0(genPath,"datSICR_valid_1a(iv)"), tempPath)
datSICR_valid_1a4 <- copy(datSICR_valid[, list(LoanID, Date, Outcome_Act=SICR_target, Outcome_Exp=ExpDisc, Prob_Score=ExpProb)]); rm(datSICR_valid)
if (!exists('datSICR_valid')) unpack.ffdf(paste0(genPath,"datSICR_valid_1a(v)"), tempPath)
datSICR_valid_1a5 <- copy(datSICR_valid[, list(LoanID, Date, Outcome_Act=SICR_target, Outcome_Exp=ExpDisc, Prob_Score=ExpProb)]); rm(datSICR_valid)
if (!exists('datSICR_valid')) unpack.ffdf(paste0(genPath,"datSICR_valid_1a(vi)"), tempPath)
datSICR_valid_1a6 <- copy(datSICR_valid[, list(LoanID, Date, Outcome_Act=SICR_target, Outcome_Exp=ExpDisc, Prob_Score=ExpProb)]); rm(datSICR_valid)
if (!exists('datSICR_valid')) unpack.ffdf(paste0(genPath,"datSICR_valid_1a(vii)"), tempPath)
datSICR_valid_1a7 <- copy(datSICR_valid[, list(LoanID, Date, Outcome_Act=SICR_target, Outcome_Exp=ExpDisc, Prob_Score=ExpProb)]); rm(datSICR_valid)



# --- 2. ROC-analysis using pROC-package
# See https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
# Mixed with https://cran.r-project.org/web/packages/ROCit/vignettes/my-vignette.html

# - Set confidence level for bootstrapping the uncertainty of AUC/Gini-measures
alpha <- 0.05

# - Create ROC-object | probabilities vs discrete labels
pROC_obj1a_i_logit <- roc(formula= Outcome_Act~Prob_Score, data=datSICR_valid_1a1, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
pROC_obj1a_ii_logit <- roc(formula= Outcome_Act~Prob_Score, data=datSICR_valid_1a2, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
pROC_obj1a_iii_logit <- roc(formula= Outcome_Act~Prob_Score, data=datSICR_valid_1a3, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
pROC_obj1a_iv_logit <- roc(formula= Outcome_Act~Prob_Score, data=datSICR_valid_1a4, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
pROC_obj1a_v_logit <- roc(formula= Outcome_Act~Prob_Score, data=datSICR_valid_1a5, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
pROC_obj1a_vi_logit <- roc(formula= Outcome_Act~Prob_Score, data=datSICR_valid_1a6, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
pROC_obj1a_vii_logit <- roc(formula= Outcome_Act~Prob_Score, data=datSICR_valid_1a7, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)



# --- 3. Creating unified ROC-graph
# - Prepare graphing parameters
col.v <- brewer.pal(9, "Set1")[c(2,3,4,5,1,7,9)]
ltype.v <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash","solid")
chosenFont <- "Cambria"

# - Plot ROC-curves
plot(pROC_obj1a_i_logit, auc.polygon=F, max.auc.polygon=T, grid=T, 
     col = col.v[1], auc.polygon.col=col.v[2], lty=ltype.v[1],
     xlab = "False Positive Rate (%)", ylab="True Positive Rate (%)",
     identity.col="gray25", identity=T,  legacy.axes=T, main="", yaxt='n', xaxt='n')
plot(pROC_obj1a_ii_logit, add=T, col = col.v[2], lty=ltype.v[2])
plot(pROC_obj1a_iii_logit, add=T, col = col.v[3], lty=ltype.v[3])
plot(pROC_obj1a_iv_logit, add=T, col = col.v[4], lty=ltype.v[4])
plot(pROC_obj1a_v_logit, add=T, col = col.v[5], lty=ltype.v[5])
plot(pROC_obj1a_vi_logit, add=T, col = col.v[6], lty=ltype.v[6])
plot(pROC_obj1a_vii_logit, add=T, col = col.v[7], lty=ltype.v[7])

# Add y-axis
ticks <- seq(0,100,length.out=6)
axis(side=2,at=ticks,labels=paste0(ticks,"%"))
axis(side=1,at=ticks,labels=paste0(rev(ticks),"%"), mgp=c(3,2,1))

# Add legend: AUC-values and corresponding Gini-values
auc.v <- c(paste0("1a(i)-LR.   AUC: ", sprintf("%.1f",pROC_obj1a_i_logit$auc), "% ± ", sprintf("%.2f",(pROC_obj1a_i_logit$ci[3]-pROC_obj1a_i_logit$ci[1])/2),
                  "%; Gini: ", sprintf("%.1f",pROC_obj1a_i_logit$auc*2-100), "%"),
           paste0("1a(ii)-LR.  AUC: ", sprintf("%.1f",pROC_obj1a_ii_logit$auc), "% ± ", sprintf("%.2f",(pROC_obj1a_ii_logit$ci[3]-pROC_obj1a_ii_logit$ci[1])/2),
                  "%; Gini: ", sprintf("%.1f",pROC_obj1a_ii_logit$auc*2-100), "%"),
           paste0("1a(iii)-LR. AUC: ", sprintf("%.1f",pROC_obj1a_iii_logit$auc), "% ± ", sprintf("%.2f",(pROC_obj1a_iii_logit$ci[3]-pROC_obj1a_iii_logit$ci[1])/2),
                  "%; Gini: ", sprintf("%.1f",pROC_obj1a_iii_logit$auc*2-100), "%"),
           paste0("1a(iv)-LR.  AUC: ", sprintf("%.1f",pROC_obj1a_iv_logit$auc), "% ± ", sprintf("%.2f",(pROC_obj1a_iv_logit$ci[3]-pROC_obj1a_iv_logit$ci[1])/2),
                  "%; Gini: ", sprintf("%.1f",pROC_obj1a_iv_logit$auc*2-100), "%"),
           paste0("1a(v)-LR.   AUC: ", sprintf("%.1f",pROC_obj1a_v_logit$auc), "% ± ", sprintf("%.2f",(pROC_obj1a_v_logit$ci[3]-pROC_obj1a_v_logit$ci[1])/2),
                  "%; Gini: ", sprintf("%.1f",pROC_obj1a_v_logit$auc*2-100), "%"),
           paste0("1a(vi)-LR.  AUC: ", sprintf("%.1f",pROC_obj1a_vi_logit$auc), "% ± ", sprintf("%.2f",(pROC_obj1a_vi_logit$ci[3]-pROC_obj1a_vi_logit$ci[1])/2),
                  "%; Gini: ", sprintf("%.1f",pROC_obj1a_vi_logit$auc*2-100), "%"),
           paste0("1a(vii)-LR. AUC: ", sprintf("%.1f",pROC_obj1a_vii_logit$auc), "% ± ", sprintf("%.2f",(pROC_obj1a_vii_logit$ci[3]-pROC_obj1a_vii_logit$ci[1])/2),
                  "%; Gini: ", sprintf("%.1f",pROC_obj1a_vii_logit$auc*2-100), "%"))
legend(x=73.4, y=67, legend=auc.v, col=col.v, lwd=2, horiz=F, lty=ltype.v, cex=0.75, y.intersp=0.95)

# Add legend: Line-graph descriptions
leg.v <- vector("expression",5)
leg.v[[1]] <- bquote("1a(i):   Logistic Regression with"~italic(d)==1*","~italic(s)==1*","~italic(k)==3)
leg.v[[2]] <- bquote("1a(ii):  Logistic Regression with"~italic(d)==1*","~italic(s)==1*","~italic(k)==6)
leg.v[[3]] <- bquote("1a(iii): Logistic Regression with"~italic(d)==1*","~italic(s)==1*","~italic(k)==9)
leg.v[[4]] <- bquote("1a(iv):  Logistic Regression with"~italic(d)==1*","~italic(s)==1*","~italic(k)==12)
leg.v[[5]] <- bquote("1a(v):   Logistic Regression with"~italic(d)==1*","~italic(s)==1*","~italic(k)==18)
leg.v[[6]] <- bquote("1a(vi):  Logistic Regression with"~italic(d)==1*","~italic(s)==1*","~italic(k)==24)
leg.v[[7]] <- bquote("1a(vii): Logistic Regression with"~italic(d)==1*","~italic(s)==1*","~italic(k)==36)
legend(x=82.6, y=33, legend=leg.v, col=col.v, lwd=2, horiz=F, lty=ltype.v, cex=0.75, y.intersp=0.95)

# Save plot
dev.copy(png, file=paste0(genFigPath,"ROC_1a_logit_prob_k-values.png"), width=1000, height=1000, res=170, family=chosenFont); dev.off()

# - Cleanup
rm(pROC_obj1a_i_logit, pROC_obj1a_ii_logit, pROC_obj1a_iii_logit, pROC_obj1a_iv_logit,
   pROC_obj1a_v_logit, pROC_obj1a_vi_logit, pROC_obj1a_vii_logit,
   datSICR_valid_1a1, datSICR_valid_1a2, datSICR_valid_1a3, datSICR_valid_1a4, datSICR_valid_1a5,
   datSICR_valid_1a6, datSICR_valid_1a7); gc()





# ------ 4. ROC-curves across SICR-definitions (Discrete) | Varying k-parameter

# --- 1. Load each sample into memory and bind together successively
if (!exists('datSICR_valid')) unpack.ffdf(paste0(genPath,"datSICR_valid_1a(i)"), tempPath)
datSICR_valid_1a1 <- copy(datSICR_valid[, list(LoanID, Date, Outcome_Act=SICR_target, Outcome_Exp=ExpDisc, Prob_Score=ExpProb)]); rm(datSICR_valid)
if (!exists('datSICR_valid')) unpack.ffdf(paste0(genPath,"datSICR_valid_1a(ii)"), tempPath)
datSICR_valid_1a2 <- copy(datSICR_valid[, list(LoanID, Date, Outcome_Act=SICR_target, Outcome_Exp=ExpDisc, Prob_Score=ExpProb)]); rm(datSICR_valid)
if (!exists('datSICR_valid')) unpack.ffdf(paste0(genPath,"datSICR_valid_1a(iii)"), tempPath)
datSICR_valid_1a3 <- copy(datSICR_valid[, list(LoanID, Date, Outcome_Act=SICR_target, Outcome_Exp=ExpDisc, Prob_Score=ExpProb)]); rm(datSICR_valid)
if (!exists('datSICR_valid')) unpack.ffdf(paste0(genPath,"datSICR_valid_1a(iv)"), tempPath)
datSICR_valid_1a4 <- copy(datSICR_valid[, list(LoanID, Date, Outcome_Act=SICR_target, Outcome_Exp=ExpDisc, Prob_Score=ExpProb)]); rm(datSICR_valid)
if (!exists('datSICR_valid')) unpack.ffdf(paste0(genPath,"datSICR_valid_1a(v)"), tempPath)
datSICR_valid_1a5 <- copy(datSICR_valid[, list(LoanID, Date, Outcome_Act=SICR_target, Outcome_Exp=ExpDisc, Prob_Score=ExpProb)]); rm(datSICR_valid)
if (!exists('datSICR_valid')) unpack.ffdf(paste0(genPath,"datSICR_valid_1a(vi)"), tempPath)
datSICR_valid_1a6 <- copy(datSICR_valid[, list(LoanID, Date, Outcome_Act=SICR_target, Outcome_Exp=ExpDisc, Prob_Score=ExpProb)]); rm(datSICR_valid)
if (!exists('datSICR_valid')) unpack.ffdf(paste0(genPath,"datSICR_valid_1a(vii)"), tempPath)
datSICR_valid_1a7 <- copy(datSICR_valid[, list(LoanID, Date, Outcome_Act=SICR_target, Outcome_Exp=ExpDisc, Prob_Score=ExpProb)]); rm(datSICR_valid)



# --- 2. ROC-analysis using pROC-package
# See https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
# Mixed with https://cran.r-project.org/web/packages/ROCit/vignettes/my-vignette.html

# - Set confidence level for bootstrapping the uncertainty of AUC/Gini-measures
alpha <- 0.05

# - Create ROC-object | probabilities vs discrete labels
pROC_obj1a_i_logit_d <- roc(formula= Outcome_Act~Outcome_Exp, data=datSICR_valid_1a1, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
pROC_obj1a_ii_logit_d <- roc(formula= Outcome_Act~Outcome_Exp, data=datSICR_valid_1a2, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
pROC_obj1a_iii_logit_d <- roc(formula= Outcome_Act~Outcome_Exp, data=datSICR_valid_1a3, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
pROC_obj1a_iv_logit_d <- roc(formula= Outcome_Act~Outcome_Exp, data=datSICR_valid_1a4, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
pROC_obj1a_v_logit_d <- roc(formula= Outcome_Act~Outcome_Exp, data=datSICR_valid_1a5, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
pROC_obj1a_vi_logit_d <- roc(formula= Outcome_Act~Outcome_Exp, data=datSICR_valid_1a6, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
pROC_obj1a_vii_logit_d <- roc(formula= Outcome_Act~Outcome_Exp, data=datSICR_valid_1a7, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)



# --- 3. Creating unified ROC-graph
# - Prepare graphing parameters
col.v <- brewer.pal(9, "Set1")[c(2,3,4,5,1,7,9)]
ltype.v <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash","solid")
chosenFont <- "Cambria"

# - Plot ROC-curves
plot(pROC_obj1a_i_logit_d, auc.polygon=F, max.auc.polygon=T, grid=T, 
     col = col.v[1], auc.polygon.col=col.v[2], lty=ltype.v[1],
     xlab = "False Positive Rate (%)", ylab="True Positive Rate (%)",
     identity.col="gray25", identity=T,  legacy.axes=T, main="", yaxt='n', xaxt='n')
plot(pROC_obj1a_ii_logit_d, add=T, col = col.v[2], lty=ltype.v[2])
plot(pROC_obj1a_iii_logit_d, add=T, col = col.v[3], lty=ltype.v[3])
plot(pROC_obj1a_iv_logit_d, add=T, col = col.v[4], lty=ltype.v[4])
plot(pROC_obj1a_v_logit_d, add=T, col = col.v[5], lty=ltype.v[5])
plot(pROC_obj1a_vi_logit_d, add=T, col = col.v[6], lty=ltype.v[6])
plot(pROC_obj1a_vii_logit_d, add=T, col = col.v[7], lty=ltype.v[7])

# Add y-axis
ticks <- seq(0,100,length.out=6)
axis(side=2,at=ticks,labels=paste0(ticks,"%"))
axis(side=1,at=ticks,labels=paste0(rev(ticks),"%"), mgp=c(3,2,1))

# Add legend: AUC-values and corresponding Gini-values
auc.v <- c(paste0("1a(i)-LR.   AUC: ", sprintf("%.1f", pROC_obj1a_i_logit_d$auc), "% ± ", sprintf("%.2f",(pROC_obj1a_i_logit_d$ci[3]-pROC_obj1a_i_logit_d$ci[1])/2),
                  "%; Gini: ", sprintf("%.1f", pROC_obj1a_i_logit_d$auc*2-100), "%"),
           paste0("1a(ii)-LR.  AUC: ", sprintf("%.1f", pROC_obj1a_ii_logit_d$auc), "% ± ", sprintf("%.2f",(pROC_obj1a_ii_logit_d$ci[3]-pROC_obj1a_ii_logit_d$ci[1])/2),
                  "%; Gini: ", sprintf("%.1f", pROC_obj1a_ii_logit_d$auc*2-100), "%"),
           paste0("1a(iii)-LR. AUC: ", sprintf("%.1f", pROC_obj1a_iii_logit_d$auc), "% ± ", sprintf("%.2f",(pROC_obj1a_iii_logit_d$ci[3]-pROC_obj1a_iii_logit_d$ci[1])/2),
                  "%; Gini: ", sprintf("%.1f", pROC_obj1a_iii_logit_d$auc*2-100), "%"),
           paste0("1a(iv)-LR.  AUC: ", sprintf("%.1f", pROC_obj1a_iv_logit_d$auc), "% ± ", sprintf("%.2f",(pROC_obj1a_iv_logit_d$ci[3]-pROC_obj1a_iv_logit_d$ci[1])/2),
                  "%; Gini: ", sprintf("%.1f", pROC_obj1a_iv_logit_d$auc*2-100), "%"),
           paste0("1a(v)-LR.   AUC: ", sprintf("%.1f", pROC_obj1a_v_logit_d$auc), "% ± ", sprintf("%.2f",(pROC_obj1a_v_logit_d$ci[3]-pROC_obj1a_v_logit_d$ci[1])/2),
                  "%; Gini: ", sprintf("%.1f", pROC_obj1a_v_logit_d$auc*2-100), "%"),
           paste0("1a(vi)-LR.  AUC: ", sprintf("%.1f", pROC_obj1a_vi_logit_d$auc), "% ± ", sprintf("%.2f",(pROC_obj1a_vi_logit_d$ci[3]-pROC_obj1a_vi_logit_d$ci[1])/2),
                  "%; Gini: ", sprintf("%.1f", pROC_obj1a_vi_logit_d$auc*2-100), "%"),
           paste0("1a(vii)-LR. AUC: ", sprintf("%.1f", pROC_obj1a_vii_logit_d$auc), "% ± ", sprintf("%.2f",(pROC_obj1a_vii_logit_d$ci[3]-pROC_obj1a_vii_logit_d$ci[1])/2),
                  "%; Gini: ", sprintf("%.1f", pROC_obj1a_vii_logit_d$auc*2-100), "%"))
legend(x=73.4, y=67, legend=auc.v, col=col.v, lwd=2, horiz=F, lty=ltype.v, cex=0.75, y.intersp=0.95)

# Add legend: Line-graph descriptions
leg.v <- vector("expression",5)
leg.v[[1]] <- bquote("1a(i):   Logistic Regression with"~italic(d)==1*","~italic(s)==1*","~italic(k)==3)
leg.v[[2]] <- bquote("1a(ii):  Logistic Regression with"~italic(d)==1*","~italic(s)==1*","~italic(k)==6)
leg.v[[3]] <- bquote("1a(iii): Logistic Regression with"~italic(d)==1*","~italic(s)==1*","~italic(k)==9)
leg.v[[4]] <- bquote("1a(iv):  Logistic Regression with"~italic(d)==1*","~italic(s)==1*","~italic(k)==12)
leg.v[[5]] <- bquote("1a(v):   Logistic Regression with"~italic(d)==1*","~italic(s)==1*","~italic(k)==18)
leg.v[[6]] <- bquote("1a(vi):  Logistic Regression with"~italic(d)==1*","~italic(s)==1*","~italic(k)==24)
leg.v[[7]] <- bquote("1a(vii): Logistic Regression with"~italic(d)==1*","~italic(s)==1*","~italic(k)==36)
legend(x=82.6, y=33, legend=leg.v, col=col.v, lwd=2, horiz=F, lty=ltype.v, cex=0.75, y.intersp=0.95)

# Save plot
dev.copy(png, file=paste0(genFigPath,"ROC_1a_logit_disc_k-values.png"), width=1000, height=1000, res=170, family=chosenFont); dev.off()

# - Cleanup
rm(pROC_obj1a_i_logit_d, pROC_obj1a_ii_logit_d, pROC_obj1a_iii_logit_d, pROC_obj1a_iv_logit_d,
   pROC_obj1a_v_logit_d, pROC_obj1a_vi_logit_d, pROC_obj1a_vii_logit_d); gc()


# --- Grand Cleanup
rm(datSICR_valid_1a1, datSICR_valid_1a2, datSICR_valid_1a3, datSICR_valid_1a4, datSICR_valid_1a5, datSICR_valid_1a6, datSICR_valid_1a7)
gc()

