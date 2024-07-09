# ============================== SICR-DEFINITION ANALYSIS ===============================
# Script for comparing the SICR-models and results across various SICR-definitions.
# In particular, we analyse definition class 1 and compare results across (s,k)-parameters
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
#   - 3a.SICR_def_<>_logit.R | the 3a-series of scripts for definitions 1a-c, for (i)-(iv)

# -- Inputs:
#   - performance_measures_<> | aggregated performance measures for given SICR-definition (3a)
#   - datSICR_smp_<> | specific SICR-sample upon which resampling scheme is applied (3a)

# -- Outputs:
#   - <analytics>
# =======================================================================================





# ------ 1. Performance measure analysis across SICR-definitions

# - Load into memory
if (!exists('performance_measures_1a_i')) unpack.ffdf(paste0(genObjPath,"performance_measures_1a(i)"), tempPath)
if (!exists('performance_measures_1a_ii')) unpack.ffdf(paste0(genObjPath,"performance_measures_1a(ii)"), tempPath)
if (!exists('performance_measures_1a_iii')) unpack.ffdf(paste0(genObjPath,"performance_measures_1a(iii)"), tempPath)
if (!exists('performance_measures_1a_iv')) unpack.ffdf(paste0(genObjPath,"performance_measures_1a(iv)"), tempPath)
if (!exists('performance_measures_1b_i')) unpack.ffdf(paste0(genObjPath,"performance_measures_1b(i)"), tempPath)
if (!exists('performance_measures_1b_ii')) unpack.ffdf(paste0(genObjPath,"performance_measures_1b(ii)"), tempPath)
if (!exists('performance_measures_1b_iii')) unpack.ffdf(paste0(genObjPath,"performance_measures_1b(iii)"), tempPath)
if (!exists('performance_measures_1b_iv')) unpack.ffdf(paste0(genObjPath,"performance_measures_1b(iv)"), tempPath)
if (!exists('performance_measures_1c_i')) unpack.ffdf(paste0(genObjPath,"performance_measures_1c(i)"), tempPath)
if (!exists('performance_measures_1c_ii')) unpack.ffdf(paste0(genObjPath,"performance_measures_1c(ii)"), tempPath)
if (!exists('performance_measures_1c_iii')) unpack.ffdf(paste0(genObjPath,"performance_measures_1c(iii)"), tempPath)
if (!exists('performance_measures_1c_iv')) unpack.ffdf(paste0(genObjPath,"performance_measures_1c(iv)"), tempPath)

# - Merge datasets together
performance_measures <- data.table(rbind(performance_measures_1a_i, performance_measures_1a_ii, performance_measures_1a_iii, performance_measures_1a_iv,
                                         performance_measures_1b_i, performance_measures_1b_ii, performance_measures_1b_iii, performance_measures_1b_iv,
                                         performance_measures_1c_i, performance_measures_1c_ii, performance_measures_1c_iii, performance_measures_1c_iv), 
                                   key="SICR_definition")

# - Cleanup
rm(performance_measures_1a_i, performance_measures_1a_ii, performance_measures_1a_iii, performance_measures_1a_iv,
   performance_measures_1b_i, performance_measures_1b_ii, performance_measures_1b_iii, performance_measures_1b_iv,
   performance_measures_1c_i, performance_measures_1c_ii, performance_measures_1c_iii, performance_measures_1c_iv); gc()

# - Analysis
performance_measures[s>1]
### RESULTS: 

sprintf("%.2f", (performance_measures$CI_upper_prob - performance_measures$CI_lower_prob)/2)[5:12]
sprintf("%.2f",(performance_measures$CI_upper_discrete - performance_measures$CI_lower_discrete)/2)[5:12]
sprintf("%.2f", performance_measures$std_dev_SICR_rate_Act*100)[5:12]

# summary plots
plot(performance_measures$k, performance_measures$std_dev, type="b") # called "flexibility \omega" in main text
plot(performance_measures$k, performance_measures$std_dev_SICR_rate_Act, type="b") # called "instability \sigma" in main text





# ------ 2. SICR-Incidence rates over time across SICR-definitions: 1b (d=1,s=2, k) | Varying k-parameter

# --- 1. Load each sample into memory and bind together successively
SICR_label <- "1b(i)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR_1b <- copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target , SICR_Def=SICR_label)]); rm(datSICR_smp)
SICR_label <- "1b(ii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR_1b <- rbind(datSICR_1b, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label)])); rm(datSICR_smp)
SICR_label <- "1b(iii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR_1b <- rbind(datSICR_1b, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label)])); rm(datSICR_smp)
SICR_label <- "1b(iv)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR_1b <- rbind(datSICR_1b, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label)])); rm(datSICR_smp)



# --- 2. Graph data preparation and aggregation

# - Transform factor back to numeric variables for aggregation purposes
datSICR_1b[, SICR_events := as.numeric(levels(SICR_events))[SICR_events]]

# - Aggregate to monthly level and observe up to given point
SICR_StartDte <- rollforward(min(datSICR_1b$Date, na.rm=T) + month(1))
# NOTE: We reduced the sampling window by 1 month to avoid unexplained spike in event rate, likely due to underlying data issues from source
SICR_EndDte <- max(datSICR_1b$Date, na.rm=T)
port.aggr <- datSICR_1b[SICR_def==0,list(EventRate = sum(SICR_events, na.rm=T)/.N, AtRisk = .N),
                        by=list(SICR_Def, Date)][Date >= SICR_StartDte & Date <= SICR_EndDte,] %>% setkey(SICR_Def,Date)
describe(port.aggr$SICR_Def)

# - Recast/pivot data in order to create useful summaries across all time series
port.aggr2 <- port.aggr %>% pivot_wider(id_cols = c(Date), names_from = c(SICR_Def), values_from = c(EventRate)) %>% as.data.table()

# - Calculate useful summaries over rate time series
stdevs_1b_full <- c(sd(port.aggr2$`1b(i)`, na.rm=T), sd(port.aggr2$`1b(ii)`, na.rm=T), 
                    sd(port.aggr2$`1b(iii)`, na.rm=T), sd(port.aggr2$`1b(iv)`, na.rm=T))
stdevs_1b_gfc <- c(port.aggr2[Date <= "2009-12-31", sd(`1b(i)`,na.rm=T)], port.aggr2[Date <= "2009-12-31", sd(`1b(ii)`,na.rm=T)], 
                   port.aggr2[Date <= "2009-12-31", sd(`1b(iii)`,na.rm=T)], port.aggr2[Date <= "2009-12-31", sd(`1b(iv)`,na.rm=T)])
stdevs_1b_postgfc <- c(port.aggr2[Date > "2009-12-31", sd(`1b(i)`,na.rm=T)], port.aggr2[Date > "2009-12-31", sd(`1b(ii)`,na.rm=T)], 
                       port.aggr2[Date > "2009-12-31", sd(`1b(iii)`,na.rm=T)], port.aggr2[Date > "2009-12-31", sd(`1b(iv)`,na.rm=T)])
first_1b <- c(port.aggr2[, `1b(i)`[1]], port.aggr2[, `1b(ii)`[1]], port.aggr2[, `1b(iii)`[1]],
              port.aggr2[, `1b(iv)`[1]])
max_1b <- c(max(port.aggr2$`1b(i)`, na.rm=T), max(port.aggr2$`1b(ii)`, na.rm=T), max(port.aggr2$`1b(iii)`, na.rm=T),
            max(port.aggr2$`1b(iv)`, na.rm=T))
max_locales_1b <- c( port.aggr2[, Date[which(`1b(i)`==max(`1b(i)`,na.rm=T))]], port.aggr2[, Date[which(`1b(ii)`==max(`1b(ii)`,na.rm=T))]],
                     port.aggr2[, Date[which(`1b(iii)`==max(`1b(iii)`,na.rm=T))]], port.aggr2[, Date[which(`1b(iv)`==max(`1b(iv)`,na.rm=T))]])
mean_1b <- c(mean(port.aggr2$`1b(i)`, na.rm=T), mean(port.aggr2$`1b(ii)`, na.rm=T), mean(port.aggr2$`1b(iii)`, na.rm=T),
             mean(port.aggr2$`1b(iv)`, na.rm=T))
mean_1b_postgfc <- c(port.aggr2[Date > "2009-12-31", mean(`1b(i)`,na.rm=T)], port.aggr2[Date > "2009-12-31", mean(`1b(ii)`,na.rm=T)], 
                     port.aggr2[Date > "2009-12-31", mean(`1b(iii)`,na.rm=T)], port.aggr2[Date > "2009-12-31", mean(`1b(iv)`,na.rm=T)])

# - quick graphs of summaries to detect patterns
plot(stdevs_1b_full, x=c(3,6,9,12), xlab="k-value", ylab="Standard deviation of SICR-incidence rate", type="b")
plot(stdevs_1b_gfc, x=c(3,6,9,12), xlab="k-value", ylab="Standard deviation of SICR-incidence rate (2008-GFC)", type="b")
plot(stdevs_1b_postgfc, x=c(3,6,9,12), xlab="k-value", ylab="Standard deviation of SICR-incidence rate (after 2008-GFC)", type="b")
plot(max_1b, x=c(3,6,9,12), xlab="k-value", ylab="Max SICR-incidence rate", type="b")
plot(max_locales_1b, x=c(3,6,9,12), xlab="k-value", ylab="2007-date of Max SICR-incidence rate", type="b")
plot(mean_1b, x=c(3,6,9,12), xlab="k-value", ylab="Mean SICR-incidence rate", type="b")
plot(mean_1b_postgfc, x=c(3,6,9,12), xlab="k-value", ylab="Mean SICR-incidence rate (after 2008-GFC)", type="b")
plot(first_1b, x=c(3,6,9,12), xlab="k-value", ylab="First SICR-incidence rate", type="b")
plot(max_1b-first_1b, x=c(3,6,9,12), xlab="k-value", ylab="Differential SICR-incidence rate between max and first", type="b")
plot(max_1b-mean_1b_postgfc, x=c(3,6,9,12), xlab="k-value", ylab="Differential SICR-incidence rate between max and mean (post 2008-GFC)", type="b")

# - Create graphing dataset for secondary graph that shows series summaries
datGraph_1b <- data.table(k=c(3,6,9,12), Rate_Max=max_1b, Rate_First=first_1b, Rate_Mean=mean_1b, Rate_mean_postGFC=mean_1b_postgfc,
                          Diff_First_Max=max_1b-first_1b, Diff_Max_Mean_postGFC=max_1b-mean_1b_postgfc)

# - Create graphing-related bins for facetting
port.aggr[, SICR_Def_Facet := "1b"]
port.aggr[, Facet_math := factor(SICR_Def_Facet,labels = c("'SICR-definition class 1b ('*italic(d)==1*','~italic(s)==2*')'"))]

# - Create ancillary graphing dataset to annotate certain features within eventual main graph
datAnnotate <- data.table(Date=max_locales_1b, EventRate=max_1b, SICR_Def=c("1b(i)", "1b(ii)", "1b(iii)", "1b(iv)"),
                          SICR_Def_Facet="1b")



# --- 3. Graphing | Main Time Graph
# - Graphing parameters
col.v <- brewer.pal(7, "Dark2")
x.label.period <- 6
chosenFont <- "Cambria"
label.v <- c("1b(i)"=  bquote("1b(i):   "*italic(k)==3), 
             "1b(ii)"= bquote("1b(ii):  "*italic(k)==6), 
             "1b(iii)"=bquote("1b(iii): "*italic(k)==9),
             "1b(iv)"= bquote("1b(iv):  "*italic(k)==12))

# - 1. Create main time series graph
(g <- ggplot(port.aggr, aes(x=Date, y=EventRate, group=SICR_Def)) + theme_minimal() + 
    labs(x="Reporting date (months)", y="Conditional SICR-rate (%) [Actual]") + 
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
    facet_grid(Facet_math ~., scales="free", labeller=label_parsed) + 
    scale_colour_manual(name="SICR-Definition", values=col.v, labels=label.v) + 
    scale_shape_manual(name="SICR-Definition", values=c(16,17,15,8), labels=label.v) + 
    scale_linetype_discrete(name="SICR-Definition", labels=label.v) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(x.label.period, " month"), date_labels = "%b %Y") )

# - Create dataset for inset graph to show summaries
datSummary <- data.table(k=c(3,6,9,12), SICR_Def=c("1b(i)", "1b(ii)", "1b(iii)", "1b(iv)"), 
                         SICR_sd = stdevs_1b_full, SICR_mean = mean_1b) %>% 
  pivot_longer(cols=SICR_sd:SICR_mean, names_to="Summary_Type", values_to = "Summary_Value") %>% as.data.table()

# - 2. Create inset graph for summaries; standard deviation + mean
(g2 <- ggplot(datSummary, aes(x=k, y=Summary_Value, group=Summary_Type)) + theme_minimal() +
    labs(x=bquote(italic(k)*"-values"), y="Summary Statistic (%)") + 
    theme(text=element_text(family=chosenFont), legend.position = c(0.6,0.6),
          axis.text.y=element_text(size=9),
          panel.grid.major = element_blank(),  panel.grid.minor=element_blank(), 
          panel.background=element_rect(color="black", fill="white"),
          plot.background=element_rect(color="white"),
          plot.margin=unit(c(0,0,0,0), "mm")) + 
    # main line graph with overlaid points
    geom_line(aes(linetype=Summary_Type), colour="gray20", linewidth=0.3) + geom_point(aes(colour=SICR_Def, shape=SICR_Def)) + 
    # facets & scale options
    scale_colour_manual(name="", values=col.v, guide="none") + 
    scale_shape_manual(name="", values=c(16,17,15,8), guide="none") + 
    scale_linetype_discrete(name="", labels=c("SICR_mean"="Mean", "SICR_sd"="Standard deviation")) + 
    scale_x_continuous(breaks=pretty_breaks(n=8)) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent))

# - Merge graphs
(gmain <- g + annotation_custom2(grob=ggplotGrob(g2), data=port.aggr[SICR_Def_Facet == "1b",], 
                                 xmin=as.Date("2012-06-30"), xmax=as.Date("2020-01-31"), ymin=0.035, ymax=0.084))

# - Save graph
dpi <- 200
ggsave(gmain, file=paste0(genFigPath, "TimeGraph_SICR-Incidence_Actual_1b.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")



# --- 4. Graphing | Secondary k-value summary graph

# - Subset and recast graphing dataset to show only certain measures of interest
datGraph_1b.aggr <- pivot_longer(datGraph_1b[,list(k, a_Rate_First=Rate_First, b_Rate_Max=Rate_Max, c_Rate_mean_postGFC=Rate_mean_postGFC, 
                                                   d_Diff_First_Max=Diff_First_Max, e_Diff_Max_Mean_postGFC=Diff_Max_Mean_postGFC)],
                                 cols=a_Rate_First:e_Diff_Max_Mean_postGFC, names_to="Aggregate_Type", values_to="Aggregate_Value") %>% as.data.table()
datGraph_1b.aggr[, Aggregate_Type2 := substr(Aggregate_Type,1,2)] # aesthetic reasons so that we can have two size scales in effect

# - Subset again for encircling certain points
datGraph_1b.aggr2 <- subset(datGraph_1b.aggr, k %in% c(6,9) & Aggregate_Type %in% c("d_Diff_First_Max","e_Diff_Max_Mean_postGFC"), 
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
(g4 <- ggplot(datGraph_1b.aggr, aes(x=k, y=Aggregate_Value, group=Aggregate_Type)) + theme_minimal() + 
    labs(x=bquote(italic(k)*"-value in SICR-definition class 1b"), y="Summary statistic (%)") + 
    theme(text=element_text(family=chosenFont),legend.position = "right",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Aggregate_Type, linetype=Aggregate_Type, linewidth=Aggregate_Type)) + 
    geom_point(aes(colour=Aggregate_Type, shape=Aggregate_Type, size=Aggregate_Type2)) + 
    # Encircle certain points
    geom_point(data=datGraph_1b.aggr2, aes(x=k, y=Aggregate_Value, group=Aggregate_Type), size=7, colour="black", shape=1) + 
    # facets & scale options
    scale_colour_manual(name="Summary Type", values=col.v, labels=label.v) + 
    scale_shape_manual(name="Summary Type", values=c(7,8,15, 16, 17), labels=label.v) + 
    scale_linetype_manual(name="Summary Type", values=c("twodash", "dotdash", "dotted", "solid", "dashed"), labels=label.v) + 
    scale_size_manual(name="Summary Type", values=size.v, labels=label.v) + 
    scale_linewidth_manual(name="Summary Type", values=size.v, labels=label.v) + 
    guides(colour=guide_legend(ncol=1,byrow=T), size="none") + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_continuous(breaks=pretty_breaks(n=6)) )

# - Save graph
dpi <- 200
ggsave(g4, file=paste0(genFigPath, "Summaries_SICR-Incidence_Actual_1b.png"), width=1100/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Cleanup
rm(port.aggr, datGraph_1b, datGraph_1b.aggr2, g, g2, gmain, g4, port.aggr2, datSummary, datAnnotate)





# ------ 3. SICR-Incidence rates over time across SICR-definitions: 1c (d=1,s=3, k) | Varying k-parameter

# --- 1. Load each sample into memory and bind together successively
SICR_label <- "1c(i)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR_1c <- copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target , SICR_Def=SICR_label)]); rm(datSICR_smp)
SICR_label <- "1c(ii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR_1c <- rbind(datSICR_1c, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label)])); rm(datSICR_smp)
SICR_label <- "1c(iii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR_1c <- rbind(datSICR_1c, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label)])); rm(datSICR_smp)
SICR_label <- "1c(iv)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR_1c <- rbind(datSICR_1c, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label)])); rm(datSICR_smp)



# --- 2. Graph data preparation and aggregation

# - Transform factor back to numeric variables for aggregation purposes
datSICR_1c[, SICR_events := as.numeric(levels(SICR_events))[SICR_events]]

# - Aggregate to monthly level and observe up to given point
SICR_StartDte <- rollforward(min(datSICR_1c$Date, na.rm=T) + months(2))
# NOTE: We reduced the sampling window by 2 months to avoid unexplained spike in event rate, likely due to underlying data issues from source
SICR_EndDte <- max(datSICR_1c$Date, na.rm=T)
port.aggr <- datSICR_1c[SICR_def==0,list(EventRate = sum(SICR_events, na.rm=T)/.N, AtRisk = .N),
                        by=list(SICR_Def, Date)][Date >= SICR_StartDte & Date <= SICR_EndDte,] %>% setkey(SICR_Def,Date)
describe(port.aggr$SICR_Def)

# - Recast/pivot data in order to create useful summaries across all time series
port.aggr2 <- port.aggr %>% pivot_wider(id_cols = c(Date), names_from = c(SICR_Def), values_from = c(EventRate)) %>% as.data.table()

# - Calculate useful summaries over rate time series
stdevs_1c_full <- c(sd(port.aggr2$`1c(i)`, na.rm=T), sd(port.aggr2$`1c(ii)`, na.rm=T), 
                    sd(port.aggr2$`1c(iii)`, na.rm=T), sd(port.aggr2$`1c(iv)`, na.rm=T))
stdevs_1c_gfc <- c(port.aggr2[Date <= "2009-12-31", sd(`1c(i)`,na.rm=T)], port.aggr2[Date <= "2009-12-31", sd(`1c(ii)`,na.rm=T)], 
                   port.aggr2[Date <= "2009-12-31", sd(`1c(iii)`,na.rm=T)], port.aggr2[Date <= "2009-12-31", sd(`1c(iv)`,na.rm=T)])
stdevs_1c_postgfc <- c(port.aggr2[Date > "2009-12-31", sd(`1c(i)`,na.rm=T)], port.aggr2[Date > "2009-12-31", sd(`1c(ii)`,na.rm=T)], 
                       port.aggr2[Date > "2009-12-31", sd(`1c(iii)`,na.rm=T)], port.aggr2[Date > "2009-12-31", sd(`1c(iv)`,na.rm=T)])
first_1c <- c(port.aggr2[, `1c(i)`[1]], port.aggr2[, `1c(ii)`[1]], port.aggr2[, `1c(iii)`[1]],
              port.aggr2[, `1c(iv)`[1]])
max_1c <- c(max(port.aggr2$`1c(i)`, na.rm=T), max(port.aggr2$`1c(ii)`, na.rm=T), max(port.aggr2$`1c(iii)`, na.rm=T),
            max(port.aggr2$`1c(iv)`, na.rm=T))
max_locales_1c <- c( port.aggr2[, Date[which(`1c(i)`==max(`1c(i)`,na.rm=T))]], port.aggr2[, Date[which(`1c(ii)`==max(`1c(ii)`,na.rm=T))]],
                     port.aggr2[, Date[which(`1c(iii)`==max(`1c(iii)`,na.rm=T))]], port.aggr2[, Date[which(`1c(iv)`==max(`1c(iv)`,na.rm=T))]])
mean_1c <- c(mean(port.aggr2$`1c(i)`, na.rm=T), mean(port.aggr2$`1c(ii)`, na.rm=T), mean(port.aggr2$`1c(iii)`, na.rm=T),
             mean(port.aggr2$`1c(iv)`, na.rm=T))
mean_1c_postgfc <- c(port.aggr2[Date > "2009-12-31", mean(`1c(i)`,na.rm=T)], port.aggr2[Date > "2009-12-31", mean(`1c(ii)`,na.rm=T)], 
                     port.aggr2[Date > "2009-12-31", mean(`1c(iii)`,na.rm=T)], port.aggr2[Date > "2009-12-31", mean(`1c(iv)`,na.rm=T)])

# - quick graphs of summaries to detect patterns
plot(stdevs_1c_full, x=c(3,6,9,12), xlab="k-value", ylab="Standard deviation of SICR-incidence rate", type="b")
plot(stdevs_1c_gfc, x=c(3,6,9,12), xlab="k-value", ylab="Standard deviation of SICR-incidence rate (2008-GFC)", type="b")
plot(stdevs_1c_postgfc, x=c(3,6,9,12), xlab="k-value", ylab="Standard deviation of SICR-incidence rate (after 2008-GFC)", type="b")
plot(max_1c, x=c(3,6,9,12), xlab="k-value", ylab="Max SICR-incidence rate", type="b")
plot(max_locales_1c, x=c(3,6,9,12), xlab="k-value", ylab="2007-date of Max SICR-incidence rate", type="b")
plot(mean_1c, x=c(3,6,9,12), xlab="k-value", ylab="Mean SICR-incidence rate", type="b")
plot(mean_1c_postgfc, x=c(3,6,9,12), xlab="k-value", ylab="Mean SICR-incidence rate (after 2008-GFC)", type="b")
plot(first_1c, x=c(3,6,9,12), xlab="k-value", ylab="First SICR-incidence rate", type="b")
plot(max_1c-first_1c, x=c(3,6,9,12), xlab="k-value", ylab="Differential SICR-incidence rate between max and first", type="b")
plot(max_1c-mean_1c_postgfc, x=c(3,6,9,12), xlab="k-value", ylab="Differential SICR-incidence rate between max and mean (post 2008-GFC)", type="b")

# - Create graphing dataset for secondary graph that shows series summaries
datGraph_1c <- data.table(k=c(3,6,9,12), Rate_Max=max_1c, Rate_First=first_1c, Rate_Mean=mean_1c, Rate_mean_postGFC=mean_1c_postgfc,
                          Diff_First_Max=max_1c-first_1c, Diff_Max_Mean_postGFC=max_1c-mean_1c_postgfc)

# - Create graphing-related bins for facetting
port.aggr[, SICR_Def_Facet := "1c"]
port.aggr[, Facet_math := factor(SICR_Def_Facet,labels = c("'SICR-definition class 1c ('*italic(d)==1*','~italic(s)==3*')'"))]

# - Create ancillary graphing dataset to annotate certain features within eventual main graph
datAnnotate <- data.table(Date=max_locales_1c, EventRate=max_1c, SICR_Def=c("1c(i)", "1c(ii)", "1c(iii)", "1c(iv)"),
                          SICR_Def_Facet="1c")



# --- 3. Graphing | Main Time Graph
# - Graphing parameters
col.v <- brewer.pal(7, "Dark2")
x.label.period <- 6
chosenFont <- "Cambria"
label.v <- c("1c(i)"=  bquote("1c(i):   "*italic(k)==3), 
             "1c(ii)"= bquote("1c(ii):  "*italic(k)==6), 
             "1c(iii)"=bquote("1c(iii): "*italic(k)==9),
             "1c(iv)"= bquote("1c(iv):  "*italic(k)==12))

# - 1. Create main time series graph
(g <- ggplot(port.aggr, aes(x=Date, y=EventRate, group=SICR_Def)) + theme_minimal() + 
    labs(x="Reporting date (months)", y="Conditional SICR-rate (%) [Actual]") + 
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
    facet_grid(Facet_math ~., scales="free", labeller=label_parsed) + 
    scale_colour_manual(name="SICR-Definition", values=col.v, labels=label.v) + 
    scale_shape_manual(name="SICR-Definition", values=c(16,17,15,8), labels=label.v) + 
    scale_linetype_discrete(name="SICR-Definition", labels=label.v) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(x.label.period, " month"), date_labels = "%b %Y") )

# - Create dataset for inset graph to show summaries
datSummary <- data.table(k=c(3,6,9,12), SICR_Def=c("1c(i)", "1c(ii)", "1c(iii)", "1c(iv)"), 
                         SICR_sd = stdevs_1c_full, SICR_mean = mean_1c) %>% 
  pivot_longer(cols=SICR_sd:SICR_mean, names_to="Summary_Type", values_to = "Summary_Value") %>% as.data.table()

# - 2. Create inset graph for summaries; standard deviation + mean
(g2 <- ggplot(datSummary, aes(x=k, y=Summary_Value, group=Summary_Type)) + theme_minimal() +
    labs(x=bquote(italic(k)*"-values"), y="Summary Statistic (%)") + 
    theme(text=element_text(family=chosenFont), legend.position = c(0.6,0.6),
          axis.text.y=element_text(size=9),
          panel.grid.major = element_blank(),  panel.grid.minor=element_blank(), 
          panel.background=element_rect(color="black", fill="white"),
          plot.background=element_rect(color="white"),
          plot.margin=unit(c(0,0,0,0), "mm")) + 
    # main line graph with overlaid points
    geom_line(aes(linetype=Summary_Type), colour="gray20", linewidth=0.3) + geom_point(aes(colour=SICR_Def, shape=SICR_Def)) + 
    # facets & scale options
    scale_colour_manual(name="", values=col.v, guide="none") + 
    scale_shape_manual(name="", values=c(16,17,15,8), guide="none") + 
    scale_linetype_discrete(name="", labels=c("SICR_mean"="Mean", "SICR_sd"="Standard deviation")) + 
    scale_x_continuous(breaks=pretty_breaks(n=8)) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent))

# - Merge graphs
(gmain <- g + annotation_custom2(grob=ggplotGrob(g2), data=port.aggr[SICR_Def_Facet == "1c",], 
                                 xmin=as.Date("2012-06-30"), xmax=as.Date("2020-01-31"), ymin=0.03, ymax=0.073))

# - Save graph
dpi <- 200
ggsave(gmain, file=paste0(genFigPath, "TimeGraph_SICR-Incidence_Actual_1c.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")



# --- 4. Graphing | Secondary k-value summary graph

# - Subset and recast graphing dataset to show only certain measures of interest
datGraph_1c.aggr <- pivot_longer(datGraph_1c[,list(k, a_Rate_First=Rate_First, b_Rate_Max=Rate_Max, c_Rate_mean_postGFC=Rate_mean_postGFC, 
                                                   d_Diff_First_Max=Diff_First_Max, e_Diff_Max_Mean_postGFC=Diff_Max_Mean_postGFC)],
                                 cols=a_Rate_First:e_Diff_Max_Mean_postGFC, names_to="Aggregate_Type", values_to="Aggregate_Value") %>% as.data.table()
datGraph_1c.aggr[, Aggregate_Type2 := substr(Aggregate_Type,1,2)] # aesthetic reasons so that we can have two size scales in effect

# - Subset again for encircling certain points
datGraph_1c.aggr2 <- subset(datGraph_1c.aggr, k %in% c(6,9) & Aggregate_Type %in% c("d_Diff_First_Max","e_Diff_Max_Mean_postGFC"), 
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
(g4 <- ggplot(datGraph_1c.aggr, aes(x=k, y=Aggregate_Value, group=Aggregate_Type)) + theme_minimal() + 
    labs(x=bquote(italic(k)*"-value in SICR-definition class 1c"), y="Summary statistic (%)") + 
    theme(text=element_text(family=chosenFont),legend.position = "right",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Aggregate_Type, linetype=Aggregate_Type, linewidth=Aggregate_Type)) + 
    geom_point(aes(colour=Aggregate_Type, shape=Aggregate_Type, size=Aggregate_Type2)) + 
    # Encircle certain points
    geom_point(data=datGraph_1c.aggr2, aes(x=k, y=Aggregate_Value, group=Aggregate_Type), size=7, colour="black", shape=1) + 
    # facets & scale options
    scale_colour_manual(name="Summary Type", values=col.v, labels=label.v) + 
    scale_shape_manual(name="Summary Type", values=c(7,8,15, 16, 17), labels=label.v) + 
    scale_linetype_manual(name="Summary Type", values=c("twodash", "dotdash", "dotted", "solid", "dashed"), labels=label.v) + 
    scale_size_manual(name="Summary Type", values=size.v, labels=label.v) + 
    scale_linewidth_manual(name="Summary Type", values=size.v, labels=label.v) + 
    guides(colour=guide_legend(ncol=1,byrow=T), size="none") + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_continuous(breaks=pretty_breaks(n=6)) )

# - Save graph
dpi <- 200
ggsave(g4, file=paste0(genFigPath, "Summaries_SICR-Incidence_Actual_1c.png"), width=1100/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Cleanup
rm(port.aggr, datGraph_1c, datGraph_1c.aggr2, g, g2, gmain, g4, port.aggr2, datSummary, datAnnotate)





# ------ 4. Consolidated Time graphs of actual SICR-rates across k-values (1b + 1c)

# --- 1. Graph data preparation and aggregation, as originally extracted in sections 2-3 of this script 4b

# - Aggregate to monthly level and observe up to given point
SICR_StartDte_1b <- rollforward(min(datSICR_1c$Date, na.rm=T) + month(1))
SICR_StartDte_1c <- rollforward(min(datSICR_1c$Date, na.rm=T) + months(2))
# NOTE: We reduced the sampling window by 1-2 months to avoid unexplained spike in event rate, likely due to underlying data issues from source
SICR_EndDte <- max(datSICR_1c$Date, na.rm=T)
port.aggr_1b <- datSICR_1b[SICR_def==0,list(EventRate = sum(SICR_events, na.rm=T)/.N, AtRisk = .N),
                           by=list(SICR_Def, Date)][Date >= SICR_StartDte_1b & Date <= SICR_EndDte,] %>% setkey(SICR_Def,Date)
port.aggr_1c <- datSICR_1c[SICR_def==0,list(EventRate = sum(SICR_events, na.rm=T)/.N, AtRisk = .N),
                           by=list(SICR_Def, Date)][Date >= SICR_StartDte_1c & Date <= SICR_EndDte,] %>% setkey(SICR_Def,Date)

# - Recast/pivot data in order to create useful summaries across all time series
port.aggr2_1b <- port.aggr_1b %>% pivot_wider(id_cols = c(Date), names_from = c(SICR_Def), values_from = c(EventRate)) %>% as.data.table()
port.aggr2_1c <- port.aggr_1c %>% pivot_wider(id_cols = c(Date), names_from = c(SICR_Def), values_from = c(EventRate)) %>% as.data.table()

# - Calculate useful summaries over rate time series
stdevs_1b_full <- c(sd(port.aggr2_1b$`1b(i)`, na.rm=T), sd(port.aggr2_1b$`1b(ii)`, na.rm=T), 
                    sd(port.aggr2_1b$`1b(iii)`, na.rm=T), sd(port.aggr2_1b$`1b(iv)`, na.rm=T))
stdevs_1c_full <- c(sd(port.aggr2_1c$`1c(i)`, na.rm=T), sd(port.aggr2_1c$`1c(ii)`, na.rm=T), 
                    sd(port.aggr2_1c$`1c(iii)`, na.rm=T), sd(port.aggr2_1c$`1c(iv)`, na.rm=T))
first_1b <- c(port.aggr2_1b[, `1b(i)`[1]], port.aggr2_1b[, `1b(ii)`[1]], port.aggr2_1b[, `1b(iii)`[1]],
              port.aggr2_1b[, `1b(iv)`[1]])
first_1c <- c(port.aggr2_1c[, `1c(i)`[1]], port.aggr2_1c[, `1c(ii)`[1]], port.aggr2_1c[, `1c(iii)`[1]],
              port.aggr2_1c[, `1c(iv)`[1]])
max_1b <- c(max(port.aggr2_1b$`1b(i)`, na.rm=T), max(port.aggr2_1b$`1b(ii)`, na.rm=T), max(port.aggr2_1b$`1b(iii)`, na.rm=T),
            max(port.aggr2_1b$`1b(iv)`, na.rm=T))
max_1c <- c(max(port.aggr2_1c$`1c(i)`, na.rm=T), max(port.aggr2_1c$`1c(ii)`, na.rm=T), max(port.aggr2_1c$`1c(iii)`, na.rm=T),
            max(port.aggr2_1c$`1c(iv)`, na.rm=T))
max_locales_1b <- c( port.aggr2_1b[, Date[which(`1b(i)`==max(`1b(i)`,na.rm=T))]], port.aggr2_1b[, Date[which(`1b(ii)`==max(`1b(ii)`,na.rm=T))]],
                     port.aggr2_1b[, Date[which(`1b(iii)`==max(`1b(iii)`,na.rm=T))]], port.aggr2_1b[, Date[which(`1b(iv)`==max(`1b(iv)`,na.rm=T))]])
max_locales_1c <- c( port.aggr2_1c[, Date[which(`1c(i)`==max(`1c(i)`,na.rm=T))]], port.aggr2_1c[, Date[which(`1c(ii)`==max(`1c(ii)`,na.rm=T))]],
                     port.aggr2_1c[, Date[which(`1c(iii)`==max(`1c(iii)`,na.rm=T))]], port.aggr2_1c[, Date[which(`1c(iv)`==max(`1c(iv)`,na.rm=T))]])
mean_1b <- c(mean(port.aggr2_1b$`1b(i)`, na.rm=T), mean(port.aggr2_1b$`1b(ii)`, na.rm=T), mean(port.aggr2_1b$`1b(iii)`, na.rm=T),
             mean(port.aggr2_1b$`1b(iv)`, na.rm=T))
mean_1c <- c(mean(port.aggr2_1c$`1c(i)`, na.rm=T), mean(port.aggr2_1c$`1c(ii)`, na.rm=T), mean(port.aggr2_1c$`1c(iii)`, na.rm=T),
             mean(port.aggr2_1c$`1c(iv)`, na.rm=T))
mean_1b_postgfc <- c(port.aggr2_1b[Date > "2009-12-31", mean(`1b(i)`,na.rm=T)], port.aggr2_1b[Date > "2009-12-31", mean(`1b(ii)`,na.rm=T)], 
                     port.aggr2_1b[Date > "2009-12-31", mean(`1b(iii)`,na.rm=T)], port.aggr2_1b[Date > "2009-12-31", mean(`1b(iv)`,na.rm=T)])
mean_1c_postgfc <- c(port.aggr2_1c[Date > "2009-12-31", mean(`1c(i)`,na.rm=T)], port.aggr2_1c[Date > "2009-12-31", mean(`1c(ii)`,na.rm=T)], 
                     port.aggr2_1c[Date > "2009-12-31", mean(`1c(iii)`,na.rm=T)], port.aggr2_1c[Date > "2009-12-31", mean(`1c(iv)`,na.rm=T)])

# - Create graphing dataset for secondary graph that shows series summaries
datGraph_1b <- data.table(k=c(3,6,9,12), Rate_Max=max_1b, Rate_First=first_1b, Rate_Mean=mean_1b, Rate_mean_postGFC=mean_1b_postgfc,
                          Diff_First_Max=max_1b-first_1b, Diff_Max_Mean_postGFC=max_1b-mean_1b_postgfc)
datGraph_1c <- data.table(k=c(3,6,9,12), Rate_Max=max_1c, Rate_First=first_1c, Rate_Mean=mean_1c, Rate_mean_postGFC=mean_1c_postgfc,
                          Diff_First_Max=max_1c-first_1c, Diff_Max_Mean_postGFC=max_1c-mean_1c_postgfc)

# - Create graphing-related bins for facetting and associated facet labels using plotmath
port.aggr_1b[, SICR_Def_Facet := "1b"]; facetMath_1b <- "'SICR-definition class 1b ('*italic(d)==1*','~italic(s)==2*')'"
port.aggr_1b[, Facet_math := facetMath_1b]
port.aggr_1c[, SICR_Def_Facet := "1c"]; facetMath_1c <-"'SICR-definition class 1c ('*italic(d)==1*','~italic(s)==3*')'"
port.aggr_1c[, Facet_math := facetMath_1c]

# - Create ancillary graphing dataset to annotate certain features within eventual main graph
# NOTE: SICR-labels are stripped of SICR-definition information to prepare for coalescing across s-values (1b,1c) 
# by having a single legend variable denoting k-values across s-values
datAnnotate_1b <- data.table(Date=max_locales_1b, EventRate=max_1b, SICR_Def_k=c("a_i", "b_ii", "c_iii", "d_iv"),
                             Facet_math = facetMath_1b)
datAnnotate_1c <- data.table(Date=max_locales_1c, EventRate=max_1c, SICR_Def_k=c("a_i", "b_ii", "c_iii", "d_iv"),
                             Facet_math = facetMath_1c)

# - Fuse graphing objects across definition-classes
port.aggr <- rbind(port.aggr_1b, port.aggr_1c)
datAnnotate <- rbind(datAnnotate_1b, datAnnotate_1c)

# - Create new legend variable to coalesce k-values across s-values (1b,1c) to facilitate graphing
port.aggr[, SICR_Def_k := case_when(grepl("(i)", SICR_Def, fixed=T) ~ "a_i", grepl("(ii)", SICR_Def, fixed=T) ~ "b_ii",
                                    grepl("(iii)", SICR_Def, fixed=T) ~ "c_iii", grepl("(iv)", SICR_Def, fixed=T) ~ "d_iv")]
describe(port.aggr$SICR_Def_k)



# --- 2. Graphing | Main Time Graph + Inset graphs
# - Graphing parameters
col.v <- brewer.pal(8, "Dark2")
x.label.period <- 6
chosenFont <- "Cambria"
label.v <- c("a_i"=  bquote("(i):   "*italic(k)==3),
             "b_ii"= bquote("(ii):  "*italic(k)==6), 
             "c_iii"=bquote("(iii): "*italic(k)==9),
             "d_iv"= bquote("(iv):  "*italic(k)==12))

# - 1. Create main time series graph
(g <- ggplot(port.aggr, aes(x=Date, y=EventRate, group=SICR_Def_k)) + theme_minimal() + 
    labs(x="Reporting date (months)", y="Conditional SICR-rate (%) [Actual]") + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90),
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # overlay maxima
    geom_point(data=datAnnotate, aes(x=Date,y=EventRate,group=SICR_Def_k,colour=SICR_Def_k), size=5, shape=1, show.legend=F) +     
    # main line graph with overlaid points
    geom_line(aes(colour=SICR_Def_k, linetype=SICR_Def_k), linewidth=0.2) + 
    geom_point(aes(colour=SICR_Def_k, shape=SICR_Def_k), size=0.8) + 
    # facets & scale options
    facet_grid(Facet_math ~., scales="free", labeller=label_parsed) + 
    scale_colour_manual(name="SICR-Definition", values=col.v, labels=label.v) + 
    scale_shape_manual(name="SICR-Definition", values=c(16,17,15,8), labels=label.v) + 
    scale_linetype_discrete(name="SICR-Definition", labels=label.v) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(x.label.period, " month"), date_labels = "%b %Y") )


# - 2. Inset graphs for summarizing key statistics from time graphs of 1b, 1c
# Create graping datasets for inset graph to show summaries
datSummary_1b <- data.table(k=c(3,6,9,12), SICR_Def=c("a_i", "b_ii", "c_iii", "d_iv"), 
                         SICR_sd = stdevs_1b_full, SICR_mean = mean_1b) %>% 
  pivot_longer(cols=SICR_sd:SICR_mean, names_to="Summary_Type", values_to = "Summary_Value") %>% as.data.table()
datSummary_1c <- data.table(k=c(3,6,9,12), SICR_Def=c("a_i", "b_ii", "c_iii", "d_iv"), 
                            SICR_sd = stdevs_1c_full, SICR_mean = mean_1c) %>% 
  pivot_longer(cols=SICR_sd:SICR_mean, names_to="Summary_Type", values_to = "Summary_Value") %>% as.data.table()


# - 3. Create inset graph for summaries; standard deviation + mean | 1b
(g2_1b <- ggplot(datSummary_1b, aes(x=k, y=Summary_Value, group=Summary_Type)) + theme_minimal() +
    labs(x=bquote(italic(k)*"-values"), y="Summary Statistic (%)") + 
    theme(text=element_text(family=chosenFont), legend.position = c(0.6,0.6),
          axis.text.y=element_text(size=9),
          panel.grid.major = element_blank(),  panel.grid.minor=element_blank(), 
          panel.background=element_rect(color="black", fill="white"),
          plot.background=element_rect(color="white"),
          plot.margin=unit(c(0,0,0,0), "mm")) + 
    # main line graph with overlaid points
    geom_line(aes(linetype=Summary_Type), colour="gray20", linewidth=0.3) + geom_point(aes(colour=SICR_Def, shape=SICR_Def)) + 
    # facets & scale options
    scale_colour_manual(name="", values=col.v, guide="none") + 
    scale_shape_manual(name="", values=c(16,17,15,8), guide="none") + 
    scale_linetype_discrete(name="", labels=c("SICR_mean"="Mean", "SICR_sd"="Standard deviation")) + 
    scale_x_continuous(breaks=pretty_breaks(n=8)) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent))

# - 4. Create inset graph for summaries; standard deviation + mean | 1c
(g2_1c <- ggplot(datSummary_1c, aes(x=k, y=Summary_Value, group=Summary_Type)) + theme_minimal() +
    labs(x=bquote(italic(k)*"-values"), y="Summary Statistic (%)") + 
    theme(text=element_text(family=chosenFont), legend.position = c(0.6,0.6),
          axis.text.y=element_text(size=9),
          panel.grid.major = element_blank(),  panel.grid.minor=element_blank(), 
          panel.background=element_rect(color="black", fill="white"),
          plot.background=element_rect(color="white"),
          plot.margin=unit(c(0,0,0,0), "mm")) + 
    # main line graph with overlaid points
    geom_line(aes(linetype=Summary_Type), colour="gray20", linewidth=0.3) + geom_point(aes(colour=SICR_Def, shape=SICR_Def)) + 
    # facets & scale options
    scale_colour_manual(name="", values=col.v, guide="none") + 
    scale_shape_manual(name="", values=c(16,17,15,8), guide="none") + 
    scale_linetype_discrete(name="", labels=c("SICR_mean"="Mean", "SICR_sd"="Standard deviation")) + 
    scale_x_continuous(breaks=pretty_breaks(n=8)) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent))


# - 5. Merge graphs
(gmain <- g + annotation_custom2(grob=ggplotGrob(g2_1b), data=port.aggr[SICR_Def_Facet == "1b",], 
                                 xmin=as.Date("2012-06-30"), xmax=as.Date("2020-01-31"), ymin=0.035, ymax=0.084))
(gmain <- gmain + annotation_custom2(grob=ggplotGrob(g2_1c), data=port.aggr[SICR_Def_Facet == "1c",], 
                                 xmin=as.Date("2012-06-30"), xmax=as.Date("2020-01-31"), ymin=0.03, ymax=0.073))

# - Save graph
dpi <- 200
ggsave(gmain, file=paste0(genFigPath, "TimeGraph_SICR-Incidence_Actual_1d_bc.png"), width=1200/dpi, height=1500/dpi, dpi=dpi, bg="white")





# ------ 5. Consolidated statistical summaries of actual SICR-rates across k-values (1b + 1c)

# - Stack prepared summaries from 1b and 1c vertically, as created respectively in section 4 of this script 4b.
datGraph_1bc.aggr <- rbind(data.table(datGraph_1b.aggr,Facet="1b"), data.table(datGraph_1c.aggr,Facet="1c"))
datGraph_1bc.aggr2 <- rbind(data.table(datGraph_1b.aggr2,Facet="1b"), data.table(datGraph_1c.aggr2,Facet="1c"))
  
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
datGraph_1bc.aggr[, Facet_math := factor(Facet,labels = c("'SICR-Definition class 1b ('*italic(d)==1*','~italic(s)==2*')'",
                                                          "'SICR-Definition class 1c ('*italic(d)==1*','~italic(s)==3*')'"))]
datGraph_1bc.aggr2[, Facet_math := factor(Facet,labels = c("'SICR-Definition class 1b ('*italic(d)==1*','~italic(s)==2*')'",
                                                          "'SICR-Definition class 1c ('*italic(d)==1*','~italic(s)==3*')'"))]

# - Create corresponding graph
(g <- ggplot(datGraph_1bc.aggr, aes(x=k, y=Aggregate_Value, group=Aggregate_Type)) + theme_minimal() + 
    labs(x=bquote(italic(k)*"-value in SICR-definition class"), y="Summary statistic (%)") + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Aggregate_Type, linetype=Aggregate_Type, linewidth=Aggregate_Type)) + 
    geom_point(aes(colour=Aggregate_Type, shape=Aggregate_Type, size=Aggregate_Type2)) + 
    ## Encircle certain points
    geom_point(data=datGraph_1bc.aggr2, aes(x=k, y=Aggregate_Value, group=Aggregate_Type), size=7, colour="black", shape=1) + 
    # facets & scale options
    facet_grid(.~Facet_math, scales="free", labeller=label_parsed) + 
    scale_colour_manual(name="Summary Type", values=col.v, labels=label.v) + 
    scale_shape_manual(name="Summary Type", values=c(7,8,15, 16, 17), labels=label.v) + 
    scale_linetype_manual(name="Summary Type", values=c("twodash", "dotdash", "dotted", "solid", "dashed"), labels=label.v) + 
    scale_size_manual(name="Summary Type", values=size.v, labels=label.v) + 
    scale_linewidth_manual(name="Summary Type", values=size.v, labels=label.v) + 
    guides(colour=guide_legend(ncol=3,byrow=T), size="none") + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_continuous(breaks=pretty_breaks(n=6)) )


# - Save graph
dpi <- 220
ggsave(g, file=paste0(genFigPath, "Summaries_SICR-Incidence_Actual_1d_bc.png"), width=1600/dpi, height=1200/dpi, dpi=dpi, bg="white")





# ------ 5. CONCLUSION
# 1a)  Comparison in standard deviation of SICR-rates [stdevs_1a_full, stdevs_1b_full, stdevs_1c_full] | "Instability" \sigma
#   Upwards trend that seems to slow once reaching k=12. Likely to trend downwards should k be extended beyond 12, as in class 1a.
#   Roughly same value range (0.1% - 1.6%), though the entire range does seem to decrease slightly as s increases. This is sensible,
#     given the supposed stabilization effect of s
# 1b)  Comparison in GFC-specific standard deviation of SICR-rates [stdevs_1a_gfc, stdevs_1b_gfc, stdevs_1c_gfc]
#   Same overall upward trend as in conclusion 1a, though perhaps even more stark differences between k-values
# 1c)  Comparison in post-GFC standard deviation of SICR-rates [stdevs_1a_postgfc, stdevs_1b_postgfc, stdevs_1b_postgfc]
#   No clear pattern in results amongst s-values
# 2a)  Comparison in max-values of SICR-rates [max_1a, max_1b, max_1c]
#   Some agreement in pattern amongst s-values in that max SICR-rate increases as k-increases, generally at least.
#   Moreover, the whole value range across k-values seem to decrease slightly for larger s-values. This is sensible since larger s-values
#   makes delinquency tests more strenuous and harder to meet, which means SICR is less-prevalent - as supported by decreasing prevalence rates
#   as both s and k increases.
# 3a)  Comparison in mean-values of SICR-rates [mean_1a, mean_1b, mean_1c]:
#   General upwards trend in mean as k-increases, pattern persists across s-values. However, the whole value range across k-values seem to
#   decrease slightly for larger s-values. As in conclusion 2a, this is due to SICR-prevalence decreasing overall.
# 3b)  Comparison in post-GFC mean-values of SICR-rates [mean_1a_postgfc, mean_1b_postgfc, mean_1c_postgfc]:
#   Same patterns and conclusions as in conclusion 3a. However, the whole value range is lower than that of 3a since the effects of the
#   2008-GFC has been stripped out.
