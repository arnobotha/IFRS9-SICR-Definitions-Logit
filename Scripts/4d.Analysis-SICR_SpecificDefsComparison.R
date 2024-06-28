# ============================== SICR-DEFINITION ANALYSIS ===============================
# Script for comparing SICR-incidence rates over time across various SICR-definitions.
# In particular, we analyse definition class 1-2 and compare results across (d,s,k)-parameters
# with a special focus on k=6,9 and d=1
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
#   - 3a.SICR_def_<>_logit.R | the 3a-series of scripts for definitions 1a-2c, for (i)-(iv)

# -- Inputs:
#   - datSICR_smp_<> | specific SICR-sample upon which resampling scheme is applied (3a)

# -- Outputs:
#   - <analytics>
# =======================================================================================





# ------ 1. SICR-Incidence/prevalence across SICR-definitions

# --- 1. Load each sample into memory and bind together successively
SICR_label <- "1a(i)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target , SICR_Def=SICR_label, d=1, s=1, k=3)]); rm(datSICR_smp)
SICR_label <- "1a(ii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=1, s=1, k=6)])); rm(datSICR_smp)
SICR_label <- "1a(iii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=1, s=1, k=9)])); rm(datSICR_smp)
SICR_label <- "1a(iv)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=1, s=1, k=12)])); rm(datSICR_smp)
SICR_label <- "1a(v)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=1, s=1, k=18)])); rm(datSICR_smp)
SICR_label <- "1a(vi)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=1, s=1, k=24)])); rm(datSICR_smp)
SICR_label <- "1a(vii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=1, s=1, k=36)])); rm(datSICR_smp)
# rm(datSICR)

SICR_label <- "1b(i)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=1, s=2, k=3)])); rm(datSICR_smp)
SICR_label <- "1b(ii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=1, s=2, k=6)])); rm(datSICR_smp)
SICR_label <- "1b(iii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=1, s=2, k=9)])); rm(datSICR_smp)
SICR_label <- "1b(iv)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=1, s=2, k=12)])); rm(datSICR_smp)
# rm(datSICR)

SICR_label <- "1c(i)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target , SICR_Def=SICR_label, d=1, s=3, k=3)])); rm(datSICR_smp)
SICR_label <- "1c(ii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=1, s=3, k=6)])); rm(datSICR_smp)
SICR_label <- "1c(iii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=1, s=3, k=9)])); rm(datSICR_smp)
SICR_label <- "1c(iv)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=1, s=3, k=12)])); rm(datSICR_smp)

SICR_label <- "2a(i)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=2, s=1, k=3)])); rm(datSICR_smp)
SICR_label <- "2a(ii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=2, s=1, k=6)])); rm(datSICR_smp)
SICR_label <- "2a(iii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=2, s=1, k=9)])); rm(datSICR_smp)
SICR_label <- "2a(iv)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=2, s=1, k=12)])); rm(datSICR_smp)

SICR_label <- "2b(i)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target , SICR_Def=SICR_label, d=2, s=2, k=3)])); rm(datSICR_smp)
SICR_label <- "2b(ii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=2, s=2, k=6)])); rm(datSICR_smp)
SICR_label <- "2b(iii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=2, s=2, k=9)])); rm(datSICR_smp)
SICR_label <- "2b(iv)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=2, s=2, k=12)])); rm(datSICR_smp)

SICR_label <- "2c(i)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target , SICR_Def=SICR_label, d=2, s=3, k=3)])); rm(datSICR_smp)
SICR_label <- "2c(ii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=2, s=3, k=6)])); rm(datSICR_smp)
SICR_label <- "2c(iii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=2, s=3, k=9)])); rm(datSICR_smp)
SICR_label <- "2c(iv)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
datSICR <- rbind(datSICR, copy(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, SICR_Def=SICR_label, d=2, s=3, k=12)])); rm(datSICR_smp)




# --- 2. Graph data preparation and aggregation | Selected SICR-definitions (d=1, k=6,9, s=1...3)

# - Transform factor back to numeric variables for aggregation purposes
datSICR[, SICR_events := as.numeric(levels(SICR_events))[SICR_events]]

# - Aggregate to monthly level and observe up to given point
SICR_StartDte_1a <- rollforward(min(datSICR$Date, na.rm=T))
SICR_StartDte_1b <- rollforward(min(datSICR$Date, na.rm=T) + month(1))
SICR_StartDte_1c <- rollforward(min(datSICR$Date, na.rm=T) + months(2))
# NOTE: We reduced the sampling window by 1-2 months to avoid unexplained spike in event rate, likely due to underlying data issues from source
SICR_EndDte <- max(datSICR$Date, na.rm=T)
port.aggr_1k6s <- datSICR[SICR_def==0 & d==1 & k==6 & Date <= SICR_EndDte & ((s==1 & Date >= SICR_StartDte_1a) | (s==2 & Date >= SICR_StartDte_1b) | (s==3 & Date >= SICR_StartDte_1c)),
                          list(EventRate = sum(SICR_events, na.rm=T)/.N, AtRisk = .N),
                          by=list(SICR_Def, Date)] %>% setkey(SICR_Def,Date)
port.aggr_1k9s <- datSICR[SICR_def==0 & d==1 & k==9 & Date <= SICR_EndDte & ((s==1 & Date >= SICR_StartDte_1a) | (s==2 & Date >= SICR_StartDte_1b) | (s==3 & Date >= SICR_StartDte_1c)),
                          list(EventRate = sum(SICR_events, na.rm=T)/.N, AtRisk = .N),
                          by=list(SICR_Def, Date)] %>% setkey(SICR_Def,Date)

# - Recast/pivot data in order to create useful summaries across all time series
port.aggr2_1k6s <- port.aggr_1k6s %>% pivot_wider(id_cols = c(Date, SICR_Def), names_from = c(SICR_Def), values_from = c(EventRate)) %>% as.data.table()
port.aggr2_1k9s <- port.aggr_1k9s %>% pivot_wider(id_cols = c(Date, SICR_Def), names_from = c(SICR_Def), values_from = c(EventRate)) %>% as.data.table()

# - Calculate useful summaries over rate time series
stdevs_1k6s_full <- c(sd(port.aggr2_1k6s$`1a(ii)`, na.rm=T), sd(port.aggr2_1k6s$`1b(ii)`, na.rm=T), 
                      sd(port.aggr2_1k6s$`1c(ii)`, na.rm=T))
stdevs_1k9s_full <- c(sd(port.aggr2_1k9s$`1a(iii)`, na.rm=T), sd(port.aggr2_1k9s$`1b(iii)`, na.rm=T), 
                      sd(port.aggr2_1k9s$`1c(iii)`, na.rm=T))
first_1k6s <- c(port.aggr2_1k6s[, `1a(ii)`[1]], port.aggr2_1k6s[, `1b(ii)`[1]], port.aggr2_1k6s[, `1c(ii)`[1]])
first_1k9s <- c(port.aggr2_1k9s[, `1a(iii)`[1]], port.aggr2_1k9s[, `1b(iii)`[1]], port.aggr2_1k9s[, `1c(iii)`[1]])
max_1k6s <- c(max(port.aggr2_1k6s$`1a(ii)`, na.rm=T), max(port.aggr2_1k6s$`1b(ii)`, na.rm=T), max(port.aggr2_1k6s$`1c(ii)`, na.rm=T))
max_1k9s <- c(max(port.aggr2_1k9s$`1a(iii)`, na.rm=T), max(port.aggr2_1k9s$`1b(iii)`, na.rm=T), max(port.aggr2_1k9s$`1c(iii)`, na.rm=T))
max_locales_1k6s <- c( port.aggr2_1k6s[, Date[which(`1a(ii)`==max(`1a(ii)`,na.rm=T))]], port.aggr2_1k6s[, Date[which(`1b(ii)`==max(`1b(ii)`,na.rm=T))]], 
                       port.aggr2_1k6s[, Date[which(`1c(ii)`==max(`1c(ii)`,na.rm=T))]])
max_locales_1k9s <- c( port.aggr2_1k9s[, Date[which(`1a(iii)`==max(`1a(iii)`,na.rm=T))]], port.aggr2_1k9s[, Date[which(`1b(iii)`==max(`1b(iii)`,na.rm=T))]], 
                       port.aggr2_1k9s[, Date[which(`1c(iii)`==max(`1c(iii)`,na.rm=T))]])
mean_1k6s <- c(mean(port.aggr2_1k6s$`1a(ii)`, na.rm=T), mean(port.aggr2_1k6s$`1b(ii)`, na.rm=T), mean(port.aggr2_1k6s$`1c(ii)`, na.rm=T))
mean_1k9s <- c(mean(port.aggr2_1k9s$`1a(iii)`, na.rm=T), mean(port.aggr2_1k9s$`1b(iii)`, na.rm=T), mean(port.aggr2_1k9s$`1c(iii)`, na.rm=T))
mean_1k6s_postgfc <- c(port.aggr2_1k6s[Date > "2009-12-31", mean(`1a(ii)`,na.rm=T)], port.aggr2_1k6s[Date > "2009-12-31", mean(`1b(ii)`,na.rm=T)], 
                       port.aggr2_1k6s[Date > "2009-12-31", mean(`1c(ii)`,na.rm=T)])
mean_1k9s_postgfc <- c(port.aggr2_1k9s[Date > "2009-12-31", mean(`1a(iii)`,na.rm=T)], port.aggr2_1k9s[Date > "2009-12-31", mean(`1b(iii)`,na.rm=T)], 
                       port.aggr2_1k9s[Date > "2009-12-31", mean(`1c(iii)`,na.rm=T)])

# - Create graphing dataset for secondary graph that shows series summaries
datGraph_1k6s <- data.table(s=c(1,2,3), Rate_Max=max_1k6s, Rate_First=first_1k6s, Rate_Mean=mean_1k6s, Rate_mean_postGFC=mean_1k6s_postgfc,
                            Diff_First_Max=max_1k6s-first_1k6s, Diff_Max_Mean_postGFC=max_1k6s-mean_1k6s_postgfc)
datGraph_1k9s <- data.table(s=c(1,2,3), Rate_Max=max_1k9s, Rate_First=first_1k9s, Rate_Mean=mean_1k9s, Rate_mean_postGFC=mean_1k9s_postgfc,
                            Diff_First_Max=max_1k9s-first_1k9s, Diff_Max_Mean_postGFC=max_1k9s-mean_1k9s_postgfc)

# - Create graphing-related bins for facetting
port.aggr_1k6s[, SICR_Def_Facet := "1k6s"]; facetMath_1k6s <- "'SICR-definition ('*italic(d)==1*','~italic(k)==6*')'"
port.aggr_1k6s[, Facet_math := facetMath_1k6s]
port.aggr_1k9s[, SICR_Def_Facet := "1k9s"]; facetMath_1k9s <- "'SICR-definition ('*italic(d)==1*','~italic(k)==9*')'"
port.aggr_1k9s[, Facet_math := facetMath_1k9s]

# - Create ancillary graphing dataset to annotate certain features within eventual main graph
# NOTE: SICR-labels are stripped of SICR-definition information to prepare for coalescing across k-values
# by having a single legend variable denoting s-values across k-values
datAnnotate_1k6s <- data.table(Date=max_locales_1k6s, EventRate=max_1k6s, SICR_Def_s=c("a_s1", "b_s2", "c_s3"),
                               Facet_math = facetMath_1k6s)
datAnnotate_1k9s <- data.table(Date=max_locales_1k9s, EventRate=max_1k9s, SICR_Def_s=c("a_s1", "b_s2", "c_s3"),
                               Facet_math = facetMath_1k9s)

# - Fuse graphing objects across definition-classes
port.aggr <- rbind(port.aggr_1k6s, port.aggr_1k9s)
datAnnotate <- rbind(datAnnotate_1k6s, datAnnotate_1k9s)

# - Create new legend variable to coalesce s-values across k-values to facilitate graphing
port.aggr[, SICR_Def_s := case_when(grepl("a", SICR_Def, fixed=T) ~ "a_s1", grepl("b", SICR_Def, fixed=T) ~ "b_s2",
                                    grepl("c", SICR_Def, fixed=T) ~ "c_s3")]
describe(port.aggr$SICR_Def_s)



# --- 3. Graphing | Main Time Graph for selected SICR-definitions
# - Graphing parameters
col.v <- brewer.pal(8, "Dark2")
x.label.period <- 6
chosenFont <- "Cambria"
label.v <- c("a_s1"=  bquote("1a:   "*italic(s)==1), 
             "b_s2"=bquote("1b: "*italic(s)==2),
             "c_s3"= bquote("1c:  "*italic(s)==3))

# - 1. Create main time series graph
(g <- ggplot(port.aggr, aes(x=Date, y=EventRate, group=SICR_Def_s)) + theme_minimal() + 
    labs(x="Reporting date (months)", y="Conditional SICR-rate (%) [Actual]") + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90),
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # overlay maxima
    geom_point(data=datAnnotate, aes(x=Date,y=EventRate,group=SICR_Def_s,colour=SICR_Def_s), size=5, shape=1, show.legend=F) +     
    # main line graph with overlaid points
    geom_line(aes(colour=SICR_Def_s, linetype=SICR_Def_s), size=0.2) + 
    geom_point(aes(colour=SICR_Def_s, shape=SICR_Def_s), size=0.8) + 
    # facets & scale options
    facet_grid(Facet_math ~., scales="free", labeller=label_parsed) + 
    scale_colour_manual(name="SICR-Definition", values=col.v, labels=label.v) + 
    scale_shape_manual(name="SICR-Definition", values=c(16,17,15,8), labels=label.v) + 
    scale_linetype_discrete(name="SICR-Definition", labels=label.v) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(x.label.period, " month"), date_labels = "%b %Y") )


# - 2. Inset graphs for summarizing key statistics from time graphs of 1b, 1c
# Create graping datasets for inset graph to show summaries
datSummary_1k6s <- data.table(s=c(1,2,3), SICR_Def=c("a_s1", "b_s2", "c_s3"), 
                            SICR_sd = stdevs_1k6s_full, SICR_mean = mean_1k6s) %>% 
  pivot_longer(cols=SICR_sd:SICR_mean, names_to="Summary_Type", values_to = "Summary_Value") %>% as.data.table()
datSummary_1k9s <- data.table(s=c(1,2,3), SICR_Def=c("a_s1", "b_s2", "c_s3"), 
                            SICR_sd = stdevs_1k9s_full, SICR_mean = mean_1k9s) %>% 
  pivot_longer(cols=SICR_sd:SICR_mean, names_to="Summary_Type", values_to = "Summary_Value") %>% as.data.table()


# - 3. Create inset graph for summaries; standard deviation + mean | 1b
(g2_1b <- ggplot(datSummary_1k6s, aes(x=s, y=Summary_Value, group=Summary_Type)) + theme_minimal() +
    labs(x=bquote(italic(s)*"-values"), y="Summary Statistic (%)") + 
    theme(text=element_text(family=chosenFont), legend.position = c(0.4,0.55),
          axis.text.y=element_text(size=9),
          panel.grid.major = element_blank(),  panel.grid.minor=element_blank(), 
          panel.background=element_rect(color="black", fill="white"),
          plot.background=element_rect(color="white"),
          plot.margin=unit(c(0,0,0,0), "mm")) + 
    # main line graph with overlaid points
    geom_line(aes(linetype=Summary_Type), colour="gray20", size=0.3) + geom_point(aes(colour=SICR_Def, shape=SICR_Def)) + 
    # facets & scale options
    scale_colour_manual(name="", values=col.v, guide="none") + 
    scale_shape_manual(name="", values=c(16,17,15,8), guide="none") + 
    scale_linetype_discrete(name="", labels=c("SICR_mean"="Mean", "SICR_sd"="Standard deviation")) + 
    scale_x_continuous(breaks=pretty_breaks(n=3)) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent))

# - 4. Create inset graph for summaries; standard deviation + mean | 1c
(g2_1c <- ggplot(datSummary_1k9s, aes(x=s, y=Summary_Value, group=Summary_Type)) + theme_minimal() +
    labs(x=bquote(italic(s)*"-values"), y="Summary Statistic (%)") + 
    theme(text=element_text(family=chosenFont), legend.position = c(0.4,0.55),
          axis.text.y=element_text(size=9),
          panel.grid.major = element_blank(),  panel.grid.minor=element_blank(), 
          panel.background=element_rect(color="black", fill="white"),
          plot.background=element_rect(color="white"),
          plot.margin=unit(c(0,0,0,0), "mm")) + 
    # main line graph with overlaid points
    geom_line(aes(linetype=Summary_Type), colour="gray20", size=0.3) + geom_point(aes(colour=SICR_Def, shape=SICR_Def)) + 
    # facets & scale options
    scale_colour_manual(name="", values=col.v, guide="none") + 
    scale_shape_manual(name="", values=c(16,17,15,8), guide="none") + 
    scale_linetype_discrete(name="", labels=c("SICR_mean"="Mean", "SICR_sd"="Standard deviation")) + 
    scale_x_continuous(breaks=pretty_breaks(n=3)) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent))


# - 5. Merge graphs
(gmain <- g + annotation_custom2(grob=ggplotGrob(g2_1b), data=port.aggr[SICR_Def_Facet == "1k6s",], 
                                 xmin=as.Date("2013-06-30"), xmax=as.Date("2020-01-31"), ymin=0.038, ymax=0.087))
(gmain <- gmain + annotation_custom2(grob=ggplotGrob(g2_1c), data=port.aggr[SICR_Def_Facet == "1k9s",], 
                                     xmin=as.Date("2013-06-30"), xmax=as.Date("2020-01-31"), ymin=0.04, ymax=0.093))

# - Save graph
dpi <- 200
ggsave(gmain, file=paste0(genFigPath, "TimeGraph_SICR-Incidence_Actual_1e_k69s.png"), width=1200/dpi, height=1500/dpi, dpi=dpi, bg="white")
