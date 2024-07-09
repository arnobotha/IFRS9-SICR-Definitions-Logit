# ============================== PD-MODELLIING ===============================
# Comparing a PD-based rule to SICR using SICR-definition 1b(iii)
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
#   - 3a.PD_logit.R
#   - 3b.SICR_def_1b(iii)_logit.R

# -- Inputs:
#   - datSICR_smp | saved data with required model outputs (script 3b.SICR_def_1b(iii)_logit.R)

# -- Outputs:
#   - <AUC of PD-based approach vs chosen SICR-model>
#   - <Actual vs Expected time graph of discretised outcomes>
# ==============================================================================




# ------ 0. Setup/parameter definition


# - Graphing parameters
chosenFont <- "Cambria"
dpi <- 180

# - Define SICR-definition label
SICR_label <- "1b(iii)" 




# ------- 1. Apply a cut-off on the PD-ratio to obtain discrete outcomes and calculate AUC

# - Unpack the saved dataset that contains the required PD-ratio, as well as the predicted SICR-outcome
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)

# - Implement a threshold based on the EBA-recommended threshold and the best performing threshold based on the AUC
datSICR_smp[, PD_Disc_reg := ifelse(PD_ratio >= 2, 1, 0)]
datSICR_smp[, PD_Disc_best := ifelse(PD_ratio >= 1.2, 1, 0)]

# - Calculate the AUC
auc(datSICR_smp$SICR_target, datSICR_smp$ExpDisc) # 76.75%
auc(datSICR_smp$SICR_target, datSICR_smp$PD_Disc_reg) # 51.51%
auc(datSICR_smp$SICR_target, datSICR_smp$PD_Disc_best) # 60.75%

### RESULTS:
# On a 200% threshold, the AUC is 51.51%, not much better than flipping a random coin
# On a 180% threshold, the AUC is 56.48%, slightly better than flipping a random coin
# On a 150% threshold, the AUC is 59.91%, which at least shows some predictive power
# On a 120% threshold, the AUC is 60.75%, only slightly better than 150%
# On a 300% threshold, the AUC is 51.02%, not much better than flipping a random coin
# As the threshold becomes smaller, the AUC improves and vice versa




# ------ 2. Portfolio Analytics

# --- 1. Comparison of actual vs predicted SICR-outcomes

# - Structure different line series together
datCompare_graph <- rbind(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, Type="a_Actual")],
                          datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=PD_Disc_reg, Type="b_PD_disc_reg")],
                          datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=PD_Disc_best, Type="c_PD_disc_best")],
                          datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=ExpDisc, Type="d_SICR_model_disc")])

# - Transform factor back to numeric variables for aggregation purposes
datCompare_graph[, SICR_events := as.numeric(levels(SICR_events))[SICR_events]]

# - Aggregate to monthly level and observe up to given point
SICR_StartDte <- min(datSICR_smp$Date, na.rm=T)
SICR_EndDte <- max(datSICR_smp$Date, na.rm=T)
port.aggr <- datCompare_graph[SICR_def==0, list(EventRate = sum(SICR_events, na.rm=T)/.N, AtRisk = .N),
                              by=list(Type, Date)][Date >= SICR_StartDte & Date <= SICR_EndDte,] %>% setkey(Type,Date)

# - Aesthetics engineering
port.aggr[, Facet_label := "Compare SICR-approaches"]

# - Calculate MAE over time by line graph type in summarising differences amongst line graphs
port.aggr2 <- port.aggr %>% pivot_wider(id_cols = c(Date), names_from = c(Type), values_from = c(EventRate))
(diag.Act_PD_disc_reg <- mean(abs(port.aggr2$a_Actual - port.aggr2$b_PD_disc_reg)) * 100)
(diag.Act_PD_disc_best <- mean(abs(port.aggr2$a_Actual - port.aggr2$c_PD_disc_best)) * 100)
(diag.Act_ExpDisc <- mean(abs(port.aggr2$a_Actual - port.aggr2$d_SICR_model_disc)) * 100)

# - Calculate standard deviation of these processes
stdev_SICR_Act <- sd(port.aggr2$a_Actual, na.rm=T)
stdev_SICR_PD_disc_reg <- sd(port.aggr2$b_PD_disc_reg, na.rm=T)
stdev_SICR_PD_disc_best <- sd(port.aggr2$c_PD_disc_best, na.rm=T)
stdev_SICR_ExpDisc <- sd(port.aggr2$d_SICR_model_disc, na.rm=T)

# - Calculate so-called risk prudence degree to measure the degree to which the expected default rate exceeds the actual default rate over time
overPredictDegree_PD_disc_reg <- sum(port.aggr2$b_PD_disc_reg>=port.aggr2$a_Actual)/length(port.aggr2$b_PD_disc_reg)
overPredictDegree_PD_disc_best <- sum(port.aggr2$c_PD_disc_best>=port.aggr2$a_Actual)/length(port.aggr2$c_PD_disc_best)
overPredictDegree_ExpDisc <- sum(port.aggr2$d_SICR_model_disc>=port.aggr2$a_Actual)/length(port.aggr2$d_SICR_model_disc)

# - Graphing parameters
col.v <- brewer.pal(5, "Dark2")
label.v <- c("a_Actual"=bquote(italic(A[t])*": Actual"),
             "b_PD_disc_reg"=bquote(italic(B[t])*": Expected-PD-rule (EBA)"),
             "c_PD_disc_best"=bquote(italic(C[t])*": Expected-PD-rule (best)"),
             "d_SICR_model_disc"=bquote(italic(D[t])*": Expected-SICR-model"))

# - Create graph
(g <- ggplot(port.aggr, aes(x=Date, y=EventRate, group=Type)) + theme_minimal() + 
    labs(x="Reporting date (months)", y="Conditional SICR-rate (%)") + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90),
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Type, linetype=Type), linewidth=0.4) + 
    geom_point(aes(colour=Type, shape=Type), size=1.2) + 
    #annotations
    annotate(geom="text", x=as.Date("2015-12-31"), y=port.aggr[Date >= "2012-12-31" & Type=="a_Actual", mean(EventRate)]*3.8,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.2f", diag.Act_PD_disc_reg),"%'"),
             family=chosenFont, size=3, parse=T) + 
    annotate(geom="text", x=as.Date("2015-12-31"), y=port.aggr[Date >= "2012-12-31" & Type=="a_Actual", mean(EventRate)]*3.4,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(C[t])*': ", sprintf("%.2f", diag.Act_PD_disc_best),"%'"),
             family=chosenFont, size=3, parse=T) +
    annotate(geom="text", x=as.Date("2015-12-31"), y=port.aggr[Date >= "2012-12-31" & Type=="a_Actual", mean(EventRate)]*3.0,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(D[t])*': ", sprintf("%.2f", diag.Act_ExpDisc),"%'"),
             family=chosenFont, size=3, parse=T) +    
    # facets & scale options
    facet_grid(Facet_label ~ .) + 
    scale_colour_manual(name="", values=col.v, labels=label.v) + 
    scale_shape_discrete(name="", labels=label.v) + scale_linetype_discrete(name="", labels=label.v) + 
    #guides(colour=guide_legend(nrow=2,byrow=T)) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))

# - Save graph
ggsave(g, file=paste0(genFigPath, "TimeGraph_SICR_approaches_ActExp.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")


