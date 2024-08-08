# =============================== PD-COMPARISON ================================
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
#   - 3d.Actual_backstop_rates.R

# -- Inputs:
#   - datSICR_smp | saved data with required model outputs (script 3b.SICR_def_1b(iii)_logit.R)
#   - dat_Backstop_smp | saved data with required actual backstop target 

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




# ------- 1. Apply a cut-off on the PD-ratio to obtain discrete outcomes of the PD-comparison approach
# - Unpack the saved dataset that contains the required PD-ratio, as well as the predicted SICR-outcome
if (!exists('dat_Backstop_smp')) unpack.ffdf(paste0(genPath,"data_backstop_target"), tempPath)

# - Implement a threshold based on the EBA-recommended threshold and the best performing threshold based on the AUC
dat_Backstop_smp[, PD_Disc_reg := ifelse(PD_ratio > 2, 1, 0)]
dat_Backstop_smp[, PD_Disc_best := ifelse(PD_ratio > 1, 1, 0)]
# NOTE: The variable PD_Disc_best was interactively created using various thresholds with the results provided below

# - Calculate the AUC
auc(dat_Backstop_smp$Backstop_target_event, dat_Backstop_smp$PD_Disc_reg) # 52.44%
auc(dat_Backstop_smp$Backstop_target_event, dat_Backstop_smp$PD_Disc_best) # 66.36%

### RESULTS:
# On a 200% threshold, the AUC is 52.44%, not much better than flipping a random coin
# On a 180% threshold, the AUC is 58.57%, slightly better than flipping a random coin
# On a 150% threshold, the AUC is 62.59%, which at least shows some predictive power
# On a 120% threshold, the AUC is 63.61%, only slightly better than 150%
# On a 100% threshold, the AUC is 66.36%, which is the best performing 
# On a 300% threshold, the AUC is 51.65%, not much better than flipping a random coin
# As the threshold becomes smaller, the AUC improves and vice versa




# ------ 2. Portfolio Analytics on PD-comparison approach

# --- 1. Comparison of actual vs predicted outcomes

# - Structure different line series together
datCompare_graph <- rbind(dat_Backstop_smp[, list(LoanID, Date, Backstop_def, Backstop_events=Backstop_target_event, Type="a_Actual_Backstop")],
                          dat_Backstop_smp[, list(LoanID, Date, Backstop_def, Backstop_events=PD_Disc_reg, Type="b_PD_disc_reg")],
                          dat_Backstop_smp[, list(LoanID, Date, Backstop_def, Backstop_events=PD_Disc_best, Type="c_PD_disc_best")])

# - Aggregate to monthly level and observe up to given point
PD_StartDte <- min(dat_Backstop_smp$Date, na.rm=T)
PD_EndDte <- max(dat_Backstop_smp$Date, na.rm=T)
port.aggr <- datCompare_graph[, list(EventRate = sum(Backstop_events, na.rm=T)/.N, AtRisk = .N),
                              by=list(Type, Date)][Date >= PD_StartDte & Date <= PD_EndDte,] %>% setkey(Type,Date)

# - Aesthetics engineering
port.aggr[, Facet_label := "PD-comparison approach"]

# - Calculate MAE over time by line graph type in summarising differences amongst line graphs
port.aggr2 <- port.aggr %>% pivot_wider(id_cols = c(Date), names_from = c(Type), values_from = c(EventRate))
(diag.Act_PD_disc_reg <- mean(abs(port.aggr2$a_Actual_Backstop - port.aggr2$b_PD_disc_reg)) * 100)
(diag.Act_PD_disc_best <- mean(abs(port.aggr2$a_Actual_Backstop - port.aggr2$c_PD_disc_best)) * 100)

# - Graphing parameters
col.v <- brewer.pal(3, "Dark2")[c(1,2,3)]
label.v <- c("a_Actual_Backstop"=bquote(italic(A[t])*": Actual-Backstop-rate"),
             "b_PD_disc_reg"=bquote(italic(B[t])*": Expected (PD-approach-EBA"),
             "c_PD_disc_best"=bquote(italic(C[t])*": Expected (PD-approach-best"))
port.aggr <- port.aggr[port.aggr$Date > "2007-01-31", ]

# - Create graph
(g <- ggplot(port.aggr, aes(x=Date, y=EventRate, group=Type)) + theme_minimal() + 
    labs(x="Reporting date (months)", y="SICR-rate (%)") + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90),
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Type, linetype=Type), linewidth=0.4) + 
    geom_point(aes(colour=Type, shape=Type), size=1.2) + 
    #annotations
    annotate(geom="text", x=as.Date("2015-12-31"), y=port.aggr[Date >= "2012-12-31" & Type=="a_Actual_Backstop", mean(EventRate)]*8,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.2f", diag.Act_PD_disc_reg),"%'"),
             family=chosenFont, size=3, parse=T) + 
    annotate(geom="text", x=as.Date("2015-12-31"), y=port.aggr[Date >= "2012-12-31" & Type=="a_Actual_Backstop", mean(EventRate)]*7,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(C[t])*': ", sprintf("%.2f", diag.Act_PD_disc_best),"%'"),
             family=chosenFont, size=3, parse=T) +
    # facets & scale options
    facet_grid(Facet_label ~ .) + 
    scale_colour_manual(name="", values=col.v, labels=label.v) + 
    scale_shape_discrete(name="", labels=label.v) + scale_linetype_discrete(name="", labels=label.v) + 
    #guides(colour=guide_legend(nrow=2,byrow=T)) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))




# ------ 3. Portfolio Analytics on SICR-model

# - Unpack the saved dataset that contains the required PD-ratio, as well as the predicted SICR-outcome
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)

# - Calculate the AUC
auc(datSICR_smp$SICR_target, datSICR_smp$ExpDisc) # 76.75%

# - Structure different line series together
datCompare_SICR_graph <- rbind(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=SICR_target, Type="d_Actual_SICR")],
                               datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=ExpDisc, Type="e_SICR_model_disc")])

# - Transform factor back to numeric variables for aggregation purposes
datCompare_SICR_graph[, SICR_events := as.numeric(levels(SICR_events))[SICR_events]]

# - Aggregate to monthly level and observe up to given point
SICR_StartDte <- min(datSICR_smp$Date, na.rm=T)
SICR_EndDte <- max(datSICR_smp$Date, na.rm=T)
port.Saggr <- datCompare_SICR_graph[SICR_def==0, list(EventRate = sum(SICR_events, na.rm=T)/.N, AtRisk = .N),
                                    by=list(Type, Date)][Date >= SICR_StartDte & Date <= SICR_EndDte,] %>% setkey(Type,Date)

# - Aesthetics engineering
port.Saggr[, Facet_label := "SICR-model approach"]

# - Calculate MAE over time by line graph type in summarising differences amongst line graphs
port.Saggr2 <- port.Saggr %>% pivot_wider(id_cols = c(Date), names_from = c(Type), values_from = c(EventRate))
(diag.Act_ExpDisc <- mean(abs(port.Saggr2$d_Actual_SICR - port.Saggr2$e_SICR_model_disc)) * 100)

# - Graphing parameters
col.Sv <- brewer.pal(6, "Dark2")[c(4,6)]
label.Sv <- c("d_Actual_SICR"=bquote(italic(D[t])*": Actual SICR-rate"),
              "e_SICR_model_disc"=bquote(italic(E[t])*": Expected (SICR-model-best)"))

# - Create graph
(s <- ggplot(port.Saggr, aes(x=Date, y=EventRate, group=Type)) + theme_minimal() + 
    labs(x="Reporting date (months)", y="Conditional SICR-rate (%)") + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90),
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Type, linetype=Type), linewidth=0.4) + 
    geom_point(aes(colour=Type, shape=Type), size=1.2) + 
    #annotations
    annotate(geom="text", x=as.Date("2015-12-31"), y=port.Saggr[Date >= "2012-12-31" & Type=="d_Actual_SICR", mean(EventRate)]*4,
             label=paste0("'MAE between '*italic(D[t])*' and '*italic(E[t])*': ", sprintf("%.2f", diag.Act_ExpDisc),"%'"),
             family=chosenFont, size=3, parse=T) + 
    # facets & scale options
    facet_grid(Facet_label ~ .) + 
    scale_colour_manual(name="", values=col.Sv, labels=label.Sv) + 
    scale_shape_discrete(name="", labels=label.Sv) + scale_linetype_discrete(name="", labels=label.Sv) + 
    #guides(colour=guide_legend(nrow=2,byrow=T)) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))




# ------ 4. Portfolio Analytics on combined PD-comparison and SICR-model data

(combine_graph <- g + s + plot_layout(nrow = 2))


# - Save graph
ggsave(combine_graph, file=paste0(genFigPath, "Compare_PD_comparison_SICR_model.png"), width=1200/dpi, height=1200/dpi, dpi=dpi, bg="white")


