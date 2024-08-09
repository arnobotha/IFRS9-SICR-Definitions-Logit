# =============================== PD-COMPARISON ================================
# Comparing approaches for rendering SICR-decisions: PD-comparison vs SICR-model 
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




# ------ 0. Setup

# - Define SICR-definition label
SICR_label <- "1b(iii)" 

# - Ensure necessary data is unpacked from which graph is to be created
if (!exists('dat_Backstop_smp')) unpack.ffdf(paste0(genPath,"data_backstop_target"), tempPath)
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)




# ------ 1. Time graph of actual vs expected SICR-rates | PD-comparison approach

# - Plot actual rate alone, as part of analysis
port.aggr <- dat_Backstop_smp[Backstop_def==0, list(EventRate = sum(Backstop_target_event, na.rm=T)/.N, AtRisk = .N),by=list(Date)]
plot(port.aggr$EventRate, type="b")
### RESULTS: While there is some slight increase in the rate during during 2008-GFC, it is by no way as meaningful
# as for the 12-month default rate, nor across most SICR-rates (actual). This is the known effect of an overly
# short k-value (which is k=1 for this exercise)

# - Investigate candidate u-thresholds by calculating the implied AUC-value of the resulting SICR-predictions
vThresholds <- c(1,1.2,1.5,1.8,2,3)
vAUCs <- rep(NA, length(vThresholds))
for (i in 1:length(vThresholds)) {
  dat_Backstop_smp[, PD_Disc_candidate := ifelse(PD_ratio > vThresholds[i], 1, 0)]  
  vAUCs[i] <- auc(dat_Backstop_smp$Backstop_target_event, dat_Backstop_smp$PD_Disc_candidate)
  cat(paste0("AUC for threshold u=", vThresholds[i],": ", percent(vAUCs[i], accuracy=0.01),"\n"))
}
plot(vAUCs, vThresholds, type="b")

### RESULTS:
# On a 100% threshold, the AUC is 65.49%, which is the best performing 
# On a 120% threshold, the AUC is 62.99%, only slightly better than 150%
# On a 150% threshold, the AUC is 62.08%, which at least shows some predictive power
# On a 180% threshold, the AUC is 58.18%, slightly better than flipping a random coin
# On a 200% threshold, the AUC is 52.38%, not much better than flipping a random coin\
# On a 300% threshold, the AUC is 51.59%, not much better than flipping a random coin
# SUMMARY: As the threshold becomes smaller, the AUC improves and vice versa

# - Impose chosen u-thresholds
# NOTE: Include EBA-recommended threshold u=200% + the best performing u-threshold based on the AUC
dat_Backstop_smp[, PD_Disc_reg := ifelse(PD_ratio > 2, 1, 0)]
dat_Backstop_smp[, PD_Disc_best := ifelse(PD_ratio > 1, 1, 0)]

# - Confirm the AUC-values of these "models" that render SICR-predictions
auc(dat_Backstop_smp$Backstop_target_event, dat_Backstop_smp$PD_Disc_reg) # 52.38%
auc(dat_Backstop_smp$Backstop_target_event, dat_Backstop_smp$PD_Disc_best) # 65.49%

# - Structure data for graphing purposes
datCompare_graph <- rbind(dat_Backstop_smp[, list(LoanID, Date, Backstop_def, Backstop_events=Backstop_target_event, Type="a_Actual_Backstop")],
                          dat_Backstop_smp[, list(LoanID, Date, Backstop_def, Backstop_events=PD_Disc_reg, Type="b_PD_disc_reg")],
                          dat_Backstop_smp[, list(LoanID, Date, Backstop_def, Backstop_events=PD_Disc_best, Type="c_PD_disc_best")])

# - Aggregate to monthly level and observe up to given point
PD_StartDte <- min(dat_Backstop_smp$Date, na.rm=T) + month(1)
PD_EndDte <- max(datSICR_smp$Date, na.rm=T) # for a fair comparison, standardise time horizon across both approaches
port.aggr <- datCompare_graph[Backstop_def==0, list(EventRate = sum(Backstop_events, na.rm=T)/.N, AtRisk = .N),
                              by=list(Type, Date)][Date >= PD_StartDte & Date <= PD_EndDte,] %>% setkey(Type,Date)

# - Aesthetics engineering
port.aggr[, Facet_label := "PD-comparison approach"]

# - Calculate MAE over time by line graph type in summarising differences amongst line graphs
port.aggr2 <- port.aggr %>% pivot_wider(id_cols = c(Date), names_from = c(Type), values_from = c(EventRate))
(diag.Act_PD_disc_reg <- mean(abs(port.aggr2$a_Actual_Backstop - port.aggr2$b_PD_disc_reg)) * 100)
(diag.Act_PD_disc_best <- mean(abs(port.aggr2$a_Actual_Backstop - port.aggr2$c_PD_disc_best)) * 100)

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 215
vCol <- brewer.pal(3, "Dark2")[c(1,2,3)]
vLabel <- c("a_Actual_Backstop"=bquote(italic(A[t])*": Actual ("*italic(g)[0]>=1*"; 'Backstop')"),
             "b_PD_disc_reg"=bquote(italic(B[t])*": Expected (EBA)"),
             "c_PD_disc_best"=bquote(italic(C[t])*": Expected (best AUC)"))

# - Create graph
(g1 <- ggplot(port.aggr, aes(x=Date, y=EventRate, group=Type)) + theme_minimal() + 
    labs(x="Reporting date (months)", y="Conditional SICR-rate (%)") + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90),
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Type, linetype=Type), linewidth=0.4) + 
    geom_point(aes(colour=Type, shape=Type), size=1.2) + 
    #annotations
    annotate(geom="text", x=as.Date("2015-12-31"), y=port.aggr[Date >= "2012-12-31" & Type=="a_Actual_Backstop", mean(EventRate)]*20,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.2f", diag.Act_PD_disc_reg),"%'"),
             family=chosenFont, size=3, parse=T) + 
    annotate(geom="text", x=as.Date("2015-12-31"), y=port.aggr[Date >= "2012-12-31" & Type=="a_Actual_Backstop", mean(EventRate)]*18,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(C[t])*': ", sprintf("%.2f", diag.Act_PD_disc_best),"%'"),
             family=chosenFont, size=3, parse=T) +
    # facets & scale options
    facet_grid(Facet_label ~ .) + 
    scale_colour_manual(name="", values=vCol, labels=vLabel) + 
    scale_shape_discrete(name="", labels=vLabel) + scale_linetype_discrete(name="", labels=vLabel) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))

# - Save graph
ggsave(g1, file=paste0(genFigPath, "TimeGraph_ApproachComp_SICR-Incidence_ActExp_PD-Comparison.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")





# ------ 2. Time graph of actual vs expected SICR-rates | SICR-modelling approach

# - Calculate the AUC
auc(datSICR_smp$SICR_target, datSICR_smp$ExpDisc) # 76.75%

# - Structure data for graphing purposes
datCompare_SICR_graph <- rbind(datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=as.numeric(as.character(SICR_target)), Type="d_Actual_SICR")],
                               datSICR_smp[, list(LoanID, Date, SICR_def, SICR_events=ExpDisc, Type="e_SICR_model_disc")])

# - Aggregate to monthly level and observe up to given point
SICR_StartDte <- min(datSICR_smp$Date, na.rm=T) + month(1)
SICR_EndDte <- max(datSICR_smp$Date, na.rm=T)
port.Saggr <- datCompare_SICR_graph[SICR_def==0, list(EventRate = sum(SICR_events, na.rm=T)/.N, AtRisk = .N),
                                    by=list(Type, Date)][Date >= SICR_StartDte & Date <= SICR_EndDte,] %>% setkey(Type,Date)

# - Aesthetics engineering
port.Saggr[, Facet_label := "SICR-model approach"]

# - Calculate MAE over time by line graph type in summarising differences amongst line graphs
port.Saggr2 <- port.Saggr %>% pivot_wider(id_cols = c(Date), names_from = c(Type), values_from = c(EventRate))
(diag.Act_ExpDisc <- mean(abs(port.Saggr2$d_Actual_SICR - port.Saggr2$e_SICR_model_disc)) * 100)

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 215
vCol2 <- brewer.pal(6, "Dark2")[c(4,6)]
vLabel2 <- c("d_Actual_SICR"=bquote(italic(D[t])*": Actual [1b(iii)]"),
              "e_SICR_model_disc"=bquote(italic(E[t])*": Expected [1b(iii)]"))

# - Create graph
(g2 <- ggplot(port.Saggr, aes(x=Date, y=EventRate, group=Type)) + theme_minimal() + 
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
    scale_colour_manual(name="", values=vCol2, labels=vLabel2) + 
    scale_shape_discrete(name="", labels=vLabel2) + scale_linetype_discrete(name="", labels=vLabel2) + 
    #guides(colour=guide_legend(nrow=2,byrow=T)) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))

# - Save graph
ggsave(g2, file=paste0(genFigPath, "TimeGraph_ApproachComp_SICR-Incidence_ActExp_SICR-Model.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")




# ------ 4. Create combined time graph across both approaches


# --- V1: simply stitch ggplot2-objects together
# NOTE: this approach does introduce unnecessary elements, such as a double x-axis, which is messy

# - Bind graphs together
(gCombined<-grid.arrange(g1, g2, nrow=2))

# - Save graph
ggsave(gCombined, file=paste0(genFigPath, "TimeGraph_ApproachComp_Combined_v1.png"), width=1200/dpi, height=2000/dpi, dpi=dpi, bg="white")



# --- V2: Create combined graph using faceting

# - Structure data for graphing purposes
datCompare <- rbind(dat_Backstop_smp[, list(LoanID, Date, Status=Backstop_def, Target=Backstop_target_event, Type="a_Actual_Backstop")],
                    dat_Backstop_smp[, list(LoanID, Date, Status=Backstop_def, Target=PD_Disc_reg, Type="b_PD_disc_reg")],
                    dat_Backstop_smp[, list(LoanID, Date, Status=Backstop_def, Target=PD_Disc_best, Type="c_PD_disc_best")],
                    datSICR_smp[, list(LoanID, Date, Status=SICR_def, Target=as.numeric(as.character(SICR_target)), Type="d_Actual_SICR")],
                    datSICR_smp[, list(LoanID, Date, Status=SICR_def, Target=ExpDisc, Type="e_SICR_model_disc")])

# - Aggregate to monthly level and observe up to given point
startDte <- min(datCompare$Date, na.rm=T) + month(1)
endDte <- max(datSICR_smp$Date, na.rm=T)
port.aggr <- datCompare[Status==0, list(EventRate = sum(Target, na.rm=T)/.N, AtRisk = .N),
                              by=list(Type, Date)][Date >= startDte & Date <= endDte,] %>% setkey(Type,Date)

# - Aesthetics engineering
port.aggr[, Facet_label := 
            case_when(Type %in% c("a_Actual_Backstop", "b_PD_disc_reg", "c_PD_disc_best") ~ "PD-comparison approach",
                      TRUE~"SICR-model approach")]
vLabelMath <- c("'PD-comparison approach ('*italic(d)==1*','~italic(s)==1*','~italic(k)==1*')'",
                "'SICR-model approach [1b(iii)]: ('*italic(d)==1*','~italic(s)==2*','~italic(k)==9*')'")
port.aggr[, Facet_math := 
            factor(Facet_label,labels = vLabelMath)]

# - Calculate MAE over time by line graph type in summarising differences amongst line graphs
port.aggr2 <- port.aggr %>% pivot_wider(id_cols = c(Date), names_from = c(Type), values_from = c(EventRate))
(diag.Act_PD_disc_reg <- mean(abs(port.aggr2$a_Actual_Backstop - port.aggr2$b_PD_disc_reg),na.rm=T) * 100)
(diag.Act_PD_disc_best <- mean(abs(port.aggr2$a_Actual_Backstop - port.aggr2$c_PD_disc_best),na.rm=T) * 100)
(diag.Act_ExpDisc <- mean(abs(port.aggr2$d_Actual_SICR - port.aggr2$e_SICR_model_disc),na.rm=T) * 100)

# - Create annotation table
annY_PD <- port.aggr[Facet_label=="PD-comparison approach", max(EventRate)] # y-axis anchor point
annY_SICR <- port.aggr[Facet_label=="SICR-model approach", max(EventRate)] # y-axis anchor point
datAnnotate <- data.table(Date=c(as.Date("2015-12-31")), EventRate=c(annY_PD*0.7, annY_PD*0.60, annY_SICR*0.7),
                          Facet_math=c(vLabelMath[1], vLabelMath[1], vLabelMath[2]),
                          Label=c(paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.2f", diag.Act_PD_disc_reg),"%'"),
                                  paste0("'MAE between '*italic(A[t])*' and '*italic(C[t])*': ", sprintf("%.2f", diag.Act_PD_disc_best),"%'"),
                                  paste0("'MAE between '*italic(D[t])*' and '*italic(E[t])*': ", sprintf("%.2f", diag.Act_ExpDisc),"%'")))

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 215
vCol <- brewer.pal(6, "Dark2")[c(1,2,3,4,6)]
vLabel <- c("a_Actual_Backstop"=bquote(italic(A[t])*": Actual ("*italic(g)[0]>=1*"; 'Backstop')"),
            "b_PD_disc_reg"=bquote(italic(B[t])*": Expected (EBA)"),
            "c_PD_disc_best"=bquote(italic(C[t])*": Expected (best AUC)"),
            "d_Actual_SICR"=bquote(italic(D[t])*": Actual [1b(iii)]"),
            "e_SICR_model_disc"=bquote(italic(E[t])*": Expected [1b(iii)]"))

# - Create graph
(g3 <- ggplot(port.aggr, aes(x=Date, y=EventRate, group=Type)) + theme_minimal() + 
    labs(x="Reporting date (months)", y="Conditional SICR-rate (%)") + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90),
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Type, linetype=Type), linewidth=0.4) + 
    geom_point(aes(colour=Type, shape=Type), size=1.2) + 
    #annotations
    geom_text(data=datAnnotate, aes(x=Date, y=EventRate, group=Facet_math, label=Label), size=4, family=chosenFont, parse=T) + 
    # facets & scale options
    facet_grid(Facet_math ~ ., scales="free", labeller=label_parsed) + 
    scale_colour_manual(name="", values=vCol, labels=vLabel) + 
    scale_shape_discrete(name="", labels=vLabel) + scale_linetype_discrete(name="", labels=vLabel) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") + 
    guides(colour=guide_legend(nrow=2, byrow=T)))

# - Save graph
ggsave(g3, file=paste0(genFigPath, "TimeGraph_ApproachComp_Combined_v2.png"), width=1200/dpi, height=1500/dpi, dpi=dpi, bg="white")



# - Clean-up
rm(dat_Backstop_smp, datSICR_smp, datCompare_SICR_graph, datCompare_graph, datCompare,datAnnotate, 
   port.Saggr, port.aggr,port.aggr2, port.Saggr2, g1,g2,g3,gCombined, vLabel, vLabel2); gc()
