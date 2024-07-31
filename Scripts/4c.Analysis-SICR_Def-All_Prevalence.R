# ============================== SICR-DEFINITION ANALYSIS ===============================
# Script for comparing SICR-prevalence rates across various SICR-definitions.
# In particular, we analyse definition class 1-2 and compare results across (d,s,k)-parameters
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


# --- 2. Prevalence analysis
# 1a
datSICR.aggr <- datSICR[d==1 & s==1,list(SICR_Obs = sum(as.numeric(levels(SICR_events))[SICR_events], na.rm=T), k = mean(k),
                              Prevalence = mean(as.numeric(levels(SICR_events))[SICR_events], na.rm=T)), by=list(SICR_Def)]
datSICR.aggr
sprintf("%.2f", datSICR.aggr$Prevalence*100)
plot(datSICR.aggr$k, datSICR.aggr$SICR_Obs, type="b")
plot(datSICR.aggr$k, datSICR.aggr$Prevalence, type="b")
# Same shape regardless of SICR_Obs and prevalence, so we'll default to prevalence
datSICR.aggr.final <- data.table(datSICR.aggr, d=1, s=1)

# 1b
datSICR.aggr <- datSICR[d==1 & s==2,list(SICR_Obs = sum(as.numeric(levels(SICR_events))[SICR_events], na.rm=T), k = mean(k),
                                         Prevalence = mean(as.numeric(levels(SICR_events))[SICR_events], na.rm=T)), by=list(SICR_Def)]
datSICR.aggr
sprintf("%.2f", datSICR.aggr$Prevalence*100)
#plot(datSICR.aggr$k, datSICR.aggr$SICR_Obs, type="b")
plot(datSICR.aggr$k, datSICR.aggr$Prevalence, type="b")
datSICR.aggr.final <- rbind(datSICR.aggr.final, data.table(datSICR.aggr, d=1, s=2) )

# 1c
datSICR.aggr <- datSICR[d==1 & s==3,list(SICR_Obs = sum(as.numeric(levels(SICR_events))[SICR_events], na.rm=T), k = mean(k),
                                         Prevalence = mean(as.numeric(levels(SICR_events))[SICR_events], na.rm=T)), by=list(SICR_Def)]
datSICR.aggr
sprintf("%.2f", datSICR.aggr$Prevalence*100)
#plot(datSICR.aggr$k, datSICR.aggr$SICR_Obs, type="b")
plot(datSICR.aggr$k, datSICR.aggr$Prevalence, type="b")
datSICR.aggr.final <- rbind(datSICR.aggr.final, data.table(datSICR.aggr, d=1, s=3) )

# 2a
datSICR.aggr <- datSICR[d==2 & s==1,list(SICR_Obs = sum(as.numeric(levels(SICR_events))[SICR_events], na.rm=T), k = mean(k),
                                         Prevalence = mean(as.numeric(levels(SICR_events))[SICR_events], na.rm=T)), by=list(SICR_Def)]
datSICR.aggr
sprintf("%.2f", datSICR.aggr$Prevalence*100)
#plot(datSICR.aggr$k, datSICR.aggr$SICR_Obs, type="b")
plot(datSICR.aggr$k, datSICR.aggr$Prevalence, type="b")
datSICR.aggr.final <- rbind(datSICR.aggr.final, data.table(datSICR.aggr, d=2, s=1) )

# 2b
datSICR.aggr <- datSICR[d==2 & s==2,list(SICR_Obs = sum(as.numeric(levels(SICR_events))[SICR_events], na.rm=T), k = mean(k),
                                         Prevalence = mean(as.numeric(levels(SICR_events))[SICR_events], na.rm=T)), by=list(SICR_Def)]
datSICR.aggr
sprintf("%.2f", datSICR.aggr$Prevalence*100)
#plot(datSICR.aggr$k, datSICR.aggr$SICR_Obs, type="b")
plot(datSICR.aggr$k, datSICR.aggr$Prevalence, type="b")
datSICR.aggr.final <- rbind(datSICR.aggr.final, data.table(datSICR.aggr, d=2, s=2) )

# 2c
datSICR.aggr <- datSICR[d==2 & s==3,list(SICR_Obs = sum(as.numeric(levels(SICR_events))[SICR_events], na.rm=T), k = mean(k),
                                         Prevalence = mean(as.numeric(levels(SICR_events))[SICR_events], na.rm=T)), by=list(SICR_Def)]
datSICR.aggr
sprintf("%.2f", datSICR.aggr$Prevalence*100)
#plot(datSICR.aggr$k, datSICR.aggr$SICR_Obs, type="b")
plot(datSICR.aggr$k, datSICR.aggr$Prevalence, type="b")
datSICR.aggr.final <- rbind(datSICR.aggr.final, data.table(datSICR.aggr, d=2, s=3) )

# - Save results
write_xlsx(x=datSICR.aggr.final,path=paste0(genObjPath, "PrevalenceRates.xlsx"))

# - Cleanup
rm(datSICR); gc()

# - Calculate mean prevalence to facilitate certain analyses
vMeans_s_d1 <- datSICR.aggr.final[d==1, list(Phi_mean_s_d1 = mean(Prevalence, na.rm=T)), by=list(s)]
vMeans_s_d2 <- datSICR.aggr.final[d==2, list(Phi_mean_s_d2 = mean(Prevalence, na.rm=T)), by=list(s)]

# - Calculate ratio for inference (sec. 4.4 in SICR-article)
vMeans_s_d1 / vMeans_s_d2


# -- Analysis: Varying stickiness for class 1
plot.obj <- subset(datSICR.aggr.final, d==1 & k <= 12)
plot.obj[, Group := factor(s)]

ggplot(plot.obj,aes(x=k,y=Prevalence, group=Group)) + theme_minimal() + 
  geom_line(aes(colour=Group, linetype=Group), linewidth=1) + 
  geom_point(aes(colour=Group, shape=Group), size=3) + 
  scale_y_continuous(label=percent) + scale_x_continuous(breaks=pretty_breaks(4), label=label_number(accuracy=1))

### RESULTS: Prevalence decreases as k increases, like for class 1a

# - Cleanup
rm(datSICR.aggr, datSICR.aggr.final, vMeans_s_d1, vMeans_s_d2, plot.obj); gc()

