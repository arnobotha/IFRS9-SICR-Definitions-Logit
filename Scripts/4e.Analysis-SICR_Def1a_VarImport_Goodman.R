# ============================== SICR-DEFINITION ANALYSIS ===============================
# Script for computing and graphing Goodman variable importance across 
# SICR-definition class 1a
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Dynamic SICR-research
# SCRIPT AUTHOR(S): Esmerelda Oberholzer, Dr Arno Botha
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2b.Data_Preparation_Credit.R
#   - 2c.Data_Enrich.R
#   - 2d.Data_Fusion.R 
#   - 3a.SICR_def_<>_logit.R | the 3a-series of scripts for definitions 1a-2c, for (i)-(iv)

# -- Inputs:
#   - logit_model_chosen_<> | final fitted logit-model for given SICR-definition (3a)
#   - datSICR_smp_<> | specific SICR-sample upon which resampling scheme is applied (3a)

# -- Outputs:
#   - <analytics>
# =======================================================================================




# ------ 0. Setup/load models

# --- a) Logit models

SICR_label <- "1a(i)"
if(!exists('logit_model_chosen')) unpack.ffdf(paste0(genPath, "logit_model_", SICR_label), tempPath)
logit_model_1a_i <- logit_model_chosen; rm(logit_model_chosen)
SICR_label <- "1a(ii)"
if(!exists('logit_model_chosen')) unpack.ffdf(paste0(genPath, "logit_model_", SICR_label), tempPath)
logit_model_1a_ii <- logit_model_chosen; rm(logit_model_chosen)
SICR_label <- "1a(iii)"
if(!exists('logit_model_chosen')) unpack.ffdf(paste0(genPath, "logit_model_", SICR_label), tempPath)
logit_model_1a_iii <- logit_model_chosen; rm(logit_model_chosen)
SICR_label <- "1a(iv)"
if(!exists('logit_model_chosen')) unpack.ffdf(paste0(genPath, "logit_model_", SICR_label), tempPath)
logit_model_1a_iv <- logit_model_chosen; rm(logit_model_chosen)


# --- b) Anova-results for interpretation
summary(logit_model_1a_i)
summary(logit_model_1a_ii)
summary(logit_model_1a_iii)
summary(logit_model_1a_iv)

# - g0_Delinq: Effect on SICR-odds of 1-unit increase, i.e., change in odds ratio
vOddsChange_g0_delinq <- c(round(exp(logit_model_1a_i$coefficients["g0_Delinq"])-1, digits=1), # 19.0 times greater odds
                           round(exp(logit_model_1a_ii$coefficients["g0_Delinq"])-1, digits=1), # 8.0 times greater odds
                           round(exp(logit_model_1a_iii$coefficients["g0_Delinq"])-1, digits=1), # 5.2 times greater odds
                           round(exp(logit_model_1a_iv$coefficients["g0_Delinq"])-1, digits=1)) # 3.7 times greater odds
mean(vOddsChange_g0_delinq) # 9 times greater on average
# Would a 1-unit increase in g_0 trigger a SICR-decision theoretically, thereby embedding the backstop of IFRS 9?
logit_model_1a_i$data[g0_Delinq==0, mean(Prob_chosen_1a_i)] # 59%
logit_model_1a_i$data[g0_Delinq==1, mean(Prob_chosen_1a_i)] # 2.7%
logit_model_1a_i$data[g0_Delinq==1, mean(Pred_chosen_1a_i)] # 99.82775%
### RESULTS: Yes, for all but the most naively-chosen cut-offs

# - slc_acct_roll_ever_24_imputed: Effect on SICR-odds of 1-unit increase, i.e., change in odds ratio
vOddsChange_slc_acct_roll_ever_24_imputed <- c(round(exp(logit_model_1a_i$coefficients["slc_acct_roll_ever_24_imputed"])-1, digits=1), # 0.7 times greater odds
                           round(exp(logit_model_1a_ii$coefficients["slc_acct_roll_ever_24_imputed"])-1, digits=1), # 0.7 times greater odds
                           round(exp(logit_model_1a_iii$coefficients["slc_acct_roll_ever_24_imputed"])-1, digits=1), # 0.7 times greater odds
                           round(exp(logit_model_1a_iv$coefficients["slc_acct_roll_ever_24_imputed"])-1, digits=1)) # 0.7 times greater odds
mean(vOddsChange_slc_acct_roll_ever_24_imputed) # 0.7 times greater on average (i.e., 70% greater odds) 

# - slc_acct_arr_dir_3SAME: Effect on SICR-odds of flag relative to reference bin, i.e., change in odds ratio
vOddsChange_slc_acct_arr_dir_3SAME <- c(round(exp(logit_model_1a_i$coefficients["slc_acct_arr_dir_3SAME"])-1, digits=1), # -0.7 times greater odds
                                               round(exp(logit_model_1a_ii$coefficients["slc_acct_arr_dir_3SAME"])-1, digits=1), # -0.6 times greater odds
                                               round(exp(logit_model_1a_iii$coefficients["slc_acct_arr_dir_3SAME"])-1, digits=1), # -0.6 times greater odds
                                               round(exp(logit_model_1a_iv$coefficients["slc_acct_arr_dir_3SAME"])-1, digits=1)) # -0.5 times greater odds
mean(vOddsChange_slc_acct_arr_dir_3SAME) # -0.6 times greater on average (i.e., 60% smaller odds) 
describe(logit_model_1a_i$data$slc_acct_arr_dir_3)
### RESULTS: most prevalent bin is "SAME" (85%), i.e., maintaining zero-arrears decreases odds, which is sensibly

# - pmnt_method_grpStatement: Effect on SICR-odds of flag relative to reference bin, i.e., change in odds ratio
vOddsChange_pmnt_method_grpStatement <- c(round(exp(logit_model_1a_i$coefficients["pmnt_method_grpStatement"])-1, digits=1), # 0.7 times greater odds
                                               round(exp(logit_model_1a_ii$coefficients["pmnt_method_grpStatement"])-1, digits=1), # 0.8 times greater odds
                                               round(exp(logit_model_1a_iii$coefficients["pmnt_method_grpStatement"])-1, digits=1), # 0.8 times greater odds
                                               round(exp(logit_model_1a_iv$coefficients["pmnt_method_grpStatement"])-1, digits=1)) # 0.8 times greater odds
mean(vOddsChange_pmnt_method_grpStatement) # 0.775 times greater on average (i.e., 78% greater odds) 


# - slc_acct_pre_lim_perc_imputed: Effect on SICR-odds of 10%-unit increase, i.e., change in odds ratio
vOddsChange_slc_acct_pre_lim_perc_imputed <- c(round(exp(logit_model_1a_i$coefficients["slc_acct_pre_lim_perc_imputed"]*0.1)-1, digits=1), # -0.3 times greater odds
                                        round(exp(logit_model_1a_ii$coefficients["slc_acct_pre_lim_perc_imputed"]*0.1)-1, digits=1), # -0.3 times greater odds
                                        round(exp(logit_model_1a_iii$coefficients["slc_acct_pre_lim_perc_imputed"]*0.1)-1, digits=1), # -0.3 times greater odds
                                        round(exp(logit_model_1a_iv$coefficients["slc_acct_pre_lim_perc_imputed"]*0.1)-1, digits=1)) # -0.3 times greater odds
mean(vOddsChange_slc_acct_pre_lim_perc_imputed) # -0.3 times greater on average (i.e., 30% smaller odds) 


# - InterestRate_Margin: Effect on SICR-odds of 1%-unit increase, i.e., change in odds ratio
vOddsChange_InterestRate_Margin <- c(round(exp(logit_model_1a_i$coefficients["InterestRate_Margin"]*0.01)-1, digits=1), # 0.1 times greater odds
                                               round(exp(logit_model_1a_ii$coefficients["InterestRate_Margin"]*0.01)-1, digits=1), # 0.2 times greater odds
                                               round(exp(logit_model_1a_iii$coefficients["InterestRate_Margin"]*0.01)-1, digits=1), # 0.2 times greater odds
                                               round(exp(logit_model_1a_iv$coefficients["InterestRate_Margin"]*0.01)-1, digits=1)) # 0.2 times greater odds
mean(vOddsChange_InterestRate_Margin) # 0.175 times greater on average (i.e., 30% greater odds) 
describe(logit_model_1a_i$data$InterestRate_Margin)






# --- 1. Variable importance

# --- a) Calculation

datGraph_1a_i <- varImport_logit(logit_model_1a_i, method="stdCoef_Goodman", sig_level=0.1, impPlot=F)$data
datGraph_1a_ii <- varImport_logit(logit_model_1a_ii, method="stdCoef_Goodman", sig_level=0.1, impPlot=F)$data
datGraph_1a_iii <- varImport_logit(logit_model_1a_iii, method="stdCoef_Goodman", sig_level=0.1, impPlot=F)$data
datGraph_1a_iv <- varImport_logit(logit_model_1a_iv, method="stdCoef_Goodman", sig_level=0.1, impPlot=F)$data


# --- b) Graphing

# - Generic aesthetic engineering
vLabel <- c("g0_Delinq"="g0_Delinq", "slc_acct_roll_ever_24_imputed"="RollEver_24",
            "slc_acct_arr_dir_3SAME"="ArrearsDir_3-MILL", "slc_acct_arr_dir_3ROLLING"="ArrearsDir_3-INC",
            "pmnt_method_grpStatement"="PayMethod-PAYROLL", "slc_acct_pre_lim_perc_imputed"="Prepaid_Pc",
            "InterestRate_Margin"="InterestRate_Margin", "slc_acct_arr_dir_3MISSING_DATA"="ArrearsDir_3-MISSING", 
            "M_Repo_Rate"="Repo_Rate", "BalanceLog" = "BalanceLog",  "PerfSpell_Num" = "PerfSpell_Num", 
            "Term" = "Term", "pmnt_method_grpSalary/Suspense" = "PayMethod-SALARY",
            "pmnt_method_grpMISSING_DATA" = "PayMethod-MISSING", "M_RealGDP_Growth" = "RealGDP_Growth", 
            "M_DTI_Growth" = "DebtToIncome", "M_DTI_Growth_12" = "DebtToIncome_12",
            "M_Inflation_Growth" = "Inflation_Growth")
# NOTE: Given standardised input spaces for SICR-definition class 1a, we need only set the labels once


# -- Def 1a(i)
# - Aesthetic engineering
chosenFont <- "Cambria"; dpi <- 200; colPalette <- "BrBG"

# - Calculate contribution degrees to sum of importance measure across all variables
# NOTE: These contributions are merely ancillary and for graphing purposes.
# They should not considered too seriously, unless studied more extensively.
sumVarImport <- sum(datGraph_1a_i$Value_Abs, na.rm=T)

# - Create graph
(g <- ggplot(datGraph_1a_i, aes(x=reorder(Variable, Value_Abs))) + theme_minimal() + 
    theme(text=element_text(family=chosenFont), legend.position=c(0.8,0.6)) + 
    labs(x="Variable name", y="Standardised coefficients: Goodman") + 
    geom_col(aes(y=Value_Abs, fill=Value_Abs)) + 
    geom_label(aes(y=sumVarImport*0.05, label=paste(percent(Contribution, accuracy=0.1)), fill=Value_Abs), family=chosenFont) + 
    annotate(geom="text", x=datGraph_1a_i[.N, Variable], y=max(datGraph_1a_i$Value_Abs, na.rm=T)*0.75, family=chosenFont, size=3,
             label=paste0("Variable Importance (sum): ", comma(sumVarImport, accuracy=0.1))) + 
    scale_fill_distiller(palette=colPalette, name="Absolute value", direction=1) +
    scale_colour_distiller(palette=colPalette, name="Absolute value", direction=1) + 
    scale_x_discrete(labels=vLabel) + coord_flip())

# - Save graph
ggsave(g, file=paste0(genFigPath, "VariableImportance_stdCoef_Goodman_1a_i.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")


# -- Def 1a(ii)
# - Aesthetic engineering
chosenFont <- "Cambria"; dpi <- 200; colPalette <- "BrBG"

# - Calculate contribution degrees to sum of importance measure across all variables
# NOTE: These contributions are merely ancillary and for graphing purposes.
# They should not considered too seriously, unless studied more extensively.
sumVarImport <- sum(datGraph_1a_ii$Value_Abs, na.rm=T)

# - Create graph
(g <- ggplot(datGraph_1a_ii, aes(x=reorder(Variable, Value_Abs))) + theme_minimal() + 
    theme(text=element_text(family=chosenFont), legend.position=c(0.8,0.6)) + 
    labs(x="Variable name", y="Standardised coefficients: Goodman") + 
    geom_col(aes(y=Value_Abs, fill=Value_Abs)) + 
    geom_label(aes(y=sumVarImport*0.05, label=paste(percent(Contribution, accuracy=0.1)), fill=Value_Abs), family=chosenFont) + 
    annotate(geom="text", x=datGraph_1a_ii[.N, Variable], y=max(datGraph_1a_ii$Value_Abs, na.rm=T)*0.75, family=chosenFont, size=3,
             label=paste0("Variable Importance (sum): ", comma(sumVarImport, accuracy=0.1))) + 
    scale_fill_distiller(palette=colPalette, name="Absolute value", direction=1) +
    scale_colour_distiller(palette=colPalette, name="Absolute value", direction=1) + 
    scale_x_discrete(labels=vLabel) + coord_flip())

# - Save graph
ggsave(g, file=paste0(genFigPath, "VariableImportance_stdCoef_Goodman_1a_ii.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")


# -- Def 1a(iii)
# - Aesthetic engineering
chosenFont <- "Cambria"; dpi <- 200; colPalette <- "BrBG"

# - Calculate contribution degrees to sum of importance measure across all variables
# NOTE: These contributions are merely ancillary and for graphing purposes.
# They should not considered too seriously, unless studied more extensively.
sumVarImport <- sum(datGraph_1a_iii$Value_Abs, na.rm=T)

# - Create graph
(g <- ggplot(datGraph_1a_iii, aes(x=reorder(Variable, Value_Abs))) + theme_minimal() + 
    theme(text=element_text(family=chosenFont), legend.position=c(0.8,0.6)) + 
    labs(x="Variable name", y="Standardised coefficients: Goodman") + 
    geom_col(aes(y=Value_Abs, fill=Value_Abs)) + 
    geom_label(aes(y=sumVarImport*0.05, label=paste(percent(Contribution, accuracy=0.1)), fill=Value_Abs), family=chosenFont) + 
    annotate(geom="text", x=datGraph_1a_iii[.N, Variable], y=max(datGraph_1a_iii$Value_Abs, na.rm=T)*0.75, family=chosenFont, size=3,
             label=paste0("Variable Importance (sum): ", comma(sumVarImport, accuracy=0.1))) + 
    scale_fill_distiller(palette=colPalette, name="Absolute value", direction=1) +
    scale_colour_distiller(palette=colPalette, name="Absolute value", direction=1) + 
    scale_x_discrete(labels=vLabel) + coord_flip())

# - Save graph
ggsave(g, file=paste0(genFigPath, "VariableImportance_stdCoef_Goodman_1a_iii.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")


# -- Def 1a(iv)
# - Aesthetic engineering
chosenFont <- "Cambria"; dpi <- 200; colPalette <- "BrBG"

# - Calculate contribution degrees to sum of importance measure across all variables
# NOTE: These contributions are merely ancillary and for graphing purposes.
# They should not considered too seriously, unless studied more extensively.
sumVarImport <- sum(datGraph_1a_iv$Value_Abs, na.rm=T)

# - Create graph
(g <- ggplot(datGraph_1a_iv, aes(x=reorder(Variable, Value_Abs))) + theme_minimal() + 
    theme(text=element_text(family=chosenFont), legend.position=c(0.8,0.6)) + 
    labs(x="Variable name", y="Standardised coefficients: Goodman") + 
    geom_col(aes(y=Value_Abs, fill=Value_Abs)) + 
    geom_label(aes(y=sumVarImport*0.05, label=paste(percent(Contribution, accuracy=0.1)), fill=Value_Abs), family=chosenFont) + 
    annotate(geom="text", x=datGraph_1a_iv[.N, Variable], y=max(datGraph_1a_iv$Value_Abs, na.rm=T)*0.75, family=chosenFont, size=3,
             label=paste0("Variable Importance (sum): ", comma(sumVarImport, accuracy=0.1))) + 
    scale_fill_distiller(palette=colPalette, name="Absolute value", direction=1) +
    scale_colour_distiller(palette=colPalette, name="Absolute value", direction=1) + 
    scale_x_discrete(labels=vLabel) + coord_flip())

# - Save graph
ggsave(g, file=paste0(genFigPath, "VariableImportance_stdCoef_Goodman_1a_iv.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")




