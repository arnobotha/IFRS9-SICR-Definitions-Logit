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




# --- 1. Variable importance

# --- a) Calculation

datGraph_1a_i <- varImport_logit(logit_model_1a_i, method="stdCoef_Goodman", sig_level=0.1, impPlot=F)$data
datGraph_1a_ii <- varImport_logit(logit_model_1a_ii, method="stdCoef_Goodman", sig_level=0.1, impPlot=F)$data
datGraph_1a_iii <- varImport_logit(logit_model_1a_iii, method="stdCoef_Goodman", sig_level=0.1, impPlot=F)$data
datGraph_1a_iv <- varImport_logit(logit_model_1a_iv, method="stdCoef_Goodman", sig_level=0.1, impPlot=F)$data


# --- b) Graphing
# - Def 1a(i)
# - Aesthetic engineering
chosenFont <- "Cambria"; dpi <- 200; colPalette <- "BrBG"
vLabel <- c("g0_Delinq"="g0-Delinq", "slc_acct_roll_ever_24_imputed"="RollEver-24m",
            "slc_acct_arr_dir_3SAME"="ArrearsDir-SAME", "slc_acct_arr_dir_3ROLLING"="ArrearsDir-ROLLING",
            "pmnt_method_grpStatement"="PayMethod-STATEMENT", "slc_acct_pre_lim_perc_imputed"="Prepaid-Perc",
            "InterestRate_Margin"="InterestRate_Margin", "slc_acct_arr_dir_3MISSING_DATA"="ArrearsDir-MISSING", 
            "M_Repo_Rate"="Repo_Rate", "BalanceLog" = "Balance", "pmnt_method_grpSalary/Suspense" = "PayMethod-SALARY/SUSPENSE",
            "pmnt_method_grpMISSING_DATA" = "PayMethod-MISSING", "M_RealGDP_Growth" = "RealGDP_Growth", "M_DTI_Growth" = "DTI_Growth",
            "PerfSpell_Num" = "PerfSpell_Num", "Term" = "Term")

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

# - Def 1a(ii)
# - Aesthetic engineering
chosenFont <- "Cambria"; dpi <- 200; colPalette <- "BrBG"
vLabel <- c("g0_Delinq"="g0-Delinq", "slc_acct_roll_ever_24_imputed"="RollEver-24m",
            "slc_acct_arr_dir_3SAME"="ArrearsDir-SAME", "pmnt_method_grpStatement"="PayMethod-STATEMENT",
            "slc_acct_arr_dir_3ROLLING"="ArrearsDir-ROLLING", "slc_acct_pre_lim_perc_imputed"="Prepaid-Perc",
            "InterestRate_Margin"="InterestRate_Margin", "BalanceLog" = "Balance",
            "slc_acct_arr_dir_3MISSING_DATA"="ArrearsDir-MISSING", "M_DTI_Growth" = "DTI_Growth",
            "M_DTI_Growth_12" = "DTI_Growth_12", "pmnt_method_grpSalary/Suspense" = "PayMethod-SALARY/SUSPENSE",
            "PerfSpell_Num" = "PerfSpell_Num", "M_Repo_Rate"="Repo_Rate", "pmnt_method_grpMISSING_DATA" = "PayMethod-MISSING",
            "TimeInPerfSpell" = "TimeInPerfSpell", "M_RealGDP_Growth" = "RealGDP_Growth", "M_Inflation_Growth" = "Inflation_Growth")

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

# - Def 1a(iii)
# - Aesthetic engineering
chosenFont <- "Cambria"; dpi <- 200; colPalette <- "BrBG"
vLabel <- c("g0_Delinq"="g0-Delinq", "slc_acct_roll_ever_24_imputed"="RollEver-24m",
            "slc_acct_arr_dir_3SAME"="ArrearsDir-SAME", "pmnt_method_grpStatement"="PayMethod-STATEMENT",
            "InterestRate_Margin"="InterestRate_Margin", "slc_acct_pre_lim_perc_imputed"="Prepaid-Perc",
            "slc_acct_arr_dir_3ROLLING"="ArrearsDir-ROLLING", "M_DTI_Growth" = "DTI_Growth", "BalanceLog" = "Balance",
            "M_DTI_Growth_12" = "DTI_Growth_12", "PerfSpell_Num" = "PerfSpell_Num", "slc_acct_arr_dir_3MISSING_DATA"="ArrearsDir-MISSING",
            "TimeInPerfSpell" = "TimeInPerfSpell", "pmnt_method_grpSalary/Suspense" = "PayMethod-SALARY/SUSPENSE",
            "M_Repo_Rate"="Repo_Rate", "M_RealGDP_Growth" = "RealGDP_Growth", "pmnt_method_grpMISSING_DATA" = "PayMethod-MISSING")

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

# - Def 1a(iv)
# - Aesthetic engineering
chosenFont <- "Cambria"; dpi <- 200; colPalette <- "BrBG"
vLabel <- c("g0_Delinq"="g0-Delinq", "slc_acct_roll_ever_24_imputed"="RollEver-24m",
            "pmnt_method_grpStatement"="PayMethod-STATEMENT", "slc_acct_arr_dir_3SAME"="ArrearsDir-SAME",
            "slc_acct_pre_lim_perc_imputed"="Prepaid-Perc", "InterestRate_Margin"="InterestRate_Margin",
            "BalanceLog" = "Balance", "slc_acct_arr_dir_3ROLLING"="ArrearsDir-ROLLING", "M_DTI_Growth" = "DTI_Growth",
            "M_DTI_Growth_12" = "DTI_Growth_12", "PerfSpell_Num" = "PerfSpell_Num", "slc_acct_arr_dir_3MISSING_DATA"="ArrearsDir-MISSING",
            "pmnt_method_grpMISSING_DATA" = "PayMethod-MISSING", "pmnt_method_grpSalary/Suspense" = "PayMethod-SALARY/SUSPENSE",
            "TimeInPerfSpell" = "TimeInPerfSpell", "M_Repo_Rate"="Repo_Rate", "Term" = "Term")

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




