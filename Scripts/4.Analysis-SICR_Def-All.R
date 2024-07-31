# ============================== SICR-DEFINITION ANALYSIS ===============================
# Script for collating performance measures across various SICR-definitions into a single Excel sheet 
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
#   - performance_measures_<> | aggregated performance measures for given SICR-definition (3a)

# -- Outputs:
#   - PerfMeasures_All.xlsx | Microsoft Excel sheet
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
if (!exists('performance_measures_2a_i')) unpack.ffdf(paste0(genObjPath,"performance_measures_2a(i)"), tempPath)
if (!exists('performance_measures_2a_ii')) unpack.ffdf(paste0(genObjPath,"performance_measures_2a(ii)"), tempPath)
if (!exists('performance_measures_2a_iii')) unpack.ffdf(paste0(genObjPath,"performance_measures_2a(iii)"), tempPath)
if (!exists('performance_measures_2a_iv')) unpack.ffdf(paste0(genObjPath,"performance_measures_2a(iv)"), tempPath)
if (!exists('performance_measures_2b_i')) unpack.ffdf(paste0(genObjPath,"performance_measures_2b(i)"), tempPath)
if (!exists('performance_measures_2b_ii')) unpack.ffdf(paste0(genObjPath,"performance_measures_2b(ii)"), tempPath)
if (!exists('performance_measures_2b_iii')) unpack.ffdf(paste0(genObjPath,"performance_measures_2b(iii)"), tempPath)
if (!exists('performance_measures_2b_iv')) unpack.ffdf(paste0(genObjPath,"performance_measures_2b(iv)"), tempPath)
if (!exists('performance_measures_2c_i')) unpack.ffdf(paste0(genObjPath,"performance_measures_2c(i)"), tempPath)
if (!exists('performance_measures_2c_ii')) unpack.ffdf(paste0(genObjPath,"performance_measures_2c(ii)"), tempPath)
if (!exists('performance_measures_2c_iii')) unpack.ffdf(paste0(genObjPath,"performance_measures_2c(iii)"), tempPath)
if (!exists('performance_measures_2c_iv')) unpack.ffdf(paste0(genObjPath,"performance_measures_2c(iv)"), tempPath)


# - Merge datasets together
performance_measures <- data.table(rbind(performance_measures_1a_i, performance_measures_1a_ii, performance_measures_1a_iii, performance_measures_1a_iv,
                                         performance_measures_1b_i, performance_measures_1b_ii, performance_measures_1b_iii, performance_measures_1b_iv,
                                         performance_measures_1c_i, performance_measures_1c_ii, performance_measures_1c_iii, performance_measures_1c_iv,
                                         performance_measures_2a_i, performance_measures_2a_ii, performance_measures_2a_iii, performance_measures_2a_iv,
                                         performance_measures_2b_i, performance_measures_2b_ii, performance_measures_2b_iii, performance_measures_2b_iv,
                                         performance_measures_2c_i, performance_measures_2c_ii, performance_measures_2c_iii, performance_measures_2c_iv), 
                                   key="SICR_definition")

# - Cleanup
rm(performance_measures_1a_i, performance_measures_1a_ii, performance_measures_1a_iii, performance_measures_1a_iv,
   performance_measures_1b_i, performance_measures_1b_ii, performance_measures_1b_iii, performance_measures_1b_iv,
   performance_measures_1c_i, performance_measures_1c_ii, performance_measures_1c_iii, performance_measures_1c_iv,
   performance_measures_2a_i, performance_measures_2a_ii, performance_measures_2a_iii, performance_measures_2a_iv,
   performance_measures_2b_i, performance_measures_2b_ii, performance_measures_2b_iii, performance_measures_2b_iv,
   performance_measures_2c_i, performance_measures_2c_ii, performance_measures_2c_iii, performance_measures_2c_iv); gc()

# - Analysis
performance_measures

# --- Create succinct summaries for populating certain tables in SICR-article (e.g., tab:1bc-performance)
# - Instability \sigma
paste0(performance_measures$SICR_definition, ": ", percent(performance_measures$std_dev_SICR_rate_Act, accuracy=0.01))
# - AUC
paste0(performance_measures$SICR_definition,": ", sprintf("%.1f", performance_measures$AUC_prob), "% ± ", 
  sprintf("%.2f", (performance_measures$CI_upper_prob - performance_measures$CI_lower_prob)/2), "%")


# - Store results
write_xlsx(x=performance_measures,path=paste0(genObjPath, "PerfMeasures_All.xlsx"))
pack.ffdf(paste0(genObjPath, "PerformanceMeasures_All"), performance_measures); gc()

# - Cleanup
rm(performance_measures); gc()
