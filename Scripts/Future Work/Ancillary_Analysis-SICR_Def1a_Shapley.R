# ============================== SICR-MODELLIING ===============================
# Analysing Shapley values for SICR-definition 1a
# ------------------------------------------------------------------------------
# Project title: Dynamic SICR-research
# Script authors: Esmerelda Oberholzer, Dr Arno Botha
# ------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R

# -- Inputs:
#   - logit_model_chosen from SICR-definitions 1a
#   - datSICR_smp from SICR-definitions 1a

# -- Outputs:
#   - Plots that provide the contribution of the input variables to the target variable per SICR-definition

# ==============================================================================




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

# --- b) Datasets

SICR_label <- "1a(i)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
dat_SICR_1a_i <- copy(datSICR_smp); rm(datSICR_smp)
SICR_label <- "1a(ii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
dat_SICR_1a_ii <- copy(datSICR_smp); rm(datSICR_smp)
SICR_label <- "1a(iii)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
dat_SICR_1a_iii <- copy(datSICR_smp); rm(datSICR_smp)
SICR_label <- "1a(iv)"
if (!exists('datSICR_smp')) unpack.ffdf(paste0(genPath,"datSICR_smp_", SICR_label), tempPath)
dat_SICR_1a_iv <- copy(datSICR_smp); rm(datSICR_smp)




# ------ 1. Create a profile for the average account

# --- a) Calculate the mean/mode for the variables

# -- 1a(i)

# - Numeric variables
(avg_term <- mean(dat_SICR_1a_i$Term))
(avg_int_margin <- mean(dat_SICR_1a_i$InterestRate_Margin)) 
(avg_balance <- mean(dat_SICR_1a_i$BalanceLog))
(avg_pre_lim_perc <- mean(dat_SICR_1a_i$slc_acct_pre_lim_perc_imputed))
(avg_time_in_perf_spell <- mean(dat_SICR_1a_i$TimeInPerfSpell))
(avg_perf_spell_num <- mean(dat_SICR_1a_i$PerfSpell_Num))
(avg_g0_delinq <- mean(dat_SICR_1a_i$g0_Delinq))
(avg_roll_ever <- mean(dat_SICR_1a_i$slc_acct_roll_ever_24_imputed))
(avg_repo <- mean(dat_SICR_1a_i$M_Repo_Rate))
(avg_inflation <- mean(dat_SICR_1a_i$M_Inflation_Growth))
(avg_dti_lag0 <- mean(dat_SICR_1a_i$M_DTI_Growth))
(avg_dti_lag12 <- mean(dat_SICR_1a_i$M_DTI_Growth_12))
(avg_real_gdp <- mean(dat_SICR_1a_i$M_RealGDP_Growth))

# - Categorical variables
mode_pmnt_method <- unique(dat_SICR_1a_i$pmnt_method_grp)[dat_SICR_1a_i$pmnt_method_grp %>% 
                                                            match(unique(dat_SICR_1a_i$pmnt_method_grp)) %>% 
                                                            tabulate() %>% 
                                                            which.max()]
mode_acct_dir <- unique(dat_SICR_1a_i$slc_acct_arr_dir_3)[dat_SICR_1a_i$slc_acct_arr_dir_3 %>% 
                                                            match(unique(dat_SICR_1a_i$slc_acct_arr_dir_3)) %>% 
                                                            tabulate() %>% 
                                                            which.max()]
# - Create a dataset for the average account
average_account_1a_i <- data.frame(g0_Delinq = avg_g0_delinq
                                  ,PerfSpell_Num = avg_perf_spell_num
                                  ,TimeInPerfSpell = avg_time_in_perf_spell
                                  ,slc_acct_roll_ever_24_imputed = avg_roll_ever
                                  ,slc_acct_arr_dir_3 = mode_acct_dir
                                  ,BalanceLog = avg_balance
                                  ,Term = avg_term
                                  ,InterestRate_Margin = avg_int_margin
                                  ,pmnt_method_grp = mode_pmnt_method
                                  ,slc_acct_pre_lim_perc_imputed = avg_pre_lim_perc
                                  ,M_Repo_Rate = avg_repo
                                  ,M_Inflation_Growth = avg_inflation
                                  ,M_DTI_Growth = avg_dti_lag0
                                  ,M_DTI_Growth_12 = avg_dti_lag12
                                  ,M_RealGDP_Growth = avg_real_gdp
                                  )
# ensure that it is a data table
average_account_1a_i <- as.data.table(average_account_1a_i)

# -- 1a(ii)

# - Numeric variables
(avg_term <- mean(dat_SICR_1a_ii$Term))
(avg_int_margin <- mean(dat_SICR_1a_ii$InterestRate_Margin)) 
(avg_balance <- mean(dat_SICR_1a_ii$BalanceLog))
(avg_pre_lim_perc <- mean(dat_SICR_1a_ii$slc_acct_pre_lim_perc_imputed))
(avg_time_in_perf_spell <- mean(dat_SICR_1a_ii$TimeInPerfSpell))
(avg_perf_spell_num <- mean(dat_SICR_1a_ii$PerfSpell_Num))
(avg_g0_delinq <- mean(dat_SICR_1a_ii$g0_Delinq))
(avg_roll_ever <- mean(dat_SICR_1a_ii$slc_acct_roll_ever_24_imputed))
(avg_repo <- mean(dat_SICR_1a_ii$M_Repo_Rate))
(avg_inflation <- mean(dat_SICR_1a_ii$M_Inflation_Growth))
(avg_dti_lag0 <- mean(dat_SICR_1a_ii$M_DTI_Growth))
(avg_dti_lag12 <- mean(dat_SICR_1a_ii$M_DTI_Growth_12))
(avg_real_gdp <- mean(dat_SICR_1a_ii$M_RealGDP_Growth))

# - Categorical variables
mode_pmnt_method <- unique(dat_SICR_1a_ii$pmnt_method_grp)[dat_SICR_1a_ii$pmnt_method_grp %>% 
                                                            match(unique(dat_SICR_1a_ii$pmnt_method_grp)) %>% 
                                                            tabulate() %>% 
                                                            which.max()]
mode_acct_dir <- unique(dat_SICR_1a_ii$slc_acct_arr_dir_3)[dat_SICR_1a_ii$slc_acct_arr_dir_3 %>% 
                                                            match(unique(dat_SICR_1a_ii$slc_acct_arr_dir_3)) %>% 
                                                            tabulate() %>% 
                                                            which.max()]
# - Create a dataset for the average account
average_account_1a_ii <- data.frame(g0_Delinq = avg_g0_delinq
                                    ,PerfSpell_Num = avg_perf_spell_num
                                    ,TimeInPerfSpell = avg_time_in_perf_spell
                                    ,slc_acct_roll_ever_24_imputed = avg_roll_ever
                                    ,slc_acct_arr_dir_3 = mode_acct_dir
                                    ,BalanceLog = avg_balance
                                    ,Term = avg_term
                                    ,InterestRate_Margin = avg_int_margin
                                    ,pmnt_method_grp = mode_pmnt_method
                                    ,slc_acct_pre_lim_perc_imputed = avg_pre_lim_perc
                                    ,M_Repo_Rate = avg_repo
                                    ,M_Inflation_Growth = avg_inflation
                                    ,M_DTI_Growth = avg_dti_lag0
                                    ,M_DTI_Growth_12 = avg_dti_lag12
                                    ,M_RealGDP_Growth = avg_real_gdp
                                    )
# ensure that it is a data table
average_account_1a_ii <- as.data.table(average_account_1a_ii)

# -- 1a(iii)

# - Numeric variables
(avg_term <- mean(dat_SICR_1a_iii$Term))
(avg_int_margin <- mean(dat_SICR_1a_iii$InterestRate_Margin)) 
(avg_balance <- mean(dat_SICR_1a_iii$BalanceLog))
(avg_pre_lim_perc <- mean(dat_SICR_1a_iii$slc_acct_pre_lim_perc_imputed))
(avg_time_in_perf_spell <- mean(dat_SICR_1a_iii$TimeInPerfSpell))
(avg_perf_spell_num <- mean(dat_SICR_1a_iii$PerfSpell_Num))
(avg_g0_delinq <- mean(dat_SICR_1a_iii$g0_Delinq))
(avg_roll_ever <- mean(dat_SICR_1a_iii$slc_acct_roll_ever_24_imputed))
(avg_repo <- mean(dat_SICR_1a_iii$M_Repo_Rate))
(avg_inflation <- mean(dat_SICR_1a_iii$M_Inflation_Growth))
(avg_dti_lag0 <- mean(dat_SICR_1a_iii$M_DTI_Growth))
(avg_dti_lag12 <- mean(dat_SICR_1a_iii$M_DTI_Growth_12))
(avg_real_gdp <- mean(dat_SICR_1a_iii$M_RealGDP_Growth))

# - Categorical variables
mode_pmnt_method <- unique(dat_SICR_1a_iii$pmnt_method_grp)[dat_SICR_1a_iii$pmnt_method_grp %>% 
                                                             match(unique(dat_SICR_1a_iii$pmnt_method_grp)) %>% 
                                                             tabulate() %>% 
                                                             which.max()]
mode_acct_dir <- unique(dat_SICR_1a_iii$slc_acct_arr_dir_3)[dat_SICR_1a_iii$slc_acct_arr_dir_3 %>% 
                                                             match(unique(dat_SICR_1a_iii$slc_acct_arr_dir_3)) %>% 
                                                             tabulate() %>% 
                                                             which.max()]
# - Create a dataset for the average account
average_account_1a_iii <- data.frame(g0_Delinq = avg_g0_delinq
                                     ,PerfSpell_Num = avg_perf_spell_num
                                     ,TimeInPerfSpell = avg_time_in_perf_spell
                                     ,slc_acct_roll_ever_24_imputed = avg_roll_ever
                                     ,slc_acct_arr_dir_3 = mode_acct_dir
                                     ,BalanceLog = avg_balance
                                     ,Term = avg_term
                                     ,InterestRate_Margin = avg_int_margin
                                     ,pmnt_method_grp = mode_pmnt_method
                                     ,slc_acct_pre_lim_perc_imputed = avg_pre_lim_perc
                                     ,M_Repo_Rate = avg_repo
                                     ,M_Inflation_Growth = avg_inflation
                                     ,M_DTI_Growth = avg_dti_lag0
                                     ,M_DTI_Growth_12 = avg_dti_lag12
                                     ,M_RealGDP_Growth = avg_real_gdp
                                      )
# ensure that it is a data table
average_account_1a_iii <- as.data.table(average_account_1a_iii)

# -- 1a(iv)

# - Numeric variables
(avg_term <- mean(dat_SICR_1a_iv$Term))
(avg_int_margin <- mean(dat_SICR_1a_iv$InterestRate_Margin)) 
(avg_balance <- mean(dat_SICR_1a_iv$BalanceLog))
(avg_pre_lim_perc <- mean(dat_SICR_1a_iv$slc_acct_pre_lim_perc_imputed))
(avg_time_in_perf_spell <- mean(dat_SICR_1a_iv$TimeInPerfSpell))
(avg_perf_spell_num <- mean(dat_SICR_1a_iv$PerfSpell_Num))
(avg_g0_delinq <- mean(dat_SICR_1a_iv$g0_Delinq))
(avg_roll_ever <- mean(dat_SICR_1a_iv$slc_acct_roll_ever_24_imputed))
(avg_repo <- mean(dat_SICR_1a_iv$M_Repo_Rate))
(avg_inflation <- mean(dat_SICR_1a_iv$M_Inflation_Growth))
(avg_dti_lag0 <- mean(dat_SICR_1a_iv$M_DTI_Growth))
(avg_dti_lag12 <- mean(dat_SICR_1a_iv$M_DTI_Growth_12))
(avg_real_gdp <- mean(dat_SICR_1a_iv$M_RealGDP_Growth))

# - Categorical variables
mode_pmnt_method <- unique(dat_SICR_1a_iv$pmnt_method_grp)[dat_SICR_1a_iv$pmnt_method_grp %>% 
                                                              match(unique(dat_SICR_1a_iv$pmnt_method_grp)) %>% 
                                                              tabulate() %>% 
                                                              which.max()]
mode_acct_dir <- unique(dat_SICR_1a_iv$slc_acct_arr_dir_3)[dat_SICR_1a_iv$slc_acct_arr_dir_3 %>% 
                                                              match(unique(dat_SICR_1a_iv$slc_acct_arr_dir_3)) %>% 
                                                              tabulate() %>% 
                                                              which.max()]
# - Create a dataset for the average account
average_account_1a_iv <- data.frame(g0_Delinq = avg_g0_delinq
                                    ,PerfSpell_Num = avg_perf_spell_num
                                    ,TimeInPerfSpell = avg_time_in_perf_spell
                                    ,slc_acct_roll_ever_24_imputed = avg_roll_ever
                                    ,slc_acct_arr_dir_3 = mode_acct_dir
                                    ,BalanceLog = avg_balance
                                    ,Term = avg_term
                                    ,InterestRate_Margin = avg_int_margin
                                    ,pmnt_method_grp = mode_pmnt_method
                                    ,slc_acct_pre_lim_perc_imputed = avg_pre_lim_perc
                                    ,M_Repo_Rate = avg_repo
                                    ,M_Inflation_Growth = avg_inflation
                                    ,M_DTI_Growth = avg_dti_lag0
                                    ,M_DTI_Growth_12 = avg_dti_lag12
                                    ,M_RealGDP_Growth = avg_real_gdp
                                    )
# ensure that it is a data table
average_account_1a_iv <- as.data.table(average_account_1a_iv)




# ------ 2. Calculate Shapley values using the DALEX package

# - Subset the data to only include the input variables
varKeep <- c(# Delinquency-theme inputs
             "g0_Delinq", "PerfSpell_Num", "TimeInPerfSpell", "slc_acct_roll_ever_24_imputed", "slc_acct_arr_dir_3",
             # Credit-themed inputs
             "BalanceLog", "Term", "InterestRate_Margin", "pmnt_method_grp", "slc_acct_pre_lim_perc_imputed", 
             # Macroeconomic-themed inputs
             "M_Repo_Rate", "M_Inflation_Growth", "M_DTI_Growth", "M_DTI_Growth_12", "M_RealGDP_Growth"
)

# 1a(i)
inputs_SICR_1a_i <- subset(dat_SICR_1a_i, select=varKeep)
target_SICR_1a_i <- c(dat_SICR_1a_i[, ifelse(SICR_target == "1", 1, 0)])

# 1a(ii)
inputs_SICR_1a_ii <- subset(dat_SICR_1a_ii, select=varKeep)
target_SICR_1a_ii <- c(dat_SICR_1a_ii[, ifelse(SICR_target == "1", 1, 0)])

# 1a(iii)
inputs_SICR_1a_iii <- subset(dat_SICR_1a_iii, select=varKeep)
target_SICR_1a_iii <- c(dat_SICR_1a_iii[, ifelse(SICR_target == "1", 1, 0)])

# 1a(iv)
inputs_SICR_1a_iv <- subset(dat_SICR_1a_iv, select=varKeep)
target_SICR_1a_iv <- c(dat_SICR_1a_iv[, ifelse(SICR_target == "1", 1, 0)])

# - Create explainer objects and plots

# graphing parameters
chosenFont <- "Cambria"
dpi <- 250



# --- 2.1 DALEX

# 1a(i)
explainer_obj_ia_i <- DALEX::explain(model = logit_model_1a_i, data = inputs_SICR_1a_i, y = target_SICR_1a_i, label = "SICR-model 1a(i)")
shap_values_1a_i <- predict_parts(explainer = explainer_obj_ia_i, new_observation = average_account_1a_i, type = "shap", B = 25)
(e <- plot(shap_values_1a_i, show_boxplots = FALSE))

# 1a(ii)
explainer_obj_ia_ii <- DALEX::explain(model = logit_model_1a_ii, data = inputs_SICR_1a_ii, y = target_SICR_1a_ii, label = "SICR-model 1a(ii)")
shap_values_1a_ii <- predict_parts(explainer = explainer_obj_ia_ii, new_observation = average_account_1a_ii, type = "shap", B = 25)
(e <- plot(shap_values_1a_ii, show_boxplots = FALSE))

# 1a(iii)
explainer_obj_ia_iii <- DALEX::explain(model = logit_model_1a_iii, data = inputs_SICR_1a_iii, y = target_SICR_1a_iii, label = "SICR-model 1a(iii)")
shap_values_1a_iii <- predict_parts(explainer = explainer_obj_ia_iii, new_observation = average_account_1a_iii, type = "shap", B = 25)
(e <- plot(shap_values_1a_iii, show_boxplots = FALSE))

# 1a(iv)
explainer_obj_ia_iv <- DALEX::explain(model = logit_model_1a_iv, data = inputs_SICR_1a_iv, y = target_SICR_1a_iv, label = "SICR-model 1a(iv)")
shap_values_1a_iv <- predict_parts(explainer = explainer_obj_ia_iv, new_observation = average_account_1a_iv, type = "shap", B = 25)
(e <- plot(shap_values_1a_iv, show_boxplots = FALSE))



# --- 2.2 FASTSHAP

# - Set the seed to ensure reproducability 
set.seed(1051)

# 1a(i)
ptm <- proc.time()
((exp_all_ia_i <- explain(logit_model_1a_i, X = inputs_SICR_1a_i, nsim = 10, adjust = TRUE,  pred_wrapper = predict)))
autoplot(exp_all_ia_i)

# compare fastshap with DALEX for the average account
ptm <- proc.time()
((exp_all_ia_i <- explain(logit_model_1a_i, X = inputs_SICR_1a_i, newdata = average_account_1a_i, nsim = 100, adjust = TRUE,  pred_wrapper = predict)))
autoplot(exp_all_ia_i)
# RESULT: I obtain differing results. fastshap provides the absolute value of the mean contribution of every variable compared to
# DALEX that only gives the contribution for a few variables.
# What troubles me a little with the fastshap package, is the contribution of g0. 
# We know that g0 is used in defining the SICR target variable, so I would expect it to have a higher contribution. 
# Nevertheless, there are other parameters used in classifying a SICR-event, so it might be due to the stochastic nature 
# of SICR-events that other variables explain its dynamicity better.
# As a result, use the fastshap package.

# 1a(ii)
ptm <- proc.time()
((exp_all_ia_ii <- explain(logit_model_1a_ii, X = inputs_SICR_1a_ii, nsim = 10, adjust = TRUE,  pred_wrapper = predict)))
autoplot(exp_all_ia_ii)

# 1a(iii)
ptm <- proc.time()
((exp_all_ia_iii <- explain(logit_model_1a_iii, X = inputs_SICR_1a_iii, nsim = 10, adjust = TRUE,  pred_wrapper = predict)))
autoplot(exp_all_ia_iii)

# 1a(iv)
ptm <- proc.time()
((exp_all_ia_iv <- explain(logit_model_1a_iv, X = inputs_SICR_1a_iv, nsim = 10, adjust = TRUE,  pred_wrapper = predict)))
autoplot(exp_all_ia_iv)



