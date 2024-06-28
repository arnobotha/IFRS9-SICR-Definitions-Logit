# ================================ DATA PREPARATION ============================ 
# Preparing the macroeconomic data into a suitable format for model building
# ------------------------------------------------------------------------------
# PROJECT TITLE: Dynamic SICR-research
# SCRIPT AUTHOR(S): Dr Arno Botha, Esmerelda Oberholzer

# DESCRIPTION:
# This script prepares raw data into a more meaningful form to facilitate modelling.
# This preparation includes the following high-level steps:
#   1) creating some basic derived fields within macroeconomic datasets
#   2) removing redundant fields in macroeconomic datasets to optimise sizes
#   3) checking data grains and fusing datasets accordingly
#   4) Interleaving fused fields appropriately between either the left or right side 
#      during the previous merge
#   5) interpolating certain fields that represent important time series as a missing
#      value treatment that arose during merging quarterly with monthly data
#   6) checking and confirming that missingness has been successfully treated
#   7) subsetting data from pre-specified start point & scaling time series appropriately
#   8) creating macroeconomic variable lags with associated volatilities (stdev).
# ------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R

# -- Inputs:
#   - macro_data_m | monthly macroeconomic data imported in script 1
#   - macro_data_q | quarterly macroeconomic data imported in script 1

# -- Outputs:
#   - macro_data_hist | fused & prepared macroeconomic dataset
#   - dat_SICR_MVs | Features created on a subset of fields in macro_data_hist
# ==============================================================================



# --------- 1. Basic Data Cleaning & Checks
# Basic cleaning, light transforms, and confirming the supposed data grain

# -- ensure dates are correctly converted
macro_data_m[, Date_T := as.POSIXct(EffectiveDate, format="%Y-%m-%d")]
macro_data_q[, Date_T := as.POSIXct(EffectiveDate, format="%Y-%m-%d")]

# -- create YYYMM abstractions for easier reference
macro_data_m[, Period := year(Date_T)*100 + month(Date_T)]
macro_data_q[, Period := year(Date_T)*100 + month(Date_T)]

# -- remove redundant date fields 
# (these may be removed during initial data extraction within SAS, when engineering a proper data process)
macro_data_m[, `:=`(EffectiveDate = NULL, YEAR = NULL, Country = NULL, Source = NULL, Loaddatetime = NULL,
                    Process_Start_DateTime = NULL, Process_End_datetime = NULL, TABLE_freq = NULL)]
macro_data_q[, `:=`(EffectiveDate = NULL, Quarter = NULL, Country = NULL, Source = NULL, Loaddatetime = NULL,
                    Process_Start_DateTime = NULL, Process_End_DateTime = NULL, TABLE_freq = NULL)]

# [SANITY CHECK] Confirm all macroeconomic records to have no missingness in its supposed key (derived from EffectiveDate)
(check0 <- macro_data_m[is.na(Period), .N] == 0 & macro_data_q[is.na(Period), .N] == 0)
cat( check0 %?% 'SAFE: [Period]-key contains no missingness.\n' %:% 'WARNING: [Period]-key contains missingness.\n')

# -- Conditionally subset from all datasets to ensure we have non-zero values in all key fields
if (!check0) {
  macro_data_m <- subset(macro_data_m, !is.na(Period))
  macro_data_q <- subset(macro_data_q, !is.na(Period)) 
}

# - testing final data grains on proposed keys
cat( (macro_data_m[,list(Freqs = .N), by=list(Period, Scenario)][Freqs > 1, .N] == 0) %?% 'SAFE: Grain confirmed.\n' %:% 
       paste0('ERROR, grain broken for ', macro_data_m[,list(Freqs = .N), by=list(Period, Scenario)][Freqs > 1,.N], " cases.\n")
)

cat( (macro_data_q[,list(Freqs = .N), by=list(Period, Scenario)][Freqs > 1,.N] == 0) %?% cat('SAFE: Grain confirmed.\n') %:% 
       cat(paste0('ERROR, grain broken for ', macro_data_q[,list(Freqs = .N), by=list(Period, Scenario)][Freqs > 1,.N], " cases.\n"))
)

# - grains passed, create a single primary key
macro_data_m[, Key := paste0(Period,"-",Scenario)]
macro_data_q[, Key := paste0(Period,"-",Scenario)]



# --------- 2. Data fusion & Interleaving & Interpolation
# Merging monthly and quarterly data together and interleaving
# fields accordingly. Also a bit of interpolation on some time series

# - fuse data using a simple left join between monthly and quarterly
macro_data <- merge(macro_data_m, macro_data_q, by=c("Key"), all.x=T)

# - simple coalesces and interleaving transformations for one-sided missingness
# uses custom interpolation function "interleave()" defined in 0.Setup
macro_data[, Date_T := interleave(Date_T.x, Date_T.y, na.value = as.POSIXct(NA), pref='X') ]
macro_data[, Period := interleave(Period.x, Period.y, na.value = as.double(NA), pref='X') ]
macro_data[, Probability := interleave(Probability.x, Probability.y, na.value = as.double(NA), pref='X') ]
macro_data[, Type := interleave(Type.x, Type.y, na.value = as.character(NA), pref='X') ]
macro_data[, Scenario := interleave(Scenario.x, Scenario.y, na.value = as.character(NA), pref='X') ]

# - [SANITY CHECK] Are there treated fields that have innate missingness in both sides of the previous join?
test <- subset(macro_data, is.na(Probability.x) & is.na(Probability.y))
cat( (test[,.N] > 0) %?% 'WARNING: Missingness on both joining sides for [Probability].\n' %:% 'SAFE: Only one-sided missingness for [Probability].\n')

# [SANITY CHECK] Confirm successful treatment, considering previous sanity check
check1 <- all(is.na(macro_data$Date_T) == F) & all(is.na(macro_data$Period) == F) & 
  (test[,.N] > 0 & !all(is.na(macro_data$Probability) == F) | all(is.na(macro_data$Probability) == F)) & 
  all(is.na(macro_data$Type) == F) & all(is.na(macro_data$Scenario) == F)
cat( check1 %?% 'SAFE: Interleaving successful with no residual missingness where relevant.\n' %:% 'WARNING: Residual missingness detected, treatment failed.\n')

# - remove fields made redundant due to fusion
suppressWarnings(macro_data[, `:=`(Date_T.x = NULL, Date_T.y = NULL, Type.x = NULL, Type.y = NULL, 
                                   Scenario.x = NULL, Scenario.y = NULL, Period.x = NULL, Period.y = NULL,
                                   Probability.x = NULL, Probability.y = NULL)])

# - create quarterly key, given that Date_T refers to last date of each month
# and that quarterly data points are retrospective
macro_data[, Period_Qtr := case_when(
  month(Date_T) >= 1 & month(Date_T) < 4 ~ paste0(year(Date_T), "Q1"),
  month(Date_T) >= 4 & month(Date_T) < 7 ~ paste0(year(Date_T), "Q2"),
  month(Date_T) >= 7 & month(Date_T) < 10 ~ paste0(year(Date_T), "Q3"),
  month(Date_T) >= 10 ~ paste0(year(Date_T), "Q4")
)]

# - Missing value treatment: Quarterly data have missing values for interleaved months. 
# We can linearly interpolate for the in-between months
# uses custom interpolation function "interPol()" defined in 0.Setup
macro_data[, Employment_Growth_YoY := interPol(Employment_Growth_YoY), by=list(Scenario)]
macro_data[, Household_debt_Level_income := interPol(Household_debt_Level_income), by=list(Scenario)]
macro_data[, Household_DSC_Level_income := interPol(Household_DSC_Level_income), by=list(Scenario)]
macro_data[, RealGDP_Growth_yoy := interPol(RealGDP_Growth_yoy), by=list(Scenario)]
macro_data[, Consumption_Growth_yoy := interPol(Consumption_Growth_yoy), by=list(Scenario)]
macro_data[, Durables_Growth_yoy := interPol(Durables_Growth_yoy), by=list(Scenario)]
macro_data[, Nominal_GDP_Growth_yoy := interPol(Nominal_GDP_Growth_yoy), by=list(Scenario)]
macro_data[, Nominal_income_Growth_yoy := interPol(Nominal_income_Growth_yoy), by=list(Scenario)]
macro_data[, Real_income_Growth_YoY := interPol(Real_income_Growth_YoY), by=list(Scenario)]

# [SANITY CHECK] Confirm successful treatment, considering previous sanity check
check2 <- all(is.na(macro_data$Employment_Growth_YoY) == F) & all(is.na(macro_data$Household_debt_Level_income) == F) & 
  (all(is.na(macro_data$Household_DSC_Level_income) == F)) & all(is.na(macro_data$RealGDP_Growth_yoy) == F) & 
  all(is.na(macro_data$Consumption_Growth_yoy) == F) & (all(is.na(macro_data$Durables_Growth_yoy) == F)) &
  (all(is.na(macro_data$Nominal_GDP_Growth_yoy) == F)) & (all(is.na(macro_data$Nominal_income_Growth_yoy) == F)) &
  (all(is.na(macro_data$Real_income_Growth_YoY) == F))
cat( check2 %?% 'SAFE: Interpolation successful with no residual missingness where relevant.\n' %:% 'WARNING: Residual missingness detected, treatment failed.\n')

# -- remove useless macroeconomic variables that are currently not forecast
macro_data[, `:=`(rbqn_rb1419w = NULL, HPI_Level_EOP = NULL, HPI_Level_SA = NULL, HPI_level_SA_MoM_Change = NULL,
                  sahp_fnbhpgp = NULL, sahp_fnbhpwc = NULL, sahp_fnbhpkzn = NULL, sahp_fnbhpec = NULL, sahp_fnbhpoth = NULL,
                  rbqn_rb5339m = NULL)]



# --------- 3. Missing Value Treatments & Scaling
# Subsetting only relevant periods (and fields), apply missing value treatments, and scale domains

# --- a. Subsetting

# - subet only relevant macroeconomic variables (chosen by discretion) from historic data (exclude forecasts) beyond a certain point
data.start <- "1980-01-01" # other (tested) options include 1980, 1999, 2005, 2010
macro_data_hist <- subset(macro_data, Type == "Historic" & Date_T >= data.start)[, list(Key, Period, Period_Qtr, Date_T, 
                                                                                        Inflation = Inflation_Growth_YoY,
                                                                                        Repo_Rate = Repo_rate_level_eop,
                                                                                        HousePriceIndex_Rate = HPI_Growth_Yoy_perc,
                                                                                        Employment_Rate = Employment_Growth_YoY,
                                                                                        DebtServiceCosts_Rate = Household_DSC_Level_income,
                                                                                        DebtToIncome_Rate = Household_debt_Level_income,
                                                                                        RealGDP_Rate = RealGDP_Growth_yoy,
                                                                                        NominalGDP_Rate = Nominal_GDP_Growth_yoy,
                                                                                        RealIncome_Rate = Real_income_Growth_YoY,
                                                                                        NominalIncome_Rate = Nominal_income_Growth_yoy,
                                                                                        Consumption_Rate = Consumption_Growth_yoy,
                                                                                        Durables_Rate = Durables_Growth_yoy)]

# --- b. Missing value treatments

# - quickly investigate any missings by conducting high-level distribution analysis 
describe(macro_data_hist[, list(Inflation, Repo_Rate, HousePriceIndex_Rate, Employment_Rate, DebtServiceCosts_Rate, DebtToIncome_Rate, RealGDP_Rate,
                                NominalGDP_Rate, RealIncome_Rate, NominalIncome_Rate, Consumption_Rate, Durables_Rate)])
# -- Results: Some series have missing values (not previously treated during quarterly-monthly fusion)

# - Interpolate all remaining macroeconomic variables as a failsafe against missing values in some months
# uses custom interpolation function "interPol()" defined in 0.Setup
macro_data_hist[, Inflation := interPol(Inflation)]
macro_data_hist[, Repo_Rate := interPol(Repo_Rate)]
macro_data_hist[, HousePriceIndex_Rate := interPol(HousePriceIndex_Rate)]

# - check success of treatment
check3 <- !any(is.na(macro_data_hist[, list(Inflation, Repo_Rate, HousePriceIndex_Rate, Employment_Rate, DebtServiceCosts_Rate, DebtToIncome_Rate, RealGDP_Rate,
                                            NominalGDP_Rate, RealIncome_Rate, NominalIncome_Rate, Consumption_Rate, Durables_Rate)]))
# Treatment worked as expected (FALSE). No more missing values.
cat( check3 %?% 'SAFE: Interpolation successful with no residual missingness where relevant.\n' %:% 'WARNING: Residual missingness detected, treatment failed.\n')


# --- c. Scaling

# - create scaled "indices" of each macroeconomic variable
# uses custom interpolation function "scaler.norm()" defined in 0.Setup
macro_data_hist[, Inflation_I := scaler.norm(Inflation)]
macro_data_hist[, Repo_Rate_I := scaler.norm(Repo_Rate)]
macro_data_hist[, HousePriceIndex_I := scaler.norm(HousePriceIndex_Rate)]
macro_data_hist[, Employment_I := scaler.norm(Employment_Rate)]
macro_data_hist[, DSC_I := scaler.norm(DebtServiceCosts_Rate)]
macro_data_hist[, DTI_I := scaler.norm(DebtToIncome_Rate)]
macro_data_hist[, RealGDP_I := scaler.norm(RealGDP_Rate)]
macro_data_hist[, NominalGDP_I := scaler.norm(NominalGDP_Rate)]
macro_data_hist[, RealIncome_I := scaler.norm(RealIncome_Rate)]
macro_data_hist[, NominalIncome_I := scaler.norm(NominalIncome_Rate)]
macro_data_hist[, Consumption_I := scaler.norm(Consumption_Rate)]
macro_data_hist[, Durables_I := scaler.norm(Durables_Rate)]



# --------- 4. Feature engineering: Lags & Volatilities
# Create lagged variants of certain fields
# Also calculate sample variance over certain windows, so-called "volatilities"


# --- a. Subset macroeconomic fields and carry out light data preparation
# We only want the following macroeconomic variables (MVs), as found to be significant by Botha et al. (2020):
# - Real income growth rate
# - Real GDP growth rate
# - Repo rate (not scaled as we want to use it for a new variable)
# - Employment index growth rate
# - Household DDI ratio
# - Inflation growth rate

# - Subsample monthly historical macroeconomic information with some light data preparation
dat_SICR_MVs <- macro_data_hist[,list(Date=Date_T, Repo_Rate_I, Inflation_I, DTI_I, Employment_I, 
                                      RealGDP_I, RealIncome_I, M_Repo_Rate = Repo_Rate/100, 
                                      M_Inflation_Growth = round(Inflation/100,digits=4),
                                      M_DTI_Growth = round(DebtToIncome_Rate/100,digits=4),
                                      M_Emp_Growth = round(Employment_Rate/100,digits=4),
                                      M_RealGDP_Growth = round(RealGDP_Rate/100,digits=4),
                                      M_RealIncome_Growth = round(RealIncome_Rate/100,digits=4))]

# - Format date correctly
dat_SICR_MVs[, Date := as.Date(Date, format="%Y-%m-%d")]

# - Clean-up
rm(macro_data, macro_data_hist, macro_data_m, macro_data_q, test); gc()


# --- b. Test the correlation between the "raw" and scaled MVs
cor(dat_SICR_MVs$Repo_Rate_I, dat_SICR_MVs$M_Repo_Rate)
# Correlation - 100%
cor(dat_SICR_MVs$Inflation_I, dat_SICR_MVs$M_Inflation_Growth)
# Correlation - 99.99999%
cor(dat_SICR_MVs$DTI_I, dat_SICR_MVs$M_DTI_Growth)
# Correlation - 100%
cor(dat_SICR_MVs$Employment_I, dat_SICR_MVs$M_Emp_Growth)
# Correlation - 99.99993%
cor(dat_SICR_MVs$RealGDP_I, dat_SICR_MVs$M_RealGDP_Growth)
# Correlation - 99.99994%
cor(dat_SICR_MVs$RealIncome_I, dat_SICR_MVs$M_RealIncome_Growth)
# Correlation - 99.99996%
# Since the variables are all highly/perfectly correlated, no need to test both in a modelling statement
# Therefore, choose the raw macroeconomic variables to use as features in the model


# --- c. Create lagged variants of certain time series across preset windows/horizons
# create lags up to 12 months

# - Create a vector of columns to be lagged in the form "<name>_<lag length>"
givenList <- c("M_Inflation_Growth_", "M_DTI_Growth_", "M_Emp_Growth_", "M_RealIncome_Growth_", "M_RealGDP_Growth_", "M_Repo_Rate_")
colNames <- c(sapply(1:length(givenList), function(j,colList) {
              sapply(1:12, function(i,j,colList) {
              paste0(colList[j], i)}, j=j, colList=colList) },
              colList=givenList))

# - Create lagged variants as specified in the column name vector
dat_SICR_MVs[, (colNames) := lapply(1:length(colNames), function(i, dat, colNames) { 
  # testing conditions:
  # i <- 1; dat <- dat_SICR_MVs;
  lastBar <- tail(unlist(gregexpr('_', colNames[i])), n=1)
  colName <- substr(colNames[i], 1, lastBar-1)
  lagLen <- as.numeric( substr(colNames[i], lastBar+1, nchar(colNames[i])) ) 
  dat[, shift( get(colName), n=lagLen, type="lag")]
  #return (   )
}, dat=.SD, colNames=colNames)]


# --- d. Create rolling volatilities across preset periods/windows
# create volatilities up to 12 months

# - Define new volatility variable to dynamically create volatilities of x-months
dat_SICR_MVs[, M_Inflation_Growth_Vol := M_Inflation_Growth]
dat_SICR_MVs[, M_DTI_Growth_Vol := M_DTI_Growth]
dat_SICR_MVs[, M_Emp_Growth_Vol := M_Emp_Growth]
dat_SICR_MVs[, M_RealGDP_Growth_Vol := M_RealGDP_Growth]
dat_SICR_MVs[, M_Repo_Rate_Vol := M_Repo_Rate]
dat_SICR_MVs[, M_RealIncome_Growth_Vol := M_RealIncome_Growth]

# - Create a vector of columns that will be used to create volatilities in the form "<name>_<lag length>"
volList <- c("M_Inflation_Growth_Vol_", "M_DTI_Growth_Vol_", "M_Emp_Growth_Vol_", 
             "M_RealGDP_Growth_Vol_", "M_RealIncome_Growth_Vol_", "M_Repo_Rate_Vol_")
colNames_vol <- c(sapply(1:length(volList), function(j,colList) {
                  sapply(1:12, function(i,j,colList) {
                  paste0(colList[j], i)}, j=j, colList=colList) },
                  colList=volList))

# - Create volatility variants as specified in the column name (volatilities) vector
dat_SICR_MVs[, (colNames_vol) := lapply(1:length(colNames_vol), function(i, dat, colNames_vol) { 
  # testing conditions:
  # i <- 1; dat <- dat_SICR_MVs;
  lastBar <- tail(unlist(gregexpr('_', colNames_vol[i])), n=1)
  colName <- substr(colNames_vol[i], 1, lastBar-1)
  volLen <- as.numeric( substr(colNames_vol[i], lastBar+1, nchar(colNames_vol[i])) ) 
  dat[, rollapply( get(colName), width=volLen,  FUN = sd, fill = 0, align="r")]
  #return (   )
}, dat=.SD, colNames_vol=colNames_vol)]


# --- d. Remove 1-month volatilities since they are essentially just the feature itself, as well as the scaled versions of the MVs

suppressWarnings( dat_SICR_MVs[, `:=`(M_Inflation_Growth_Vol_1 = NULL, M_DTI_Growth_Vol_1 = NULL, 
                                      M_Emp_Growth_Vol_1 = NULL, M_RealIncome_Growth_Vol_1 = NULL,
                                      M_RealGDP_Growth_Vol_1 = NULL, RealIncome_I = NULL,
                                      M_Repo_Rate_Vol_1 = NULL, M_Inflation_Growth_Vol = NULL,
                                      M_DTI_Growth_Vol = NULL, M_Emp_Growth_Vol = NULL,
                                      M_RealGDP_Growth_Vol = NULL, RealGDP_I = NULL,
                                      M_Repo_Rate_Vol = NULL, M_RealIncome_Growth_Vol = NULL,
                                      Repo_Rate_I = NULL, Inflation_I = NULL, 
                                      DTI_I = NULL, Employment_I = NULL)])

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "datSICR_MVs"), dat_SICR_MVs); gc()
