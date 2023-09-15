# ===================================== DATA ENRICH =====================================
# Use the macroeconomic dataset, performing various data preparation steps on it, and
# create features using the macroeconomic variables that can be used in model development
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
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
#   8) creating different features using the macroeconomic variables
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R

# -- Inputs:
#   - various parameters set in the setup script 0
#   - macro_data_m | monthly macroeconomic data imported in script 1
#   - macro_data_q | quarterly macroeconomic data imported in script 1
#
# -- Outputs:
#   - dat_ClDiag_MVs | enriched macroeconomic dataset, with various features
# ---------------------------------------------------------------------------------------
# NOTE: This script predominantly comes from another project (SICR-modelling), 
# but with a few changes to create more features
# =======================================================================================





# ------- 1. Macroeconomic data | Basic Data Cleaning & Checks
# Basic cleaning, light transforms, and confirming the supposed data grain

ptm <- proc.time() # for runtime calculations (ignore)

# --- 1.1. Macroeconomic history + forecasts from FNB Group Economics | monthly data
# Import, then recast data into a more pliable data.table object for greater memory efficiency, during which the
# key is set based on preliminary analysis on the data grain (itself tested later again)
macro_data_m <- as.data.table(read_sas(paste0(genRawPath,"macro_data_monthly.sas7bdat")), stringsAsFactors=T,
                              key=c("EffectiveDate", "Scenario"))

# --- 1.2. Macroeconomic history + forecasts from FNB Group Economics | quarterly data
# Import, then recast data into a more pliable data.table object for greater memory efficiency, during which the
# key is set based on preliminary analysis on the data grain (itself tested later again)
macro_data_q <- as.data.table(read_sas(paste0(genRawPath,"macro_data_quarterly.sas7bdat")), stringsAsFactors=T,
                              key=c("EffectiveDate", "Scenario"))

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

# the credit index has changed - remove forecasts
macro_data_m <- subset(macro_data_m, Scenario=='Historic')
macro_data_q <- subset(macro_data_q, Type=='Historic')

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
macro_data[, Type := interleave(Type.x, Type.y, na.value = as.character(NA), pref='X') ]
macro_data[, Scenario := interleave(Scenario.x, Scenario.y, na.value = as.character(NA), pref='X') ]

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
# Create lead an lagged variables
# Create moving averages
# Create various ratios
# Create features using the Yeo Johnson transformation (Yeo and Johnson, 2000)


# --- a. Subset macroeconomic fields and carry out light data preparation
# We only want the following macroeconomic variables (MVs), as found to be significant by Botha et al. (2020):
# - Real income growth rate
# - Real GDP growth rate
# - Repo rate (not scaled as we want to use it for a new variable)
# - Employment index growth rate
# - Household DDI ratio
# - Inflation growth rate

# - Subsample monthly historical macroeconomic information with some light data preparation
dat_ClDiag_MVs <- macro_data_hist[,list(Date=Date_T, Repo_Rate_I, Inflation_I, DTI_I, Employment_I, 
                                        RealGDP_I, RealIncome_I, M_Repo_Rate = Repo_Rate/100, 
                                        M_Inflation_Growth = round(Inflation/100,digits=4),
                                        M_DTI_Growth = round(DebtToIncome_Rate/100,digits=4),
                                        M_Emp_Growth = round(Employment_Rate/100,digits=4),
                                        M_RealGDP_Growth = round(RealGDP_Rate/100,digits=4),
                                        M_RealIncome_Growth = round(RealIncome_Rate/100,digits=4))]

# - Format date correctly
dat_ClDiag_MVs[, Date := as.Date(Date, format="%Y-%m-%d")]

# - Clean-up
rm(macro_data, macro_data_hist, macro_data_m, macro_data_q); gc()


# --- b. Test the correlation between the "raw" and scaled MVs
cor(dat_ClDiag_MVs$Repo_Rate_I, dat_ClDiag_MVs$M_Repo_Rate)
# Correlation - 100%
cor(dat_ClDiag_MVs$Inflation_I, dat_ClDiag_MVs$M_Inflation_Growth)
# Correlation - 99.99999%
cor(dat_ClDiag_MVs$DTI_I, dat_ClDiag_MVs$M_DTI_Growth)
# Correlation - 100%
cor(dat_ClDiag_MVs$Employment_I, dat_ClDiag_MVs$M_Emp_Growth)
# Correlation - 99.99993%
cor(dat_ClDiag_MVs$RealGDP_I, dat_ClDiag_MVs$M_RealGDP_Growth)
# Correlation - 99.99994%
cor(dat_ClDiag_MVs$RealIncome_I, dat_ClDiag_MVs$M_RealIncome_Growth)
# Correlation - 99.99996%
# Since the variables are all highly/perfectly correlated, no need to test both in a modelling statement
# Therefore, choose the raw macroeconomic variables to use as features in the model

# - Remove the scaled variants
dat_ClDiag_MVs[, `:=`(Repo_Rate_I = NULL, Inflation_I = NULL, DTI_I = NULL, Employment_I = NULL,
                      RealGDP_I = NULL, RealIncome_I = NULL)]


# --- c. Create lagged variants of certain time series across preset windows/horizons
# create certain lags of 3, 6, 9 and 12 months
# from the SICR-research, these are the lags that are most significant
# Repo
dat_ClDiag_MVs[, M_Repo_Rate_lag3 := shift(M_Repo_Rate, n=3, type="lag")]
dat_ClDiag_MVs[, M_Repo_Rate_lag6 := shift(M_Repo_Rate, n=6, type="lag")]
dat_ClDiag_MVs[, M_Repo_Rate_lag9 := shift(M_Repo_Rate, n=9, type="lag")]
dat_ClDiag_MVs[, M_Repo_Rate_lag12 := shift(M_Repo_Rate, n=12, type="lag")]

# Inflation
dat_ClDiag_MVs[, M_Inflation_Growth_lag3 := shift(M_Inflation_Growth, n=3, type="lag")]
dat_ClDiag_MVs[, M_Inflation_Growth_lag6 := shift(M_Inflation_Growth, n=6, type="lag")]
dat_ClDiag_MVs[, M_Inflation_Growth_lag9 := shift(M_Inflation_Growth, n=9, type="lag")]
dat_ClDiag_MVs[, M_Inflation_Growth_lag12 := shift(M_Inflation_Growth, n=12, type="lag")]

# DTI
dat_ClDiag_MVs[, M_DTI_Growth_lag3 := shift(M_DTI_Growth, n=3, type="lag")]
dat_ClDiag_MVs[, M_DTI_Growth_lag6 := shift(M_DTI_Growth, n=6, type="lag")]
dat_ClDiag_MVs[, M_DTI_Growth_lag9 := shift(M_DTI_Growth, n=9, type="lag")]
dat_ClDiag_MVs[, M_DTI_Growth_lag12 := shift(M_DTI_Growth, n=12, type="lag")]

# Employment growth
dat_ClDiag_MVs[, M_Emp_Growth_lag3 := shift(M_Emp_Growth, n=3, type="lag")]
dat_ClDiag_MVs[, M_Emp_Growth_lag6 := shift(M_Emp_Growth, n=6, type="lag")]
dat_ClDiag_MVs[, M_Emp_Growth_lag9 := shift(M_Emp_Growth, n=9, type="lag")]
dat_ClDiag_MVs[, M_Emp_Growth_lag12 := shift(M_Emp_Growth, n=12, type="lag")]

# Real GDP growth
dat_ClDiag_MVs[, M_RealGDP_Growth_lag3 := shift(M_RealGDP_Growth, n=3, type="lag")]
dat_ClDiag_MVs[, M_RealGDP_Growth_lag6 := shift(M_RealGDP_Growth, n=6, type="lag")]
dat_ClDiag_MVs[, M_RealGDP_Growth_lag9 := shift(M_RealGDP_Growth, n=9, type="lag")]
dat_ClDiag_MVs[, M_RealGDP_Growth_lag12 := shift(M_RealGDP_Growth, n=12, type="lag")]

# Real income growth
dat_ClDiag_MVs[, M_RealIncome_Growth_lag3 := shift(M_RealIncome_Growth, n=3, type="lag")]
dat_ClDiag_MVs[, M_RealIncome_Growth_lag6 := shift(M_RealIncome_Growth, n=6, type="lag")]
dat_ClDiag_MVs[, M_RealIncome_Growth_lag9 := shift(M_RealIncome_Growth, n=9, type="lag")]
dat_ClDiag_MVs[, M_RealIncome_Growth_lag12 := shift(M_RealIncome_Growth, n=12, type="lag")]


# --- d. Create lead variants of certain time series across preset windows/horizons
# create certain leads of 3, 6, 9 and 12 months
# Repo
dat_ClDiag_MVs[, M_Repo_Rate_lead3 := shift(M_Repo_Rate, n=3, type="lead")]
dat_ClDiag_MVs[, M_Repo_Rate_lead6 := shift(M_Repo_Rate, n=6, type="lead")]
dat_ClDiag_MVs[, M_Repo_Rate_lead9 := shift(M_Repo_Rate, n=9, type="lead")]
dat_ClDiag_MVs[, M_Repo_Rate_lead12 := shift(M_Repo_Rate, n=12, type="lead")]

# Inflation
dat_ClDiag_MVs[, M_Inflation_Growth_lead3 := shift(M_Inflation_Growth, n=3, type="lead")]
dat_ClDiag_MVs[, M_Inflation_Growth_lead6 := shift(M_Inflation_Growth, n=6, type="lead")]
dat_ClDiag_MVs[, M_Inflation_Growth_lead9 := shift(M_Inflation_Growth, n=9, type="lead")]
dat_ClDiag_MVs[, M_Inflation_Growth_lead12 := shift(M_Inflation_Growth, n=12, type="lead")]

# DTI
dat_ClDiag_MVs[, M_DTI_Growth_lead3 := shift(M_DTI_Growth, n=3, type="lead")]
dat_ClDiag_MVs[, M_DTI_Growth_lead6 := shift(M_DTI_Growth, n=6, type="lead")]
dat_ClDiag_MVs[, M_DTI_Growth_lead9 := shift(M_DTI_Growth, n=9, type="lead")]
dat_ClDiag_MVs[, M_DTI_Growth_lead12 := shift(M_DTI_Growth, n=12, type="lead")]

# Employment growth
dat_ClDiag_MVs[, M_Emp_Growth_lead3 := shift(M_Emp_Growth, n=3, type="lead")]
dat_ClDiag_MVs[, M_Emp_Growth_lead6 := shift(M_Emp_Growth, n=6, type="lead")]
dat_ClDiag_MVs[, M_Emp_Growth_lead9 := shift(M_Emp_Growth, n=9, type="lead")]
dat_ClDiag_MVs[, M_Emp_Growth_lead12 := shift(M_Emp_Growth, n=12, type="lead")]

# Real GDP growth
dat_ClDiag_MVs[, M_RealGDP_Growth_lead3 := shift(M_RealGDP_Growth, n=3, type="lead")]
dat_ClDiag_MVs[, M_RealGDP_Growth_lead6 := shift(M_RealGDP_Growth, n=6, type="lead")]
dat_ClDiag_MVs[, M_RealGDP_Growth_lead9 := shift(M_RealGDP_Growth, n=9, type="lead")]
dat_ClDiag_MVs[, M_RealGDP_Growth_lead12 := shift(M_RealGDP_Growth, n=12, type="lead")]

# Real income growth
dat_ClDiag_MVs[, M_RealIncome_Growth_lead3 := shift(M_RealIncome_Growth, n=3, type="lead")]
dat_ClDiag_MVs[, M_RealIncome_Growth_lead6 := shift(M_RealIncome_Growth, n=6, type="lead")]
dat_ClDiag_MVs[, M_RealIncome_Growth_lead9 := shift(M_RealIncome_Growth, n=9, type="lead")]
dat_ClDiag_MVs[, M_RealIncome_Growth_lead12 := shift(M_RealIncome_Growth, n=12, type="lead")]


# --- d. Calculate moving averages
# calculate 6, 9, 12 and 15 months moving averages
# Repo
dat_ClDiag_MVs[, M_Repo_Rate_ma3 := rollapplyr(M_Repo_Rate, 3, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_Repo_Rate_ma6 := rollapplyr(M_Repo_Rate, 6, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_Repo_Rate_ma9 := rollapplyr(M_Repo_Rate, 9, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_Repo_Rate_ma12 := rollapplyr(M_Repo_Rate, 12, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_Repo_Rate_ma15 := rollapplyr(M_Repo_Rate, 15, mean, partial=TRUE)]

# Inflation
dat_ClDiag_MVs[, M_Inflation_Growth_ma3 := rollapplyr(M_Inflation_Growth, 3, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_Inflation_Growth_ma6 := rollapplyr(M_Inflation_Growth, 6, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_Inflation_Growth_ma9 := rollapplyr(M_Inflation_Growth, 9, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_Inflation_Growth_ma12 := rollapplyr(M_Inflation_Growth, 12, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_Inflation_Growth_ma15 := rollapplyr(M_Inflation_Growth, 15, mean, partial=TRUE)]

# DTI
dat_ClDiag_MVs[, M_DTI_Growth_ma3 := rollapplyr(M_DTI_Growth, 3, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_DTI_Growth_ma6 := rollapplyr(M_DTI_Growth, 6, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_DTI_Growth_ma9 := rollapplyr(M_DTI_Growth, 9, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_DTI_Growth_ma12 := rollapplyr(M_DTI_Growth, 12, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_DTI_Growth_ma15 := rollapplyr(M_DTI_Growth, 15, mean, partial=TRUE)]

# Employment growth
dat_ClDiag_MVs[, M_Emp_Growth_ma3 := rollapplyr(M_Emp_Growth, 3, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_Emp_Growth_ma6 := rollapplyr(M_Emp_Growth, 6, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_Emp_Growth_ma9 := rollapplyr(M_Emp_Growth, 9, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_Emp_Growth_ma12 := rollapplyr(M_Emp_Growth, 12, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_Emp_Growth_ma15 := rollapplyr(M_Emp_Growth, 15, mean, partial=TRUE)]

# Real GDP growth
dat_ClDiag_MVs[, M_RealGDP_Growth_ma3 := rollapplyr(M_RealGDP_Growth, 3, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_RealGDP_Growth_ma6 := rollapplyr(M_RealGDP_Growth, 6, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_RealGDP_Growth_ma9 := rollapplyr(M_RealGDP_Growth, 9, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_RealGDP_Growth_ma12 := rollapplyr(M_RealGDP_Growth, 12, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_RealGDP_Growth_ma15 := rollapplyr(M_RealGDP_Growth, 15, mean, partial=TRUE)]

# Real income growth
dat_ClDiag_MVs[, M_RealIncome_Growth_ma3 := rollapplyr(M_RealIncome_Growth, 3, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_RealIncome_Growth_ma6 := rollapplyr(M_RealIncome_Growth, 6, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_RealIncome_Growth_ma9 := rollapplyr(M_RealIncome_Growth, 9, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_RealIncome_Growth_ma12 := rollapplyr(M_RealIncome_Growth, 12, mean, partial=TRUE)]
dat_ClDiag_MVs[, M_RealIncome_Growth_ma15 := rollapplyr(M_RealIncome_Growth, 15, mean, partial=TRUE)]


# --- e. Calculate ratios of the moving averages
# calculate ratios of the following:
# ma3 / ma15 --- essentially a ratio of the 12-month ma
dat_ClDiag_MVs[, M_Repo_Rate_rat12 := M_Repo_Rate_ma3 / M_Repo_Rate_ma15]
dat_ClDiag_MVs[, M_Inflation_Growth_rat12 := M_Inflation_Growth_ma3 / M_Inflation_Growth_ma15]
dat_ClDiag_MVs[, M_DTI_Growth_rat12 := M_DTI_Growth_ma3 / M_DTI_Growth_ma15]
dat_ClDiag_MVs[, M_Emp_Growth_rat12 := M_Emp_Growth_ma3 / M_Emp_Growth_ma15]
dat_ClDiag_MVs[, M_RealGDP_Growth_rat12 := M_RealGDP_Growth_ma3 / M_RealGDP_Growth_ma15]
dat_ClDiag_MVs[, M_RealIncome_Growth_rat12 := M_RealIncome_Growth_ma3 / M_RealIncome_Growth_ma15]


# --- f. Calculate the Yeo Johnson transformation on the moving averages
# use a lambda of 2 and 2.5 as experimentation
# lambda of 2
dat_ClDiag_MVs[, M_Repo_Rate_yj2 := ifelse(M_Repo_Rate_rat12 >= 0, 
                                          (((1 + M_Repo_Rate_rat12)^2) - 1)/2, 
                                          -log(-M_Repo_Rate_rat12 + 1))]

dat_ClDiag_MVs[, M_Inflation_Growth_yj2 := ifelse(M_Inflation_Growth_rat12 >= 0,
                                                  (((1 + M_Inflation_Growth_rat12)^2) - 1)/2,
                                                  -log(-M_Inflation_Growth_rat12 + 1))]

dat_ClDiag_MVs[, M_DTI_Growth_yj2 := ifelse(M_DTI_Growth_rat12 >= 0,
                                            (((1 + M_DTI_Growth_rat12)^2) - 1)/2,
                                            -log(-M_DTI_Growth_rat12 + 1))]

dat_ClDiag_MVs[, M_Emp_Growth_yj2 := ifelse(M_Emp_Growth_rat12 >= 0,
                                            (((1 + M_Emp_Growth_rat12)^2) - 1)/2,
                                            -log(-M_Emp_Growth_rat12 + 1))]

dat_ClDiag_MVs[, M_RealGDP_Growth_yj2 := ifelse(M_RealGDP_Growth_rat12 >= 0,
                                                (((1 + M_RealGDP_Growth_rat12)^2) - 1)/2,
                                                -log(-M_RealGDP_Growth_rat12 + 1))]

dat_ClDiag_MVs[, M_RealIncome_Growth_yj2 := ifelse(M_RealIncome_Growth_rat12 >= 0,
                                                   (((1 + M_RealIncome_Growth_rat12)^2) - 1)/2,
                                                   -log(-M_RealIncome_Growth_rat12 + 1))]

# lambda of 2.5
dat_ClDiag_MVs[, M_Repo_Rate_yj2_5 := ifelse(M_Repo_Rate_rat12 >= 0,
                                             (((1 + M_Repo_Rate_rat12)^2) - 1)/2,
                                             -((((1 - M_Repo_Rate_rat12)^(2-2.5))-1)/(2-2.5)))]

dat_ClDiag_MVs[, M_Inflation_Growth_yj2_5 := ifelse(M_Inflation_Growth_rat12 >= 0,
                                                    (((1 + M_Inflation_Growth_rat12)^2) - 1)/2,
                                                    -((((1 - M_Inflation_Growth_rat12)^(2-2.5))-1)/(2-2.5)))]

dat_ClDiag_MVs[, M_DTI_Growth_yj2_5 := ifelse(M_DTI_Growth_rat12 >= 0,
                                              (((1 + M_DTI_Growth_rat12)^2) - 1)/2,
                                              -((((1 - M_DTI_Growth_rat12)^(2-2.5))-1)/(2-2.5)))]

dat_ClDiag_MVs[, M_Emp_Growth_yj2_5 := ifelse(M_Emp_Growth_rat12 >= 0,
                                              (((1 + M_Emp_Growth_rat12)^2) - 1)/2,
                                              -((((1 - M_Emp_Growth_rat12)^(2-2.5))-1)/(2-2.5)))]

dat_ClDiag_MVs[, M_RealGDP_Growth_yj2_5 := ifelse(M_RealGDP_Growth_rat12 >= 0,
                                                  (((1 + M_RealGDP_Growth_rat12)^2) - 1)/2,
                                                  -((((1 - M_RealGDP_Growth_rat12)^(2-2.5))-1)/(2-2.5)))]

dat_ClDiag_MVs[, M_RealIncome_Growth_yj2_5 := ifelse(M_RealIncome_Growth_rat12 >= 0,
                                                     (((1 + M_RealIncome_Growth_rat12)^2) - 1)/2,
                                                     -((((1 - M_RealIncome_Growth_rat12)^(2-2.5))-1)/(2-2.5)))]


# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "dat_ClDiag_MVs"), dat_ClDiag_MVs); gc()





