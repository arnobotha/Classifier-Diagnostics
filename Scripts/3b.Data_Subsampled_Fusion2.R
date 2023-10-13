# ========================== SUBSAMPLED RESAMPLING & DATA FUSION FOR PD MODELS =========================
# Subsampling and resampling data prior to fusing the input space towards PD-modelling
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha, Esmerelda Oberholzer

# DESCRIPTION:
# This script performs the following high-level tasks:
#   1) Subsample main dataset into a more manageable but still representative dataset
#   2) Fuse subsampled set with input space
#   3) Screen input space against missingness and apply appropriate treatments
#   4) Perform feature engineering (transforms, ratio-type)
# ------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2f.Data_Fusion1.R

# -- Inputs:
#   - datCredit_real | Prepared from script 2f.
#
# -- Outputs:
#   - datCredit_smp | Subsampled set, fused with input space, duly prepared
# ------------------------------------------------------------------------------------------------------



# ------ 1. Preliminaries

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4a"), tempPath)
if (!exists('datExclusions')) unpack.ffdf(paste0(genObjPath,"Exclusions-TruEnd-Enriched"), tempPath)

# - Confidence interval parameter
confLevel <- 0.95

# - Field names
stratifiers <- c("DefaultStatus1_lead_12_max", "Date") # Must at least include target variable used in graphing event rate
targetVar <- "DefaultStatus1_lead_12_max"
currStatusVar <- "DefaultStatus1"
timeVar <- "Date"

# - Subsampling & resampling parameters
smp_size <- 1500000 # fixed size of downsampled set
train_prop <- 0.7 # sampling fraction for resampling scheme


# --- Exclusions: NAs in provided stratifiers

# [DIAGNOSTIC] Account-level and dataset-wide impacts of excluding NAs in target variable,
# purely due to incomplete outcome window
diag.real_subsamp_5a <- datCredit_real[!complete.cases(mget(stratifiers)) & Counter==1, .N] / 
  datCredit_real[Counter==1, .N] * 100
diag.real_subsamp_5a_abs <- datCredit_real[!complete.cases(mget(stratifiers)), .N]
diag.real_subsamp_5a_rec <-  diag.real_subsamp_5a_abs / datCredit_real[, .N] * 100 
# - Conditional exclusion
if (diag.real_subsamp_5a > 0) {
  
  cat("EXCLUSION: Missingness detected in target variable. Prevalence: ", round(diag.real_subsamp_5a,digits=1), "% of accounts (",
      round(diag.real_subsamp_5a_rec,digits=1), "% of records).\n")
  
  # - Apply Exclusion (not necessary to mark within dataset any more, given advanced stage of data process at this point)
  # NOTE: Double conversion necessary as annoying fix; see https://github.com/Rdatatable/data.table/issues/3745
  datCredit_real <- datCredit_real %>% drop_na(all_of(stratifiers)) %>% as_tibble() %>% as.data.table()
  
  # [SANITY CHECK] Treatment success?
  check_excl5a <- datCredit_real[!complete.cases(mget(stratifiers)) & Counter == 1, .N] == 0
  cat( check_excl5a %?% 'SAFE: Exclusion successfully applied.\n' %:% 
         'WARNING: Applying Exclusion failed.\n')
  
  # - Measure impact of exclusion using prevalence rate (Default proportion)
  classPrior_remainRecs <- datCredit_real[, sum(get(currStatusVar))] / datCredit_real[,.N]
  classPrior_Remain_Diff <- diff(c( as.numeric(sub("%", "", datExclusions[.N, ClassPrior_Remain]))/100, classPrior_remainRecs))
  
  # - Create and add exclusion impact to a common table
  datExcl <- data.table("Excl_ID"=8, "Reason"="Missingness in status-field (incomplete outcome window)",
                        "Impact_Account" = diag.real_subsamp_5a, "Impact_Dataset" = diag.real_subsamp_5a_rec,
                        "Impact_records" = diag.real_subsamp_5a_abs, "Records_Remain" = datCredit_real[,.N],
                        "Impact_Dataset_Cumul" = percent(diag.real_subsamp_5a_rec/100, accuracy=0.001), 
                        "ClassPrior_Remain" = percent(classPrior_remainRecs, accuracy=0.001),
                        "classPrior_Remain_Diff" = percent(classPrior_Remain_Diff, accuracy=0.001))
  datExclusions <- rbind(datExclusions, datExcl)
  
  # - Store experimental objects | Memory optimisation
  pack.ffdf(paste0(genObjPath,"Exclusions-TruEnd-Enriched2"), datExclusions);
}






# ------ 2. Subsampled resampling scheme with 2-way stratified random sampling

# - Preliminaries
smp_perc <- smp_size / ( datCredit_real[complete.cases(mget(stratifiers)), mget(stratifiers)][,.N] ) # Implied sampling fraction for downsampling step

# - Downsample data into a set with a fixed size (using stratified sampling) before implementing resampling scheme
set.seed(1)
datCredit_smp <- datCredit_real %>% drop_na(all_of(stratifiers)) %>% group_by(across(all_of(stratifiers))) %>% slice_sample(prop=smp_perc) %>% as.data.table()
cat( (datCredit_smp[is.na(get(targetVar)), .N] == 0) %?% 'SAFE: No missingness in target variable.\n' %:% 
       'WARNING: Missingness detected in target variable.\n')
rm(datCredit_real); gc()

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "creditdata_final4b"), datCredit_smp); gc()





# ------ 3. Fuse the input space with the subsampled prepared dataset

# --- Load in main dataset (subsampled)
if (!exists('datCredit_smp')) unpack.ffdf(paste0(genPath,"creditdata_final4b"), tempPath)

# - Confirm if the input space data is loaded into memory
if (!exists('datInput.raw')) unpack.ffdf(paste0(genPath,"creditdata_input1"), tempPath)

# - Find intersection between fields in input space and those perhaps already in the main credit dataset
(overlap_flds <- intersect(colnames(datCredit_smp), colnames(datInput.raw))) # no overlapping fields

# - Remove any additional variables that are not going to be used
suppressWarnings( datInput.raw[, `:=`(slc_status_final_pred7 = NULL, slc_status_final = NULL, 
                                      slc_curing_ind = NULL, datex = NULL)])

# - Format the date in the correct format for merging
datInput.raw[, date := as.Date(date, format="%Y-%m-%d")]

# - Rename the datasets for merging
colnames(datInput.raw)[colnames(datInput.raw) %in% c("date", "acct_no")] <- c("Date", "LoanID")

# - [SANITY CHECK] Check the data grain
check_input1a <- datInput.raw[, list(Freq = .N), by=list(LoanID, Date)][Freq>1, .N]
cat( (check_input1a == 0) %?% 'SAFE: Grain of {datInput.rawdatInput.raw} confirmed.\n' %:% 
       paste0('WARNING: Grain broken in {datInput.raw} for ', check_input1a, " cases.\n") )

# - Merge on LoanID and Date by performing a left-join
datCredit_smp <- merge(datCredit_smp, datInput.raw, by=c("Date", "LoanID"), all.x=T); gc()

# [SANITY CHECK] Confirm dataset's grain after fusion
check_fuse5a <- datCredit_smp[,list(Freqs = .N), by=list(LoanID, Date)][Freqs > 1,.N]
cat( (check_fuse5a == 0) %?% 'SAFE: Grain of {datCredit_smp} confirmed after fusion.\n' %:% 
       paste0('ERROR: Grain broken in {datCredit_smp} after fusion for ', check_fuse5a, " cases.\n") )

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "creditdata_final4c"), datCredit_smp); gc()

# - Clean-up
rm(datInput.raw, data_grain_check); gc()




# ------- 4. Feature engineering for modelling purposes

# - Load in main dataset (subsampled)
if (!exists('datCredit_smp')) unpack.ffdf(paste0(genPath,"creditdata_final4c"), tempPath)


# --- 1. Missing value indicators for the input space variables
# NOTE: There are a lot of missing values for these variables because of system changes etc.
datCredit_smp[, value_ind_slc_pmnt_method := ifelse(is.na(slc_pmnt_method) | slc_pmnt_method == "", 1, 0)]
datCredit_smp[, value_ind_slc_days_excess := ifelse(is.na(slc_days_excess) | slc_days_excess == "", 1, 0)]
datCredit_smp[, value_ind_slc_acct_pre_lim_perc := ifelse(is.na(slc_acct_pre_lim_perc) | slc_acct_pre_lim_perc == "", 1, 0)]
datCredit_smp[, value_ind_slc_acct_roll_ever_24 := ifelse(is.na(slc_acct_roll_ever_24) | slc_acct_roll_ever_24 == "", 1, 0)]
datCredit_smp[, value_ind_slc_acct_arr_dir_3 := ifelse(is.na(slc_acct_arr_dir_3) | slc_acct_arr_dir_3 == "", 1, 0)]
datCredit_smp[, value_ind_slc_acct_prepaid_perc_dir_12 := ifelse(is.na(slc_acct_prepaid_perc_dir_12) | slc_acct_prepaid_perc_dir_12 == "", 1, 0)]

# - Check the missingness of the variables
# If they are more than 50% missing - remove
table(datCredit_smp$value_ind_slc_pmnt_method) %>% prop.table()              # missingness: 10.78% - keep the variable (categorical)
table(datCredit_smp$value_ind_slc_days_excess) %>% prop.table()              # missingness: 74.09% - discard the variable
table(datCredit_smp$value_ind_slc_acct_pre_lim_perc) %>% prop.table()        # missingness: 10.78% - keep the variable (numeric) 
table(datCredit_smp$value_ind_slc_acct_roll_ever_24) %>% prop.table()        # missingness: 10.79% - keep the variable (numeric + delinquency theme)     
table(datCredit_smp$value_ind_slc_acct_arr_dir_3) %>% prop.table()           # missingness: 10.78% - keep the variable (categorical + delinquency theme)        
table(datCredit_smp$value_ind_slc_acct_prepaid_perc_dir_12) %>% prop.table() # missingness: 10.78% - keep the variable (numeric)

# - Remove the variables that have missingness > 50%
suppressWarnings( datCredit_smp[, `:=`(value_ind_slc_days_excess = NULL, slc_days_excess = NULL)]); gc()



# --- 2. Missing value treatment (categorical variables)
# Treatment: create "missing"-bin for all N/A values

# - Payment method
# Merge with existing "Unknown" bin or empty values
datCredit_smp[, slc_pmnt_method := 
                 ifelse(is.na(slc_pmnt_method) | slc_pmnt_method == "" | slc_pmnt_method == "Unknown",
                        "MISSING_DATA", slc_pmnt_method)]
# [SANITY CHECK] Confirm treatment success
cat( (sum(datCredit_smp$slc_pmnt_method == "" | is.na(datCredit_smp$slc_pmnt_method) | 
            datCredit_smp$slc_pmnt_method == "Unknown") == 0) %?% 
       'SAFE: Treatment successful for [slc_pmnt_method].\n' %:% 'ERROR: Treatment failed for [slc_pmnt_method] \n' )
describe(datCredit_smp$slc_pmnt_method)


# - Account-level arrears direction vs three months ago
# Merge with existing "N/A" bin or empty values
datCredit_smp[, slc_acct_arr_dir_3 := 
                 ifelse(is.na(slc_acct_arr_dir_3) | slc_acct_arr_dir_3 == "" | slc_acct_arr_dir_3 == "N/A", 
                        "MISSING_DATA", slc_acct_arr_dir_3)]
# [SANITY CHECK] Confirm treatment success
cat( ( sum(datCredit_smp$slc_acct_arr_dir_3 == "" | is.na(datCredit_smp$slc_acct_arr_dir_3) |
             datCredit_smp$slc_acct_arr_dir_3 == "N/A") == 0) %?% 
       'SAFE: Treatment successful for [slc_acct_arr_dir_3].\n' %:% 'ERROR: Treatment failed for [slc_acct_arr_dir_3] \n' )
describe(datCredit_smp$slc_acct_arr_dir_3)



# --- 3. Missing value treatment (numeric variables)
# Analyse whether to use mean or median value imputation

# - Prepaid/available funds to limit
describe(datCredit_smp$slc_acct_pre_lim_perc); hist(datCredit_smp$slc_acct_pre_lim_perc)
datCredit_smp[is.na(slc_acct_pre_lim_perc), .N] / datCredit_smp[,.N] * 100
### RESULTS: Highly right-skewed distribution, with mean of ~0.09 vs median of 0,
# bounded by [0, 0.79] for 5%-95% percentiles; no outliers
# Use median imputation, given 10.78% missingness degree, trading off the minor distributional distortion as a result
datCredit_smp[, slc_acct_pre_lim_perc_imputed := 
                 ifelse(is.na(slc_acct_pre_lim_perc) | slc_acct_pre_lim_perc == "", 
                        median(slc_acct_pre_lim_perc, na.rm=TRUE), slc_acct_pre_lim_perc)]
# [SANITY CHECK] Confirm treatment success
cat( ( datCredit_smp[is.na(slc_acct_pre_lim_perc_imputed), .N ] == 0) %?% 
       'SAFE: Treatment successful for [slc_acct_pre_lim_perc_imputed].\n' %:% 
       'ERROR: Treatment failed for [slc_acct_pre_lim_perc_imputed] \n' )
describe(datCredit_smp$slc_acct_pre_lim_perc_imputed); hist(datCredit_smp$slc_acct_pre_lim_perc_imputed)


# - Number of times an account was in arrears over last 24 months
describe(datCredit_smp$slc_acct_roll_ever_24); hist(datCredit_smp$slc_acct_roll_ever_24)
datCredit_smp[is.na(slc_acct_roll_ever_24), .N] / datCredit_smp[,.N] * 100
### RESULTS: Highly right-skewed distribution with mean of 0.4871, though discrete values with 80% of data having 0-value.
# Use mean imputation, given 10.79% missingness degree, trading off the minor distributional distortion as a result
datCredit_smp[, slc_acct_roll_ever_24_imputed := 
                 ifelse(is.na(slc_acct_roll_ever_24) | slc_acct_roll_ever_24 == "", 
                        mean(slc_acct_roll_ever_24, na.rm=TRUE), slc_acct_roll_ever_24)]
# [SANITY CHECK] Confirm treatment success
cat( ( datCredit_smp[is.na(slc_acct_roll_ever_24_imputed), .N ] == 0) %?% 
       'SAFE: Treatment successful for [slc_acct_roll_ever_24_imputed].\n' %:% 
       'ERROR: Treatment failed for [slc_acct_roll_ever_24_imputed] \n' )
describe(datCredit_smp$slc_acct_roll_ever_24_imputed); hist(datCredit_smp$slc_acct_roll_ever_24_imputed)


# - Percentage-valued direction of prepaid/available funds - current compared to 12 months ago
describe(datCredit_smp$slc_acct_prepaid_perc_dir_12); hist(datCredit_smp[slc_acct_prepaid_perc_dir_12<=5, slc_acct_prepaid_perc_dir_12])
datCredit_smp[is.na(slc_acct_prepaid_perc_dir_12), .N] / datCredit_smp[,.N] * 100
### RESULTS: Highly right-skewed distribution, with mean of ~1.6m vs median of 0, 
# bounded by [0, 3.34] for 5%-95% percentiles; some very large outliers
### AB: Scope for extreme value treatment if those outliers are correct; or use winsorized mean (Std-PrinciplesForDataPrep)
# Use median imputation, given 10.78% missingness degree, trading off the minor distributional distortion as a result
datCredit_smp[, slc_acct_prepaid_perc_dir_12_imputed := 
                 ifelse(is.na(slc_acct_prepaid_perc_dir_12) | slc_acct_prepaid_perc_dir_12 == "", 
                        median(slc_acct_prepaid_perc_dir_12, na.rm=TRUE), slc_acct_prepaid_perc_dir_12)]
# [SANITY CHECK] Confirm treatment success
cat( ( datCredit_smp[is.na(slc_acct_prepaid_perc_dir_12_imputed), .N] == 0) %?% 
       'SAFE: Treatment successful for [slc_acct_prepaid_perc_dir_12_imputed].\n' %:% 
       'ERROR: Treatment failed for [slc_acct_prepaid_perc_dir_12_imputed] \n' )
describe(datCredit_smp$slc_acct_prepaid_perc_dir_12_imputed); hist(datCredit_smp[slc_acct_prepaid_perc_dir_12_imputed<=5, slc_acct_prepaid_perc_dir_12_imputed])


# - InterestRate_Margin (incorporating risk-based pricing info)
describe(datCredit_smp$InterestRate_Margin); hist(datCredit_smp$InterestRate_Margin, breaks="FD")
datCredit_smp[is.na(InterestRate_Margin), .N] / datCredit_smp[,.N] * 100
### RESULTS: Highly right-skewed distribution (as expected), with mean of -0.007 vs median of -0.008, 
# bounded by [-0.02, 0.01] for 5%-95% percentiles; some negative outliers distort shape of distribution
# Use median imputation, given 0.46% missingness degree
datCredit_smp[, InterestRate_Margin_imputed := 
                ifelse(is.na(InterestRate_Margin) | InterestRate_Margin == "", 
                       median(InterestRate_Margin, na.rm=TRUE), InterestRate_Margin)]
# [SANITY CHECK] Confirm treatment success
cat( ( datCredit_smp[is.na(InterestRate_Margin_imputed), .N] == 0) %?% 
       'SAFE: Treatment successful for [InterestRate_Margin_imputed].\n' %:% 
       'ERROR: Treatment failed for [InterestRate_Margin_imputed] \n' )
describe(datCredit_smp$InterestRate_Margin_imputed); hist(datCredit_smp$InterestRate_Margin_imputed, breaks="FD")



# --- 4. Feature Engineering: Exploring transformations

# - Log-transform of Balance
describe(datCredit_smp$Balance); hist(datCredit_smp$Balance, breaks="FD")
datCredit_smp[, BalanceLog := ifelse(Balance > 0, log(Balance), log(1))]
# [SANITY CHECK] Check new feature for illogical values
cat( ( datCredit_smp[is.na(BalanceLog), .N] == 0) %?% 
       'SAFE: New feature [BalanceLog] has logical values.\n' %:% 
       'WARNING: New feature [BalanceLog] has illogical values \n' )
describe(datCredit_smp$BalanceLog); hist(datCredit_smp$BalanceLog, breaks="FD")
### RESULTS: Large spike at zero values with some negative outliers, which skews mean to 11.8 (median: 12.8)
# However, majority of distribution's bulk is left-skewed between 5 and 15, aside from the large spike at 0
### AB: Scope to use Yeo-Johnson transform from Marcel's work (Std-PrinciplesForDataPrep)


# - YJ-transform of InterestRate_Margin
### AB: Will need some help here. Consult with Marcel


# --- 5. Feature Engineering: ratio-type variables

# - Loan age to loan term
datCredit_smp[, AgeToTerm := Age_Adj/Term] # where the loan is in its lifetime
# [SANITY CHECK] Check new feature for illogical values
cat( ( datCredit_smp[is.na(AgeToTerm), .N] == 0) %?% 
       'SAFE: New feature [AgeToTerm] has logical values.\n' %:% 
       'WARNING: New feature [AgeToTerm] has illogical values \n' )
describe(datCredit_smp$AgeToTerm); hist(datCredit_smp[AgeToTerm<2, AgeToTerm])
### RESULTS: Highly right-skewed distribution as expected, with mean of 0.37 vs median of 0.29,
# bounded by [0.02917, 0.9] for 5%-95% percentiles; some large outliers (max: 198)


# - Balance to loan term | how much is still outstanding compared to Principal/Limit
datCredit_smp[, BalanceToPrincipal := Balance/Principal]
# [SANITY CHECK] Check new feature for illogical values
cat( ( datCredit_smp[is.na(BalanceToPrincipal), .N] == 0) %?% 
       'SAFE: New feature [BalanceToPrincipal] has logical values.\n' %:% 
       'WARNING: New feature [BalanceToPrincipal] has illogical values \n' )
# distributional analysis
describe(datCredit_smp$BalanceToPrincipal); hist(datCredit_smp$BalanceToPrincipal)
### RESULTS: Highly left-skewed distribution, with mean of 0.6967 vs median of 0.8464,
# bounded by [~0.000, 0.997] for 5%-95% percentiles; no outliers



# --- 6. Featuring Engineering: General

# - Condense the payment group
datCredit_smp[, pmnt_method_grp := 
                case_when(slc_pmnt_method == "Debit Order FNB account" | slc_pmnt_method == "Debit Order other bank" ~ "Debit Order",
                          slc_pmnt_method == "Salary" | slc_pmnt_method == "Suspense" ~ "Salary/Suspense",
                          TRUE ~ slc_pmnt_method)]
# [SANITY CHECK] Check new feature for illogical values
cat( ( datCredit_smp[is.na(pmnt_method_grp), .N] == 0) %?% 
       'SAFE: New feature [pmnt_method_grp] has logical values.\n' %:% 
       'WARNING: New feature [pmnt_method_grp] has illogical values \n' )
describe(datCredit_smp$pmnt_method_grp)
### RESULTS: Bins grouped logically such that each bin now has sufficient observations


### AB: Note: Consider discretising interestrate_margin with portfolio-level view



# ------ 5. Apply basic cross-validation resampling scheme with 2-way stratified sampling
datCredit_smp[, Ind := 1:.N] # prepare for resampling scheme

# - Implement resampling scheme using given main sampling fraction
set.seed(1)
datCredit_train <- datCredit_smp %>% group_by(across(all_of(stratifiers))) %>% slice_sample(prop=train_prop) %>% as.data.table()
datCredit_valid <- subset(datCredit_smp, !(Ind %in% datCredit_train$Ind)) %>% as.data.table()

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "creditdata_train"), datCredit_train); gc()
pack.ffdf(paste0(genPath, "creditdata_valid"), datCredit_valid); gc()

