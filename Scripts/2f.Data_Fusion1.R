# ===================================== DATA FUSION =====================================
# Basic fusion between credit dataset and macroeconomic dataset
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha, Esmerelda Oberholzer

# DESCRIPTION:
# This script performs the following high-level tasks:
#   1) Removes a few variables that are unlikely to be useful within the context of 
#      analysing/modelling of default risk.
#   2) Fuses macroeconomic data unto the main credit dataset
#   3) Creates a preliminary target/outcome variable for modelling default risk
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2f.Data_Fusion1.R
#
# -- Inputs:
#   - datCredit_real | Prepared credit data from script 2d
#   - datMV | Prepared macroeconomic dataset from script 2e
#
# -- Outputs:
#   - datCredit_allBasic | enriched credit dataset, fused with various input fields
# =======================================================================================




# ------- 1. Apply exclusions on the credit dataset to increase available memory

ptm <- proc.time() # for runtime calculations (ignore)

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final3"), tempPath)
if (!exists('datMV')) unpack.ffdf(paste0(genPath,"datMV"), tempPath)

# - Check the impact of the exclusions from script 2d | RECORD-LEVEL
(exclusions_credit <- datCredit_real[ExclusionID != 0, .N] / datCredit_real[, .N] * 100)
# Exclusion's impact: 6.0530%

# - Check the combined impact for possible overlaps | RECORD-LEVEL
(exclusions_all <- datCredit_real[ExclusionID != 0 | is.na(PerfSpell_Num), .N] / datCredit_real[, .N] * 100)
# Total exclusions' impact: 10.6505%

# - Now apply the exclusions (but keep the default exposures)
datCredit_real <- subset(datCredit_real, ExclusionID == 0); gc()

# - Checks
sum(datCredit_real$ExclusionID > 0) == 0 # check - success

# - Remove unnecessary variables
datCredit_real <- subset(datCredit_real, select = -c(ExclusionID))



# ------- 2. Fuse the macroeconomic data with the credit data

# - Find intersection between fields in the credit dataset and the macroeconomic dataset
(overlap_flds <- intersect(colnames(datCredit_real), colnames(datMV))) # no overlapping fields except Date

# - Remove fields that will not likely be used in the eventual analysis/modelling of default risk, purely to save memory
names(datCredit_real)
# AB: Commented fields marked for deletion. I strongly suspect these will be useful in exploratory analysis, if not modelling
# Further, we'll optimise for memory via the subsampled resampling scheme later
# Unless we need to discuss, the uncommented parts can be grouped together neatly while deleting the commented fields
datCredit_real <- subset(datCredit_real, 
                         select = -c(Age, #PerfSpell_Key, New_Ind, Max_Counter, Date_Origination, Principal,
                                     AccountStatus, #Instalment, Arrears, 
                                     DelinqState_g0, #DefaultStatus1, DefSpell_Num, TimeInDefSpell,
                                     DefSpell_LeftTrunc, DefSpell_Event, DefSpell_Censored,
                                     DefSpellResol_TimeEnd, #DefSpell_Age, DefSpellResol_Type_Hist,
                                     HasLeftTruncPerfSpell, DefSpell_LastStart, ReceiptPV, LossRate_Real,
                                     PerfSpell_LeftTrunc, PerfSpell_Event, PerfSpell_Censored,
                                     PerfSpell_TimeEnd, #PerfSpellResol_Type_Hist,
                                     HasLeftTruncDefSpell, Account_Censored, #Event_Time, Event_Type,
                                     HasTrailingZeroBalances, ZeroBal_Start, NCA_CODE, STAT_CDE, LN_TPE,
                                     #DefSpell_Key, DefSpell_Counter, PerfSpell_Counter,
                                     HasWOff, WriteOff_Amt, HasSettle, EarlySettle_Amt, HasFurtherLoan, HasRedraw,
                                     HasClosure, CLS_STAMP, Curing_Ind, BOND_IND, Undrawn_Amt, # TreatmentID,
                                     slc_past_due_amt, WOff_Ind, EarlySettle_Ind, #PerfSpell_Num, PerfSpell_Age,
                                     FurtherLoan_Amt, FurtherLoan_Ind, Redrawn_Ind, Redrawn_Amt, Repaid_Ind, HasRepaid)); gc()

# - Merge on Date by performing a left-join
datCredit_real <- merge(datCredit_real, datMV, by="Date", all.x=T); gc()

# - Create Interest Rate margin using the repo rate + 3.5% (Prime Rate's definition in South Africa)
datCredit_real <- datCredit_real %>% mutate(InterestRate_Margin = round(InterestRate_Nom - (M_Repo_Rate+0.035), digits=4)) %>%
  relocate(InterestRate_Margin, .after=InterestRate_Nom)

# - Validate merging success by checking for missingness (should be zero)
list_merge_variables <- list(colnames(datMV))
results_missingness <- list()
for (i in 1:length(list_merge_variables)){
  output <- sum(is.na(datCredit_real$list_merge_variables[i]))
  results_missingness[[i]] <- output
}
length(which(results_missingness > 0)) == 0 # confirmed, no missing values

# - Clean-up
rm(datMV, list_merge_variables, results_missingness); gc()



# ------- 3. Create preliminary target/outcome variables for stated modelling objective

# - Creating 12-month default indicators using the worst-ever approach
# NOTE: This step deliberately spans both performing and default spells
# Note: Need to specify a (k+1)-window for the "frollapply()" function, e.g., a 12-month outcome implies 13
# Uses the custom function "imputLastKnown" defined in script 0
datCredit_real[, DefaultStatus1_lead_12_max := imputeLastKnown(frollapply(x=DefaultStatus1, n=13, align="left", FUN=max)), by=list(LoanID)]
datCredit_real$DefaultStatus1_lead_12_max %>% table() %>% prop.table() 
### RESULTS: 92.04% of observations had not defaulted in the next 12 months from reporting date, whilst 7.96% of accounts had.

# - Relocate variable next to current default status variable
datCredit_real <- datCredit_real %>% relocate(DefaultStatus1_lead_12_max, .after=DefaultStatus1)




# ------- 4. Pack objects to disk

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "creditdata_final4a"), datCredit_real); gc()
proc.time() - ptm # IGNORE: elapsed runtime
