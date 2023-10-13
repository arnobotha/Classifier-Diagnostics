# ===================================== DATA FUSION =====================================
# Fuse the prepared and previously enriched credit dataset with a separate input space
# dataset that contains several fields that may prove predictive, as well as with
# prepared macroeconomic data
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha, Esmerelda Oberholzer

# DESCRIPTION:
# This script joins the previously prepared credit data with the macroeconomic data to
# provide one dataset that can be used for model development
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#
# -- Inputs:
#   - datCredit_real | Prepared credit data from script 2d.Data_Enrich
#   - dat_ClDiag_MVs | Prepared macroeconomic dataset from script 2e.Data_Enrich_MV
#
# -- Outputs:
#   - datCredit_allInputs | enriched credit dataset, fused with various input fields
# =======================================================================================


# AB (2023-09-12): Questionable, given that script 2f already performs much (if not all) of this.


# ------- 1. Fuse the macroeconomic data with the credit data

ptm <- proc.time() # for runtime calculations (ignore)

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final3"), tempPath)
if (!exists('dat_ClDiag_MVs')) unpack.ffdf(paste0(genPath,"dat_ClDiag_MVs"), tempPath)

# - Find intersection between fields in the credit dataset and the macroeconomic dataset
(overlap_flds <- intersect(colnames(datCredit_real), colnames(dat_ClDiag_MVs))) # no overlapping fields except Date

# - Remove fields that will not likely be used to save memory
names(datCredit_real)
datCredit_real <- subset(datCredit_real, 
                         select = -c(PerfSpell_Key, Age, New_Ind, Max_Counter, Date_Origination, Principal,
                                     Instalment, Arrears, AccountStatus, 
                                     DelinqState_g0, DefaultStatus1, DefSpell_Num, TimeInDefSpell,
                                     DefSpell_LeftTrunc, DefSpell_Event, DefSpell_Censored,
                                     DefSpellResol_TimeEnd, DefSpell_Age, DefSpellResol_Type_Hist, 
                                     HasLeftTruncPerfSpell, DefSpell_LastStart, ReceiptPV, LossRate_Real,
                                     PerfSpell_LeftTrunc, PerfSpell_Event, PerfSpell_Censored,
                                     PerfSpell_TimeEnd, PerfSpellResol_Type_Hist,
                                     HasLeftTruncDefSpell, Event_Time, Event_Type, Account_Censored,
                                     HasTrailingZeroBalances, ZeroBal_Start, NCA_CODE, STAT_CDE,
                                     DefSpell_Key, DelinqState_g0, DefSpell_Counter, PerfSpell_Counter,
                                     HasWOff, WriteOff_Amt, HasSettle, EarlySettle_Amt, HasFurtherLoan, HasRedraw,
                                     HasClosure, CLS_STAMP, TreatmentID, Curing_Ind, BOND_IND, Undrawn_Amt,
                                     slc_past_due_amt, WOff_Ind, EarlySettle_Ind, PerfSpell_Num, PerfSpell_Age,
                                     FurtherLoan_Amt, Redrawn_Amt, Repaid_Ind, HasRepaid)); gc()

# - Merge on Date by performing a left-join
datCredit_real <- merge(datCredit_real, dat_ClDiag_MVs, by="Date", all.x=T); gc()

# - Create Interest Rate margin using the repo rate + 3.5% (Prime Rate's definition in South Africa)
datCredit_real <- datCredit_real %>% mutate(InterestRate_Margin = round(InterestRate_Nom - (M_Repo_Rate+0.035), digits=4)) %>%
  relocate(InterestRate_Margin, .after=InterestRate_Nom)

# - Validate merging success by checking for missingness (should be zero)
list_merge_variables <- list(colnames(dat_ClDiag_MVs))
results_missingness <- list()
for (i in 1:length(list_merge_variables)){
  output <- sum(is.na(datCredit_real$list_merge_variables[i]))
  results_missingness[[i]] <- output
}
length(which(results_missingness > 0)) == 0 # confirmed, no missing values

# - Clean-up
rm(dat_ClDiag_MVs, list_merge_variables, results_missingness); gc()





# ------- 2. Pack objects to disk

# - Save to disk (zip) for quick disk-based retrieval later
datCredit_allInputs <- datCredit_real; rm(datCredit_real); gc() # rename object to preserve parity with modelling scripts
pack.ffdf(paste0(genPath, "creditdata_allinputs"), datCredit_allInputs)
gc()

