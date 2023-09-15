# =============================== DATA FUSION - EXCLUDING DEFAULT SPELLS ===============================
# Creating a new dataset, used ONLY for analytics, which by excluding default spells from the xXx
# dataset.
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Default survival modelling
# SCRIPT AUTHOR(S): Marcel Muller
# ------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup
#   - 2d.Data_Fusion

# -- Inputs:
#   - datCredit_real | Prepared from scipt 2d.
#
# -- Outputs:
#   - Some graphs to explore the accuracy of different resampling schemes.
# ------------------------------------------------------------------------------------------------------

# AB (2023-09-12):  The below is just copied from other project, not massaged into context yet. 


# --- Load in Dataset
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4d"), tempPath)

# --- Applying the exclusions
datCredit_real <- subset(datCredit_real, !is.na(PerfSpell_Num))

# --- [SANITY CHECK]
# - Creating checks
sum_defaults <- sum(is.na(datCredit_real$PerfSpell_Num) > 0)
check.fuse1 <- sum_defaults == 0

# - Reporting
cat(check.fuse1 %?% 'SAFE: All default spells removed, fusion successfull.' %:%
      'WARNING: Default spells detected, fusion not successfull.')

# Conditional reporting
if (check.fuse1 == 0) {
  cat('NOTE: ', check.fuse1, 'observations from default spells detected',"\n",sep="\t")
}

# --- Saving endpoint
pack.ffdf(paste0(genPath,"creditdata_analytics"), datCredit_real)

