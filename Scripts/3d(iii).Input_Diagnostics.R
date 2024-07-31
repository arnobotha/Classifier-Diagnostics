# ========================== MODEL DEFAULT RISK - INPUT DIAGNOSTICS ========================================
# This script performs input diagnostics on the three logit models. Mainly it assessess the variable importance
# of the three models using z-scores and standardised coefficients.
# -----------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Marcel Muller

# DESCRIPTION:
# This script uses the previously selected variables in fitting different logit models according to their
# level of complexity. The variable importance of these variables are then assessed.
# -----------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 0a.CustomFunctions.R
#   - 3b.Data_Subsample_Fusion2
#   - 3c(i).Model_DefaultRisk_Basic
#   - 3c(ii).Model_DefaultRisk_Intermediate
#   - 3c(iii).Model_DefaultRisk_Advanced

# -- Inputs:
#   - datCredit_train | Prepared credit data from script 3b
#   - datCredit_valid | Prepared credit data from script 3b
#   - Final model input variables
# -- Outputs:
#   - Some graphs showcasing variable importance
# ===========================================================================================================

# ------ 1. Preliminaries
ptm <- proc.time() # for runtime calculations (ignore)

# - Graphing Parameters
chosenFont <- "Cambria"
dpi <- 180

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)
if (!exists('datCredit_smp')) unpack.ffdf(paste0(genPath,"creditdata_smp"), tempPath)

# - Subset to exclude default spells
datCredit_train <- datCredit_train %>% subset(DefaultStatus1==0)
datCredit_valid <- datCredit_valid %>% subset(DefaultStatus1==0)
datCredit_smp <- datCredit_smp %>% subset(DefaultStatus1==0)

# - Load in basic, intermediate, and advanced model formulas
# Basic model
unpack.ffdf(paste0(genObjPath, "Basic_Com_Formula"), tempPath)
# Intermediate model
unpack.ffdf(paste0(genObjPath, "Int_Formula"), tempPath)
# Advanced model
unpack.ffdf(paste0(genObjPath, "Adv_Formula"), tempPath)




# ------ 2. Variable importance assessment
# --- 2.1 Fitting the models
# - Basic model
logitMod_Basic <- glm(inputs_bas, data=datCredit_train, family="binomial")

# - Intermediate model
logitMod_Int <- glm(inputs_int, data=datCredit_train, family="binomial")

# - Advanced model
logitMod_Adv <- glm(inputs_adv, data=datCredit_train, family="binomial")


# --- 2.2 Variable importance
# - Standardised Coefficients using Z-Scores (i.e., standardising the input space and fitting the model)
# Basic model
(varImp_Bas_ZScore <- varImport_logit(logitMod_Basic, method="stdCoef_ZScores", impPlot=T, pd_plot=T, plotName=paste0(genFigPath,"VariableImportance_ZScore_Bas.png")))
# Intermediate model
(varImp_Int_ZScore <- varImport_logit(logitMod_Int, method="stdCoef_ZScores", impPlot=T, pd_plot=T, plotName=paste0(genFigPath,"VariableImportance_ZScore_Int.png")))
# Advanced model
(varImp_Adv_ZScore <- varImport_logit(logitMod_Adv, method="stdCoef_ZScores", impPlot=T, pd_plot=T, plotName=paste0(genFigPath,"VariableImportance_ZScore_Adv.png")))

# - Standardised Coefficients using Menard's Method (i.e., standardising the coefficients of the model)
# Basic model
(varImp_Bas_Menard <- varImport_logit(logitMod_Basic, method="stdCoef_Menard", impPlot=T, pd_plot=T, plotName=paste0(genFigPath,"VariableImportance_Menard_Bas.png")))
# Intermediate model
(varImp_Int_Menard <- varImport_logit(logitMod_Int, method="stdCoef_Menard", impPlot=T, pd_plot=T, plotName=paste0(genFigPath,"VariableImportance_Menard_Int.png")))
# Advanced model
(varImp_Adv_Menard <- varImport_logit(logitMod_Adv, method="stdCoef_Menard", impPlot=T, pd_plot=T, plotName=paste0(genFigPath,"VariableImportance_Menard_Adv.png")))


