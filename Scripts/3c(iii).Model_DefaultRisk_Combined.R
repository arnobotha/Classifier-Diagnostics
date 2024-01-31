# ========================= MODEL DEFAULT RISK - COMBINED ===============================
# Develop several "combined" logistic regression models ranging from models with few
# features to models with more features to predict default risk.
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha, Esmerelda Oberholzer

# DESCRIPTION:
# This script uses the previously prepared credit dataset fused with macroeconomic
# variables to create multiple logistic regression models for default. The focus of this
# script is combining the selected features from the "basic" varaible selection script
# and the "macroeconomic" script.
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 0a.CustomFunctions.R
#
# -- Inputs:
#   - datCredit_real | Prepared credit data from script 2f
#
# -- Outputs:
#   - 
# =======================================================================================




# ------ 1. Preliminaries

ptm <- proc.time() # for runtime calculations (ignore)

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)

# - Subset to exclude default spells (NOT SURE IF THIS SHOULD BE DONE, BUT CAN'T THINK OF WHY YOU WOULD WANT TO PREDICT DEFAULT WHEN IN DEFAULT)
datCredit_train <- datCredit_train %>% subset(DefaultStatus1==0)
datCredit_valid <- datCredit_valid %>% subset(DefaultStatus1==0)

# - Load in all formulas
# Basic variables
unpack.ffdf(paste0(genObjPath, "Basic_Com_Formula"), tempPath)
# Macroeconomic variables
unpack.ffdf(paste0(genObjPath, "Mac_Com_Full_Formula"), tempPath); unpack.ffdf(paste0(genObjPath, "Mac_Com_Theme_Formula"), tempPath)




# ------ 2. Combing the basic variables with the macroeconomic vairables form the thematic selection
# --- Concatenate all variables into a single formula
inputs_com1 <- c(labels(terms(inputs_fin_com)), inputs_mac_com_fin_theme)
# --- Create formula
form_com1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_com1, collapse="+")))
# --- Fitting the full model
logitMod_com1 <- glm(form_com1, data=datCredit_train, family="binomial")
# --- Deviance and AIC
summary(logitMod_com1) # Null deviance = 254945; Residual deviance = 167618; AIC = 167714
### RESULTS:    Not all variables are significant.
### CONCLUSION: Run a best subset selection.

# --- Best subset selection
logitMod_com_best1 <- MASS::stepAIC(logitMod_com1, direction="both")
# Start AIC = 167713.6
# End AIC = 62863.1
# - AIC and deviance
summary(logitMod_com_best1)
# - Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_com_best1), confint.default(logitMod_com_best1))), 3)
# - FIRM analysis
varImport_logit(logitMod_com_best1, method="pd", plot=T, pd_plot=T, sig_level=0.1) # Top 2 variables: Error in partial.default(...): NA not found in training data
# --- ROC analysis
datCredit_train[, prob_com_best1 := predict(logitMod_com_best1, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_com_best1 := predict(logitMod_com_best1, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_com_best1) # 91.75%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob__com_best1) # 91.36%
# --- VIF analysis
car::vif(logitMod_com1)

### RESULTS:  ``Not all variables are significant, especially the macroeconomic variables.
###           ``The standard errors of the estimated coefficients are high for some aggregated variables and some SD macroeconomic variables.
###             The VIFs of some aggregated variables and some SD macroeconomic variables are high.

### CONCLUSION:`Compare full model to reduced model.

# --- Clean up
rm(inputs_com1, logitMod_com1)
datCredit_train[, prob_com1:=NULL]; datCredit_valid[, prob_com1:=NULL]






# ------ 6. Save the combined models
# - Full model (including SD variables)
pack.ffdf(paste0(genObjPath, "Com_Full_Formula"), form_com1); gc()
# - Reduced model (excluding all SD variables)
pack.ffdf(paste0(genObjPath, "Com_Red_Formula"), form_com2); gc()
# - Best subset selection for full model (including SD variables)
pack.ffdf(paste0(genObjPath, "Com_Full_Sub_Formula"), form_com3); gc()
# - Best subset selection for reduced model (excluding SD variables)
pack.ffdf(paste0(genObjPath, "Com_Red_Sub_Formula"), form_com4); gc()




# ------ 7. Clean up
# --- Clean up
rm(logitMod_com1, logitMod_com2, logitMod_com3, logitMod_com4, form_com1, form_com2, form_com3, form_com4)
datCredit_train[, prob_com4:=NULL]; datCredit_valid[, prob_com4:=NULL]




