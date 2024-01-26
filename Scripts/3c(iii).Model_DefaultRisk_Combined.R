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
unpack.ffdf(paste0(genObjPath, "ALI_Formula"), tempPath); unpack.ffdf(paste0(genObjPath, "Del_Formula"), tempPath)
unpack.ffdf(paste0(genObjPath, "Beh_Formula"), tempPath); unpack.ffdf(paste0(genObjPath, "Por_Formula"), tempPath)
unpack.ffdf(paste0(genObjPath, "Dev_Formula"), tempPath); unpack.ffdf(paste0(genObjPath, "Mac_Repo_Formula"), tempPath)
unpack.ffdf(paste0(genObjPath, "Mac_Full_Formula"), tempPath); unpack.ffdf(paste0(genObjPath, "Mac_Red_Formula"), tempPath)
unpack.ffdf(paste0(genObjPath, "Mac_Full_Sub_Formula"), tempPath); unpack.ffdf(paste0(genObjPath, "Mac_Red_Sub_Formula"), tempPath)




# ------ 2. Combined variables, excluding all SD macroeconomic variables
# --- Concatenate all variables into a single formula
inputs_com1 <- c(labels(terms(inputs_ali_fin)), labels(terms(inputs_del_fin)), labels(terms(inputs_beh_fin)), labels(terms(inputs_por_fin)),
                 labels(terms(form_mac_final1)))
# --- Create formula
form_com1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_com1, collapse="+")))
# --- Fitting the full model
logitMod_com1 <- glm(form_com1, data=datCredit_train, family="binomial")
# --- Deviance and AIC
summary(logitMod_com1) # Null deviance = 248088; Residual deviance = 105949; AIC = 106119
# --- Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_com1), confint.default(logitMod_com1))), 3)
# --- ROC analysis
datCredit_train[, prob_com1 := predict(logitMod_com1, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_com1 := predict(logitMod_com1, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_com1) # 91.75%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_com1) # 91.36%
# --- VIF analysis
car::vif(logitMod_com1)

### RESULTS:  ``Not all variables are significant, especially the macroeconomic variables.
###           ``The standard errors of the estimated coefficients are high for some aggregated variables and some SD macroeconomic variables.
###             The VIFs of some aggregated variables and some SD macroeconomic variables are high.

### CONCLUSION:`Compare full model to reduced model.

# --- Clean up
rm(inputs_com1, logitMod_com1)
datCredit_train[, prob_com1:=NULL]; datCredit_valid[, prob_com1:=NULL]




# ------ 3. Combined variables, excluding all SD macroeconomic variables
# --- Concatenate all variables into a single formula
inputs_com2 <- c(labels(terms(inputs_ali_fin)), labels(terms(inputs_del_fin)), labels(terms(inputs_beh_fin)), labels(terms(inputs_por_fin)),
                 labels(terms(form_mac_final2)))
# --- Create formula
form_com2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_com2, collapse="+")))
# --- Fitting the full model
logitMod_com2 <- glm(form_com2, data=datCredit_train, family="binomial")
# --- Deviance and AIC
summary(logitMod_com2) # Null deviance = 248088; Residual deviance = 106032; AIC = 106156
# --- Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_com1), confint.default(logitMod_com1))), 3)
# --- ROC analysis
datCredit_train[, prob_com2 := predict(logitMod_com2, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_com2 := predict(logitMod_com2, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_com2) # 91.75%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_com2) # 91.35%
# --- VIF analysis
car::vif(logitMod_com2)

### RESULTS:  ``Not all variables are significant, especially the macroeconomic variables.
###           ``The standard errors of the estimated coefficients are high for some aggregated variables.
###             The AIC of the reduced model is higher than the full model (106156 vs 106119).
###             The AUC of the reduced model is slightly lower than the full model on the validation set (91.35% vs 91.36%).
###             The VIFs of some aggregated variables and some SD macroeconomic variables are high.

### CONCLUSION:`Compare the full- and reduced models to their best subset counterparts.

# --- Clean up
rm(inputs_com2, logitMod_com2)
datCredit_train[, prob_com2:=NULL]; datCredit_valid[, prob_com2:=NULL]




# ------ 4. Combined variables, all variables and the best subset of the full macroeconomic variables (including all SD macroeconomic variables)
# --- Concatenate all variables into a single formula
inputs_com3 <- c(labels(terms(inputs_ali_fin)), labels(terms(inputs_del_fin)), labels(terms(inputs_beh_fin)), labels(terms(inputs_por_fin)),
                 labels(terms(form_mac_sub_fin1)))
# --- Create formula
form_com3 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_com3, collapse="+")))
# --- Fitting the full model
logitMod_com3 <- glm(form_com3, data=datCredit_train, family="binomial")
# --- Deviance and AIC
summary(logitMod_com3) # Null deviance = 248088; Residual deviance = 105979; AIC = 106117
# --- Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_com3), confint.default(logitMod_com3))), 3)
# --- ROC analysis
datCredit_train[, prob_com3 := predict(logitMod_com3, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_com3 := predict(logitMod_com3, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_com3) # 91.75%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_com3) # 91.35%
# --- VIF analysis
car::vif(logitMod_com3)

### RESULTS:  ``Not all variables are significant, especially the macroeconomic variables.
###           ``The standard errors of the estimated coefficients are high for some aggregated variables.
###             The AIC of the full best subset model is higher than the full model (106117 vs 106119).
###             The AUC of the full subset model is slightly lower than the full model on the validation set (91.35% vs 91.36%).
###             The VIFs of some aggregated variables and some SD macroeconomic variables are high.

### CONCLUSION:`Rather use the full subset model as opposed to the full model as it has fewer variables whilst having a better model fit and better predictive power.
###             Compare the full subset model to the reduced subset model.

# --- Clean up
rm(inputs_com3, logitMod_com3)
datCredit_train[, prob_com3:=NULL]; datCredit_valid[, prob_com3:=NULL]




# ------ 5 Combined variables, all variables and the best subset of the full macroeconomic variables (including all SD macroeconomic variables)
# --- Concatenate all variables into a single formula
inputs_com4 <- c(labels(terms(inputs_ali_fin)), labels(terms(inputs_del_fin)), labels(terms(inputs_beh_fin)), labels(terms(inputs_por_fin)),
                 labels(terms(form_mac_sub_fin2)))
# --- Create formula
form_com4 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_com4, collapse="+")))
# --- Fitting the full model
logitMod_com4 <- glm(form_com4, data=datCredit_train, family="binomial")
# --- Deviance and AIC
summary(logitMod_com4) # Null deviance = 248088; Residual deviance = 106043; AIC = 106153
# --- Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_com4), confint.default(logitMod_com4))), 3)
# --- ROC analysis
datCredit_train[, prob_com4 := predict(logitMod_com4, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_com4 := predict(logitMod_com4, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_com4) # 91.75%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_com4) # 91.35%
# --- VIF analysis
car::vif(logitMod_com4)
# --- Model parsimony comparison
length(labels(terms(logitMod_com1))); length(labels(terms(logitMod_com2))); length(labels(terms(logitMod_com3))); length(labels(terms(logitMod_com4)))
### RESULTS:  ``Not all variables are significant, especially the macroeconomic variables.
###           ``The standard errors of the estimated coefficients are high for some aggregated variables.
###             The AIC of the reduced subset model is sligtly lower than the reduced model (106153 vs 106156).
###             The AUC of the reduced subset model is exactly the same as the reduced model on the validation set (91.35% vs 91.35%).
###             The AIC of the reduced subset model is higher than the full subset model (106153 vs 106117).
###             The AUC of the reduced subset model is exactly the same as the full subset model on the validation set (91.35% vs 91.35%).
###             The VIFs of some aggregated variables and some SD macroeconomic variables are high.

### CONCLUSION:`Use the reduced subset model as it is the most parsimonious model (46 variables compared to 76-, 53-, and 60 variables in the full-, reduced-, and full subset models) and has equal AUCs compared to the other models.

# --- Clean updata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==
rm(inputs_com4)
datCredit_train[, prob_com4:=NULL]; datCredit_valid[, prob_com4:=NULL]




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






# --- Loading in the final combined models
# unpack.ffdf(paste0(genObjPath, "Com_Full_Formula"), tempPath); unpack.ffdf(paste0(genObjPath, "Com_Red_Formula"), tempPath)
# unpack.ffdf(paste0(genObjPath, "Com_Full_Sub_Formula"), tempPath); unpack.ffdf(paste0(genObjPath, "Com_Red_Sub_Formula"), tempPath)
