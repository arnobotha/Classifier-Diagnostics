# ======================= MODEL DEFAULT RISK - MACROECOMIC ==============================
# Develop several forward looking (containing macroeconomic variables)
# logistic regression models ranging from models with few features to models with more
# features to predict default risk.
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha, Esmerelda Oberholzer

# DESCRIPTION:
# This script uses the previously prepared credit dataset fused with macroeconomic
# variables to create multiple logistic regression models for default. The focus of this
# script is forward looking information (macroeconomic variables).
# For all macroeconomic variables, the following engineered variables were created:
#     - Lag orders of 1-,2-,3-,...,24 months
#     - Volatility with windows of 1-,2-,3-,...,24 months
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

# - Subset to exclude default spells
datCredit_train <- datCredit_train %>% subset(DefaultStatus1==0)
datCredit_valid <- datCredit_valid %>% subset(DefaultStatus1==0)

# - Creating a dataset to be used for plotting
datPlot <- data.table(Base_Variable=c(rep("M_Repo_Rate",3),rep("M_Inflation_Growth",3), rep("M_RealIncome_Growth",3), rep("M_DTI_Growth",3), rep("M_Emp_Growth",3), rep("M_RealGDP_Growth",3)),
                      Model=rep(c("Lagged","SD","Combined"),6),
                      Number_Of_Vars=rep(0,18),
                      AUC=rep(0,18))
k <-1 # Counter for the rows in datPlot




# ------ 2. Repo rate - [M_Repo_Rate]
# ---- 2.1 Lags
# --- 2.1.1 Full logit model with all lagged variables related to [M_Repo_Rate]
# - Constructing a formula containing all related lagged variables to [M_Repo_Rate]
ColNames1 <- colnames(datCredit_train)[which(grepl("M_Repo_Rate", colnames(datCredit_train)) & !grepl("M_Repo_Rate_SD", colnames(datCredit_train)))]
form_mac_repo1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames1, collapse="+")))
# - Fitting the full model
logitMod_mac_repo1 <- glm(inputs_mac_repo1 <- form_mac_repo1
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_repo1) # Null deviance = 274496; Residual deviance = 271532; AIC = 271548
# - Variable importance (use the p-value test from model to only focus on the significant variables)
varImport_logit(logitMod_mac_repo1, method="odds_ratio", same_scales = T, plot=T, sig_level = 0.10) # Top variables: [M_Repo_Rate], [M_Repo_Rate_12] | All other variables are insignificant according to an alpha threshold of 0.1
# - ROC analysis
datCredit_valid[, prob_mac_repo1 := predict(logitMod_mac_repo1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_repo1) # 57.21%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           Significant variables are [M_Repo_Rate] and [M_Repo_Rate_12]

# --- 2.1.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_repo_best1 <- MASS::stepAIC(logitMod_mac_repo1, direction="both")
# Start AIC = 271547.7
# End AIC = 271541.7
# - Deviance and AIC
summary(logitMod_mac_repo_best1) # Null deviance = 274496; Residual deviance = 271534; AIC = 271542
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_repo_best1, method="odds_ratio", same_scales = T, plot=T, sig_level = 0.1) # Top 3 variables: [M_Repo_Rate], [M_Repo_Rate_9], and [M_Repo_Rate_12]
# - ROC analysis
datCredit_valid[, prob_mac_repo_best := predict(logitMod_mac_repo_best1, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_repo_best1 <- auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_repo_best)) # 57.23%
### Results:  All estimated coefficients and their associated standard errors seem reasonable 
###           Final selection is [M_Repo_Rate], [M_Repo_Rate_6], [M_Repo_Rate_9], and [M_Repo_Rate_12]

### CONCLUSION:   Use selection from best subset procedure

# --- 2.1.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_repo_best1$coefficients[-1]),
                AUC=AUC_logitMod_mac_repo_best1)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_repo1=NULL, prob_mac_repo_best=NULL)]
rm(ColNames1, logitMod_mac_repo1, inputs_mac_repo1, form_mac_repo1, AUC_logitMod_mac_repo_best1)


# ---- 2.2 Standard deviations (volatilities)
# --- 2.2.1 Full logit model with all standard deviation variables related to [M_Repo_Rate]
# - Constructing a formula containing all related standard deviation variables to [M_Repo_Rate]
ColNames2 <- colnames(datCredit_train)[which(grepl("M_Repo_Rate", colnames(datCredit_train)) & grepl("M_Repo_Rate_SD", colnames(datCredit_train)))]
form_mac_repo2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames2, collapse="+")))
# - Fitting the full model
logitMod_mac_repo2 <- glm(inputs_mac_repo2 <- form_mac_repo2
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_repo2) # Null deviance = 274496; Residual deviance = 273813; AIC = 273825
# - Variable importance 
varImport_logit(logitMod_mac_repo2, method="odds_ratio", same_scales = T, plot=T, sig_level = 0.1) # Top 3 variables: [M_Repo_Rate_SD_12], [M_Repo_Rate_SD_4], and [M_Repo_Rate_SD_9]
# - ROC analysis
datCredit_valid[, prob_mac_repo2 := predict(logitMod_mac_repo2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_repo2) # 54.7%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           Significant variables are [M_Repo_Rate_SD_4], [M_Repo_Rate_SD_9], and [M_Repo_Rate_SD_12]

# --- 2.2.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_repo_best2 <- MASS::stepAIC(logitMod_mac_repo2, direction="both")
# Start AIC = AIC=273825.5
# End AIC = 273822
# - Deviance and AIC
summary(logitMod_mac_repo_best2) # Null deviance = 274496; Residual deviance = 273814; AIC = 273822
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_repo_best2, method="odds_ratio", same_scales = T, plot=T, sig_level = 0.1) # Top 3 variables: [M_Repo_Rate_SD_12], [M_Repo_Rate_SD_4], and [M_Repo_Rate_SD_9]
# - ROC analysis
datCredit_valid[, prob_mac_repo_best := predict(logitMod_mac_repo_best2, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_repo_best2 <- auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_repo_best)) # 54.7%
### Results:  All estimated coefficients and their associated standard errors seem reasonable 
###           Final selection is [M_Repo_Rate_SD_4], [M_Repo_Rate_SD_9], and [M_Repo_Rate_SD_12]

### CONCLUSION:   Use selection from best subset procedure

# --- 2.2.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_repo_best2$coefficients[-1]),
                AUC=AUC_logitMod_mac_repo_best2)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_repo2=NULL, prob_mac_repo_best=NULL)]
rm(ColNames2, logitMod_mac_repo2, inputs_mac_repo2, form_mac_repo2, AUC_logitMod_mac_repo_best2)


# ---- 2.3 All macroeconomic variables related to [M_Repo_Rate]
# --- 2.3.1 Full model
# - Constructing a formula containing all related variables to [M_Repo_Rate]
inputs_mac_repo_com <- c(names(model.frame(logitMod_mac_repo_best1))[-1], names(model.frame(logitMod_mac_repo_best2))[-1])
form_mac_repo_com <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_mac_repo_com, collapse="+")))
# - Fitting the full model
logitMod_mac_repo3 <- glm(form_mac_repo_com
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_repo3) # Null deviance = 274496; Residual deviance = 271486; AIC = 271500
# - Variable importance
varImport_logit(logitMod_mac_repo3, method="odds_ratio", same_scales = F, plot=T, sig_level = 0.1) # Top 3 variables: [M_Repo_Rate], [M_Repo_Rate_SD_12], and [M_Repo_Rate_SD_9]
# - ROC analysis
datCredit_valid[, prob_mac_repo3 := predict(logitMod_mac_repo3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_repo3) # 57.03%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           Significant variables are [M_Repo_Rate], [M_Repo_Rate_9], [M_Repo_Rate_12], [M_Repo_Rate_SD_4], [M_Repo_Rate_SD_9], and [M_Repo_Rate_SD_12]

# --- 2.3.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_repo_best3 <- MASS::stepAIC(logitMod_mac_repo3, direction="both")
# Start AIC = AIC=271500.3
# End AIC = 271499.7
# - Deviance and AIC
summary(logitMod_mac_repo_best3) # Null deviance = 274496; Residual deviance = 271488; AIC = 271500
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_repo_best3, method="odds_ratio", same_scales=F, plot=T, sig_level=0.1) # Top 3 variables: [M_Repo_Rate_], [M_Repo_Rate_SD_12], and [M_Repo_Rate_SD_9]
# - ROC analysis
datCredit_valid[, prob_mac_repo_best := predict(logitMod_mac_repo_best3, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_repo_best3<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_repo_best)) # 57.04%
### Results:    All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_Repo_Rate_SD_4], [M_Repo_Rate_SD_9], and [M_Repo_Rate_SD_12]
###             The model with the combined variables performs as marginally worse than the model with the lags and better than the model with the standard deviations (57.04% for the full model vs 57.23% and 54.7%)

### CONCLUSION: The added complexity of the SD variables does not warrant the gain in the AUC (which actually deteriorates). Only use the lagged variables.

# --- 2.3.3. Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_repo_best3$coefficients[-1]),
                AUC=AUC_logitMod_mac_repo_best3)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_repo3=NULL, prob_mac_repo_best=NULL)]
rm(logitMod_mac_repo3, inputs_mac_repo_com, form_mac_repo_com, AUC_logitMod_mac_repo_best3)


# ---- 2.4 Clean up
# - Saving the selected varaibles to the disk
inputs_mac_repo_fin<-names(model.frame(logitMod_mac_repo_best1))[-1]
pack.ffdf(paste0(genObjPath, "Mac_Repo_Formula"), inputs_mac_repo_fin); gc()
# - Cleaning up the enviroment
rm(logitMod_mac_repo_best1, logitMod_mac_repo_best2, logitMod_mac_repo_best3)



# ------ 3. Inflation Growth Rate - [M_Inflation_Growth]
# ---- 3.1 Lags
# --- 3.1.1 Full logit model with all lagged variables related to [M_Repo_Rate]
# - Constructing a formula containing all related lagged variables to [M_Repo_Rate]
ColNames1 <- colnames(datCredit_train)[which(grepl("M_Inflation_Growth", colnames(datCredit_train)) & !grepl("M_Inflation_Growth_SD", colnames(datCredit_train)))]
form_mac_infl1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames1, collapse="+")))
# - Fitting the full model
logitMod_mac_infl1 <- glm(inputs_mac_infl1 <- form_mac_infl1
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_infl1) # Null deviance = 274496; Residual deviance = 272242; AIC = 272258
# - Variable importance
varImport_logit(logitMod_mac_infl1, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top 3 variables: [M_Inflation_Growth], [M_Inflation_Growth_1], and [M_Inflation_Growth_12]
# - ROC analysis
datCredit_valid[, prob_mac_infl1 := predict(logitMod_mac_infl1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_infl1) # 56.36%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           Significant variables are [M_Inflation_Growth] , [M_Inflation_Growth_1], and [M_Inflation_Growth_12]

# --- 3.1.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_infl_best1 <- MASS::stepAIC(logitMod_mac_infl1, direction="both")
# Start AIC = 272258.1
# End AIC = 272253
# - Deviance and AIC
summary(logitMod_mac_infl_best1) # Null deviance = 274496; Residual deviance = 272243; AIC = 272253
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_infl_best1, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top 3 variables: [M_Inflation_Growth], [M_Inflation_Growth_1], and [M_Inflation_Growth_12]
# - ROC analysis
datCredit_valid[, prob_mac_infl_best := predict(logitMod_mac_infl_best1, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_infl_best1<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_infl_best)) # 56.38%
### Results:  All estimated coefficients and their associated standard errors seem reasonable 
###           Final selection is [M_Inflation_Growth], [M_Inflation_Growth_1], [M_Inflation_Growth_12], and [M_Inflation_Growth_2]

### CONCLUSION:   Use selection from best subset procedure

# --- 3.1.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_infl_best1$coefficients[-1]),
                AUC=AUC_logitMod_mac_infl_best1)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_infl1=NULL, prob_mac_infl_best=NULL)]
rm(ColNames1, logitMod_mac_infl1, inputs_mac_infl1, form_mac_infl1, AUC_logitMod_mac_infl_best1)


# ---- 3.2 Standard deviations (volatilities)
# --- 3.2.1 Full logit model with all standard deviation variables related to [M_infl_Rate]
# - Constructing a formula containing all related standard deviation variables to [M_infl_Rate]
ColNames2 <- colnames(datCredit_train)[which(grepl("M_Inflation_Growth", colnames(datCredit_train)) & grepl("M_Inflation_Growth_SD", colnames(datCredit_train)))]
form_mac_infl2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames2, collapse="+")))
# - Fitting the full model
logitMod_mac_infl2 <- glm(inputs_mac_infl2 <- form_mac_infl2
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_infl2) # Null deviance = 274496; Residual deviance = 272871; AIC = 272883
# - Variable importance
varImport_logit(logitMod_mac_infl2, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top variables: [M_Inflation_Growth_SD_12], [M_Inflation_Growth_SD_4]
# - ROC analysis
datCredit_valid[, prob_mac_infl2 := predict(logitMod_mac_infl2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_infl2) # 55.32%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           Significant variables are [M_Inflation_Growth_SD_4] and [M_Inflation_Growth_SD_12]

# --- 3.2.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_infl_best2 <- MASS::stepAIC(logitMod_mac_infl2, direction="both")
# Start AIC = AIC=272882.8
# End AIC = AIC=272878.6
# - Deviance and AIC
summary(logitMod_mac_infl_best2) # Null deviance = 274496; Residual deviance = 272873; AIC = 272879
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_infl_best2, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top variables: [M_Inflation_Growth_SD_12] and [M_Inflation_Growth_SD_4]
# - ROC analysis
datCredit_valid[, prob_mac_infl_best := predict(logitMod_mac_infl_best2, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_infl_best2<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_infl_best)) # 55.29%
### Results:  All estimated coefficients and their associated standard errors seem reasonable 
###           Final selection is [M_Inflation_Growth_SD_12] and [M_Inflation_Growth_SD_4]

### CONCLUSION:   Use selection from best subset procedure

# --- 3.2.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_infl_best2$coefficients[-1]),
                AUC=AUC_logitMod_mac_infl_best2)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_infl2=NULL, prob_mac_infl_best=NULL)]
rm(ColNames2, logitMod_mac_infl2, inputs_mac_infl2, form_mac_infl2, AUC_logitMod_mac_infl_best2)


# ---- 3.3 All macroeconomic variables related to [M_infl_Rate]
# --- 3.3.1 Full model
# - Constructing a formula containing all related variables to [M_infl_Rate]
inputs_mac_infl_com <- c(names(model.frame(logitMod_mac_infl_best1))[-1], names(model.frame(logitMod_mac_infl_best2))[-1])
form_mac_infl_com <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_mac_infl_com, collapse="+")))
# - Fitting the full model
logitMod_mac_infl3 <- glm(form_mac_infl_com
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_infl3) # Null deviance = 274496; Residual deviance = 272132; AIC = 272146
# - Variable importance
varImport_logit(logitMod_mac_infl3, method="odds_ratio", same_scales=F, plot=T, sig_level=0.1) # Top 3 variables: [M_Inflation_Growth], [M_Inflation_Growth_1], and [M_Inflation_Growth_SD_4]
# - ROC analysis
datCredit_valid[, prob_mac_infl3 := predict(logitMod_mac_infl3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_infl3) # 56.27%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           Significant variables are [M_Inflation_Growth], [M_Inflation_Growth_1], [M_Inflation_Growth_12], [M_Inflation_Growth_SD_4], and [M_Inflation_Growth_SD_12]

# --- 3.3.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_infl_best3 <- MASS::stepAIC(logitMod_mac_infl3, direction="both")
# Start AIC = AIC=272146.3
# End AIC = 272144.3
# - Deviance and AIC
summary(logitMod_mac_infl_best3) # Null deviance = 274496; Residual deviance = 272132; AIC = 272144
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_infl_best3, method="odds_ratio", same_scales=F, plot=T, sig_level=0.1) # Top 3 variables: [M_Inflation_Growth], [M_Inflation_Growth_1], and [M_Inflation_Growth_SD_4]
# - ROC analysis
datCredit_valid[, prob_mac_infl_best := predict(logitMod_mac_infl_best3, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_infl_best3<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_infl_best)) # 56.27%
### Results:    All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_Inflation_Growth], [M_Inflation_Growth_1], [M_Inflation_Growth_12], [M_Inflation_Growth_SD_4], and [M_Inflation_Growth_SD_12]
###             The model with the combined variables performs worse than the model with the lags, but better than the model with the standard deviations (56.27% for the full model vs 56.38% and 55.29%)

### CONCLUSION: Use the lags only for this macroeconomic variable since it produces the highest AUC whilst having less variables than the other models.

# --- 3.3.3. Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_infl_best3$coefficients[-1]),
                AUC=AUC_logitMod_mac_infl_best3)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_infl3=NULL, prob_mac_infl_best=NULL)]
rm(logitMod_mac_infl3, inputs_mac_infl_com, form_mac_infl_com)

# ---- 3.4 Clean up
# - Saving the selected varaibles to the disk
inputs_mac_infl_fin<-names(model.frame(logitMod_mac_infl_best1))[-1]
pack.ffdf(paste0(genObjPath, "Mac_Infl_Formula"),inputs_mac_infl_fin); gc()
# - Cleaning up the enviroment
rm(logitMod_mac_infl_best1, logitMod_mac_infl_best2, logitMod_mac_infl_best3)




# ------ 4. Real Income Growth Rate - [M_RealIncome_Growth]
# ---- 4.1 Lags
# --- 4.1.1 Full logit model with all lagged variables related to [M_Repo_Rate]
# - Constructing a formula containing all related lagged variables to [M_Repo_Rate]
ColNames1 <- colnames(datCredit_train)[which(grepl("M_RealIncome_Growth", colnames(datCredit_train)) & !grepl("M_RealIncome_Growth_SD", colnames(datCredit_train)))]
form_mac_RinG1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames1, collapse="+")))
# - Fitting the full model
logitMod_mac_RinG1 <- glm(inputs_mac_RinG1 <- form_mac_RinG1
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_RinG1) # Null deviance = 274496; Residual deviance = 272283; AIC = 272299
# - Variable importance
varImport_logit(logitMod_mac_RinG1, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top 3 variables: [M_RealIncome_Growth_12], [M_RealIncome_Growth_1], and [M_RealIncome_Growth_9]
# - ROC analysis
datCredit_valid[, prob_mac_RinG1 := predict(logitMod_mac_RinG1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RinG1) # 56.85%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           Significant variables are [M_RealIncome_Growth], [M_RealIncome_Growth_1], [M_RealIncome_Growth_9], and [M_RealIncome_Growth_12]

# --- 4.1.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_RinG_best1 <- MASS::stepAIC(logitMod_mac_RinG1, direction="both")
# Start AIC = 272298.6
# End AIC = 272293.6
# - Deviance and AIC
summary(logitMod_mac_RinG_best1) # Null deviance = 274496; Residual deviance = 272284; AIC = 272294
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_RinG_best1, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top 3 variables: [M_RealIncome_Growth_12], [M_RealIncome_Growth_9], and [M_RealIncome_Growth_1]
# - ROC analysis
datCredit_valid[, prob_mac_RinG_best := predict(logitMod_mac_RinG_best1, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_Ring_best1<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RinG_best)) # 56.86%
### Results:  All estimated coefficients and their associated standard errors seem reasonable 
###           Final selection is [M_RealIncome_Growth], [M_RealIncome_Growth_1], [M_RealIncome_Growth_9], and [M_RealIncome_Growth_12]

### CONCLUSION:   Use selection from best subset procedure

# --- 4.1.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_RinG_best1$coefficients[-1]),
                AUC=AUC_logitMod_mac_Ring_best1)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_RinG1=NULL, prob_mac_RinG_best=NULL)]
rm(ColNames1, logitMod_mac_RinG1, inputs_mac_RinG1, form_mac_RinG1, AUC_logitMod_mac_Ring_best1)


# ---- 4.2 Standard deviations (volatilities)
# --- 4.2.1 Full logit model with all standard deviation variables related to [M_RinG_Rate]
# - Constructing a formula containing all related standard deviation variables to [M_RinG_Rate]
ColNames2 <- colnames(datCredit_train)[which(grepl("M_RealIncome_Growth", colnames(datCredit_train)) & grepl("M_RealIncome_Growth_SD", colnames(datCredit_train)))]
form_mac_RinG2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames2, collapse="+")))
# - Fitting the full model
logitMod_mac_RinG2 <- glm(inputs_mac_RinG2 <- form_mac_RinG2
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_RinG2) # Null deviance = 274496; Residual deviance = 274465; AIC = 274477
# - Variable importance
varImport_logit(logitMod_mac_RinG2, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top 3 variables: No significant variables in model, thus no variable importance can be conducted
# - ROC analysis
datCredit_valid[, prob_mac_RinG2 := predict(logitMod_mac_RinG2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RinG2) # 51.52%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           There are no significant variables

# --- 4.2.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_RinG_best2 <- MASS::stepAIC(logitMod_mac_RinG2, direction="both")
# Start AIC = AIC=274476.9
# End AIC = AIC=274470.5
# - Deviance and AIC
summary(logitMod_mac_RinG_best2) # Null deviance = 274496; Residual deviance = 274467; AIC = 274471
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_RinG_best2, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top variable: [M_RealIncome_Growth_SD_9]
# - ROC analysis
datCredit_valid[, prob_mac_RinG_best := predict(logitMod_mac_RinG_best2, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_Ring_best2<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RinG_best)) # 51.54%
### Results:  All estimated coefficients and their associated standard errors seem reasonable 
###           Final selection is [M_RealIncome_Growth_SD_9]

### CONCLUSION:   Use selection from best subset procedure

# --- 4.2.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_RinG_best2$coefficients[-1]),
                AUC=AUC_logitMod_mac_Ring_best2)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_RinG2=NULL, prob_mac_RinG_best=NULL)]
rm(ColNames2, logitMod_mac_RinG2, inputs_mac_RinG2, form_mac_RinG2, AUC_logitMod_mac_Ring_best2)


# ---- 4.3 All macroeconomic variables related to [M_RinG_Rate]
# --- 4.3.1 Full model
# - Constructing a formula containing all related variables to [M_RinG_Rate]
inputs_mac_RinG_com <- c(names(model.frame(logitMod_mac_RinG_best1))[-1], names(model.frame(logitMod_mac_RinG_best2))[-1])
form_mac_RinG_com <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_mac_RinG_com, collapse="+")))
# - Fitting the full model
logitMod_mac_RinG3 <- glm(form_mac_RinG_com
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_RinG3) # Null deviance = 274496; Residual deviance = 272132; AIC = 272164
# - Variable importance
varImport_logit(logitMod_mac_RinG3, method="odds_ratio", same_scales=F, plot=T, sig_level=0.1) # Top 3 variables: [M_RealIncome_Growth_12], [M_RealIncome_Growth_SD_9], and [M_RealIncome_Growth_1]
# - ROC analysis
datCredit_valid[, prob_mac_RinG3 := predict(logitMod_mac_RinG3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RinG3) # 56.94%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           Significant variables are [M_RealIncome_Growth], [M_RealIncome_Growth_1], [M_RealIncome_Growth_9], [M_RealIncome_Growth_12], and [M_RealIncome_Growth_SD_9]

# --- 4.3.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_RinG_best3 <- MASS::stepAIC(logitMod_mac_RinG3, direction="both")
# Start AIC = AIC=272176.3
# End AIC = 272176.3
# - Deviance and AIC
summary(logitMod_mac_RinG_best3) # Null deviance = 274496; Residual deviance = 272164; AIC = 272176
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_RinG_best3, method="odds_ratio", same_scales=F, plot=T, sig_level=0.1) # Top 3 variables: [M_RealIncome_Growth_12], [M_RealIncome_Growth_9], and [M_RealIncome_Growth_1]
# - ROC analysis
datCredit_valid[, prob_mac_RinG_best := predict(logitMod_mac_RinG_best3, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_Ring_best3<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RinG_best)) # 56.94%
### Results:    All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_RealIncome_Growth], [M_RealIncome_Growth_1], [M_RealIncome_Growth_9], [M_RealIncome_Growth_12], and [M_RealIncome_Growth_SD_9]
###             The best subset procedure produces the same full model it was given.

### CONCLUSION: Use the final set of combined variables as it produces the model with the highest AUC (56.94% vs 56.86% for the model with the lags and 51.54% for the model with the SD variables)

# --- 4.3.3. Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_RinG_best3$coefficients[-1]),
                AUC=AUC_logitMod_mac_Ring_best3)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_RinG3=NULL, prob_mac_RinG_best=NULL)]
rm(logitMod_mac_RinG3, inputs_mac_RinG_com, form_mac_RinG_com, AUC_logitMod_mac_Ring_best3)


# ---- 4.4 Clean up
# - Saving the selected varaibles to the disk
inputs_mac_RinG_fin <- names(model.frame(logitMod_mac_RinG_best3))[-1]
pack.ffdf(paste0(genObjPath, "Mac_RinG_Formula"), inputs_mac_RinG_fin); gc()
# - Cleaning up the enviroment
rm(logitMod_mac_RinG_best1, logitMod_mac_RinG_best2, logitMod_mac_RinG_best3)




# ------ 5. Dept-to-Income Growth Rate - [M_DTI_Growth]
# ---- 5.1 Lags
# --- 5.1.1 Full logit model with all lagged variables related to [M_Repo_Rate]
# - Constructing a formula containing all related lagged variables to [M_Repo_Rate]
ColNames1 <- colnames(datCredit_train)[which(grepl("M_DTI_Growth", colnames(datCredit_train)) & !grepl("M_DTI_Growth_SD", colnames(datCredit_train)))]
form_mac_DTI1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames1, collapse="+")))
# - Fitting the full model
logitMod_mac_DTI1 <- glm(inputs_mac_DTI1 <- form_mac_DTI1
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_DTI1) # Null deviance = 274496; Residual deviance = 271802; AIC = 271818
# - Variable importance
varImport_logit(logitMod_mac_DTI1, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top 3 variables: [M_DTI_Growth], [M_DTI_Growth_1], and [M_DTI_Growth_12]
# - ROC analysis
datCredit_valid[, prob_mac_DTI1 := predict(logitMod_mac_DTI1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_DTI1) # 57.91%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           Significant variables are [M_DTI_Growth], [M_DTI_Growth_1], [M_DTI_Growth_3], [M_DTI_Growth_6], [M_DTI_Growth_9], and [M_DTI_Growth_12]

# --- 5.1.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_DTI_best1 <- MASS::stepAIC(logitMod_mac_DTI1, direction="both")
# Start AIC = 271817.8
# End AIC = 271815.9
# - Deviance and AIC
summary(logitMod_mac_DTI_best1) # Null deviance = 274496; Residual deviance = 271802; AIC = 271816
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_DTI_best1, method="odds_ratio", same_scales=F, plot=T, sig_level=0.1) # Top 3 variables: [M_DTI_Growth], [M_DTI_Growth_1], and [M_DTI_Growth_3]
# - ROC analysis
datCredit_valid[, prob_mac_DTI_best := predict(logitMod_mac_DTI_best1, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_DTI_best1<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_DTI_best)) # 57.91%
### Results:  All estimated coefficients and their associated standard errors seem reasonable 
###           Final selection is [M_DTI_Growth], [M_DTI_Growth_1], [M_DTI_Growth_3], [M_DTI_Growth_3], [M_DTI_Growth_6], [M_DTI_Growth_9], [M_DTI_Growth_12]

### CONCLUSION:   Use selection from best subset procedure

# --- 5.1.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_DTI_best1$coefficients[-1]),
                AUC=AUC_logitMod_mac_DTI_best1)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_DTI1=NULL, prob_mac_DTI_best=NULL)]
rm(ColNames1, logitMod_mac_DTI1, inputs_mac_DTI1, form_mac_DTI1,AUC_logitMod_mac_DTI_best1)


# ---- 5.2 Standard deviations (volatilities)
# --- 5.2.1 Full logit model with all standard deviation variables related to [M_DTI_Rate]
# - Constructing a formula containing all related standard deviation variables to [M_DTI_Rate]
ColNames2 <- colnames(datCredit_train)[which(grepl("M_DTI_Growth", colnames(datCredit_train)) & grepl("M_DTI_Growth_SD", colnames(datCredit_train)))]
form_mac_DTI2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames2, collapse="+")))
# - Fitting the full model
logitMod_mac_DTI2 <- glm(inputs_mac_DTI2 <- form_mac_DTI2
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_DTI2) # Null deviance = 274496; Residual deviance = 274422; AIC = 274434
# - Variable importance
varImport_logit(logitMod_mac_DTI2, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top 3 variables: [M_RealIncome_Growth_SD_12], [M_RealIncome_Growth_SD_9]
# - ROC analysis
datCredit_valid[, prob_mac_DTI2 := predict(logitMod_mac_DTI2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_DTI2) # 52.16%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           Significant variables are [M_DTI_Growth_SD_9] and [M_DTI_Growth_SD_12]

# --- 5.2.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_DTI_best2 <- MASS::stepAIC(logitMod_mac_DTI2, direction="both")
# Start AIC = AIC=274434.3
# End AIC = AIC=274430.3
# - Deviance and AIC
summary(logitMod_mac_DTI_best2) # Null deviance = 274496; Residual deviance = 274422; AIC = 274430
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_DTI_best2, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top variable: [M_DTI_Growth_SD_12], [M_DTI_Growth_SD_9], and [M_DTI_Growth_SD_5]
# - ROC analysis
datCredit_valid[, prob_mac_DTI_best := predict(logitMod_mac_DTI_best2, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_DTI_best2<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_DTI_best)) # 52.18%
### Results:  All estimated coefficients and their associated standard errors seem reasonable 
###           Final selection is [M_DTI_Growth_SD_5], [M_DTI_Growth_SD_9], and [M_DTI_Growth_SD_12]

### CONCLUSION:   Use selection from best subset procedure

# --- 5.2.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_DTI_best2$coefficients[-1]),
                AUC=AUC_logitMod_mac_DTI_best2)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_DTI2=NULL, prob_mac_DTI_best=NULL)]
rm(ColNames2, logitMod_mac_DTI2, inputs_mac_DTI2, form_mac_DTI2, AUC_logitMod_mac_DTI_best2)


# ---- 5.3 All macroeconomic variables related to [M_DTI_Rate]
# --- 5.3.1 Full model
# - Constructing a formula containing all related variables to [M_DTI_Rate]
inputs_mac_DTI_com <- c(names(model.frame(logitMod_mac_DTI_best1))[-1], names(model.frame(logitMod_mac_DTI_best2))[-1])
form_mac_DTI_com <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_mac_DTI_com, collapse="+")))
# - Fitting the full model
logitMod_mac_DTI3 <- glm(form_mac_DTI_com
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_DTI3) # Null deviance = 274496; Residual deviance = 271745; AIC = 271765
# - Variable importance
varImport_logit(logitMod_mac_DTI3, method="odds_ratio", same_scales=F, plot=T, sig_level=0.1) # Top 3 variables: [M_DTI_Rate_Growth], [M_DTI_Rate_Growth_12], and [M_DTI_Rate_Growth_3]
# - ROC analysis
datCredit_valid[, prob_mac_DTI3 := predict(logitMod_mac_DTI3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_DTI3) # 57.93%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           Significant variables are [M_DTI_Growth], [M_DTI_Growth_1], [M_DTI_Growth_3], [M_DTI_Growth_6], [M_DTI_Growth_12], [M_DTI_Growth_SD_9], and [M_DTI_Growth_SD_12]

# --- 5.3.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_DTI_best3 <- MASS::stepAIC(logitMod_mac_DTI3, direction="both")
# Start AIC = AIC=271765.3
# End AIC = 271761.7
# - Deviance and AIC
summary(logitMod_mac_DTI_best3) # Null deviance = 274496; Residual deviance = 271746; AIC = 271762
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_DTI_best3, method="odds_ratio", same_scales=F, plot=T, sig_level=0.1) # Top 3 variables: [M_DTI_Growth_, [M_DTI_Growth_12], and [M_DTI_Growth_3]
# - ROC analysis
datCredit_valid[, prob_mac_DTI_best := predict(logitMod_mac_DTI_best3, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_DTI_best3<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_DTI_best)) # 57.92%
### Results:    All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_DTI_Growth], [M_DTI_Growth_1], [M_DTI_Growth_3], [M_DTI_Growth_6], [M_DTI_Growth_12], [M_DTI_Growth_SD_9], and [M_DTI_Growth_SD_12]
###             The combined model has an AUC of 57.92% compared of the lagged model with an AUC of 57.91% and the SD model with an AUC of 52.18%.

### CONCLUSION: Use only the lags for this macroeconomic variable as it produces a high AUC compared to the combined model whilst using less variables (5 vs 6 variables)

# --- 5.3.3. Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_DTI_best3$coefficients[-1]),
                AUC=AUC_logitMod_mac_DTI_best3)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_DTI3=NULL, prob_mac_DTI_best=NULL)]
rm(logitMod_mac_DTI3, inputs_mac_DTI_com, form_mac_DTI_com, AUC_logitMod_mac_DTI_best3)


# ---- 5.4 Clean up
# - Saving the selected varaibles to the disk
inputs_mac_DTI_fin <- names(model.frame(logitMod_mac_DTI_best1))[-1]
pack.ffdf(paste0(genObjPath, "Mac_DTI_Formula"), inputs_mac_DTI_fin); gc()
# - Clean up
rm(logitMod_mac_DTI_best1, logitMod_mac_DTI_best2, logitMod_mac_DTI_best3)




# ------ 6. Employment Growth Rate - [M_Emp_Growth]
# ---- 6.1 Lags
# --- 6.1.1 Full logit model with all lagged variables related to [M_Repo_Rate]
# - Constructing a formula containing all related lagged variables to [M_Repo_Rate]
ColNames1 <- colnames(datCredit_train)[which(grepl("M_Emp_Growth", colnames(datCredit_train)) & !grepl("M_Emp_Growth_SD", colnames(datCredit_train)))]
form_mac_EmpG1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames1, collapse="+")))
# - Fitting the full model
logitMod_mac_EmpG1 <- glm(inputs_mac_EmpG1 <- form_mac_EmpG1
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_EmpG1) # Null deviance = 274496; Residual deviance = 273110; AIC = 273126
# - Variable importance
varImport_logit(logitMod_mac_EmpG1, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top variables: [M_Emp_Growth_12], [M_Emp_Growth_9]
# - ROC analysis
datCredit_valid[, prob_mac_EmpG1 := predict(logitMod_mac_EmpG1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_EmpG1) # 56.51%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           Significant variables are [M_Emp_Growth_9] and [M_Emp_Growth_12]

# --- 6.1.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_EmpG_best1 <- MASS::stepAIC(logitMod_mac_EmpG1, direction="both")
# Start AIC = 273125.8
# End AIC = 273120.1
# - Deviance and AIC
summary(logitMod_mac_EmpG_best1) # Null deviance = 274496; Residual deviance = 273112; AIC = 273120
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_EmpG_best1, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top 3 variables: [M_RealGDP_Growth_12], [M_RealGDP_Growth_1], and [M_RealGDP_Growth_9]
# - ROC analysis
datCredit_valid[, prob_mac_EmpG_best := predict(logitMod_mac_EmpG_best1, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_EmpG_best1<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_EmpG_best)) # 56.47%
### Results:  All estimated coefficients and their associated standard errors seem reasonable 
###           Final selection is [M_Emp_Growth_1], [M_Emp_Growth_9], and [M_Emp_Growth_12]

### CONCLUSION:   Use selection from best subset procedure

# --- 6.1.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_EmpG_best1$coefficients[-1]),
                AUC=AUC_logitMod_mac_EmpG_best1)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_EmpG1=NULL, prob_mac_EmpG_best=NULL)]
rm(ColNames1, logitMod_mac_EmpG1, inputs_mac_EmpG1, form_mac_EmpG1, AUC_logitMod_mac_EmpG_best1)


# ---- 6.2 Standard deviations (volatilities)
# --- 6.2.1 Full logit model with all standard deviation variables related to [M_EmpG_Rate]
# - Constructing a formula containing all related standard deviation variables to [M_EmpG_Rate]
ColNames2 <- colnames(datCredit_train)[which(grepl("M_Emp_Growth", colnames(datCredit_train)) & grepl("M_Emp_Growth_SD", colnames(datCredit_train)))]
form_mac_EmpG2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames2, collapse="+")))
# - Fitting the full model
logitMod_mac_EmpG2 <- glm(inputs_mac_EmpG2 <- form_mac_EmpG2
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_EmpG2) # Null deviance = 274496; Residual deviance = 274404; AIC = 274416
# - Variable importance
varImport_logit(logitMod_mac_EmpG2, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top 3 variables: [M_Emp_Growth_SD_12], [M_Emp_Growth_SD_9]
# - ROC analysis
datCredit_valid[, prob_mac_EmpG2 := predict(logitMod_mac_EmpG2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_EmpG2) # 51.38%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           Significant variables are [M_Emp_Growth_SD_9] and [M_Emp_Growth_SD_12]

# --- 6.2.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_EmpG_best2 <- MASS::stepAIC(logitMod_mac_EmpG2, direction="both")
# Start AIC = AIC=274416.3
# End AIC = AIC=274413.1
# - Deviance and AIC
summary(logitMod_mac_EmpG_best2) # Null deviance = 274496; Residual deviance = 274405; AIC = 274413
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_EmpG_best2, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top variables: [M_Emp_Growth_SD_12], [M_Emp_Growth_SD_9], and [M_Emp_Growth_SD_6]
# - ROC analysis
datCredit_valid[, prob_mac_EmpG_best := predict(logitMod_mac_EmpG_best2, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_EmpG_best2<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_EmpG_best)) # 51.37%
### Results:  All estimated coefficients and their associated standard errors seem reasonable 
###           Final selection is [M_Emp_Growth_SD_12], [M_Emp_Growth_SD_9], and [M_Emp_Growth_SD_6]

### CONCLUSION:   Use selection from best subset procedure

# --- 6.2.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_EmpG_best2$coefficients[-1]),
                AUC=AUC_logitMod_mac_EmpG_best2)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_EmpG2=NULL, prob_mac_EmpG_best=NULL)]
rm(ColNames2, logitMod_mac_EmpG2, inputs_mac_EmpG2, form_mac_EmpG2, AUC_logitMod_mac_EmpG_best2)


# ---- 6.3 All macroeconomic variables related to [M_EmpG_Rate]
# --- 6.3.1 Full model
# - Constructing a formula containing all related variables to [M_EmpG_Rate]
inputs_mac_EmpG_com <- c(names(model.frame(logitMod_mac_EmpG_best1))[-1], names(model.frame(logitMod_mac_EmpG_best2))[-1])
form_mac_EmpG_com <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_mac_EmpG_com, collapse="+")))
# - Fitting the full model
logitMod_mac_EmpG3 <- glm(form_mac_EmpG_com
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_EmpG3) # Null deviance = 274496; Residual deviance = 272493; AIC = 272507
# - Variable importance
varImport_logit(logitMod_mac_EmpG3, method="odds_ratio", same_scales=F, plot=T, sig_level=0.1) # Top 3 variables: [M_Emp_Growth_12], [M_Emp_Growth_SD_12], and [M_Emp_Growth_1]
# - ROC analysis
datCredit_valid[, prob_mac_EmpG3 := predict(logitMod_mac_EmpG3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_EmpG3) # 57.17%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           Significant variables are [M_Emp_Growth_1], [M_Emp_Growth_9], [M_Emp_Growth_12], [M_Emp_Growth_SD_6], [M_Emp_Growth_SD_9], and [M_Emp_Growth_SD_12]

# --- 6.3.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_EmpG_best3 <- MASS::stepAIC(logitMod_mac_EmpG3, direction="both")
# Start AIC = 272507
# End AIC = 272507
# - Deviance and AIC
summary(logitMod_mac_EmpG_best3) # Null deviance = 274496; Residual deviance = 272493; AIC = 272507
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_EmpG_best3, method="odds_ratio", same_scales=F, plot=T, sig_level=0.1) # Top 3 varaibles [M_Emp_Growth_12], [M_Emp_Growth_SD_12], and [M_Emp_Growth_1]
# - ROC analysis
datCredit_valid[, prob_mac_EmpG_best := predict(logitMod_mac_EmpG_best3, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_EmpG_best3<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_EmpG_best)) # 57.17%
### Results:    All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_Emp_Growth_1], [M_Emp_Growth_9], [M_Emp_Growth_12], [M_Emp_Growth_SD_6], [M_Emp_Growth_SD_9], and [M_Emp_Growth_SD_12]
###             The combined model has an AUC of 57.17% compared of the lagged model with an AUC of 56.47% and the SD model with an AUC of 51.37%.

### CONCLUSION: Use the combined model as it has the highest AUC (the SD variable has a suprisingly  positive effect on the AUC)

# --- 6.3.3. Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_EmpG_best3$coefficients[-1]),
                AUC=AUC_logitMod_mac_EmpG_best3)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_EmpG3=NULL, prob_mac_EmpG_best=NULL)]
rm(logitMod_mac_EmpG3, inputs_mac_EmpG_com, form_mac_EmpG_com, AUC_logitMod_mac_EmpG_best3)


# ---- 6.4 Clean up
# - Saving the selected varaibles to the disk
inputs_mac_EmpG_fin <- names(model.frame(logitMod_mac_EmpG_best3))[-1]
pack.ffdf(paste0(genObjPath, "Mac_EmpG_Formula"), inputs_mac_EmpG_fin); gc()
# - Clean up
rm(logitMod_mac_EmpG_best1, logitMod_mac_EmpG_best2, logitMod_mac_EmpG_best3)




# ------ 7. Real GDP Growth Rate - [M_RealGDP_Growth]
# ---- 7.1 Lags
# --- 7.1.1 Full logit model with all lagged variables related to [M_Repo_Rate]
# - Constructing a formula containing all related lagged variables to [M_Repo_Rate]
ColNames1 <- colnames(datCredit_train)[which(grepl("M_RealGDP_Growth", colnames(datCredit_train)) & !grepl("M_RealGDP_Growth_SD", colnames(datCredit_train)))]
form_mac_RGDP1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames1, collapse="+")))
# - Fitting the full model
logitMod_mac_RGDP1 <- glm(inputs_mac_RGDP1 <- form_mac_RGDP1
                         , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_RGDP1) # Null deviance = 274496; Residual deviance = 272303; AIC = 272319
# - Variable importance
varImport_logit(logitMod_mac_RGDP1, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top 3 variables: [M_RealGDP_Growth_12], [M_RealGDP_Growth_1], and [M_RealGDP_Growth_9]
# - ROC analysis
datCredit_valid[, prob_mac_RGDP1 := predict(logitMod_mac_RGDP1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RGDP1) # 57.05%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           Significant variables are [M_RealGDP_Growth], [M_RealGDP_Growth_1], [M_RealGDP_Growth_9], and [M_RealGDP_Growth_12]

# --- 7.1.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_RGDP_best1 <- MASS::stepAIC(logitMod_mac_RGDP1, direction="both")
# Start AIC = 272319.2
# End AIC = 272315.6
# - Deviance and AIC
summary(logitMod_mac_RGDP_best1) # Null deviance = 274496; Residual deviance = 272306; AIC = 272316
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_RGDP_best1, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top 3 variables: [M_RealGDP_Growth_12], [M_RealGDP_Growth_1], and [M_RealGDP_Growth_9]
# - ROC analysis
datCredit_valid[, prob_mac_RGDP_best := predict(logitMod_mac_RGDP_best1, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_RGDP_best1<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RGDP_best)) # 57.09%
### Results:  All estimated coefficients and their associated standard errors seem reasonable 
###           Final selection is [M_RealGDP_Growth], [M_RealGDP_Growth_1], [M_RealGDP_Growth_9], and [M_RealGDP_Growth_12]

### CONCLUSION:   Use selection from best subset procedure

# --- 7.1.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_RGDP_best1$coefficients[-1]),
                AUC=AUC_logitMod_mac_RGDP_best1)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_RGDP1=NULL, prob_mac_RGDP_best=NULL)]
rm(ColNames1, logitMod_mac_RGDP1, inputs_mac_RGDP1, form_mac_RGDP1, AUC_logitMod_mac_RGDP_best1)


# ---- 7.2 Standard deviations (volatilities)
# --- 7.2.1 Full logit model with all standard deviation variables related to [M_RGDP_Rate]
# - Constructing a formula containing all related standard deviation variables to [M_RGDP_Rate]
ColNames2 <- colnames(datCredit_train)[which(grepl("M_RealGDP_Growth", colnames(datCredit_train)) & grepl("M_RealGDP_Growth_SD", colnames(datCredit_train)))]
form_mac_RGDP2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames2, collapse="+")))
# - Fitting the full model
logitMod_mac_RGDP2 <- glm(inputs_mac_RGDP2 <- form_mac_RGDP2
                         , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_RGDP2) # Null deviance = 274496; Residual deviance = 274391; AIC = 274403
# - Variable importance
varImport_logit(logitMod_mac_RGDP2, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top variable: [M_RealIncome_Growth_SD_12]
# - ROC analysis
datCredit_valid[, prob_mac_RGDP2 := predict(logitMod_mac_RGDP2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RGDP2) # 50.23%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           Significant varaibles are [M_RealGDP_Growth_SD_12]

# --- 7.2.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_RGDP_best2 <- MASS::stepAIC(logitMod_mac_RGDP2, direction="both")
# Start AIC = AIC=274403.3
# End AIC = AIC=274397.4
# - Deviance and AIC
summary(logitMod_mac_RGDP_best2) # Null deviance = 274496; Residual deviance = 274393; AIC = 274397
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_RGDP_best2, method="odds_ratio", same_scales=T, plot=T, sig_level=0.1) # Top variable: [M_RealGDP_Growth_SD_12]
# - ROC analysis
datCredit_valid[, prob_mac_RGDP_best := predict(logitMod_mac_RGDP_best2, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_RGDP_best2<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RGDP_best)) # 50.15%
### Results:  All estimated coefficients and their associated standard errors seem reasonable 
###           Final selection is [M_RealGDP_Growth_SD_12]

### CONCLUSION:   Use selection from best subset procedure

# --- 7.2.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_RGDP_best2$coefficients[-1]),
                AUC=AUC_logitMod_mac_RGDP_best2)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_RGDP2=NULL, prob_mac_RGDP_best=NULL)]
rm(ColNames2, logitMod_mac_RGDP2, inputs_mac_RGDP2, form_mac_RGDP2, AUC_logitMod_mac_RGDP_best2)


# ---- 7.3 All macroeconomic variables related to [M_RGDP_Rate]
# --- 7.3.1 Full model
# - Constructing a formula containing all related variables to [M_RGDP_Rate]
inputs_mac_RGDP_com <- c(names(model.frame(logitMod_mac_RGDP_best1))[-1], names(model.frame(logitMod_mac_RGDP_best2))[-1])
form_mac_RGDP_com <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_mac_RGDP_com, collapse="+")))
# - Fitting the full model
logitMod_mac_RGDP3 <- glm(form_mac_RGDP_com
                         , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_RGDP3) # Null deviance = 274496; Residual deviance = 271906; AIC = 271918
# - Variable importance
varImport_logit(logitMod_mac_RGDP3, method="odds_ratio", same_scales=F, plot=T, sig_level=0.1) # Top 3 variables: [M_RealGDP_Growth_12], [M_RealGDP_Growth_SD_12], and [M_RealGDP_Growth_9]
# - ROC analysis
datCredit_valid[, prob_mac_RGDP3 := predict(logitMod_mac_RGDP3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RGDP3) # 57.43%
### RESULTS:  Estimated coefficients and the associated standard errors seem reasonable
###           Significant variables are [M_RealGDP_Growth_1], [M_RealGDP_Growth_9], [M_RealGDP_Growth_12], and [M_RealGDP_Growth_SD_12]

# --- 7.3.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_RGDP_best3 <- MASS::stepAIC(logitMod_mac_RGDP3, direction="both")
# Start AIC = 271917.9
# End AIC = 271916
# - Deviance and AIC
summary(logitMod_mac_RGDP_best3) # Null deviance = 274496; Residual deviance = 271906; AIC = 271916
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_RGDP_best3, method="odds_ratio", same_scales=F, plot=T, sig_level=0.1) # Top 3 variables: [M_RealGDP_Growth_12], [M_RealGDP_Growth_SD_12], and [M_RealGDP_Growth_9]
# - ROC analysis
datCredit_valid[, prob_mac_RGDP_best := predict(logitMod_mac_RGDP_best3, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_RGDP_best3<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RGDP_best)) # 57.43%
### Results:    All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_RealGDP_Growth_1], [M_RealGDP_Growth_9], [M_RealGDP_Growth_12], and [M_RealGDP_Growth_SD_12]
###             The combined model has an AUC of 57.43% compared of the lagged model with an AUC of 57.09% and the SD model with an AUC of 50.23%.

### CONCLUSION: Use the combined model as it has the highest AUC (the SD variable has a suprisingly small positive effect on the AUC)

# --- 7.3.3. Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_RGDP_best3$coefficients[-1]),
                AUC=AUC_logitMod_mac_RGDP_best3)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_RGDP3=NULL, prob_mac_RGDP_best=NULL)]
rm(logitMod_mac_RGDP3, inputs_mac_RGDP_com, form_mac_RGDP_com, AUC_logitMod_mac_RGDP_best3)


# ---- 7.4 Clean up
# - Saving the selected variables to the disk
inputs_mac_RGDP_fin <- names(model.frame(logitMod_mac_RGDP_best3))[-1]
pack.ffdf(paste0(genObjPath, "Mac_RGDP_Formula"), inputs_mac_RGDP_fin); gc()
# - Saving the plotting dataset
pack.ffdf(paste0(genObjPath, "Mac_Models_Summary"), datPlot); gc()

# - Clean up
rm(logitMod_mac_RGDP_best1, logitMod_mac_RGDP_best2, logitMod_mac_RGDP_best3)




# ------ 8. All macroeconomic variables, as selected in the prior themes, combined
# ---- 8.1 Logit model with all variables as selected from each theme
# --- 8.1.1 Full model
# - Loading in the selected variables from each macroeconomic variable (if not in memory)
if (!exists('inputs_mac_repo')) unpack.ffdf(paste0(genObjPath,"Mac_Repo_Formula"), tempPath); if (!exists('inputs_mac_infl')) unpack.ffdf(paste0(genObjPath,"Mac_Infl_Formula"), tempPath)
if (!exists('inputs_mac_RinG')) unpack.ffdf(paste0(genObjPath,"Mac_RinG_Formula"), tempPath); if (!exists('inputs_mac_DTI')) unpack.ffdf(paste0(genObjPath,"Mac_DTI_Formula"), tempPath)
if (!exists('inputs_mac_Emp')) unpack.ffdf(paste0(genObjPath,"Mac_EmpG_Formula"), tempPath); if (!exists('inputs_mac_RGDP')) unpack.ffdf(paste0(genObjPath,"Mac_RGDP_Formula"), tempPath)
# - Constructing a formula containing all variables
ColNames7 <- c(inputs_mac_repo_fin, inputs_mac_infl_fin, inputs_mac_RinG_fin, inputs_mac_DTI_fin,
               inputs_mac_EmpG_fin, inputs_mac_RGDP_fin)
form_mac_final1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames7, collapse="+")))
# - Fitting the full model
logitMod_mac_final1 <- glm(form_mac_final1, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_final1) # Null deviance = 274496; Residual deviance = 270892; AIC = 270950
# - Odds Ratio analysis
varImport_logit(logitMod_mac_final1, method="odds_ratio", same_scales=F, plot=T, sig_level=0.1) # Top 3 variables: [M_RealGDP_Growth_12], [M_RealIncome_Growth_12], and [M_DTI_Growth]
# - ROC analysis
datCredit_valid[, prob_mac_final1 := predict(logitMod_mac_final1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_final1) # 58.4%
# - Conduct a VIF analysis
(vif_mac_final1 <- car::vif(logitMod_mac_final1))

### RESULTS:    About halve of the variables are significant, and VERY few variables have VIF below 10 (which is expected)
###             All fitted coefficients seem to have reasonable standard errors.

### CONLCUSION: Run a best subset selection to find the "optimal" set of macroeconomic variables.
###             Save formula for the combined variable selection script.

# ---- 8.1.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_final_best1 <- MASS::stepAIC(logitMod_mac_final1, direction="both")
# Start AIC = 270950
# End AIC = 270936.9
# - Deviance and AIC
summary(logitMod_mac_final_best1) # Null deviance = 274496; Residual deviance = 270899; AIC = 270937
# - Odds Ratio analysis
varImport_logit(logitMod_mac_final_best1, method="odds_ratio", same_scales=F, plot=T, sig_level=0.1) # Top 3 variables: [M_RealIncome_Growth_12], [M_RealGDP_Growth_12], and [M_RealIncome_Growth_9]
# - FIRM analysis
varImport_logit(logitMod_mac_final_best1, method="pd", plot=T, pd_plot=T, sig_level=0.1) # Top 3 variables: [M_RealIncome_Growth_12], [M_RealGDP_Growth_12], and [M_RealIncome_Growth_9]
# - ROC analysis
datCredit_train[, prob_mac_final_best1 := predict(logitMod_mac_final_best1, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_mac_final_best1 := predict(logitMod_mac_final_best1, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_mac_final_best1) # 58.55%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_final_best1) # 58.4%
(vif_mac_final_best1 <- car::vif(logitMod_mac_final_best1))
length(labels(terms(logitMod_mac_final1))); length(labels(terms(logitMod_mac_final_best1)))
### Results:  All variables are significant
###           The all standard errors of the estimated coefficients seem reasonable.
###           The AIC value of the best subset selection model is lower than the full model (270950 vs 270937).
###           The AUC of the best subset selection model is exactly the same as the reduced model using the validation dataset (58.4% vs 58.4%).
###           The AUCs of the best subset model as obtained by an ROC analysis on the training- and validation datasets are very similar (58.55 vs 58.4%) and thus there doesn't seem to be overfitting
###           The best subset selection reduces the total number of variables to 18, down from 28.

### CONCLUSION: Use the selected variables from the best subset selection model within the combined variable selection.
###             Save formula for the combined variable selection script.

# --- 8.1.3 Clean up
# - Saving the selected variables to the disk
inputs_mac_com_fin_theme <- names(model.frame(logitMod_mac_final_best1))[-1]
pack.ffdf(paste0(genObjPath, "Mac_Com_Theme_Formula"), inputs_mac_com_fin_theme); gc()
# - Cleaning up the environment
datCredit_valid[, prob_mac_final_best1:=NULL]; datCredit_train[, prob_mac_final_best1:=NULL]
rm(logitMod_mac_final1, logitMod_mac_final_best1, inputs_mac_com_fin_theme)




# ---- 8.2 Logit model with all variables
# --- 8.2.1 Full model
# - Constructing a formula containing all variables
ColNames8 <- colnames(datCredit_train)[which(grepl("M_", colnames(datCredit_train)))]
form_mac_final2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames8, collapse="+")))
# - Fitting the full model
logitMod_mac_final2 <- glm(form_mac_final2, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_final2) # Null deviance = 274496; Residual deviance = 270734; AIC = 270880
# - Odds Ratio analysis
varImport_logit(logitMod_mac_final2, method="odds_ratio", same_scales=F, plot=T, sig_level=0.1) # Top 3 variables: [M_RealGDP_Growth_SD_6], [M_RealGDP_Growth_SD_5], and [M_RealGDP_Growth_SD_9]
# - ROC analysis
datCredit_valid[, prob_mac_final2 := predict(logitMod_mac_final2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_final2) # 58.62%
# - Conduct a VIF analysis
(vif_mac_final2 <- car::vif(logitMod_mac_final2))

### RESULTS:    The majority of the variables are insignificant, and VERY few variables have VIF below 10 (which is expected)
###             Some fitted coefficients seem to have unreasonably large standard errors.

### CONLCUSION: Run a best subset selection to find the "optimal" set of macroeconomic variables.
###             Save formula for the combined variable selection script.

# ---- 8.1.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_final_best2 <- MASS::stepAIC(logitMod_mac_final2, direction="both")
# Start AIC = 270879.7
# End AIC = 270820.3
# - Deviance and AIC
summary(logitMod_mac_final_best1) # Null deviance = 274496; Residual deviance = 270737; AIC = 270809
# - Odds Ratio analysis
varImport_logit(logitMod_mac_final1, method="odds_ratio", same_scales=F, plot=T, sig_level=0.1) # Top 3 variables: [M_RealGDP_Growth_12], [M_RealIncome_Growth_12], and [M_DTI_Growth]
# - ROC analysis
datCredit_valid[, prob_mac_final_best1 := predict(logitMod_mac_final_best1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_final_best1) # 54.91%
(vif_mac_final_best1 <- car::vif(logitMod_mac_final_best1))
length(labels(terms(logitMod_mac_final1))); length(labels(terms(logitMod_mac_final_best1)))
### Results:  All variables are significant, with a few that have p-values close (but over) to the cut-off of 0.05.
###           The standard errors of the estimated coefficients of the SD variables are higher then the lagged variables.
###           The AIC value of the best subset selection model is lower than the full model (270809 vs 270883).
###           The AUC of the best subset selection model is slightly lower than the reduced model (54.91% vs 54.96%).
###           The best subset selection reduces the total number of variables to 18, down from 28.

### CONCLUSION: Consider the selected variables from the best subset selection model within the combined variable selection.
###             Save formula for the combined variable selection script.


# ---- 8.3 Final macroeconomic variables
# --- Model using variables as selected by the various themes
form_mac_fin_theme <- logitMod_mac_final_best1$formula
pack.ffdf(paste0(genObjPath, "Mac_Full_Formula"), form_mac_final1); gc()

# --- Model using all variables
form_mac_fin_all <- logitMod_mac_final_best1$formula
pack.ffdf(paste0(genObjPath, "Mac_Full_Sub_Formula"), form_mac_sub_fin1); gc()


# --- 8.4 Clean up
datCredit_valid[, `:=` (prob_mac_final1=NULL, prob_mac_final2=NULL, prob_mac_final_best1=NULL, prob_mac_final_best2=NULL)]
rm(ColNames7, ColNames8, form_mac_final1, form_mac_final2, logitMod_mac_final1, logitMod_mac_final2, inputs_mac_sub_fin1, inputs_mac_sub_fin2,
   vif_mac_final1, vif_mac_final2, vif_mac_final_best1, vif_mac_final_best2)




# ------ 9. Thematic variable selection analysis
# --- Loading in the plotting data
if (!exists('datPlot')) unpack.ffdf(paste0(genObjPath,"Mac_Models_Summary"), tempPath)

# --- Plotting parameters
col.v <- brewer.pal(9, "Blues")[c(4,7,9)]
col.v2 <- rep(c(col.v[2], col.v[3], col.v[1]),6)
col.v3 <- rep("white", 18)
label.v <- c("Combined", "Lagged", "SD")

# --- Plot
(g_mac_theme_sum <- ggplot(datPlot, aes(x=Base_Variable, y=AUC, group=Model)) +
   theme_minimal() +
   geom_col(aes(colour=Model, fill=Model), position="dodge") +
   geom_label(aes(label=sprintf("%.3f",AUC)), fill = col.v2, colour = col.v3, position=position_dodge(0.9)) +
   scale_colour_manual(name="Model", values=col.v, labels=label.v) +
   scale_fill_manual(name="Model", values=col.v, labels=label.v) +
   theme(legend.position = "bottom"))

# --- Save plot
dpi<-240
ggsave(g_mac_theme_sum, file=paste0(genFigPath, "MacroVars_Select_Combined_Themes_Summary.png"), width=2400/dpi, height=1333/dpi, dpi=dpi, bg="white")

# --- Clean up
rm(dpi, col.v, vol.v2, col.v3, label.v, g_mac_theme_sum); gc()





