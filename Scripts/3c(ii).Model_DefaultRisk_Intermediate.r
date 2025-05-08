# ========================== DEFAULT RISK - INTERMEDIATE =================================
# Develops the input space of an intermediate-complexity level PD-model
# ----------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Marcel Muller (MM), Dr Arno Botha (AB)

# DESCRIPTION:
# This script uses the previously prepared credit dataset fused with macroeconomic
# variables to create multiple logistic regression models for default. The focus of this
# script is on delinquency- and forward looking information (macroeconomic variables).
# Variables are selected thematically (where the input space is divided into themes)
# and by using the combined input space.
# For the delinquency variables, existing variables (the ones with "slc_" in their name)
# are considered along with spell- and delinquency (arrear)-level variables.
# For all macroeconomic variables, the following engineered variables were created:
#     - Lag orders of 1-,2-,3-,...,24 months
#     - Volatility with windows of 1-,2-,3-,...,24 months
# ----------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2f.Data_Fusion1.R
#   - 3b.Data_Subsampled_Fusion2.R
#
# -- Inputs:
#   - datCredit_train | Prepared credit data from script 3b
#   - datCredit_valid | Prepared credit data from script 3b
#
# -- Outputs:
#   - A graph showcasing the predictive performance of each "macroeconomic theme".
#   - A graph showcasing the variable importance ranking of two macroeconomic variables.
#   - A formula for the final intermediate model.
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




# ------ 2. Basic (vanilla/un-lagged/base) macroeconomic variables
# --- 2.1 Full model
# - Constructing a formula containing all vanilla/un-lagged/base variables
form_mac_base <- as.formula(paste("DefaultStatus1_lead_12_max~M_Repo_Rate+M_Inflation_Growth+M_DTI_Growth+M_RealGDP_Growth+M_RealIncome_Growth+M_Emp_Growth"))
# - Fitting the model
logitMod_mac_base1 <- glm(form_mac_base
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_base1)
### Results: Null deviance = 279124; Residual deviance = 275931; AIC = 275945
# - Variable importance
varImport_logit(logitMod_mac_base1, method="stdCoef_Goodman", impPlot=T, sig_level = 0.10)
### RESULTS: Top variables: [M_Repo_Rate], [M_DTI_Growth], and [M_RealIncome_Growth]
# - ROC analysis
datCredit_valid[, prob_mac_base1 := predict(logitMod_mac_base1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_base1)
### RESULTS:    57.83%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             All macroeconomic variables are significant, i.e.: [M_Repo_Rate], [M_Inflation_Growth], [M_DTI_Growth], [M_RealGDP_Growth], [M_RealIncome_Growth], [M_Emp_Growth]
###             Proceed to testing each variable, and its associated lags and standard deviations, separately with in its own theme.

# --- 2.2 Clean up
rm(form_mac_base, logitMod_mac_base1)
datCredit_valid[,prob_mac_base1:=NULL]




# ------ 3. Repo rate - [M_Repo_Rate]
# ---- 3.1 Lags
# --- 3.1.1 Full logit model with all lagged variables related to [M_Repo_Rate]
# - Constructing a formula containing all related lagged variables to [M_Repo_Rate]
ColNames1 <- colnames(datCredit_train)[which(grepl("M_Repo_Rate", colnames(datCredit_train)) & !grepl("M_Repo_Rate_SD", colnames(datCredit_train)))]
form_mac_repo1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames1, collapse="+")))
# - Fitting the full model
logitMod_mac_repo1 <- glm(inputs_mac_repo1 <- form_mac_repo1
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_repo1)
### Results: Null deviance = 279124; Residual deviance = 276289; AIC = 276305
# - Coefficient of determination
coefDeter_glm(logitMod_mac_repo1)
### RESULTS: 1.02%
# - Variable importance
varImport_logit(logitMod_mac_repo1, method="stdCoef_Goodman", impPlot=T, sig_level = 0.10)
### RESULTS: Top variables: [M_Repo_Rate], [M_Repo_Rate_12] | All other variables are insignificant according to an alpha threshold of 0.1
# - ROC analysis
datCredit_valid[, prob_mac_repo1 := predict(logitMod_mac_repo1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_repo1)
### RESULTS:    57.12%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             Significant variables are [M_Repo_Rate] and [M_Repo_Rate_12]
###             Proceed to using best subset selection on the input space.

# --- 3.1.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_repo_best1 <- MASS::stepAIC(logitMod_mac_repo1, direction="both")
# Start AIC = 276305.1
# End AIC = 276300.1
# - Deviance and AIC
summary(logitMod_mac_repo_best1)
### RESULTS: Null deviance = 279124; Residual deviance = 276290; AIC = 276300
# - Coefficient of determination
coefDeter_glm(logitMod_mac_repo_best1)
### RESULTS: 1.02%
# - Variable importance
varImport_logit(logitMod_mac_repo_best1, method="stdCoef_Goodman", impPlot=T, sig_level = 0.1)
### RESULTS: Top 3 variables: [M_Repo_Rate], [M_Repo_Rate_6], and [M_Repo_Rate_12]
# - ROC analysis
datCredit_valid[, prob_mac_repo_best := predict(logitMod_mac_repo_best1, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_repo_best1 <- auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_repo_best))
### RESULTS:    57.14%
### CONCLUSION:  All estimated coefficients and their associated standard errors seem reasonable 
###              Final selection is [M_Repo_Rate], [M_Repo_Rate_9], and [M_Repo_Rate_12]
###              Proceed to investigating the SD variables for [M_Repo_Rate]

# --- 3.1.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_repo_best1$coefficients[-1]),
                AUC=AUC_logitMod_mac_repo_best1)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_repo1=NULL, prob_mac_repo_best=NULL)]
rm(ColNames1, logitMod_mac_repo1, inputs_mac_repo1, form_mac_repo1, AUC_logitMod_mac_repo_best1)


# ---- 3.2 Standard deviations (volatilities)
# --- 3.2.1 Full logit model with all standard deviation variables related to [M_Repo_Rate]
# - Constructing a formula containing all related standard deviation variables to [M_Repo_Rate]
ColNames2 <- colnames(datCredit_train)[which(grepl("M_Repo_Rate", colnames(datCredit_train)) & grepl("M_Repo_Rate_SD", colnames(datCredit_train)))]
form_mac_repo2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames2, collapse="+")))
# - Fitting the full model
logitMod_mac_repo2 <- glm(inputs_mac_repo2 <- form_mac_repo2
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_repo2)
### RESULTS: Null deviance = 279124; Residual deviance = 278278; AIC = 278290
# - Coefficient of determination
coefDeter_glm(logitMod_mac_repo2)
### RESULTS: 0.30%
# - Variable importance 
varImport_logit(logitMod_mac_repo2, method="stdCoef_Goodman", impPlot=T, sig_level = 0.1)
### RESULTS: Top 3 variables: [M_Repo_Rate_SD_12], [M_Repo_Rate_SD_9], and [M_Repo_Rate_SD_4]
# - ROC analysis
datCredit_valid[, prob_mac_repo2 := predict(logitMod_mac_repo2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_repo2)
### RESULTS:    55.47%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             Significant variables are [M_Repo_Rate_SD_4], [M_Repo_Rate_SD_9], and [M_Repo_Rate_SD_12]
###             Proceed to using best subset selection on the input space.

# --- 3.2.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_repo_best2 <- MASS::stepAIC(logitMod_mac_repo2, direction="both")
# Start AIC = 278290.1
# End AIC = 278288.2
# - Deviance and AIC
summary(logitMod_mac_repo_best2)
### RESULTS: Null deviance = 279124; Residual deviance = 278280; AIC = 278288
# - Coefficient of determination
coefDeter_glm(logitMod_mac_repo_best2)
### RESULTS: 0.30%
# - Variable importance
varImport_logit(logitMod_mac_repo_best2, method="stdCoef_Goodman", impPlot=T, sig_level = 0.1)
### RESULTS: Top 3 variables: [M_Repo_Rate_SD_12], [M_Repo_Rate_SD_4], and [M_Repo_Rate_SD_9]
# - ROC analysis
datCredit_valid[, prob_mac_repo_best := predict(logitMod_mac_repo_best2, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_repo_best2 <- auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_repo_best))
### RESULTS:    55.45%
### CONCLUSION: All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_Repo_Rate_SD_4], [M_Repo_Rate_SD_9], and [M_Repo_Rate_SD_12]
###             Proceed to investigating the combined input space (for the lagged- and SD variables) as select in the previous subsections

# --- 3.2.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_repo_best2$coefficients[-1]),
                AUC=AUC_logitMod_mac_repo_best2)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_repo2=NULL, prob_mac_repo_best=NULL)]
rm(ColNames2, logitMod_mac_repo2, inputs_mac_repo2, form_mac_repo2, AUC_logitMod_mac_repo_best2)


# ---- 3.3 All macroeconomic variables related to [M_Repo_Rate]
# --- 3.3.1 Full model
# - Constructing a formula containing all related variables to [M_Repo_Rate]
inputs_mac_repo_com <- c(names(model.frame(logitMod_mac_repo_best1))[-1], names(model.frame(logitMod_mac_repo_best2))[-1])
form_mac_repo_com <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_mac_repo_com, collapse="+")))
# - Fitting the full model
logitMod_mac_repo3 <- glm(form_mac_repo_com
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_repo3)
### RESULTS: Null deviance = 279124; Residual deviance = 276130; AIC = 276146
# - Coefficient of determination
coefDeter_glm(logitMod_mac_repo3)
### RESULTS: 1.07%
# - Variable importance
varImport_logit(logitMod_mac_repo3, method="stdCoef_Goodman", impPlot=T, sig_level = 0.1)
### RESULTS: Top 3 variables: [M_Repo_Rate], [M_Repo_Rate_SD_12], and [M_Repo_Rate_SD_4]
# - ROC analysis
datCredit_valid[, prob_mac_repo3 := predict(logitMod_mac_repo3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_repo3)
### RESULTS:    57.13%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             Significant variables are [M_Repo_Rate], [M_Repo_Rate_9], [M_Repo_Rate_SD_4], [M_Repo_Rate_SD_9], and [M_Repo_Rate_SD_12]
###             Proceed to best subset selection for the input space.

# --- 3.3.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_repo_best3 <- MASS::stepAIC(logitMod_mac_repo3, direction="both")
# Start AIC = AIC=276145.8
# End AIC = 276142.5
# - Deviance and AIC
summary(logitMod_mac_repo_best3)
### RESULTS: Null deviance = 279124; Residual deviance = 276131; AIC = 276143
# - Coefficient of determination
coefDeter_glm(logitMod_mac_repo_best3)
### RESULTS: 1.07%
# - Variable importance
varImport_logit(logitMod_mac_repo_best3, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_Repo_Rate], [M_Repo_Rate_SD_12], and [M_Repo_Rate_SD_4]
# - ROC analysis
datCredit_valid[, prob_mac_repo_best := predict(logitMod_mac_repo_best3, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_repo_best3<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_repo_best))
### RESULTS:    57.12%
### CONCLUSION: All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_Repo_Rate], [M_Repo_Rate_6], and [M_Repo_Rate_SD_4], [M_Repo_Rate_SD_9], and [M_Repo_Rate_SD_12]

### COMPARISON: The model with the combined variables performs marginally worse than the model with the lags and better than the model with the standard deviations (57.12% for the full model vs 57.14% and 55.45%)

### CONCLUSION: The added complexity of the SD variables does not warrant the gain in the AUC (which actually deteriorates).
###             Only use the lagged variables.

# --- 3.3.3. Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_repo_best3$coefficients[-1]),
                AUC=AUC_logitMod_mac_repo_best3)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_repo3=NULL, prob_mac_repo_best=NULL)]
rm(logitMod_mac_repo3, inputs_mac_repo_com, form_mac_repo_com, AUC_logitMod_mac_repo_best3)


# ---- 3.4 Clean up
# - Saving the selected varaibles to the disk
inputs_mac_repo_fin<-names(model.frame(logitMod_mac_repo_best1))[-1]
pack.ffdf(paste0(genObjPath, "Mac_Repo_Formula"), inputs_mac_repo_fin); gc()
# - Cleaning up the enviroment
rm(logitMod_mac_repo_best1, logitMod_mac_repo_best2, logitMod_mac_repo_best3)




# ------ 4. Inflation Growth Rate - [M_Inflation_Growth]
# ---- 4.1 Lags
# --- 4.1.1 Full logit model with all lagged variables related to [M_Inflation_Growth]
# - Constructing a formula containing all related lagged variables to [M_Inflation_Growth]
ColNames1 <- colnames(datCredit_train)[which(grepl("M_Inflation_Growth", colnames(datCredit_train)) & !grepl("M_Inflation_Growth_SD", colnames(datCredit_train)))]
form_mac_infl1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames1, collapse="+")))
# - Fitting the full model
logitMod_mac_infl1 <- glm(inputs_mac_infl1 <- form_mac_infl1
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_infl1)
### RESULTS: Null deviance = 279124; Residual deviance = 276736; AIC = 276752
# - Coefficient of determination
coefDeter_glm(logitMod_mac_infl1)
### RESULTS: 0.86%
# - Variable importance
varImport_logit(logitMod_mac_infl1, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_Inflation_Growth], [M_Inflation_Growth_12], and [M_Inflation_Growth_1]
# - ROC analysis
datCredit_valid[, prob_mac_infl1 := predict(logitMod_mac_infl1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_infl1)
### RESULTS:    56.64%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             Significant variables are [M_Inflation_Growth] , [M_Inflation_Growth_1], and [M_Inflation_Growth_12]
###             Proceed to using best subset selection on the input space.

# --- 4.1.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_infl_best1 <- MASS::stepAIC(logitMod_mac_infl1, direction="both")
# Start AIC = 276752.2
# End AIC = 276749.3
# - Deviance and AIC
summary(logitMod_mac_infl_best1)
### RESULTS: Null deviance = 279124; Residual deviance = 276737; AIC = 276749
# - Coefficient of determination
coefDeter_glm(logitMod_mac_infl_best1)
### RESULTS: 0.85%
# - Variable importance
varImport_logit(logitMod_mac_infl_best1, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_Inflation_Growth], [M_Inflation_Growth_12], and [M_Inflation_Growth_1]
# - ROC analysis
datCredit_valid[, prob_mac_infl_best := predict(logitMod_mac_infl_best1, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_infl_best1<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_infl_best))
### RESULTS: 56.67%
### CONCLUSION: All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_Inflation_Growth], [M_Inflation_Growth_1], [M_Inflation_Growth_2], [M_Inflation_Growth_9]. and [M_Inflation_Growth_12]
###             Proceed to to investigating the SD variables for [M_Inflation_Growth]

# --- 4.1.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_infl_best1$coefficients[-1]),
                AUC=AUC_logitMod_mac_infl_best1)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_infl1=NULL, prob_mac_infl_best=NULL)]
rm(ColNames1, logitMod_mac_infl1, inputs_mac_infl1, form_mac_infl1, AUC_logitMod_mac_infl_best1)


# ---- 4.2 Standard deviations (volatilities)
# --- 4.2.1 Full logit model with all standard deviation variables related to [M_Inflation_Growth]
# - Constructing a formula containing all related standard deviation variables to [M_Inflation_Growth]
ColNames2 <- colnames(datCredit_train)[which(grepl("M_Inflation_Growth", colnames(datCredit_train)) & grepl("M_Inflation_Growth_SD", colnames(datCredit_train)))]
form_mac_infl2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames2, collapse="+")))
# - Fitting the full model
logitMod_mac_infl2 <- glm(inputs_mac_infl2 <- form_mac_infl2
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_infl2)
### RESULTS: Null deviance = 279124; Residual deviance = 277521; AIC = 277533
# - Coefficient of determination
coefDeter_glm(logitMod_mac_infl2)
### RESULTS: 0.57%
# - Variable importance
varImport_logit(logitMod_mac_infl2, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top variables: [M_Inflation_Growth_SD_12], [M_Inflation_Growth_SD_4]
# - ROC analysis
datCredit_valid[, prob_mac_infl2 := predict(logitMod_mac_infl2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_infl2)
### RESTULS:    55.85%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             Significant variables are [M_Inflation_Growth_SD_4] and [M_Inflation_Growth_SD_12]
###             Proceed with best subset selection

# --- 4.2.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_infl_best2 <- MASS::stepAIC(logitMod_mac_infl2, direction="both")
# Start AIC = AIC=277533
# End AIC = AIC=277527.8
# - Deviance and AIC
summary(logitMod_mac_infl_best2)
### RESULTS: Null deviance = 279124; Residual deviance = 277522; AIC = 277528
# - Coefficient of determination
coefDeter_glm(logitMod_mac_infl_best2)
### RESULTS: 0.57%
# - Variable importance
varImport_logit(logitMod_mac_infl_best2, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top variables: [M_Inflation_Growth_SD_12] and [M_Inflation_Growth_SD_4]
# - ROC analysis
datCredit_valid[, prob_mac_infl_best := predict(logitMod_mac_infl_best2, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_infl_best2<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_infl_best))
### RESULTS:  55.86%
### Results:  All estimated coefficients and their associated standard errors seem reasonable 
###           Final selection is [M_Inflation_Growth_SD_12] and [M_Inflation_Growth_SD_4]
###           Proceed to investigating the combined input space (for the lagged- and SD variables) as select in the previous subsections

# --- 4.2.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_infl_best2$coefficients[-1]),
                AUC=AUC_logitMod_mac_infl_best2)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_infl2=NULL, prob_mac_infl_best=NULL)]
rm(ColNames2, logitMod_mac_infl2, inputs_mac_infl2, form_mac_infl2, AUC_logitMod_mac_infl_best2)


# ---- 4.3 All macroeconomic variables related to [M_Inflation_Growth]
# --- 4.3.1 Full model
# - Constructing a formula containing all related variables to [M_Inflation_Growth]
inputs_mac_infl_com <- c(names(model.frame(logitMod_mac_infl_best1))[-1], names(model.frame(logitMod_mac_infl_best2))[-1])
form_mac_infl_com <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_mac_infl_com, collapse="+")))
# - Fitting the full model
logitMod_mac_infl3 <- glm(form_mac_infl_com
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_infl3)
### RESULTS: Null deviance = 279124; Residual deviance = 276641; AIC = 276657
# - Coefficient of determination
coefDeter_glm(logitMod_mac_infl3)
### RESULTS: 0.89%
# - Variable importance
varImport_logit(logitMod_mac_infl3, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_Inflation_Growth], [M_Inflation_Growth_SD_12], and [M_Inflation_Growth_SD_4]
# - ROC analysis
datCredit_valid[, prob_mac_infl3 := predict(logitMod_mac_infl3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_infl3)
### RESTULS:    56.70%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             Significant variables are [M_Inflation_Growth], [M_Inflation_Growth_1], [M_Inflation_Growth_9], [M_Inflation_Growth_12], [M_Inflation_Growth_SD_4], and [M_Inflation_Growth_SD_12]
###             Proceed to using best subset selection on the input space.

# --- 4.3.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_infl_best3 <- MASS::stepAIC(logitMod_mac_infl3, direction="both")
# Start AIC = AIC=276657.3
# End AIC = 276655.8
# - Deviance and AIC
summary(logitMod_mac_infl_best3)
### RESULTS: Null deviance = 279124; Residual deviance = 276642; AIC = 276656
# - Coefficient of determination
coefDeter_glm(logitMod_mac_infl_best3)
### RESULTS: 0.89%
# - Variable importance
varImport_logit(logitMod_mac_infl_best3, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_Inflation_Growth], [M_Inflation_Growth_SD_4], and [M_Inflation_Growth_SD_12]
# - ROC analysis
datCredit_valid[, prob_mac_infl_best := predict(logitMod_mac_infl_best3, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_infl_best3<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_infl_best))
### RESULTS:    56.72%
### CONCLUSION: All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_Inflation_Growth], [M_Inflation_Growth_1], [M_Inflation_Growth_9], [M_Inflation_Growth_12], [M_Inflation_Growth_SD_4], and [M_Inflation_Growth_SD_12]

### COMPARISON: The model with the combined variables performs marginally better than the model with the lags and better than the model with the standard deviations (56.72% for the full model vs 56.67% and 55.86%)

### CONCLUSION: Use the lags only for this macroeconomic variable since the added complexity of the volatilities don't justify the added complexity.

# --- 4.3.3. Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_infl_best3$coefficients[-1]),
                AUC=AUC_logitMod_mac_infl_best3)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_infl3=NULL, prob_mac_infl_best=NULL)]
rm(logitMod_mac_infl3, inputs_mac_infl_com, form_mac_infl_com)


# ---- 4.4 Clean up
# - Saving the selected varaibles to the disk
inputs_mac_infl_fin<-names(model.frame(logitMod_mac_infl_best1))[-1]
pack.ffdf(paste0(genObjPath, "Mac_Infl_Formula"),inputs_mac_infl_fin); gc()
# - Cleaning up the enviroment
rm(logitMod_mac_infl_best1, logitMod_mac_infl_best2, logitMod_mac_infl_best3)




# ------ 5. Real Income Growth Rate - [M_RealIncome_Growth]
# ---- 5.1 Lags
# --- 5.1.1 Full logit model with all lagged variables related to [M_RealIncome_Growth]
# - Constructing a formula containing all related lagged variables to [M_RealIncome_Growth]
ColNames1 <- colnames(datCredit_train)[which(grepl("M_RealIncome_Growth", colnames(datCredit_train)) & !grepl("M_RealIncome_Growth_SD", colnames(datCredit_train)))]
form_mac_RinG1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames1, collapse="+")))
# - Fitting the full model
logitMod_mac_RinG1 <- glm(inputs_mac_RinG1 <- form_mac_RinG1
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_RinG1)
### RESULTS: Null deviance = 279124; Residual deviance = 276643; AIC = 276659
# - Coefficient of determination
coefDeter_glm(logitMod_mac_RinG1)
### RESULTS: 0.89%
# - Variable importance
varImport_logit(logitMod_mac_RinG1, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_RealIncome_Growth_12] and [M_RealIncome_Growth_9]
# - ROC analysis
datCredit_valid[, prob_mac_RinG1 := predict(logitMod_mac_RinG1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RinG1)
### RESULTS:    57.30%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             Significant variables are [M_RealIncome_Growth_9] and [M_RealIncome_Growth_12]
###             Proceed to using best subset selection on the input space.

# --- 5.1.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_RinG_best1 <- MASS::stepAIC(logitMod_mac_RinG1, direction="both")
# Start AIC = 276659.1
# End AIC = 276652.4
# - Deviance and AIC
summary(logitMod_mac_RinG_best1)
### RESULTS: Null deviance = 279124; Residual deviance = 276644; AIC = 276652
# - Coefficient of determination
coefDeter_glm(logitMod_mac_RinG_best1)
### RESULTS: 0.89%
# - Variable importance
varImport_logit(logitMod_mac_RinG_best1, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_RealIncome_Growth_12], [M_RealIncome_Growth_9], and [M_RealIncome_Growth_2]
# - ROC analysis
datCredit_valid[, prob_mac_RinG_best := predict(logitMod_mac_RinG_best1, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_Ring_best1<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RinG_best))
### RESULTS:    57.31%
### CONCLUSION: All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_RealIncome_Growth_2], [M_RealIncome_Growth_9], and [M_RealIncome_Growth_12]
###             Proceed to inverstigating the SD variables related to [M_RealIncome_Growth]

# --- 5.1.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_RinG_best1$coefficients[-1]),
                AUC=AUC_logitMod_mac_Ring_best1)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_RinG1=NULL, prob_mac_RinG_best=NULL)]
rm(ColNames1, logitMod_mac_RinG1, inputs_mac_RinG1, form_mac_RinG1, AUC_logitMod_mac_Ring_best1)


# ---- 5.2 Standard deviations (volatilities)
# --- 5.2.1 Full logit model with all standard deviation variables related to [M_RealIncome_Growth]
# - Constructing a formula containing all related standard deviation variables to [M_RealIncome_Growth]
ColNames2 <- colnames(datCredit_train)[which(grepl("M_RealIncome_Growth", colnames(datCredit_train)) & grepl("M_RealIncome_Growth_SD", colnames(datCredit_train)))]
form_mac_RinG2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames2, collapse="+")))
# - Fitting the full model
logitMod_mac_RinG2 <- glm(inputs_mac_RinG2 <- form_mac_RinG2
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_RinG2)
### RESULTS: Null deviance = 279124; Residual deviance = 279102; AIC = 279114
# - Coefficient of determination
coefDeter_glm(logitMod_mac_RinG2)
### RESULTS: 0.01%
# - Variable importance
varImport_logit(logitMod_mac_RinG2, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
## RESULTS: Top 3 variables: [M_RealIncome_Growth_SD_12], [M_RealIncome_Growth_SD_9], and [M_RealIncome_Growth_SD_5]
# - ROC analysis
datCredit_valid[, prob_mac_RinG2 := predict(logitMod_mac_RinG2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RinG2)
### RESULTS:    51.22%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             Proceed to using best subset selection on the input space.

# --- 5.2.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_RinG_best2 <- MASS::stepAIC(logitMod_mac_RinG2, direction="both")
# Start AIC = AIC=279114.3
# End AIC = AIC=279114.3
# - Deviance and AIC
summary(logitMod_mac_RinG_best2)
### RESULTS: Null deviance = 279124; Residual deviance = 279102; AIC = 279114
# - Coefficient of determination
coefDeter_glm(logitMod_mac_RinG_best2)
### RESULTS: 0.01%
# - Variable importance
varImport_logit(logitMod_mac_RinG_best2, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_RealIncome_Growth_SD_12], [M_RealIncome_Growth_SD_9], and [M_RealIncome_Growth_SD_5]
# - ROC analysis
datCredit_valid[, prob_mac_RinG_best := predict(logitMod_mac_RinG_best2, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_Ring_best2<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RinG_best))
### RESULTS:    51.22%
### CONCLUSION: All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_RealIncome_Growth_SD_4], [M_RealIncome_Growth_SD_5], [M_RealIncome_Growth_SD_6], [M_RealIncome_Growth_SD_9], and [M_RealIncome_Growth_SD_12]
###             Proceed to investigating the combined input space (both the lagged- and SD variables) as selected in the above subsections.

# --- 5.2.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_RinG_best2$coefficients[-1]),
                AUC=AUC_logitMod_mac_Ring_best2)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_RinG2=NULL, prob_mac_RinG_best=NULL)]
rm(ColNames2, logitMod_mac_RinG2, inputs_mac_RinG2, form_mac_RinG2, AUC_logitMod_mac_Ring_best2)


# ---- 5.3 All macroeconomic variables related to [M_RealIncome_Growth]
# --- 5.3.1 Full model
# - Constructing a formula containing all related variables to [M_RealIncome_Growth]
inputs_mac_RinG_com <- c(names(model.frame(logitMod_mac_RinG_best1))[-1], names(model.frame(logitMod_mac_RinG_best2))[-1])
form_mac_RinG_com <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_mac_RinG_com, collapse="+")))
# - Fitting the full model
logitMod_mac_RinG3 <- glm(form_mac_RinG_com
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_RinG3)
### RESULTS: Null deviance = 279124; Residual deviance = 276137; AIC = 276155
# - Coefficient of determination
coefDeter_glm(logitMod_mac_RinG3)
### RESULTS: 1.07%
# - Variable importance
varImport_logit(logitMod_mac_RinG3, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESTULS: Top 3 variables: [M_RealIncome_Growth_12], [M_RealIncome_Growth_SD_12], and [M_RealIncome_Growth_2]
# - ROC analysis
datCredit_valid[, prob_mac_RinG3 := predict(logitMod_mac_RinG3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RinG3)
### RESULTS:    57.65%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             Significant variables are [M_RealIncome_Growth_2], [M_RealIncome_Growth_9], [M_RealIncome_Growth_12], [M_RealIncome_Growth_SD_4], [M_RealIncome_Growth_SD_5], [M_RealIncome_Growth_6], [M_RealIncome_Growth_9], and [M_RealIncome_Growth_12]
###             Proceed with using best subset selection on the input space.

# --- 5.3.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_RinG_best3 <- MASS::stepAIC(logitMod_mac_RinG3, direction="both")
# Start AIC = AIC=276155.1
# End AIC = 276155.1
### RESULTS:  The best subset procedure produces the same full model it was given.
# - Deviance and AIC
summary(logitMod_mac_RinG_best3)
### RESULTS: Null deviance = 279124; Residual deviance = 276137; AIC = 276155
# - Coefficient of determination
coefDeter_glm(logitMod_mac_RinG_best3)
### RESULTS: 1.07%
# - Variable importance
varImport_logit(logitMod_mac_RinG_best3, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_RealIncome_Growth_12], [M_RealIncome_Growth_SD_12], and [M_RealIncome_Growth_2]
# - ROC analysis
datCredit_valid[, prob_mac_RinG_best := predict(logitMod_mac_RinG_best3, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_Ring_best3<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RinG_best))
### RESULTS:    57.65%
### CONCLUSION: All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_RealIncome_Growth_2], [M_RealIncome_Growth_9], [M_RealIncome_Growth_12], [M_RealIncome_Growth_SD_4],
###                                [M_RealIncome_Growth_SD_5], [M_RealIncome_Growth_SD_6], [M_RealIncome_Growth_SD_9], and [M_RealIncome_Growth_SD_12]

### COMPARISON: The final set of combined variables produces the model with the highest AUC (57.65% vs 57.31% for the model with the lags and 51.22% for the model with the SD variables).
###             This difference is however very small (0.34%) and within the "margin of error" for the ROC analysis.

### CONCLUSION: Use only the lagged variables as there is no clear evidence that the volatilities add a significant lift in the predictive performance of the model.


# --- 5.3.3. Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_RinG_best3$coefficients[-1]),
                AUC=AUC_logitMod_mac_Ring_best3)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_RinG3=NULL, prob_mac_RinG_best=NULL)]
rm(logitMod_mac_RinG3, inputs_mac_RinG_com, form_mac_RinG_com, AUC_logitMod_mac_Ring_best3)


# ---- 5.4 Clean up
# - Saving the selected varaibles to the disk
inputs_mac_RinG_fin <- names(model.frame(logitMod_mac_RinG_best1))[-1]
pack.ffdf(paste0(genObjPath, "Mac_RinG_Formula"), inputs_mac_RinG_fin); gc()
# - Cleaning up the enviroment
rm(logitMod_mac_RinG_best1, logitMod_mac_RinG_best2, logitMod_mac_RinG_best3)




# ------ 6. Dept-to-Income Growth Rate - [M_DTI_Growth]
# ---- 6.1 Lags
# --- 6.1.1 Full logit model with all lagged variables related to [M_DTI_Growth]
# - Constructing a formula containing all related lagged variables to [M_DTI_Growth]
ColNames1 <- colnames(datCredit_train)[which(grepl("M_DTI_Growth", colnames(datCredit_train)) & !grepl("M_DTI_Growth_SD", colnames(datCredit_train)))]
form_mac_DTI1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames1, collapse="+")))
# - Fitting the full model
logitMod_mac_DTI1 <- glm(inputs_mac_DTI1 <- form_mac_DTI1
                         , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_DTI1)
### RESULTS: Null deviance = 279124; Residual deviance = 276522; AIC = 276538
# - Coefficient of determination
coefDeter_glm(logitMod_mac_DTI1)
### RESULTS: 0.93%
# - Variable importance
varImport_logit(logitMod_mac_DTI1, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_DTI_Growth_12], [M_DTI_Growth], and [M_DTI_Growth_3]
# - ROC analysis
datCredit_valid[, prob_mac_DTI1 := predict(logitMod_mac_DTI1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_DTI1)
### RESULTS:    57.71%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             Significant variables are [M_DTI_Growth], [M_DTI_Growth_3], [M_DTI_Growth_6], [M_DTI_Growth_9], and [M_DTI_Growth_12]
###             Proceed to using best subset selection on the input space.

# --- 6.1.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_DTI_best1 <- MASS::stepAIC(logitMod_mac_DTI1, direction="both")
# Start AIC = 276538.3
# End AIC = 276537.7
# - Deviance and AIC
summary(logitMod_mac_DTI_best1)
### RESULTS: Null deviance = 279124; Residual deviance = 276524; AIC = 276538
# - Coefficient of determination
coefDeter_glm(logitMod_mac_DTI_best1)
### RESULTS: 0.93%
# - Variable importance
varImport_logit(logitMod_mac_DTI_best1, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_DTI_Growth_12], [M_DTI_Growth], and [M_DTI_Growth_3]
# - ROC analysis
datCredit_valid[, prob_mac_DTI_best := predict(logitMod_mac_DTI_best1, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_DTI_best1<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_DTI_best))
### RESULTS:    57.71%
### CONCLUSION: All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_DTI_Growth], [M_DTI_Growth_1], [M_DTI_Growth_3], [M_DTI_Growth_6], [M_DTI_Growth_9], [M_DTI_Growth_12]
###             Proceed with investigating the SD variables related to [M_DTI_Growth].

# --- 6.1.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_DTI_best1$coefficients[-1]),
                AUC=AUC_logitMod_mac_DTI_best1)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_DTI1=NULL, prob_mac_DTI_best=NULL)]
rm(ColNames1, logitMod_mac_DTI1, inputs_mac_DTI1, form_mac_DTI1,AUC_logitMod_mac_DTI_best1)


# ---- 6.2 Standard deviations (volatilities)
# --- 6.2.1 Full logit model with all standard deviation variables related to [M_DTI_Rate]
# - Constructing a formula containing all related standard deviation variables to [M_DTI_Rate]
ColNames2 <- colnames(datCredit_train)[which(grepl("M_DTI_Growth", colnames(datCredit_train)) & grepl("M_DTI_Growth_SD", colnames(datCredit_train)))]
form_mac_DTI2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames2, collapse="+")))
# - Fitting the full model
logitMod_mac_DTI2 <- glm(inputs_mac_DTI2 <- form_mac_DTI2
                         , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_DTI2)
### RESULTS: Null deviance = 279124; Residual deviance = 279032; AIC = 279044
# - Coefficient of determination
coefDeter_glm(logitMod_mac_DTI2)
### RESULTS: 0.03%%
# - Variable importance
varImport_logit(logitMod_mac_DTI2, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
## RESULTS: Top 3 variables: [M_DTI_Growth_SD_12] and [M_DTI_Growth_SD_9]
# - ROC analysis
datCredit_valid[, prob_mac_DTI2 := predict(logitMod_mac_DTI2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_DTI2)
### RESULTS:    53.26%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             Significant variables are [M_DTI_Growth_SD_4], [M_DTI_Growth_SD_9] and [M_DTI_Growth_SD_12]
###             Proceed to using best subset selection on the input space.

# --- 6.2.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_DTI_best2 <- MASS::stepAIC(logitMod_mac_DTI2, direction="both")
# Start AIC = AIC=279043.9
# End AIC = AIC=279042.1
# - Deviance and AIC
summary(logitMod_mac_DTI_best2)
### RESULTS: Null deviance = 279124; Residual deviance = 279032; AIC = 279042
# - Coefficient of determination
coefDeter_glm(logitMod_mac_DTI2)
### RESULTS: 0.03%%
# - Variable importance
varImport_logit(logitMod_mac_DTI_best2, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_DTI_Growth_SD_12], [M_DTI_Growth_SD_9], and [M_DTI_Growth_SD_5]
# - ROC analysis
datCredit_valid[, prob_mac_DTI_best := predict(logitMod_mac_DTI_best2, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_DTI_best2<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_DTI_best))
### RESULTS:    53.24%
### CONCLUSION: All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_DTI_Growth_SD_4], [M_DTI_Growth_SD_5], [M_DTI_Growth_SD_9], and [M_DTI_Growth_SD_12]
###             Investigate the combined inputs space (both the lagged- and SD variables) as selected in the above subsecitons.

# --- 6.2.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_DTI_best2$coefficients[-1]),
                AUC=AUC_logitMod_mac_DTI_best2)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_DTI2=NULL, prob_mac_DTI_best=NULL)]
rm(ColNames2, logitMod_mac_DTI2, inputs_mac_DTI2, form_mac_DTI2, AUC_logitMod_mac_DTI_best2)


# ---- 6.3 All macroeconomic variables related to [M_DTI_Rate]
# --- 6.3.1 Full model
# - Constructing a formula containing all related variables to [M_DTI_Rate]
inputs_mac_DTI_com <- c(names(model.frame(logitMod_mac_DTI_best1))[-1], names(model.frame(logitMod_mac_DTI_best2))[-1])
form_mac_DTI_com <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_mac_DTI_com, collapse="+")))
# - Fitting the full model
logitMod_mac_DTI3 <- glm(form_mac_DTI_com
                         , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_DTI3)
### RESULTS: Null deviance = 279124; Residual deviance = 276445; AIC = 276467
# - Coefficient of determination
coefDeter_glm(logitMod_mac_DTI3)
### RESULTS: 0.96%
# - Variable importance
varImport_logit(logitMod_mac_DTI3, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_DTI_Rate_Growth_12], [M_DTI_Growth_SD_9], and [M_DTI_Rate_Growth_3]
# - ROC analysis
datCredit_valid[, prob_mac_DTI3 := predict(logitMod_mac_DTI3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_DTI3)
### RESULTS:    57.59%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             Significant variables are [M_DTI_Growth], [M_DTI_Growth_3], [M_DTI_Growth_6], [M_DTI_Growth_12], [M_DTI_Growth_SD_4], [M_DTI_Growth_SD_9], and [M_DTI_Growth_SD_12]
###             Proceed with using a best subset selection on the input space.

# --- 6.3.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_DTI_best3 <- MASS::stepAIC(logitMod_mac_DTI3, direction="both")
# Start AIC = AIC=276467.1
# End AIC = 276466.2
# - Deviance and AIC
summary(logitMod_mac_DTI_best3)
### RESULTS: Null deviance = 279124; Residual deviance = 276446; AIC = 276466
# - Coefficient of determination
coefDeter_glm(logitMod_mac_DTI_best3)
### RESULTS: 0.96%
# - Variable importance
varImport_logit(logitMod_mac_DTI_best3, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
###  RESULTS: Top 3 variables: [M_DTI_Growth_12], [M_DTI_Growth_SD_9], and [M_DTI_Growth_3]
# - ROC analysis
datCredit_valid[, prob_mac_DTI_best := predict(logitMod_mac_DTI_best3, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_DTI_best3<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_DTI_best))
### RESULTS:    57.56%
### CONCLUSION: All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_DTI_Growth], [M_DTI_Growth_1], [M_DTI_Growth_3], [M_DTI_Growth_6], [M_DTI_Growth_12],
###                                [M_DTI_Growth_SD_4], [M_DTI_Growth_SD_5], [M_DTI_Growth_SD_9], and [M_DTI_Growth_SD_12]

### COMPARISON: The combined model has an AUC of 57.56% compared of the lagged model with an AUC of 57.71% and the SD model with an AUC of 53.24%.

### CONCLUSION: Use only the lags for this macroeconomic variable as it produces a higher AUC compared to the combined model (although only marginally) whilst using less variables (6 vs 7 variables).

# --- 6.3.3. Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_DTI_best3$coefficients[-1]),
                AUC=AUC_logitMod_mac_DTI_best3)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_DTI3=NULL, prob_mac_DTI_best=NULL)]
rm(logitMod_mac_DTI3, inputs_mac_DTI_com, form_mac_DTI_com, AUC_logitMod_mac_DTI_best3)


# ---- 6.4 Clean up
# - Saving the selected varaibles to the disk
inputs_mac_DTI_fin <- names(model.frame(logitMod_mac_DTI_best1))[-1]
pack.ffdf(paste0(genObjPath, "Mac_DTI_Formula"), inputs_mac_DTI_fin); gc()
# - Clean up
rm(logitMod_mac_DTI_best1, logitMod_mac_DTI_best2, logitMod_mac_DTI_best3)




# ------ 7. Employment Growth Rate - [M_Emp_Growth]
# ---- 7.1 Lags
# --- 7.1.1 Full logit model with all lagged variables related to [M_Emp_Growth]
# - Constructing a formula containing all related lagged variables to [M_Emp_Growth]
ColNames1 <- colnames(datCredit_train)[which(grepl("M_Emp_Growth", colnames(datCredit_train)) & !grepl("M_Emp_Growth_SD", colnames(datCredit_train)))]
form_mac_EmpG1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames1, collapse="+")))
# - Fitting the full model
logitMod_mac_EmpG1 <- glm(inputs_mac_EmpG1 <- form_mac_EmpG1
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_EmpG1)
### RESULTS: Null deviance = 279124; Residual deviance = 277766; AIC = 277782
# - Coefficient of determination
coefDeter_glm(logitMod_mac_EmpG1)
### RESULTS: 0.49%
# - Variable importance
varImport_logit(logitMod_mac_EmpG1, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top variables: [M_Emp_Growth_12] and [M_Emp_Growth_9]
# - ROC analysis
datCredit_valid[, prob_mac_EmpG1 := predict(logitMod_mac_EmpG1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_EmpG1)
### RESULTS:    56.67%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             Significant variables are [M_Emp_Growth_9] and [M_Emp_Growth_12]
###             Proceed with best subset selection for the input space.

# --- 7.1.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_EmpG_best1 <- MASS::stepAIC(logitMod_mac_EmpG1, direction="both")
# Start AIC = 277781.9
# End AIC = 277777.7
# - Deviance and AIC
summary(logitMod_mac_EmpG_best1)
### RESULTS:  Null deviance = 279124; Residual deviance = 277768; AIC = 277778
# - Coefficient of determination
coefDeter_glm(logitMod_mac_EmpG_best1)
### RESULTS: 0.49%
# - Variable importance
varImport_logit(logitMod_mac_EmpG_best1, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS:  Top 3 variables: [M_Emp_Growth_12], [M_Emp_Growth_9], and [M_Emp_Growth_1]
# - ROC analysis
datCredit_valid[, prob_mac_EmpG_best := predict(logitMod_mac_EmpG_best1, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_EmpG_best1<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_EmpG_best))
### RESULTS:    56.67%
### CONCLUSION: All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_Emp_Growth], [M_Emp_Growth_2], [M_Emp_Growth_9], and [M_Emp_Growth_12]
###             Proceed to investigating the SD variables relating to [M_Emp_Growth].

# --- 7.1.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_EmpG_best1$coefficients[-1]),
                AUC=AUC_logitMod_mac_EmpG_best1)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_EmpG1=NULL, prob_mac_EmpG_best=NULL)]
rm(ColNames1, logitMod_mac_EmpG1, inputs_mac_EmpG1, form_mac_EmpG1, AUC_logitMod_mac_EmpG_best1)


# ---- 7.2 Standard deviations (volatilities)
# --- 7.2.1 Full logit model with all standard deviation variables related to [M_Emp_Growth]
# - Constructing a formula containing all related standard deviation variables to [M_Emp_Growth]
ColNames2 <- colnames(datCredit_train)[which(grepl("M_Emp_Growth", colnames(datCredit_train)) & grepl("M_Emp_Growth_SD", colnames(datCredit_train)))]
form_mac_EmpG2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames2, collapse="+")))
# - Fitting the full model
logitMod_mac_EmpG2 <- glm(inputs_mac_EmpG2 <- form_mac_EmpG2
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_EmpG2)
### RESULTS: Null deviance = 279124; Residual deviance = 278995; AIC = 279007
# - Coefficient of determination
coefDeter_glm(logitMod_mac_EmpG2)
### RESULTS: 0.05%
# - Variable importance
varImport_logit(logitMod_mac_EmpG2, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top variable: [M_Emp_Growth_SD_12]
# - ROC analysis
datCredit_valid[, prob_mac_EmpG2 := predict(logitMod_mac_EmpG2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_EmpG2)
### RESULTS:    52.23%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             Significant variables are [M_Emp_Growth_SD_12]
###             Proceed with using best subset selection for the input space.

# --- 7.2.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_EmpG_best2 <- MASS::stepAIC(logitMod_mac_EmpG2, direction="both")
# Start AIC = AIC=279006.9
# End AIC = AIC=279004.2
# - Deviance and AIC
summary(logitMod_mac_EmpG_best2)
### RESULTS: Null deviance = 279124; Residual deviance = 278996; AIC = 279004
# - Coefficient of determination
coefDeter_glm(logitMod_mac_EmpG2)
### RESULTS: 0.05%
# - Variable importance
varImport_logit(logitMod_mac_EmpG_best2, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top variables: [M_Emp_Growth_SD_12], [M_Emp_Growth_SD_5], and [M_Emp_Growth_SD_9]
# - ROC analysis
datCredit_valid[, prob_mac_EmpG_best := predict(logitMod_mac_EmpG_best2, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_EmpG_best2<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_EmpG_best))
### RESULTS:    52.23%
### CONCLUSION: All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_Emp_Growth_SD_5], [M_Emp_Growth_SD_9], and [M_Emp_Growth_SD_12]
###             Proceed to investigating the combined input space (both the lagged- and SD variables) for [M_Emp_Growth].

# --- 7.2.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_EmpG_best2$coefficients[-1]),
                AUC=AUC_logitMod_mac_EmpG_best2)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_EmpG2=NULL, prob_mac_EmpG_best=NULL)]
rm(ColNames2, logitMod_mac_EmpG2, inputs_mac_EmpG2, form_mac_EmpG2, AUC_logitMod_mac_EmpG_best2)


# ---- 7.3 All macroeconomic variables related to [M_EmpG_Rate]
# --- 7.3.1 Full model
# - Constructing a formula containing all related variables to [M_EmpG_Rate]
inputs_mac_EmpG_com <- c(names(model.frame(logitMod_mac_EmpG_best1))[-1], names(model.frame(logitMod_mac_EmpG_best2))[-1])
form_mac_EmpG_com <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_mac_EmpG_com, collapse="+")))
# - Fitting the full model
logitMod_mac_EmpG3 <- glm(form_mac_EmpG_com
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_EmpG3)
### RESULTS: Null deviance = 279124; Residual deviance = 277218; AIC = 277234
# - Coefficient of determination
coefDeter_glm(logitMod_mac_EmpG3)
### RESULTS: 0.68%
# - Variable importance
varImport_logit(logitMod_mac_EmpG3, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_Emp_Growth_12], [M_Emp_Growth], and [M_Emp_Growth_SD_9]
# - ROC analysis
datCredit_valid[, prob_mac_EmpG3 := predict(logitMod_mac_EmpG3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_EmpG3)
### RESULTS:    57.35%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             Significant variables are [M_Emp_Growth], [M_Emp_Growth_2], [M_Emp_Growth_9], [M_Emp_Growth_12], [M_Emp_Growth_SD_5], [M_Emp_Growth_SD_9], and [M_Emp_Growth_SD_12]
###             Proceed to using best subset selection for the input space.

# --- 7.3.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_EmpG_best3 <- MASS::stepAIC(logitMod_mac_EmpG3, direction="both")
# Start AIC = 277234.2
# End AIC = 277231.3
# - Deviance and AIC
summary(logitMod_mac_EmpG_best3)
### RESULTS: Null deviance = 279124; Residual deviance = 277219; AIC = 277231
# - Coefficient of determination
coefDeter_glm(logitMod_mac_EmpG_best3)
### RESULTS: 0.68%
# - Odds Ratio analysis 
varImport_logit(logitMod_mac_EmpG_best3, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 varaibles [M_Emp_Growth], [M_Emp_Growth_12], and [M_Emp_Growth_SD_12]
# - ROC analysis
datCredit_valid[, prob_mac_EmpG_best := predict(logitMod_mac_EmpG_best3, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_EmpG_best3<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_EmpG_best))
### RESULTS:    57.34%
### CONCLUSION: All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_Emp_Growth], [M_Emp_Growth_1], [M_Emp_Growth_9], [M_Emp_Growth_12], [M_Emp_Growth_SD_5], and [M_Emp_Growth_SD_12]

### COMPARISON: The combined model has an AUC of 57.34% compared of the lagged model with an AUC of 56.67% and the SD model with an AUC of 52.23%.

### CONCLUSION: Use the lagged model, although it has slightly worse performance (0.67% smaller AUC), there is not enough evidence to justify the inclusion of the volatilities.


# --- 7.3.3. Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_EmpG_best3$coefficients[-1]),
                AUC=AUC_logitMod_mac_EmpG_best3)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_EmpG3=NULL, prob_mac_EmpG_best=NULL)]
rm(logitMod_mac_EmpG3, inputs_mac_EmpG_com, form_mac_EmpG_com, AUC_logitMod_mac_EmpG_best3)


# ---- 7.4 Clean up
# - Saving the selected varaibles to the disk
inputs_mac_EmpG_fin <- names(model.frame(logitMod_mac_EmpG_best1))[-1]
pack.ffdf(paste0(genObjPath, "Mac_EmpG_Formula"), inputs_mac_EmpG_fin); gc()
# - Clean up
rm(logitMod_mac_EmpG_best1, logitMod_mac_EmpG_best2, logitMod_mac_EmpG_best3)




# ------ 8. Real GDP Growth Rate - [M_RealGDP_Growth]
# ---- 8.1 Lags
# --- 8.1.1 Full logit model with all lagged variables related to [M_RealGDP_Growth]
# - Constructing a formula containing all related lagged variables to [M_RealGDP_Growth]
ColNames1 <- colnames(datCredit_train)[which(grepl("M_RealGDP_Growth", colnames(datCredit_train)) & !grepl("M_RealGDP_Growth_SD", colnames(datCredit_train)))]
form_mac_RGDP1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames1, collapse="+")))
# - Fitting the full model
logitMod_mac_RGDP1 <- glm(inputs_mac_RGDP1 <- form_mac_RGDP1
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_RGDP1)
### RESULTS: Null deviance = 279124; Residual deviance = 276764; AIC = 276780
# - Coefficient of determination
coefDeter_glm(logitMod_mac_RGDP1)
### RESULTS: 0.85%
# - Variable importance
varImport_logit(logitMod_mac_RGDP1, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_RealGDP_Growth_12] and [M_RealGDP_Growth_9]
# - ROC analysis
datCredit_valid[, prob_mac_RGDP1 := predict(logitMod_mac_RGDP1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RGDP1)
### RESULTS:    57.15%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             Significant variables are [M_RealGDP_Growth_9] and [M_RealGDP_Growth_12]
###             Proceed to using best subset selection on the input space.

# --- 8.1.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_RGDP_best1 <- MASS::stepAIC(logitMod_mac_RGDP1, direction="both")
# Start AIC = 276779.5
# End AIC = 276773.6
# - Deviance and AIC
summary(logitMod_mac_RGDP_best1)
### RESULTS: Null deviance = 279124; Residual deviance = 276766; AIC = 276774
# - Coefficient of determination
coefDeter_glm(logitMod_mac_RGDP_best1)
### RESULTS: 0.84%
# - Variable importance
varImport_logit(logitMod_mac_RGDP_best1, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_RealGDP_Growth_12], [M_RealGDP_Growth_1], and [M_RealGDP_Growth_9]
# - ROC analysis
datCredit_valid[, prob_mac_RGDP_best := predict(logitMod_mac_RGDP_best1, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_RGDP_best1<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RGDP_best))
### RESULTS:    57.12%
### CONCLUSION: All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_RealGDP_Growth_1], [M_RealGDP_Growth_9], and [M_RealGDP_Growth_12]
###             Proceed to investigating all SD variables relating to [M_RealGDP_Growth].

# --- 8.1.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_RGDP_best1$coefficients[-1]),
                AUC=AUC_logitMod_mac_RGDP_best1)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_RGDP1=NULL, prob_mac_RGDP_best=NULL)]
rm(ColNames1, logitMod_mac_RGDP1, inputs_mac_RGDP1, form_mac_RGDP1, AUC_logitMod_mac_RGDP_best1)


# ---- 8.2 Standard deviations (volatilities)
# --- 8.2.1 Full logit model with all standard deviation variables related to [M_RealGDP_Growth]
# - Constructing a formula containing all related standard deviation variables to [M_RealGDP_Growth]
ColNames2 <- colnames(datCredit_train)[which(grepl("M_RealGDP_Growth", colnames(datCredit_train)) & grepl("M_RealGDP_Growth_SD", colnames(datCredit_train)))]
form_mac_RGDP2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames2, collapse="+")))
# - Fitting the full model
logitMod_mac_RGDP2 <- glm(inputs_mac_RGDP2 <- form_mac_RGDP2
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_RGDP2)
### RESULTS: Null deviance = 279124; Residual deviance = 279090; AIC = 279102
# - Coefficient of determination
coefDeter_glm(logitMod_mac_RGDP2)
### RESULTS: 0.01%
# - Variable importance
varImport_logit(logitMod_mac_RGDP2, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top variable: [M_RealIncome_Growth_SD_5], [M_RealIncome_Growth_SD_4], and [M_RealIncome_Growth_SD_12]
# - ROC analysis
datCredit_valid[, prob_mac_RGDP2 := predict(logitMod_mac_RGDP2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RGDP2)
### RESULTS:    49.02%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             No significant variables (although, a few are close to the 0.05 threshold)
###             Model is reverse ranking, could be due to some portions/segments of the variable(s) that have a different risk profile compared to the rest of the variable(s)
###             Proceed to using best subset selection on the input space.

# --- 8.2.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_RGDP_best2 <- MASS::stepAIC(logitMod_mac_RGDP2, direction="both")
# Start AIC = 279102.5
# End AIC = 279100.8
# - Deviance and AIC
summary(logitMod_mac_RGDP_best2)
### RESULTS: Null deviance = 279124; Residual deviance = 279091; AIC = 279101
# - Coefficient of determination
coefDeter_glm(logitMod_mac_RGDP_best2)
### RESULTS: 0.01%
# - Variable importance 
varImport_logit(logitMod_mac_RGDP_best2, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_RealGDP_Growth_SD_12] [M_RealGDP_Growth_SD_5], and [M_RealGDP_Growth_SD_6]
# - ROC analysis
datCredit_valid[, prob_mac_RGDP_best := predict(logitMod_mac_RGDP_best2, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_RGDP_best2<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RGDP_best))
### RESULTS:    48.94%
### CONCLUSION: All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_RealGDP_Growth_SD_4], [M_RealGDP_Growth_SD_5], [M_RealGDP_Growth_SD_6], and [M_RealGDP_Growth_SD_12]
###             Although the variables are reverse ranking, this bahaviour could changed when combined with the lagged variables.
###             Proceed to investigating the combined input space (both the lagged- and SD variables) as selected in the subsections above.

# --- 8.2.3 Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_RGDP_best2$coefficients[-1]),
                AUC=AUC_logitMod_mac_RGDP_best2)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_RGDP2=NULL, prob_mac_RGDP_best=NULL)]
rm(ColNames2, logitMod_mac_RGDP2, inputs_mac_RGDP2, form_mac_RGDP2, AUC_logitMod_mac_RGDP_best2)


# ---- 8.3 All macroeconomic variables related to [M_RealGDP_Growth]
# --- 8.3.1 Full model
# - Constructing a formula containing all related variables to [M_RealGDP_Growth]
inputs_mac_RGDP_com <- c(names(model.frame(logitMod_mac_RGDP_best1))[-1], names(model.frame(logitMod_mac_RGDP_best2))[-1])
form_mac_RGDP_com <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_mac_RGDP_com, collapse="+")))
# - Fitting the full model
logitMod_mac_RGDP3 <- glm(form_mac_RGDP_com
                          , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_RGDP3)
### RESULTS: Null deviance = 279124; Residual deviance = 276255; AIC = 276271
# - Coefficient of determination
coefDeter_glm(logitMod_mac_RGDP3)
### RESULTS: 1.03%
# - Variable importance
varImport_logit(logitMod_mac_RGDP3, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_RealGDP_Growth_1], [M_RealGDP_Growth_12], and [M_RealGDP_Growth_SD_12]
# - ROC analysis
datCredit_valid[, prob_mac_RGDP3 := predict(logitMod_mac_RGDP3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RGDP3)
### RESULTS:    57.84%
### CONCLUSION: Estimated coefficients and the associated standard errors seem reasonable
###             Significant variables are [M_RealGDP_Growth_1], [M_RealGDP_Growth_9], [M_RealGDP_Growth_12], [M_RealGDP_Growth_SD_4],
###                                       [M_RealGDP_Growth_SD_5], [M_RealGDP_Growth_SD_6], and [M_RealGDP_Growth_SD_12]
###             Proceed to using best subset selection on the input space.

# --- 8.3.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_RGDP_best3 <- MASS::stepAIC(logitMod_mac_RGDP3, direction="both")
# Start AIC = 276270.9
# End AIC = 276270.9
# - Deviance and AIC
summary(logitMod_mac_RGDP_best3)
### RESUTLS: Null deviance = 279124; Residual deviance = 276255; AIC = 276271
# - Coefficient of determination
coefDeter_glm(logitMod_mac_RGDP_best3)
### RESULTS: 1.03%
# - Variable importance 
varImport_logit(logitMod_mac_RGDP_best3, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_RealGDP_Growth_1], [M_RealGDP_Growth_12], and [M_RealGDP_Growth_SD_12]
# - ROC analysis
datCredit_valid[, prob_mac_RGDP_best := predict(logitMod_mac_RGDP_best3, newdata = datCredit_valid, type="response")]
(AUC_logitMod_mac_RGDP_best3<-auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_RGDP_best))
### RESULTS:    57.84%
### CONCLUSION: All estimated coefficients and their associated standard errors seem reasonable 
###             Final selection is [M_RealGDP_Growth_1], [M_RealGDP_Growth_12], [M_RealGDP_Growth_SD_4], [M_RealGDP_Growth_SD_4],
###                                [M_RealGDP_Growth_SD_5], [M_RealGDP_Growth_SD_6], and [M_RealGDP_Growth_SD_12]
###             The combination of the lagged and SD variables have resolved the issue where the SD variables were reverse ranking

### COMPARISON: The combined model has an AUC of 57.84% compared of the lagged model with an AUC of 57.12% and the SD model with an AUC of 48.94%.

### CONCLUSION: Use the lagged model, although the combined model has a slightly higher AUC, it is in the "margin of error". There is thus not strong enough evidence to suggest that the volatilities should be kept in the model.

# --- 8.3.3. Clean up
# - Saving the relevant model information
datPlot[k, `:=`(Number_Of_Vars=length(logitMod_mac_RGDP_best3$coefficients[-1]),
                AUC=AUC_logitMod_mac_RGDP_best3)]
# - Updating the counter value for the plotting dataset
k <- k+1
# - Cleaning up the environment
datCredit_valid[, `:=` (prob_mac_RGDP3=NULL, prob_mac_RGDP_best=NULL)]
rm(logitMod_mac_RGDP3, inputs_mac_RGDP_com, form_mac_RGDP_com, AUC_logitMod_mac_RGDP_best3)


# ---- 8.4 Clean up
# - Saving the selected variables to the disk
inputs_mac_RGDP_fin <- names(model.frame(logitMod_mac_RGDP_best1))[-1]
pack.ffdf(paste0(genObjPath, "Mac_RGDP_Formula"), inputs_mac_RGDP_fin); gc()
# - Saving the plotting dataset
pack.ffdf(paste0(genObjPath, "Mac_Models_Summary"), datPlot); gc()

# - Clean up
rm(logitMod_mac_RGDP_best1, logitMod_mac_RGDP_best2, logitMod_mac_RGDP_best3)




# ------ 9. Thematic variable selection analysis
# --- Loading in the plotting data
if (!exists('datPlot')) unpack.ffdf(paste0(genObjPath,"Mac_Models_Summary"), tempPath)

# --- Adjusting the dataset to enable easy annotations/labels
datPlot[, Label:=paste0(sprintf("%.1f",AUC*100),"%")]

# --- Plotting parameters
col.v <- brewer.pal(9, "Blues")[c(4,7,9)]
col.v2 <- rep(c(col.v[2], col.v[3], col.v[1]),6)
col.v3 <- rep("white", 18)
label.v <- c("Combined", '"Best" lag orders', '"Best" window lengths in aggregation (SD)')

# --- Plot
(g_mac_theme_sum <- ggplot(datPlot, aes(x=Base_Variable, y=AUC, group=Model)) +
    theme_minimal() + theme(legend.position = "bottom") + labs(x="Base Variable", y="AUC(%)") +
    geom_col(aes(colour=Model, fill=Model), position="dodge") +
    geom_label(aes(label=Label), fill = col.v2, colour = col.v3, position=position_dodge(0.9)) +
    scale_colour_manual(name="Model:", values=col.v, labels=label.v) +
    scale_fill_manual(name="Model:", values=col.v, labels=label.v) +
    scale_y_continuous(breaks=pretty_breaks(), label=percent))

# --- Save plot
dpi<-240
ggsave(g_mac_theme_sum, file=paste0(genFigPath, "MacroVars_Select_Combined_Themes_Summary.png"), width=3000/dpi, height=1000/dpi, dpi=dpi, bg="white")

# --- Clean up
rm(dpi, col.v, vol.v2, col.v3, label.v, datPlot, g_mac_theme_sum); gc()




# ------ 10. All macroeconomic variables as selected thematically and from the full macroeconomic input space
# ---- 10.1 Logit model with all variables, as selected from each thematically, combined
# --- 10.1.1 Full model
# - Loading in the selected variables from each macroeconomic variable (if not in memory)
if (!exists('inputs_mac_repo')) unpack.ffdf(paste0(genObjPath,"Mac_Repo_Formula"), tempPath)
if (!exists('inputs_mac_infl')) unpack.ffdf(paste0(genObjPath,"Mac_Infl_Formula"), tempPath)
if (!exists('inputs_mac_RinG')) unpack.ffdf(paste0(genObjPath,"Mac_RinG_Formula"), tempPath)
if (!exists('inputs_mac_DTI')) unpack.ffdf(paste0(genObjPath,"Mac_DTI_Formula"), tempPath)
if (!exists('inputs_mac_Emp')) unpack.ffdf(paste0(genObjPath,"Mac_EmpG_Formula"), tempPath)
if (!exists('inputs_mac_RGDP')) unpack.ffdf(paste0(genObjPath,"Mac_RGDP_Formula"), tempPath)
# - Constructing a formula containing all variables
ColNames7 <- c(inputs_mac_repo_fin, inputs_mac_infl_fin, inputs_mac_RinG_fin, inputs_mac_DTI_fin,
               inputs_mac_EmpG_fin, inputs_mac_RGDP_fin)
form_mac_final1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames7, collapse="+")))
# - Fitting the full model
logitMod_mac_final1 <- glm(form_mac_final1, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_final1)
### RESULTS: Null deviance = 279124; Residual deviance = 275677; AIC = 275729
# - Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_mac_final1)
### RESULTS: 1.23%
# - Variable importance
varImport_logit(logitMod_mac_final1, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_RealIncome_Growth_12], [M_RealIncome_Growth_9], and [M_Inflation_Growth_9]
# - ROC analysis
datCredit_valid[, prob_mac_final1 := predict(logitMod_mac_final1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_final1)
### RESULTS: 58.22%
# - Conduct a VIF analysis
(vif_mac_final1 <- car::vif(logitMod_mac_final1))

### RESULTS:    About halve of the variables are significant, and no variable has a VIF below 10 (as expected)
###             All fitted coefficients seem to have reasonable standard errors.

### CONLCUSION: Run a best subset selection to find the "optimal" set of macroeconomic variables from this combined input space.
###             Save formula for the combined variable selection script.

# ---- 10.1.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_final_best1 <- MASS::stepAIC(logitMod_mac_final1, direction="both")
# Start AIC = 275729
# End AIC = 275717.1
# - Deviance and AIC
summary(logitMod_mac_final_best1)
### RESULTS: Null deviance = 279124; Residual deviance = 275679; AIC = 275717
# - Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_mac_final_best1)
### RESULTS: 1.23%
# - Odds Ratio analysis 
round(exp(cbind(OR = coef(logitMod_mac_final_best1), confint.default(logitMod_mac_final_best1))), 3)
### RESULTS: All variables have an odds ratio that is relatively larger/smaller than one indicating that the variables are significant in predicting default
# - Residual deviance analysis
resid_deviance_glm(logitMod_mac_final_best1)
### RESULTS: Model fit is somewhat strained (1 diagnostics gave warnings)
# - Variable importance
varImport_logit(logitMod_mac_final_best1, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_RealIncome_Growth_12], [M_RealIncome_Growth_12], and [M_RealIncome_Growth_9]
# - ROC analysis
datCredit_train[, prob_mac_final_best1 := predict(logitMod_mac_final_best1, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_mac_final_best1 := predict(logitMod_mac_final_best1, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_mac_final_best1) 
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_final_best1)
### RESULTS: Training = 58.32%
###          Validation = 58.22%
(vif_mac_final_best1 <- car::vif(logitMod_mac_final_best1))
length(labels(terms(logitMod_mac_final1))); length(labels(terms(logitMod_mac_final_best1)))
### Results:  All variables are significant (although, some are just over the 0.05 threshold)
###           The all standard errors of the estimated coefficients seem reasonable.
###           The coefficient of determination is relatively weak at 1.23% and is exactly the same as the full model.
###           The residual deviance analysis indicates that the model fit is strenuous.
###           The AIC value of the best subset selection model is slightly lower than the full model (275717 vs 275729).
###           The AUC of the best subset selection model is identical to that of the reduced model using the validation dataset (58.22% vs 58.22%).
###           The AUCs of the best subset model as obtained by an ROC analysis on the training- and validation datasets are very similar (58.22% vs 58.32%) and thus there doesn't seem to be overfitting
###           The best subset selection reduces the total number of variables to 18, down from 25.

### CONCLUSION: Use the selected variables from the best subset selection model within the combined variable selection.
###             Save formula for the combined variable selection script.

# --- 10.1.3 Clean up
# - Saving the selected variables to the disk
inputs_mac_com_fin_theme <- names(model.frame(logitMod_mac_final_best1))[-1]
pack.ffdf(paste0(genObjPath, "Mac_Com_Theme_Formula"), inputs_mac_com_fin_theme); gc()
# - Cleaning up the environment
datCredit_valid[, prob_mac_final_best1:=NULL]; datCredit_train[, prob_mac_final_best1:=NULL]
rm(logitMod_mac_final1, logitMod_mac_final_best1, inputs_mac_com_fin_theme, vif_mac_final1, vif_mac_final_best1)



# ---- 10.2 Logit model with the full macroeconomic input space (non-thematic variable selection)
### NOTE: This input space excludes the volatilities as to align with the results from the thematic variable selection (which subsequently also excludes the volatilities)
# --- 9.2.1 Full model
# - Constructing a formula containing all variables
ColNames8 <- colnames(datCredit_train)[which(grepl("M_", colnames(datCredit_train)))]; ColNames8 <- ColNames8[which(!grepl("_SD_", ColNames8))]
form_mac_final2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames8, collapse="+")))
# - Fitting the full model
logitMod_mac_final2 <- glm(form_mac_final2, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_mac_final2)
### RESULTS: Null deviance = 279124 ; Residual deviance = 275644; AIC = 275730
# - Coefficient of determination
coefDeter_glm(logitMod_mac_final2)
### RESULTS: 1.25%
# - Odds Ratio analysis
varImport_logit(logitMod_mac_final2, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_RealIncome_Growth_12], [M_Emp_Growth], and [M_RealIncome_Growth]
# - ROC analysis
datCredit_valid[, prob_mac_final2 := predict(logitMod_mac_final2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_final2)
### RESULTS: 58.20%
# - Conduct a VIF analysis
(vif_mac_final2 <- car::vif(logitMod_mac_final2))

### RESULTS:    About halve of the variables are insignificant, and NO variable has a VIF below 10 (which is expected)
###             Some fitted coefficients seem to have unreasonably large standard errors.

### CONLCUSION: Run a best subset selection to find the "optimal" set of macroeconomic variables.
###             Save formula for the combined variable selection script.

# ---- 10.2.2 Best subset selection
# - Conducting the best subset procedure
logitMod_mac_final_best2 <- MASS::stepAIC(logitMod_mac_final2, direction="both")
# Start AIC = 275730.3
# End AIC = 275702.8
# - Deviance and AIC
summary(logitMod_mac_final_best2)
### RESULTS: Null deviance = 279124; Residual deviance = 275655; AIC = 275703
# - Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_mac_final_best2)
### RESULTS: 1.24%
# - Odds Ratio analysis 
round(exp(cbind(OR = coef(logitMod_mac_final_best2), confint.default(logitMod_mac_final_best2))), 3)
### RESULTS: All variables have an odds ratio that is relatively larger/smaller than one indicating that the variables are significant in predicting default
# - Residual deviance analysis
resid_deviance_glm(logitMod_mac_final_best2)
### RESULTS: Model fit is somewhat strained (1 diagnostic gave warnings)
# - Variable importance
varImport_logit(logitMod_mac_final_best2, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
### RESULTS: Top 3 variables: [M_RealIncome_Growth_12], [M_Repo_Rate_12], and [M_DTI_Growth_3]
# - ROC analysis
datCredit_train[, prob_mac_final_best2 := predict(logitMod_mac_final_best2, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_mac_final_best2 := predict(logitMod_mac_final_best2, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_mac_final_best2)
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_final_best2)
### RESULTS: Training dataset   = 58.37%
###          Validation dataset = 58.21%
(vif_mac_final_best2 <- car::vif(logitMod_mac_final_best2))
length(labels(terms(logitMod_mac_final2))); length(labels(terms(logitMod_mac_final_best2)))
### Results:    All variables are significant, however, some do have p-values just above to the threshold of 0.05.
###             The coefficient of determination is relatively weak for both the best subset selection and the full model at 1.25% and 1.24%, respectively.
###             The residual deviance analysis indicates that the model fit is a bit strenuous (one warning message).
###             The AIC value of the best subset selection model is lower than the full model (275703 vs 275730).
###             The AUC of the best subset selection model is almost identical to that of the full model on the validation dataset (58.21% vs 58.20%).
###             The AUCs of the best subset model, as obtained by an ROC analysis on the training- and validation datasets, are very similar (58.37% vs 58.21%) and thus there doesn't seem to be overfitting
###             The best subset model has 23 variables compared to the the full model that has 42 variables.

### CONCLUSION: Compare the input space of the "thematic-" and "full" models.


# --- 10.2.3 Clean up
# - Saving the selected variables to the disk
inputs_mac_com_fin_all <- names(model.frame(logitMod_mac_final_best2))[-1]
pack.ffdf(paste0(genObjPath, "Mac_Com_All_Formula"), inputs_mac_com_fin_all); gc()
# - Cleaning up the environment
datCredit_valid[, prob_mac_final_best1:=NULL]; datCredit_train[, prob_mac_final_best1:=NULL]
rm(logitMod_mac_final1, vif_mac_final1, vif_mac_final2, vif_mac_final_best1, vif_mac_final_best2)


# --- 10.3 Final comparison and conclusion on thematic variable selection
# - Refitting the models if they are not in memory
if (!exists("logitMod_mac_final_best1")) {
  unpack.ffdf(paste0(genObjPath, "Mac_Com_Theme_Formula"), tempPath)
  logitMod_mac_final_best1 <- glm(as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_mac_com_fin_theme, collapse = "+"))), data=datCredit_train, family="binomial")
}
if (!exists("logitMod_mac_final_best2")) {
  unpack.ffdf(paste0(genObjPath, "Mac_Com_All_Formula"), tempPath)
  logitMod_mac_final_best2 <- glm(as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_mac_com_fin_all, collapse = "+"))), data=datCredit_train, family="binomial")
}
# - Deviance and AIC
summary(logitMod_mac_final_best1); summary(logitMod_mac_final_best2)
# - Coefficient of determination
coefDeter_glm(logitMod_mac_final_best1); coefDeter_glm(logitMod_mac_final_best2)
# - ROC Analysis
datCredit_valid[, prob_mac_final_best1 := predict(logitMod_mac_final_best1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_mac_final_best2 := predict(logitMod_mac_final_best2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_final_best1) 
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_mac_final_best2)
# - Number of variables of the "thematic-" and "full" model
length(labels(terms(logitMod_mac_final_best1))); length(labels(terms(logitMod_mac_final_best2)))
# - Input space of the "thematic-" and "full" model
(logitMod_mac_final_best1_inputs <- labels(terms(logitMod_mac_final_best1))); (logitMod_mac_final_best2_inputs <-  labels(terms(logitMod_mac_final_best2)))
# - The shared variables
(shared_var <- logitMod_mac_final_best2_inputs %in% logitMod_mac_final_best1_inputs)
sum(shared_var); logitMod_mac_final_best2_inputs[shared_var]
# - The differing variables in the "thematic" model
(differ_var_theme <- !(logitMod_mac_final_best1_inputs %in% logitMod_mac_final_best2_inputs))
sum(differ_var_theme); logitMod_mac_final_best1_inputs[differ_var_theme]
# - The differing variables in the "full" model
(differ_var_full <- !(logitMod_mac_final_best2_inputs %in% logitMod_mac_final_best1_inputs))
sum(differ_var_full); logitMod_mac_final_best2_inputs[differ_var_full]
# - Variable importance
varImport_logit(logitMod_mac_final_best1, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)
varImport_logit(logitMod_mac_final_best2, method="stdCoef_Goodman", impPlot=T, sig_level=0.1)

### COMPARISON: The "thematic" model has a higher AIC than the "full" model (275717 vs 275703).
###             The "thematic" model has as slightly lower coefficient of determination than the "full" model (1.23% vs 1.24%).
###             The "thematic" model has a almost identical AUC than the "full" model (58.22% vs 58.21%) on the validation set.
###             The "thematic" model has less variables than the "full" model (18 vs 23).
###             The models share a lot of the higher lag-order variables, whilst the lower lag-order variables are quite different.
###             The number of shared variables is 13, more specifically the shared variables are:
###                 [M_Repo_Rate], [M_Emp_Growth], [M_RealIncome_Growth_2], ["M_DTI_Growth_3], [M_DTI_Growth_6], [M_Inflation_Growth_9], [M_RealGDP_Growth_9],
###                 [M_RealIncome_Growth_9], [M_Repo_Rate_12], [M_Inflation_Growth_12], [M_Emp_Growth_12], [M_RealGDP_Growth_12], [M_RealIncome_Growth_12]
###             The number of differing variables in the "thematic" model is 5, more specifically the differing variables in the "thematic" model are:
###                 [M_Repo_Rate_9], [M_Inflation_Growth_2], [M_DTI_Growth_1], [M_DTI_Growth_12], [M_DTI_Growth_9]
###             The number of differing variables in the "full" model is 10, more specifically the differing variables in the "full" model are:
###                 [M_DTI_Growth], [M_RealGDP_Growth], [M_RealIncome_Growth], [M_Emp_Growth_1], [M_RealGDP_Growth_1], [M_RealIncome_Growth_1],
###                 [M_RealIncome_Growth_2], [M_Inflation_Growth_6], [M_Emp_Growth_6], and [M_RealIncome_Growth_6]
###             The shared variables in the models have similar rankings in their importance; i.e., a "more important" variable in one model will be ranked the same in the other model.
###               - Of the 13 shared variables, 7 are ranked within 4 positions of each other in both models.
###             Analysis of the variables:
###               - 	The input space of the "thematic" model comprises mainly of recent values (1-3 months) of the macroeconomic variables and longer lagged version (9-12 months).
###                   This intuitively implies a correlation structure where today's macroeconomic conditions are driven by the conditions of approximately one year ago.
###               - 	The input space of the "full" model implies a similar, albeit more complex, correlation structure
###               -   The "thematic" model seems to capture this correlation structure with fewer variables (i.e., more simplistic), whilst yiedling almost identical performance of the "full" model.
###             The most "important" variables of the "thematic" model include: [M_RealIncome_Growth_12], [M_RealIncome_Growth_9], and [M_RealIncome_Growth_2]
###             The most "important" variables of the "full" model include:     [M_RealIncome_Growth_12], [M_Repo_Rate_12], and [M_DTI_Growth_3]

### CONCLUSION: The "thematic" model places high value on the real income growth variables, whilst the "full" model has a larger variety of what is considered to be "important"; therefore, the "full" model is more intuitive.
###             Due to the more simplisitc nature of the "thematic" model and its adequate prediction performance, it's input space is suggested for further use.


# --- 10.4 Clean up
datCredit_valid[, `:=` (prob_mac_final1=NULL, prob_mac_final2=NULL, prob_mac_final_best1=NULL, prob_mac_final_best2=NULL)]
rm(ColNames7, ColNames8, form_mac_final1, form_mac_final2, logitMod_mac_final1, logitMod_mac_final2, inputs_mac_com_fin_theme, inputs_mac_com_fin_all,
   vif_mac_final1, vif_mac_final2, vif_mac_final_best1, vif_mac_final_best2)


# --- 10.5 Variable importance comparison
# - Refitting the models if they are not in memory
if (!exists("logitMod_mac_final_best1")) {
  unpack.ffdf(paste0(genObjPath, "Mac_Com_Theme_Formula"), tempPath)
  logitMod_mac_final_best1 <- glm(as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_mac_com_fin_theme, collapse = "+"))), data=datCredit_train, family="binomial")
}
if (!exists("logitMod_mac_final_best2")) {
  unpack.ffdf(paste0(genObjPath, "Mac_Com_All_Formula"), tempPath)
  logitMod_mac_final_best2 <- glm(as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_mac_com_fin_all, collapse = "+"))), data=datCredit_train, family="binomial")
}

# - Getting the variable rankings
varImport_logit_Mod_mac_final_best1 <- varImport_logit(logitMod_mac_final_best1, method="stdCoef_Goodman", impPlot=T, sig_level=1)
varImport_logit_Mod_mac_final_best2 <- varImport_logit(logitMod_mac_final_best2, method="stdCoef_Goodman", impPlot=T, sig_level=1)
# - Getting the shared variables
shared_var2 <- inputs_mac_com_fin_all %in% inputs_mac_com_fin_theme
# - Create the plotting dataset
datPlot_varImport <- rbind(data.table(varImport_logit_Mod_mac_final_best1$data, Model="Thematic"),
                           data.table(varImport_logit_Mod_mac_final_best2$data, Model="Full"))
# - Creating a variable used for colour aesthetics in the graph
datPlot_varImport[Model=="Thematic" & Variable %in% inputs_mac_com_fin_all[shared_var2], Model:="Thematic - Shared"]
datPlot_varImport[Model=="Thematic", Model:="Thematic - Unique"]
datPlot_varImport[Model=="Full" & Variable %in% inputs_mac_com_fin_all[shared_var2], Model:="Full - Shared"]
datPlot_varImport[Model=="Full", Model:="Full - Unique"]
datPlot_varImport[, Model := factor(Model, levels=c("Thematic - Shared", "Full - Shared", "Thematic - Unique", "Full - Unique"))]
datPlot_varImport <- datPlot_varImport %>% arrange(Model)
# - Labels
datPlot_varImport[, Label:=Rank]
# - Plotting parameters
col.v <- brewer.pal(11, "Spectral")[c(1,9,2,8)]
col.v2 <- rep("white", datPlot_varImport[Model %in% c("Thematic -Shared", "Full - Shared"),.N]*2)
label.v <- c("Thematic - Shared"="Thematic - Shared", "Full - Shared"="Full - Shared", "Thematic - Unique"="Thematic - Unique", "Full - Unique"="Full - Unique")
# - Plot of shared variables | Absolute rank
(g_mac_theme_full_shared_comp <- ggplot(datPlot_varImport[Model %in% c("Thematic - Shared", "Full - Shared")], aes(x=Variable, y=Rank, group=Model)) +
    theme_minimal() + theme(legend.position = "bottom") + labs(x="Variable", y="Rank") +
    geom_col(aes(colour=Model, fill=Model), position="dodge") + coord_flip() +
    geom_label(aes(label=Label, fill=Model), colour = col.v2, position=position_dodge(0.9), show.legend = F) +
    scale_colour_manual(name="Model:", values=col.v, labels=label.v) +
    scale_fill_manual(name="Model:", values=col.v, labels=label.v) +
    scale_y_continuous(breaks=pretty_breaks()))
# Save plot
dpi<-240
ggsave(g_mac_theme_full_shared_comp, file=paste0(genFigPath, "MacroVars_Theme_Full_VarImport_Shared_Compare.png"), width=3000/dpi, height=1000/dpi, dpi=dpi, bg="white")
# Clean up
rm(col.v, col.v2, label.v, g_mac_theme_full_shared_comp); gc()
# - Plot of shared variables | Difference in rank
# Adjusting the plotting dataset to get the difference in ranks of the shared variables
datPlot_varImport2 <- datPlot_varImport[Model %in% c("Thematic - Shared", "Full - Shared"),list(Variable, Rank, Model)] %>%
  pivot_wider(names_from=Model, values_from = Rank) %>% as.data.table()
colnames(datPlot_varImport2) <- c("Variable", "Rank_Thematic", "Rank_Full")
datPlot_varImport2[, Rank_Diff:=Rank_Thematic-Rank_Full]
# Labels
datPlot_varImport2[, Label:=Rank_Diff]
# Plotting parameters
col.v <- brewer.pal(11, "Spectral")[c(1)]
col.v2 <- rep("white", datPlot_varImport2[,.N])
# Plot
(g_mac_theme_full_shared_comp2 <- ggplot(datPlot_varImport2, aes(x=Variable, y=Rank_Diff)) +
    theme_minimal() + theme(legend.position = "bottom") + labs(x="Variable", y="Rank Difference (Thematic Model Used as Reference)") +
    geom_col(position="dodge", col=col.v, fill=col.v) + coord_flip() +
    geom_label(aes(label=Label), fill=col.v, colour = col.v2, position=position_dodge(0.9)) +
    scale_colour_manual(name="Rank Difference of Thematic Model Relative to Full Model", values = col.v) + 
    scale_fill_manual(name="Rank Difference of Thematic Model Relative to Full Model", values=col.v) +
    scale_y_discrete(breaks=pretty_breaks()))
# Save plot
ggsave(g_mac_theme_full_shared_comp2, file=paste0(genFigPath, "MacroVars_Theme_Full_VarImport_Shared_Compare2.png"), width=3000/dpi, height=1000/dpi, dpi=dpi, bg="white")
# Clean up
rm(col.v, col.v2); gc()
# - Plot of unique variables' importance
# Plotting parameters
col.v <- brewer.pal(11, "Spectral")[c(1,9,2,8)]
col.v2 <- rep("white", datPlot_varImport[Model %in% c("Thematic - Unique", "Full - Unique"),.N])
label.v <- c("Thematic - Shared"="Thematic - Shared", "Full - Shared"="Full - Shared", "Thematic - Unique"="Thematic - Unique", "Full - Unique"="Full - Unique")
# Plot
(g_mac_theme_full_shared_comp3 <- ggplot(datPlot_varImport[Model %in% c("Thematic - Unique", "Full - Unique")], aes(x=Variable, y=Rank, group=Model)) +
    theme_minimal() + theme(legend.position = "bottom") + labs(x="Variable", y="Rank") +
    geom_col(aes(colour=Model, fill=Model), position="dodge") + coord_flip() +
    geom_label(aes(label=Label, fill=Model), col=col.v2, show.legend = F) +
    scale_colour_manual(name="Model:", values=col.v, labels=label.v) +
    scale_fill_manual(name="Model:", values=col.v, labels=label.v) +
    scale_y_continuous(breaks=pretty_breaks()))
# Save plot
ggsave(g_mac_theme_full_shared_comp3, file=paste0(genFigPath, "MacroVars_Theme_Full_VarImport_Shared_Compare3.png"), width=3000/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Clean up
rm(dpi, col.v, col.v2, label.v, datPlot_varImport, datPlot_varImport2,
   g_mac_theme_full_comp, g_mac_theme_full_comp2, g_mac_theme_full_shared_comp3); gc()


# --- 10.6 Clean up
rm(logitMod_mac_final_best1, logitMod_mac_final_best2, shared_var, shared_var2, differ_var_theme, differ_var_full,
   varImport_logit_Mod_mac_final_best1, varImport_logit_Mod_mac_final_best2); gc()




# ------ 11. Basic delinquency information
### Only [g0_Delinq] is considers as "basic" information as all other variables are derived from this variable and as such are considered as "advanced" variables
# --- 11.1 Testing the non-linear relationships within [g0_Delinq] - From suggested sub-themes
# - Fitting the model
logitMod_del_exp1_1 <- glm(DefaultStatus1_lead_12_max ~ g0_Delinq
                           , data=datCredit_train, family="binomial")
logitMod_del_exp1_2 <- glm(DefaultStatus1_lead_12_max ~ g0_Delinq_fac
                           , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_del_exp1_1)
### RESULTS: Null deviance = 279124; Residual deviance = 220709; AIC = 220713
summary(logitMod_del_exp1_2)
### RESULTS: Null deviance = 279124; Residual deviance = 219954; AIC = 219960
# - ROC analysis
datCredit_valid[, prob_del_exp1_1 := predict(logitMod_del_exp1_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_del_exp1_2 := predict(logitMod_del_exp1_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del_exp1_1)
### RESULTS: 74.08749%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del_exp1_2)
### RESULTS:    74.08749%
### COMPARISON: Model with factorised delinquency variable has lower AIC (219960 vs 220713) and the same AUC (74.08749% vs 74.08749%) than the model with the raw variable 
### COMCLUSION: Use [g0_Delinq] as it has equal predictive power and results in a slightly more parsimonious model (1- vs 2 variables)

# --- 11.2 Full model analysis
# - Fitting the model
logitMod_del1 <- glm(DefaultStatus1_lead_12_max ~ g0_Delinq,
                     data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_del1)
### RESULTS: Null deviance = 279124; Residual deviance = 220709; AIC = 220713
# Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_del1)
### RESULTS: 20.93%
# Odds Ratio analysis 
round(exp(cbind(OR = coef(logitMod_del1), confint.default(logitMod_del1))), 3)
### RESULTS: [g0_Delinq] has a ratio that is relatively large implying that the variable is significant to predicting default
# Residual deviance analysis
resid_deviance_glm(logitMod_del1)
### RESULTS: Model fit is somewhat strained (1 diagnostics gave warnings)
# Variable importance
### Variable importance analysis omitted since there is only one variable in the model
# ROC analysis
datCredit_train[, prob_del1 := predict(logitMod_del1, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_del1 := predict(logitMod_del1, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_del1)
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del1)
### RESULTS: Training dataset = 73.83%
###          Validation dataset = 74.09%

# - Clean up
rm(logitMod_del_exp1_1, logitMod_del_exp1_2, logitMod_del1); gc()
datCredit_train[,prob_del1:=NULL]; datCredit_valid[, `:=` (prob_del_exp1_1=NULL, prob_del_exp1_2=NULL, prob_del1=NULL)]




# ------ 12. Combine themes - Macroeconomic information + basic delinquency information - and screen for insignificant variables| Final model (intermediate)
# --- 12.1 Preliminaries
# - Loading in variables
unpack.ffdf(paste0(genObjPath, "Mac_Com_Theme_Formula"), tempPath) # Macroeconomic variables


# --- 12.2 Screening for insignificant variables
# - Fit full logit model with all previously chosen macroeconomic information and the basic delinquency information variable
logitMod_full1 <- glm(as.formula(paste("DefaultStatus1_lead_12_max~", paste("g0_Delinq", collapse="+"), "+", paste(inputs_mac_com_fin_theme, collapse="+")))
                      , data=datCredit_train, family="binomial")
# Assess full model
summary(logitMod_full1)
### RESULTS:    Insignificant variables: [M_Repo_Rate], [M_Repo_Rate_9], [M_Inflation_Growth_12], [M_DTI_Growth_12], and [M_RealGDP_Growth_9]
### CONCLUSION: Remove insignificant variables and refit model

# - Refit model excluding insignificant variables
logitMod_full2 <- glm(as.formula(paste("DefaultStatus1_lead_12_max~", paste("g0_Delinq", collapse="+"), "+",
                                       paste(inputs_mac_com_fin_theme[-unlist(lapply(c("M_Repo_Rate", "M_Repo_Rate_9", "M_Inflation_Growth_12", "M_DTI_Growth_12", "M_RealGDP_Growth_9"), function(X) which(X==inputs_mac_com_fin_theme)))], collapse="+")))
                      , data=datCredit_train, family="binomial") 
# Assess full model
summary(logitMod_full2)
### RESULTS:    Insignificant variables: [M_Emp_Growth]
### CONCLUSION: Remove insignificant variables and refit model

# - Refit model excluding insignificant variables
logitMod_full3 <- glm(as.formula(paste("DefaultStatus1_lead_12_max~", paste("g0_Delinq", collapse="+"), "+",
                                       paste(inputs_mac_com_fin_theme[-unlist(lapply(c("M_Repo_Rate", "M_Repo_Rate_9", "M_Inflation_Growth_12", "M_DTI_Growth_12", "M_RealGDP_Growth_9", "M_Emp_Growth"), function(X) which(X==inputs_mac_com_fin_theme)))], collapse="+")))
                      , data=datCredit_train, family="binomial")
# Assess full model
summary(logitMod_full3)
### RESULTS:    Insignificant variables: None
### CONCLUSION: Proceed to subsampling

# - Save preliminary formula
inputs_int_pre <- formula(logitMod_full3)


# --- 12.3. Subsampling and refitting the final model(s)
# - Preliminaries
# Field names
stratifiers <- c("DefaultStatus1_lead_12_max", "Date") # Must at least include target variable used in graphing event rate
targetVar <- "DefaultStatus1_lead_12_max"
# Subsampling & resampling parameters
smp_size <- 250000 # fixed size of downsampled set
smp_perc_train <- smp_size / ( datCredit_train[complete.cases(mget(stratifiers)), mget(stratifiers)][,.N] ) # Implied sampling fraction for downsampling step
smp_perc_valid <- smp_size / ( datCredit_valid[complete.cases(mget(stratifiers)), mget(stratifiers)][,.N] ) # Implied sampling fraction for downsampling step

# - Downsample data into a set with a fixed size (using stratified sampling) before implementing resampling scheme
set.seed(1)
# Training dataset
datCredit_train_smp <- datCredit_train %>%group_by(across(all_of(stratifiers))) %>% slice_sample(prop=smp_perc_train) %>% as.data.table()
cat( (datCredit_train_smp[is.na(get(targetVar)), .N] == 0) %?% 'SAFE: No missingness in target variable.\n' %:% 
       'WARNING: Missingness detected in target variable.\n')
### RESULTS: Subasmpling training dataset is successful.
# Validation dataset
datCredit_valid_smp <- datCredit_valid %>%group_by(across(all_of(stratifiers))) %>% slice_sample(prop=smp_perc_valid) %>% as.data.table()
cat( (datCredit_valid_smp[is.na(get(targetVar)), .N] == 0) %?% 'SAFE: No missingness in target variable.\n' %:% 
       'WARNING: Missingness detected in target variable.\n')
### RESULTS: Subasmpling validation dataset is successful.

# - Refit models to subsampled dataset
logitMod_smp <- glm(inputs_int_pre, data=datCredit_train_smp, family="binomial")
# Deviance and AIC
summary(logitMod_smp)
### RESULTS:    Insignificant variables are: [M_DTI_Growth_1], [M_DTI_Growth_6], [M_Emp_Growth_9], [M_Emp_Growth_12]
### CONCLUSION: Refit model on the full training dataset whilst excluding insignificant variables and assess the exclusion's impact on model performance

# - Refit model with new input space on full training dataset and subsampled training dataset
# Adjust formula
inputs_int <- as.formula(paste0("DefaultStatus1_lead_12_max ~ ",paste(labels(terms(inputs_int_pre))[-c(which(labels(terms(inputs_int_pre)) %in% c("M_DTI_Growth_1", "M_DTI_Growth_6", "M_Emp_Growth_9", "M_Emp_Growth_12")))], collapse=" + ")))
logitMod_post <- glm(inputs_int, data=datCredit_train, family="binomial")
logitMod_smp_post <- glm(inputs_int, data=datCredit_train_smp, family="binomial")
# Model assessment
summary(logitMod_post)
### RESULTS: Insignificant variable(s): [M_Inflation_Growth_2]
summary(logitMod_smp_post)
### RESULTS: Insignificant variable(s): [M_Inflation_Growth_2]
### CONCLUSION: Refit model excluding insignificant variable(s)

# - Refit model with new input space
# Adjust Formula
inputs_int2 <- as.formula(paste0("DefaultStatus1_lead_12_max ~ ",paste(labels(terms(inputs_int))[-c(which(labels(terms(inputs_int)) %in% c("M_Inflation_Growth_2")))], collapse=" + ")))
logitMod_post2 <- glm(inputs_int2, data=datCredit_train, family="binomial")
logitMod_smp_post2 <- glm(inputs_int2, data=datCredit_train_smp, family="binomial")
# Model assessment
summary(logitMod_post2)
### RESULTS: Insignificant variable(s): None
summary(logitMod_smp_post2)
### RESULTS: Insignificant variable(s): None
### CONCLUSION: Proceed assess model

# - AUC comparison
datCredit_valid[, prob_int_full_pre := predict(logitMod_full3, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_int_full := predict(logitMod_post2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_int_full_pre)
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_int_full)
### RESULTS: AUC = 77.60% for model on full validation set
###          AUC = 77.60% for model on subsampled validation set
### CONCLUSION: Variable reduction has no results on model predictability, save new formula as intermediate model formula and proceed to final model assessment

# - Save formula
inputs_int <- inputs_int2
pack.ffdf(paste0(genObjPath, "Int_Formula"), inputs_int); gc()


# --- 12.4 Clean up
rm(stratifiers, targetVar, smp_size, smp_perc_train, smp_perc_valid, datCredit_train_smp, datCredit_valid_smp, inputs_int_pre, inputs_mac_com_fin_theme,
   inputs_int2, logitMod_full1, logitMod_full2, logitMod_full3, logitMod_post, logitMod_smp_post, logitMod_post2, logitMod_smp_post2); gc()




# ------ 13 Assess final model
# --- 13.1 Fit model
# - Preliminaries
# Load data
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)
# Subset to exclude default spells
datCredit_train <- datCredit_train %>% subset(DefaultStatus1==0)
datCredit_valid <- datCredit_valid %>% subset(DefaultStatus1==0)
# Load formula
unpack.ffdf(paste0(genObjPath, "Int_Formula"), tempPath)
if (!exists('inputs_int')) {
  inputs_int <- DefaultStatus1_lead_12_max ~ g0_Delinq + M_Repo_Rate_12 + M_Inflation_Growth_9 + 
    M_RealIncome_Growth_2 + M_RealIncome_Growth_9 + M_RealIncome_Growth_12 + M_DTI_Growth_3 + M_RealGDP_Growth_12
  # save formula
  pack.ffdf(paste0(genObjPath, "Int_Formula"), inputs_int)
}

# - Fit model
logitMod_Int <- glm(inputs_int, data=datCredit_train, family="binomial")


# --- 13.2 Assessment
# - Deviance and AIC
summary(logitMod_Int)
### RESULTS: Null deviance = 279124; Residual deviance = 219678; AIC = 219696

# - Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_Int)
### RESULTS: 21.3%

# - Odds Ratio analysis 
round(exp(cbind(OR = coef(logitMod_Int), confint.default(logitMod_Int))), 3)
### RESULTS: All variables have an odds ratio that is relatively larger/smaller than one indicating that the variables are significant in predicting default

# - Residual deviance analysis
resid_deviance_glm(logitMod_Int)
### RESULTS: Model fit is somewhat strained (1 diagnostics gave warnings)

# - Variable importance
varImport_logit(logitMod_Int, method="stdCoef_Goodman", sig_level=0.1, impPlot=T, plotVersionName="Intermediate")
### RESULTS: Top three variables: [g0_Delinq], [M_Repo_Rate_12], and [M_DTI_Growth_3]

# - ROC analysis
datCredit_train[, prob_final := predict(logitMod_Int, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_final := predict(logitMod_Int, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_final)
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_final)
### RESULTS: Training dataset = 77.42%
###          Validation dataset = 77.6%

# - VIF analysis
car::vif(logitMod_Int)
### RESULTS:  [M_RealIncome_Growth_9], [M_RealIncome_Growth_12], and [M_RealGDP_Growth_12] have VIFs > 10

# - Clean up
rm(logitMod_Int); gc()
datCredit_train[,prob_final:=NULL]; datCredit_valid[,prob_final:=NULL]

# - Results & Conclusion
### RESULTS:    All variables are significant and have reasonable standard errors.
###             The coefficient of determination is relatively good at 21.30%.
###             The residual deviance analysis indicates that the model fit is strenuous (1 warning).
###             Model is not overfitted as evidenced by the small change in AUC when a ROC analysis is conducted on the training- and validation datasets (77.42% vs 77.60%)
###             The VIF values are as expected, with three macroeconomic variables having high VIF values (>10)
