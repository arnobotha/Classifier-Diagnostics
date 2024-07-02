# ========================== MODEL DEFAULT RISK - BASIC =================================
# Develop several "basic" logistic regression models ranging from models with few
# features to models with more features to predict default risk. Delinquency-, portfolio-level-,
# and forward looking information is not considers among these "basic" variables.
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Marcel Muller, Dr Arno Botha

# DESCRIPTION:
# This script uses the previously prepared credit dataset to create multiple "basic" 
# logistic regression models for predicting default. The focus of this
# script is on core/basic variables that one would expect to find in any credit dataset.
# These variables are divided into themes and a "best" set of variables is chosen for
# each theme. The themes are combined in the end in selecting the "best" set of core/basic
# variables. Note that delinquency-, forward-looking-, and portfolio-level information is
# excluded in this script (see the intermediate- and advanced model scripts).
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 0a.CustomFunctions.R
#   - 3b.Data_Subsample_Fusion2
#
# -- Inputs:
#   - datCredit_train | Prepared credit data from script 3b
#   - datCredit_valid | Prepared credit data from script 3b
#
# -- Outputs:
#   - Sets of core/basic variables as selected thematically and combined.
# =======================================================================================




# ------ 1. Preliminaries

ptm <- proc.time() # for runtime calculations (ignore)

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)

# - Subset to exclude default spells
datCredit_train <- datCredit_train %>% subset(DefaultStatus1==0)
datCredit_valid <- datCredit_valid %>% subset(DefaultStatus1==0)




# ------ 2. Modelling & Feature Selection by theme "core information" (mostly account-level information)
### Modelling Tutorials: https://stats.oarc.ucla.edu/r/dae/logit-regression/
###                      https://stackoverflow.com/questions/63748817/odds-ratio-and-95-ci-for-interaction-in-logistic-model-in-r
###                      https://stackoverflow.com/questions/41384075/r-calculate-and-interpret-odds-ratio-in-logistic-regression

# --- 2.1 Correlation analysis using Spearman correlation
# - Correlation threshold
cor_thresh <- 0.6
# - Computing a correlation matrix
cor_cor_spear <- cor(x=datCredit_train[,list(Age_Adj, Term, Balance_Real, PerfSpell_Num, InterestRate_Margin_imputed_mean,
                                             Instalment_Real, Principal_Real, AgeToTerm, BalanceToPrincipal)],
                     method = "spearman")
# - Creating a correlalogram
corrplot(cor_cor_spear, type="upper")
# - Getting the lower diagonal of the correlation matrix (since the matrix is symmetrical around the diagonal)
cor_cor_spear[lower.tri(cor_cor_spear)] <- 0
# - Printing all correlations above the specified threshold
ind_cor_spear <- which(abs(cor_cor_spear)>cor_thresh & abs(cor_cor_spear)<1, arr.ind = T)
cor_cor_spear2 <- data.table(x=rownames(cor_cor_spear)[ind_cor_spear[,1]],
                             y=colnames(cor_cor_spear)[ind_cor_spear[,2]])
for(i in 1:(nrow(cor_cor_spear2))) {cat("Absolute correlation above ", cor_thresh, " found for ", cor_cor_spear2[i,1][[1]], " and ", cor_cor_spear2[i,2][[1]], "\n")}
### RESULTS: High-correlation detected within [Age_Adj], [AgeToTerm], [Balance_Real], [BalanceToPrincipal], [Instalment_Real], and [Principal_Real]

### Conclusion:
###   Remove either [Age_Adj] or [AgeToTerm]
###   Remove two of [Principal_Real], [Balance_Real] and [Instalment_Real]
###   Remove [BalanceToPrincipal]; Too many high-correlations with other variables


# --- 2.2 Experimenting (using insights from correlation analysis) and subthemes
# - [Age_Adj] vs [TimeInPerfSpell] - From the insights of the correlation analysis
# Model fitting
logitMod_cor_exp1_1 <- glm(DefaultStatus1_lead_12_max ~ Age_Adj
                           , data=datCredit_train, family="binomial")
logitMod_cor_exp1_2 <- glm(DefaultStatus1_lead_12_max ~ AgeToTerm
                           , data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_cor_exp1_1)
### RESULTS: Null deviance = 279124  ; Residual deviance = 278526  ; AIC = 278530
summary(logitMod_cor_exp1_2)
### RESULTS: Null deviance = 279124  ; Residual deviance = 278831  ; AIC = 278835
# ROC analysis
datCredit_valid[, prob_cor_exp1_1 := predict(logitMod_cor_exp1_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_cor_exp1_2 := predict(logitMod_cor_exp1_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_cor_exp1_1)
## RESUTLS: 51.612%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_cor_exp1_2)
### RESULTS: 51.614%
### COMPARISON: [Age_Adj] and [AgeToTerm]  have almost identical predictive strength, the model fit
###             with [Age_Adj] does however result in a slightly better fit.
### CONCLUSION: Use [Age_Adj].

# - [Balance] vs [Instalment] vs [Principal] - From the insights of the correlation analysis
# Model fitting
logitMod_cor_exp2_1 <- glm(DefaultStatus1_lead_12_max ~ Balance_Real
                           , data=datCredit_train, family="binomial")
logitMod_cor_exp2_2 <- glm(DefaultStatus1_lead_12_max ~ Instalment_Real
                           , data=datCredit_train, family="binomial")
logitMod_cor_exp2_3 <- glm(DefaultStatus1_lead_12_max ~ Principal_Real
                           , data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_cor_exp2_1)
### RESULTS: Null deviance = 279124; Residual deviance = 279097; AIC = 279101
summary(logitMod_cor_exp2_2)
### RESULTS: Null deviance = 279124; Residual deviance = 279123; AIC = 279127
summary(logitMod_cor_exp2_3)
### RESULTS: Null deviance = 279124; Residual deviance = 278738; AIC = 278742
# ROC analysis
datCredit_valid[, prob_cor_exp2_1 := predict(logitMod_cor_exp2_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_cor_exp2_2 := predict(logitMod_cor_exp2_2, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_cor_exp2_3 := predict(logitMod_cor_exp2_3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_cor_exp2_1)
### RESULTS: 48.93%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_cor_exp2_2)
### RESULTS: 47.98%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_cor_exp2_3)
### RESULTS:    55.75%
### CONCLUSION: Both [Balance_Real] and [Instalment_Real] are period-level variables, whilst [Principal_Real] is an account-level variable. Therefore, keep [Principal_Real] and decide between the other two period-level variables.
###             [Balance_Real] and [Instalment_Real] result in model with AUCs that have reversed in direction,
###             which might be due to possible non-linearity within certain "buckets"; this isssue is resolved within script 3c(vi)
###             Deciding between the two variables proceeds from a practical and intuitive perspective:
###               - [Principal_Real] and [Balance_Real] give a perspective on where the loan account is currently within its life cycle and may thus be preferred.
###               - [Principal_Real] and [Instalment_Real] may on the other hand give a perspective on the loan facility's size, and also risk-based pricing principles.
###               - The inclusion of [Instalment_Real] may result in "double counting" when combined with [g0_Delinq] since the latter is calculated using the former.
###             [Principal_Real] and [Balance_Real] are selected.

# - [InterestRate_Margin_imputed_mean] vs [InterestRate_Margin_bin] - From suggested sub-themes
# Model fitting
logitMod_cor_exp_3_1 <- glm(DefaultStatus1_lead_12_max ~ InterestRate_Margin_imputed_mean
                            , data=datCredit_train, family="binomial")
logitMod_cor_exp_3_2 <- glm(DefaultStatus1_lead_12_max ~ InterestRate_Margin_imputed_bin
                            , data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_cor_exp_3_1)
### RESULTS: Null deviance = 279124; Residual deviance = 277321; AIC = 277325
summary(logitMod_cor_exp_3_2)
### RESULTS: Null deviance = 279124; Residual deviance = 277049; AIC = 277055
# ROC analysis
datCredit_valid[, prob_cor_exp3_1 := predict(logitMod_cor_exp_3_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_cor_exp3_2 := predict(logitMod_cor_exp_3_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_cor_exp3_1)
## RESULTS; 57.22%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_cor_exp3_2)
### RESULTS:    56.75%
### COMPARISON: Model with binned variable has lower AIC (277055 vs 277325), but has a lower AUC than the model with the raw (imputed) variable (57.22% vs 56.75%).
### CONCLUSION: Use [InterestRate_Margin_imputed_mean].

# - Clean up
rm(logitMod_cor_exp1_1, logitMod_cor_exp1_2, logitMod_cor_exp2_1, logitMod_cor_exp2_2, logitMod_cor_exp2_3, logitMod_cor_exp_3_1, logitMod_cor_exp_3_2); gc()
datCredit_valid[,`:=` (prob_cor_exp1_1=NULL, prob_cor_exp1_2=NULL, prob_cor_exp2_1=NULL, prob_cor_exp2_2=NULL, prob_cor_exp2_3=NULL, prob_cor_exp3_1=NULL, prob_cor_exp3_2=NULL)]


# --- 2.3 Full logit model with all account-level information
# NOTE: Certain variables are excluded using insights from previous analysis: [AgeToTerm]; [Instalment]; [InterestRate_Margin_imputed_bin]
logitMod_cor1 <- glm(DefaultStatus1_lead_12_max ~ Age_Adj + Term + PerfSpell_Num +
                       InterestRate_Margin_imputed_mean + Principal_Real + Balance_Real
                     , data=datCredit_train, family="binomial")
### RESULTS: No insignificant variables
### CONCLUSION: Proceed with variable selection.
# - Analysis
# Deviance and AIC
summary(logitMod_cor1)
### RESULTS: Null deviance = 279124; Residual deviance = 272800; AIC = 262814
# Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_cor1)
### RESULTS: 5.85%
# Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_cor1), confint.default(logitMod_cor1))), 3)
### RESULTS: odds ratios of [Term], [Principal_Real], and [Balance_Real] are all close to 1, which limits their usefulness (however, the range of these variables may be influencing this result; a standardised odds ratio may be more insightful)
# Residual deviance analysis
resid_deviance_glm(logitMod_cor1)
### RESULTS: Model fit is strained (3 diagnostics gave warnings)
# Variable importance
(varImport <- varImport_logit(logitMod_cor1, method="stdCoef_ZScores", impPlot=T))
### RESULTS: Top three variables: [Principal_Real], [Balance_Real], and [PerfSpell_Num]
# ROC analysis
datCredit_valid[, prob_cor1 := predict(logitMod_cor1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_cor1)# datCredit_valid[prob_cor1==0,.N]; datCredit_valid[prob_cor1==1,.N] # There are no probabilities that are exactly equal to 0 or 1
### RESULTS: 69.96%
### CONCLUSION: Proceed to best subset selection


# --- 2.4 Best subset selection
# - Best subset selection
logitMod_cor_best <- MASS::stepAIC(logitMod_cor1, direction="both")
### WARNING:   glm.fit: fitted probabilities numerically 0 or 1
# Start AIC = 262814.3
# End AIC = 262814.3
summary(logitMod_cor_best)
### RESULTS:    Entire input space returned after have been run through the best subset selection
### CONCLUSION: Reperform analysis (as in section 2.3); helpful when the resulting input space differs from full "inputted" model

# - Model analysis
# Deviance and AIC
summary(logitMod_cor_best)
### RESULTS: Null deviance = 279124; Residual deviance = 272800; AIC = 262814
# Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_cor_best)
### RESULTS: 5.85%
# Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_cor_best), confint.default(logitMod_cor_best))), 3)
### RESULTS: odds ratios of [Term], [Principal_Real], and [Balance_Real] are all close to 1, which limits their usefulness (however, the range of these variables may be influencing this result; a standardised odds ratio may be more insightful)
# Residual deviance analysis
resid_deviance_glm(logitMod_cor_best)
### RESULTS: Model fit is strained (3 diagnostics gave warnings)
# Variable importance
(varImport <- varImport_logit(logitMod_cor_best, method="stdCoef_ZScores", impPlot=T))
### RESULTS: Top three variables: [Principal_Real], [Balance_Real], and [PerfSpell_Num]
# ROC analysis
datCredit_valid[, prob_cor_best := predict(logitMod_cor_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_cor_best)# datCredit_valid[prob_cor_best==0,.N]; datCredit_valid[prob_cor_best==1,.N] # There are no probabilities that are exactly equal to 0 or 1
### RESULTS: 69.96%
### CONCLUSION: Proceed to combining all basic modelling themes

# - Clean up
rm(logitMod_cor1, logitMod_cor_best); gc()
datCredit_valid[,`:=`(prob_cor_best=NULL)]

# - Save preliminary formula
inputs_cor_pre <-  DefaultStatus1_lead_12_max ~ Age_Adj + Term + PerfSpell_Num + InterestRate_Margin_imputed_mean + Principal_Real + Balance_Real




# ------ 3. Combine themes (account level information) and screen for insignificant variables| Final model (intermediate)
# --- 3.1 Screening for insignificant variables
# - Create formula
inputs_bas_pre <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(labels(terms(inputs_cor_pre)), collapse="+")))

# - Fit full model with all previously chosen core variables
logitMod_bas1 <- glm(inputs_bas_pre, data=datCredit_train, family="binomial")
# Assess model
summary(logitMod_bas1)
### RESULTS: Insignificant variables: None
### CONCLUSION: Subsample data and refit model to ensure selected variables are adequate


# --- 3.4 Subsampling and refitting the final model(s)
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
logitMod_smp <- glm(inputs_bas_pre, data=datCredit_train_smp, family="binomial")
# Deviance and AIC
summary(logitMod_smp)
### RESULTS:    Insignificant variables are: None
### CONCLUSION: Save formula as basic model formula and proceed to final model assessment

# - Save formula
inputs_bas <- inputs_bas_pre
pack.ffdf(paste0(genObjPath, "Basic_Com_Formula"), inputs_bas); gc()


# --- 3.5 Clean up
rm(stratifiers, targetVar, smp_size, smp_perc_train, smp_perc_valid, datCredit_train_smp, datCredit_valid_smp, inputs_bas_pre,
   logitMod_bas1, logitMod_cor1, logitMod_full1, logitMod_smp); gc()




# ------ 4. Assess final model
# --- 4.1 Fit model
# - Preliminaries
# Load data
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)
# Subset to exclude default spells
datCredit_train <- datCredit_train %>% subset(DefaultStatus1==0)
datCredit_valid <- datCredit_valid %>% subset(DefaultStatus1==0)
# Load formula
unpack.ffdf(paste0(genObjPath, "Basic_Com_Formula"), tempPath)
if (!exists('inputs_bas')) {
  inputs_bas <- DefaultStatus1_lead_12_max ~ Age_Adj + Term + PerfSpell_Num + InterestRate_Margin_imputed_mean + Principal_Real + Balance_Real
  pack.ffdf(paste0(genObjPath, "Basic_Com_Formula"), inputs_bas)
}

# - Fit model
logitMod_Basic  <- glm(inputs_bas, data=datCredit_train, family="binomial")


# --- 4.2 Assessment
# - Deviance and AIC
summary(logitMod_Basic )
### RESULTS: Null deviance = 279124; Residual deviance = 262800; AIC = 262814

# - Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_Basic )
### RESULTS: 5.85%

# - Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_Basic ), confint.default(logitMod_Basic ))), 3)
### RESULTS: odds ratios of [Term], [Principal_Real], and [Balance_Real] are all close to 1, which limits their usefulness (however, the range of these variables may be influencing this result; a standardised odds ratio may be more insightful)

# - Residual deviance analysis
resid_deviance_glm(logitMod_Basic )
### RESULTS: Model fit is strained (3 diagnostics gave warnings)

# - Variable importance
varImport_logit(logitMod_Basic , method="stdCoef_Goodman", sig_level=0.1, impPlot=T, plotVersionName="Basic")
### RESULTS: Top three variables: [PerfSpell_Num], [Balance_Real], and [Principal_Real]

# - ROC analysis
datCredit_train[, prob_bas := predict(logitMod_Basic , newdata = datCredit_train, type="response")]
datCredit_valid[, prob_bas := predict(logitMod_Basic , newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_bas)
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_bas)
### RESULTS: Training dataset = 69.73%
###          Validation dataset = 69.96%

# - VIF analysis
car::vif(logitMod_Basic )
### RESULTS:  [Principal_Real] and [Balance_Real] have a high VIF (> 10), which is expected

# - Clean up
rm(inputs_fin_bas, logitMod_Basic ); gc()
datCredit_train[,prob_bas:=NULL]; datCredit_valid[,prob_bas:=NULL]

### RESULTS:  All variables are significant and have reasonable standard errors.
###           The coefficient of determination is relatively good at 5.76%.
###           The residual deviance analysis indicates that the model fit is strenuous.
###           NA single VIF between two associated variables is above 10, but it is expected and of no concern.
###           Model is not overfitted as evidenced by almost identical AUCs when a ROC analysis is conducted on the training- and validation datasets (69.66% vs 69.87%)

### FINAL SELECTION: [Age_Adj], [Term], [PerfSpell_Num], [InterestRate_Margin_imputed_mean],
###                  [Principal_Real], [Balance_Real]

### CONCLUSION: Proceed to variable selection for the intermediate model
