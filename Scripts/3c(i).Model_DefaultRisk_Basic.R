# ========================== MODEL DEFAULT RISK - BASIC =================================
# Develop several "basic" logistic regression models ranging from models with few
# features to models with more features to predict default risk. Delinquency-, portfolio-level-,
# and forward looking information is not considers among these "basic" variables.
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha, Marcel Muller

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




# ------ 2. Modelling & Feature Selection by theme "Account-level information"'
# Modelling Tutorials: https://stats.oarc.ucla.edu/r/dae/logit-regression/
# https://stackoverflow.com/questions/63748817/odds-ratio-and-95-ci-for-interaction-in-logistic-model-in-r
# https://stackoverflow.com/questions/41384075/r-calculate-and-interpret-odds-ratio-in-logistic-regression

# --- 2.1 Correlation analysis using Spearman correlation
# - Correlation threshold
cor_thresh <- 0.6
# - Computing a correlation matrix
cor_ali_spear <- cor(x=datCredit_train[,list(Age_Adj, Term, Balance, InterestRate_Margin_imputed_mean,
                                             Instalment, Principal, AgeToTerm, BalanceToPrincipal)],
                    method = "spearman")
# - Creating a correlalogram
corrplot(cor_ali_spear, type="upper")
# - Getting the lower diagonal of the correlation matrix (since the matrix is symmetrical around the diagonal)
cor_ali_spear[lower.tri(cor_ali_spear)] <- 0
# - Printing all correlations above the specified threshold
ind_ali_spear <- which(abs(cor_ali_spear)>cor_thresh & abs(cor_ali_spear)<1, arr.ind = T)
cor_ali_spear2 <- data.table(x=rownames(cor_ali_spear)[ind_ali_spear[,1]],
                            y=colnames(cor_ali_spear)[ind_ali_spear[,2]])
for(i in 1:(nrow(cor_ali_spear2))) {cat("Absolute correlation above ", cor_thresh, " found for ", cor_ali_spear2[i,1][[1]], " and ", cor_ali_spear2[i,2][[1]], "\n")}
### RESULTS: High-correlation detected within [Age_Adj], [AgeToTerm], [Balance], [BalanceToPrincipal], [Instalment], and [Principal]

### Conclusion:
###   Remove either [Age_Adj] or [AgeToTerm]
###   Remove two of [Principal], [Balance] and [Instalment]
###   Remove [BalanceToPrincipal]; Too many high-correlations with other variables


# --- 2.2 Experimenting (using insights from correlation analysis) and subthemes
# - [Age_Adj] vs [TimeInPerfSpell] - From the insights of the correlation analysis
# Model fitting
logitMod_ali_exp1_1 <- glm(DefaultStatus1_lead_12_max ~ Age_Adj
                           , data=datCredit_train, family="binomial")
logitMod_ali_exp1_2 <- glm(DefaultStatus1_lead_12_max ~ AgeToTerm
                           , data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_ali_exp1_1)
### RESULTS: Null deviance = 275184  ; Residual deviance = 274579  ; AIC = 274583
summary(logitMod_ali_exp1_2)
### RESULTS: Null deviance = 275184  ; Residual deviance = 274940  ; AIC = 274944
# ROC analysis
datCredit_valid[, prob_ali_exp1_1 := predict(logitMod_ali_exp1_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_ali_exp1_2 := predict(logitMod_ali_exp1_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp1_1)
## RESUTLS: 51.112%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp1_2)
### RESULTS: 51.1172%
### COMPARISON: [Age_Adj] and [AgeToTerm]  have almost identical predictive strength, the model fit
###             with [AgeToTerm] does however result in a slightly better fit.
### CONCLUSION: Use [Age_Adj].

# - [Balance] vs [Instalment] vs [Principal] - From the insights of the correlation analysis
# Model fitting
logitMod_ali_exp2_1 <- glm(DefaultStatus1_lead_12_max ~ Balance
                           , data=datCredit_train, family="binomial")
logitMod_ali_exp2_2 <- glm(DefaultStatus1_lead_12_max ~ Instalment
                           , data=datCredit_train, family="binomial")
logitMod_ali_exp2_3 <- glm(DefaultStatus1_lead_12_max ~ Principal
                           , data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_ali_exp2_1)
### RESULTS: Null deviance = 275184; Residual deviance = 275014; AIC = 275018
summary(logitMod_ali_exp2_2)
### RESULTS: Null deviance = 275184; Residual deviance = 274908; AIC = 274912
summary(logitMod_ali_exp2_3)
### RESULTS: Null deviance = 275184; Residual deviance = 274908; AIC = 273603
# ROC analysis
datCredit_valid[, prob_ali_exp2_1 := predict(logitMod_ali_exp2_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_ali_exp2_2 := predict(logitMod_ali_exp2_2, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_ali_exp2_3 := predict(logitMod_ali_exp2_3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp2_1)
### RESULTS: 52.19%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp2_2)
### RESULTS: 53.47%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp2_3)
### RESULTS:    57.5%
### CONCLUSION: Both [Balance] and [Instalment] are period-level variables, whilst [Principal] is an account-level variable. Therefore, keep [Principal] and decide between the other two period-level variables.
###             Keep [Balance] as it has a lower correlation (0.8045) with [Principal] compared to the correlation between [Instalment] and [Principal] (0.8971).

# - [InterestRate_Margin_imputed_mean] vs [InterestRate_Margin_bin] - From suggested sub-themes
# Model fitting
logitMod_ali_exp_3_1 <- glm(DefaultStatus1_lead_12_max ~ InterestRate_Margin_imputed_mean
                          , data=datCredit_train, family="binomial")
logitMod_ali_exp_3_2 <- glm(DefaultStatus1_lead_12_max ~ InterestRate_Margin_imputed_bin
                            , data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_ali_exp_3_1)
### RESULTS: Null deviance = 275184; Residual deviance = 273429; AIC = 273433
summary(logitMod_ali_exp_3_2)
### RESULTS: Null deviance = 275184; Residual deviance = 273163; AIC = 273169
# ROC analysis
datCredit_valid[, prob_ali_exp3_1 := predict(logitMod_ali_exp_3_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_ali_exp3_2 := predict(logitMod_ali_exp_3_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp3_1)
## RESULTS; 56.71%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp3_2)
### RESULTS:    56.14%
### COMPARISON: Model with binned variable has lower AIC (273169 vs 273433) and lower AUC than the model with the raw (imputed) variable (56.14% vs 56.71%).
### CONCLUSION: Use [InterestRate_Margin_imputed_mean].

# - Clean up
rm(logitMod_ali_exp1_1, logitMod_ali_exp1_2, logitMod_ali_exp2_1, logitMod_ali_exp2_2, logitMod_ali_exp2_3, logitMod_ali_exp_3_1, logitMod_ali_exp_3_2); gc()
datCredit_valid[,`:=` (prob_ali_exp1_1=NULL, prob_ali_exp1_2=NULL, prob_ali_exp2_1=NULL, prob_ali_exp2_2=NULL, prob_ali_exp2_3=NULL, prob_ali_exp3_1=NULL, prob_ali_exp3_2=NULL)]


# --- 2.3a Full logit model with all account-level information | Full analysis
# NOTE: Certain variables are excluded using insights from previous analysis: [Age_Adj]; [Instalment]; [AgeToTerm]; [BalanceToPrincipal]
logitMod_ali1 <- glm(DefaultStatus1_lead_12_max ~ Age_Adj + Term + Balance +
                       InterestRate_Margin_imputed_mean + Principal
                       , data=datCredit_train, family="binomial")
### WARNING       :   glm.fit: fitted probabilities numerically 0 or 1
### INVESTIGATION :   Model fit seems fine, predictions investigated below (those made on the validation set) and none are exactly 0 or 1.
# - Deviance and AIC
summary(logitMod_ali1)
### RESULTS: Null deviance = 275184; Residual deviance = 268931; AIC = 268943
# - Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_ali1)
### RESULTS: 2.27%
# - Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_ali1), confint.default(logitMod_ali1))), 3)
### RESULTS: odds ratios of [Term], [Balance], and [Principal] are all practically 1, which limits their usefulness (however, the range of these variables may be influencing this result; a standardised odds ratio may be more insightful)
# - Residual deviance analysis
resid_deviance_glm(logitMod_ali1)
### RESULTS: Model fit is somewhat strained (all 3 diagnostics gave warnings)
# - Variable importance
varImport <- varImport_logit(logitMod_ali1, method="stdCoef_ZScores", impPlot=T)
### RESULTS: Top three variables: [Principal], [Balance], and [Age_Adj]
# - ROC analysis
datCredit_valid[, prob_ali1 := predict(logitMod_ali1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali1)# datCredit_valid[prob_ali1==0,.N]; datCredit_valid[prob_ali1==1,.N] # There are no probabilities that are exactly equal to 0 or 1
### RESULTS: 63.12%


# --- 2.3b Best subset selection | Full analysis
### AB: Marcel, please perform full analysis on this model as well, in mirroring what we did in sec. 2.3a above
# - Best subset selection
logitMod_ali_best <- MASS::stepAIC(logitMod_ali1, direction="both")
### WARNING:   glm.fit: fitted probabilities numerically 0 or 1
# Start AIC = 268942.9
# End AIC = 268942.9
summary(logitMod_ali_best)
### RESULTS:    Entire input space returned after have been run through the best subset selection
### CONCLUSION: Use analysis of full model in 2.3a as this will be identical to this model.
# - Clean up
rm(logitMod_ali1, logitMod_ali_best)
datCredit_valid[,`:=`(prob_ali1=NULL)]


# --- 2.4 Final account-level information variables
# - Final variables
### CONCLUSION: Use [AgeToTerm], [Term], [Balance], [Principal], and [InterestRate_Margin_imputed_mean] as the account-level information variables.
# - Save variables
inputs_ali_fin <-  DefaultStatus1_lead_12_max ~ AgeToTerm + Term + Balance + Principal + InterestRate_Margin_imputed_mean
pack.ffdf(paste0(genObjPath, "ALI_Formula"), inputs_ali_fin); gc()


# --- 2.6 Clean up
rm(cor_ali_spear, ind_row_spear, ind_col_spear, cor_ali_spear2,
   logitMod_ali_best); gc()




# ------ 3. Modelling & Feature Selection by combining all previous themes
# --- Create full model formula
# - Load in all thematic variables
unpack.ffdf(paste0(genObjPath, "ALI_Formula"), tempPath)
# - Create formula
inputs_full <- labels(terms(inputs_ali_fin))
form_com_full <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_full, collapse="+")))

# --- Full model
# - Full logit model with all combined thematically selected variables
logitMod_full1 <- glm(form_com_full, data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_full1)
### RESULTS: Null deviance = 275184; Residual deviance = 269450; AIC = 269462
# Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_full1)
### RESULTS: 2.08%
# Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_full1), confint.default(logitMod_full1))), 3)
# Residual deviance analysis
resid_deviance_glm(logitMod_full1)
### RESULTS: Model fit is somewhat strained (3 diagnostics gave warnings)
# Variable importance
varImport_logit(logitMod_full1, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top three variables: [Principal], [Balance], and [InterestRate_Margin]
# ROC analysis
datCredit_train[, prob_full1 := predict(logitMod_full1, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_full1 := predict(logitMod_full1, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_full1)
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_full1)
### RESULTS: Training dataset = 63.24%
###          Validation dataset = 63.24%
# VIF analysis
car::vif(logitMod_full1)
### RESULTS:  Both [Principal] and [Balance] have VIFs that are larger than 10, but this is expected since they are both highly correlated

### RESULTS:  All variables are significant and have reasonable standard errors.
###           The coefficient of determination is relatively low at 2.08%.
###           The residual deviance analysis indicates that the model fit is strenuous.
###           Some VIFs are over 10, but majority are not; thus no cause for concern.
###           Model is not overfitted as evidenced by identical AUCs when a ROC analysis is conducted on the training- and validaiton datasets (63.24% vs 63.24%)

### CONCLUSION: Use all the variables from the resulting account-level information and behavioral variables themes.
###             These models on their own are not strong predictors and it is recommended that they are at least used with the intermediate variables.

# --- Save model formula to disk
# - Final variables
### CONCLUSION: Use [AgeToTerm], [Term], [Balance],, [Principal], [InterestRate_Margin_imputed_mean]
# - Save variables
inputs_fin_bas <- formula(logitMod_full1)
pack.ffdf(paste0(genObjPath, "Basic_Com_Formula"), inputs_fin_bas); gc()


# --- Clean up
rm(inputs_fin_bas, inputs_ali_fin); gc()




# ------ 4. Subsampling and refitting the final model(s)
# --- 4.1 Preliminaries
# - Field names
stratifiers <- c("DefaultStatus1_lead_12_max", "Date") # Must at least include target variable used in graphing event rate
targetVar <- "DefaultStatus1_lead_12_max"

# - Subsampling & resampling parameters
smp_size <- 250000 # fixed size of downsampled set
smp_perc <- smp_size / ( datCredit_train[complete.cases(mget(stratifiers)), mget(stratifiers)][,.N] ) # Implied sampling fraction for downsampling step

# --- 4.2 Downsample data into a set with a fixed size (using stratified sampling) before implementing resampling scheme
set.seed(1)
datCredit_smp <- datCredit_train %>%group_by(across(all_of(stratifiers))) %>% slice_sample(prop=smp_perc) %>% as.data.table()
cat( (datCredit_smp[is.na(get(targetVar)), .N] == 0) %?% 'SAFE: No missingness in target variable.\n' %:% 
       'WARNING: Missingness detected in target variable.\n')
### RESULTS: Subasmpling is successful.

# --- 4.3 Refitting final model
# - Load in model input space
unpack.ffdf(paste0(genObjPath, "Basic_Com_Formula"), tempPath)
# - Refit model to subsampled dataset
logitMod_full1_smp <- glm(inputs_fin_bas, data=datCredit_smp, family="binomial")
# - Deviance and AIC
summary(logitMod_full1_smp)
### RESULTS:    Insignificant variables are: None
### CONCLUSION: Save to assume that these variables are indeed significant in predicting default (at least within this input space)

# --- 4.4 Clean up
rm(stratifiers, targetVar, smp_size, smp_perc, datCredit_smp, logitMod_full1_smp); gc()



