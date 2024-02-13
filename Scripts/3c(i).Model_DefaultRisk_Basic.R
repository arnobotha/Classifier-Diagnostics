# ========================== MODEL DEFAULT RISK - BASIC =================================
# Develop several "basic" logistic regression models ranging from models with few
# features to models with more features to predict default risk. Delinquency-, portfolio-level-,
# and forward looking information is not considers among these "basic" variables.
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha, Esmerelda Oberholzer, Marcel Muller

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
summary(logitMod_ali_exp1_1) # Null deviance = 275184  ; Residual deviance = 274579  ; AIC = 274583 
summary(logitMod_ali_exp1_2) # Null deviance = 275184  ; Residual deviance = 274940  ; AIC = 274944
# ROC analysis
datCredit_valid[, prob_ali_exp1_1 := predict(logitMod_ali_exp1_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_ali_exp1_2 := predict(logitMod_ali_exp1_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp1_1) # 51.112%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp1_2) # 51.1172%
### RESULTS:    [Age_Adj] and [AgeToTerm]  have almost identical predictive strength, the model fit
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
summary(logitMod_ali_exp2_1) # Null deviance = 275184; Residual deviance = 275014; AIC = 275018
summary(logitMod_ali_exp2_2) # Null deviance = 275184; Residual deviance = 274908; AIC = 274912
summary(logitMod_ali_exp2_3) # Null deviance = 275184; Residual deviance = 274908; AIC = 273603
# ROC analysis
datCredit_valid[, prob_ali_exp2_1 := predict(logitMod_ali_exp2_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_ali_exp2_2 := predict(logitMod_ali_exp2_2, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_ali_exp2_3 := predict(logitMod_ali_exp2_3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp2_1) # 52.19%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp2_2) # 53.47%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp2_3) # 57.5%
### CONCLUSION:   Both [Balance] and [Instalment] are period-level variables, whilst [Principal] is an account-level variable. Therefore, keep [Principal] and decide between the other two period-level variables.
###               Keep [Balance] as it has a lower correlation (0.8045) with [Principal] compared to the correlation between [Instalment] and [Principal] (0.8971).

# - [InterestRate_Margin_imputed_mean] vs [InterestRate_Margin_bin] - From suggested sub-themes
# Model fitting
logitMod_ali_exp_3_1 <- glm(DefaultStatus1_lead_12_max ~ InterestRate_Margin_imputed_mean
                          , data=datCredit_train, family="binomial")
logitMod_ali_exp_3_2 <- glm(DefaultStatus1_lead_12_max ~ InterestRate_Margin_imputed_bin
                            , data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_ali_exp_3_1) # Null deviance = 275184; Residual deviance = 273409; AIC = 273413
summary(logitMod_ali_exp_3_2) # Null deviance = 275184; Residual deviance = 273156; AIC = 273162
# ROC analysis
datCredit_valid[, prob_ali_exp3_1 := predict(logitMod_ali_exp_3_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_ali_exp3_2 := predict(logitMod_ali_exp_3_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp3_1) # 56.72%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp3_2) # 56.13%
### RESULTS   : Model with binned variable has lower AIC (273162 vs 273413) and lower AUC than the model with the raw (imputed) variable (56.13% vs 56.72%).
### CONCLUSION: Use [InterestRate_Margin_imputed_mean].

# - Clean up
rm(logitMod_ali_exp1_1, logitMod_ali_exp1_2, logitMod_ali_exp2_1, logitMod_ali_exp2_2, logitMod_ali_exp2_3, logitMod_ali_exp_3_1, logitMod_ali_exp_3_2); gc()
datCredit_valid[,`:=` (prob_ali_exp1_1=NULL, prob_ali_exp1_2=NULL, prob_ali_exp2_1=NULL, prob_ali_exp2_2=NULL, prob_ali_exp2_3=NULL, prob_ali_exp3_1=NULL, prob_ali_exp3_2=NULL)]


<<<<<<< HEAD
# --- 2.3 Best subset selection | Full analysis
# - Full logit model with all account-level information - Exclude variables using insights from above analysis: [Age_Adj]; [Instalment]; [AgeToTerm]; [BalanceToPrincipal]
logitMod_ali1 <- glm(DefaultStatus1_lead_12_max ~ AgeToTerm + Term + Balance +
                       Principal + InterestRate_Margin_imputed_mean
=======
# --- 2.3a Full logit model with all account-level information | Full analysis
# NOTE: Certain variables are excluded using insights from previous analysis: [Age_Adj]; [Instalment]; [AgeToTerm]; [BalanceToPrincipal]
logitMod_ali1 <- glm(DefaultStatus1_lead_12_max ~ TimeInPerfSpell + Term + Balance +
                       InterestRate_Margin_imputed_mean + Principal
>>>>>>> 8f91472be449deaececf574d398a79db846076eb
                       , data=datCredit_train, family="binomial")
### WARNING       :   glm.fit: fitted probabilities numerically 0 or 1
### INVESTIGATION :   Model fit seems fine, predictions investigated below (those made on the validation set) and none are exactly 0 or 1.
# - Deviance and AIC
summary(logitMod_ali1) # Null deviance = 275184; Residual deviance = 269438; AIC = 269450
# - Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_ali1) # 2.4%
# - Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_ali1), confint.default(logitMod_ali1))), 3)
### RESULTS: odds ratios of [Term], [Balance], and [Principal] are all practically 1, which limits their usefulness
# - Residual deviance analysis
resid_deviance_glm(logitMod_ali1)
### RESULTS: Model fit is somewhat strained (all 3 diagnostics gave warnings)
# - ROC analysis
datCredit_valid[, prob_ali1 := predict(logitMod_ali1, newdata = datCredit_valid, type="response")]
<<<<<<< HEAD
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali1)# datCredit_valid[prob_ali1==0,.N]; datCredit_valid[prob_ali1==1,.N] # There are no probabilities that are exactly equal to 0 or 1 # 63.22%
=======
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali1) # 63.64%


# - Variable importance analysis
### SCRATCH-start
logit_model <- logitMod_ali1
rm(logit_model,)
### SCRATCH-end
varImport_logit(logitMod_ali1, plot=T) 



# --- 2.3b Best subset selection | Full analysis
### AB: Marcel, please perform full analysis on this model as well, in mirroring what we did in sec. 2.3a above
>>>>>>> 8f91472be449deaececf574d398a79db846076eb
# - Best subset selection
logitMod_ali_best <- MASS::stepAIC(logitMod_ali1, direction="both")
### WARNING:   glm.fit: fitted probabilities numerically 0 or 1
# Start AIC = 269449.9
# End AIC = 269449.9
### Entire input space returned after have been run through the best subset selection
# - Deviance and AIC
summary(logitMod_ali_best) # Null deviance = 275184; Residual deviance = 269438; AIC = 269450
# - Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_ali_best) # 2.09%
# - Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_ali_best), confint.default(logitMod_ali_best))), 3)
### RESULTS: odds ratios of [Term], [Balance], and [Principal] are all practically 1, however the range of these variables might not give the full picture
# - Variable importance
varImport_logit(logitMod_ali_best, method="absCoef", standardise=T, sig_level=0.1, plot=T) # Top three variables: [InterestRate_Margin_imputed_mean], [AgeToTerm], and [Term]
# - Clean up
rm(logit_ali1)
datCredit_valid[,`:=`(prob_ali1=NULL, prob_ali_best1)]


# --- 2.5 Final account-level information variables
# - Final variables
### CONCLUSION: Use [AgeToTerm], [Term], [Balance], [Principal], and [InterestRate_Margin_imputed_mean] as the account-level information variables.
# - Save variables
inputs_ali_fin <-  DefaultStatus1_lead_12_max ~ AgeToTerm + Term + Balance + Principal + InterestRate_Margin_imputed_mean
pack.ffdf(paste0(genObjPath, "ALI_Formula"), inputs_ali_fin); gc()


# --- 2.6 Clean up
rm(cor_ali_spear, ind_row_spear, ind_col_spear, cor_ali_spear2, inputs_ali_fin,
   logitMod_ali_best)




# ------ 3. Modelling & Feature Selection by theme "Behavioural information"
# --- 3.1 Correlation analysis using Spearman correlation
# - Correlation threshold
cor_thresh <- 0.6
# - Computing the correlation matrix
cor_beh_spear <-cor(x=datCredit_train[,list(slc_acct_prepaid_perc_dir_12_imputed_med,
                                            slc_acct_pre_lim_perc_imputed_med)], # [slc_pmnt_method] and [slc_pmnt_grp] are excluded since they are categorical variables
                    method = "spearman")
# - Creating a correlalogram
corrplot(cor_beh_spear, type="upper")
# - Setting the bottom halve of the correlation matrix to zero, since the matrix is symettrical around the diagonal
cor_beh_spear[lower.tri(cor_beh_spear)] <- 0
# - Printing all correlations above the specified threshold
ind_row_spear <- which(abs(cor_beh_spear)>cor_thresh & abs(cor_beh_spear)<1)
ind_col_spear <- ind_row_spear %% ncol(cor_beh_spear) + ncol(cor_beh_spear)*(ind_row_spear %% ncol(cor_beh_spear) == 0)
cor_beh_spear2 <- data.table(x=rownames(cor_beh_spear)[floor((ind_row_spear-1)/ncol(cor_beh_spear)) + 1],
                             y=colnames(cor_beh_spear)[ind_col_spear])
for(i in 1:(nrow(cor_beh_spear2))) {cat("Absolute correlation above ", cor_thresh, " found for ", cor_beh_spear2[i,1][[1]], " and ", cor_beh_spear2[i,2][[1]], "\n")}
### RESULTS: High-correlation detected within [slc_acct_prepaid_perc_dir_12_imputed_med] and [slc_acct_pre_lim_perc_imputed_med]
### Conclusion: Remove either [slc_acct_prepaid_perc_dir_12_imputed_med] or [slc_acct_pre_lim_perc_imputed_med]


# --- 3.2 Experimenting (using insights from correlation analysis) and subthemes
# - slc_acct_prepaid_perc_dir_12_imputed vs [slc_acct_pre_lim_perc_imputed] - From the insights of the correlation analysis
# Model fitting
logitMod_beh_exp1_1 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_prepaid_perc_dir_12_imputed_med
                           , data=datCredit_train, family="binomial")
### WARNING : glm.fit: fitted probabilities numerically 0 or 1
### INVESTIGATION :   Model fit seems fine, predictions investigated below (those made on the validation set) and none are exactly 0 or 1.
logitMod_beh_exp1_2 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_pre_lim_perc_imputed_med
                           , data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_beh_exp1_1) # Null deviance = 275184; Residual deviance = 275147; AIC = 275151
summary(logitMod_beh_exp1_2) # Null deviance = 275184; Residual deviance = 266557; AIC = 266561
# ROC analysis
datCredit_valid[, prob_beh_exp1_1 := predict(logitMod_beh_exp1_1, newdata = datCredit_valid, type="response")]; # datCredit_valid[prob_beh_exp1_1==0,.N]; datCredit_valid[prob_beh_exp1_1==1,.N] # There are no probabilities that are exactly equal to 0 or 1
datCredit_valid[, prob_beh_exp1_2 := predict(logitMod_beh_exp1_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp1_1) # 61.37%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp1_2) # 64.46%
### CONCLUSION:   Use [slc_acct_pre_lim_perc_imputed] as it has results in a model with a lower AIC and higher AUC

# - [slc_pmnt_method] vs [pmnt_method_grp] - From suggested sub-themes
# Model fitting
logitMod_beh_exp2_1 <- glm(DefaultStatus1_lead_12_max ~ slc_pmnt_method
                          , data=datCredit_train, family = "binomial")
logitMod_beh_exp2_2 <- glm(DefaultStatus1_lead_12_max ~ pmnt_method_grp
                           , data=datCredit_train, family = "binomial")
# - Deviance and AIC
summary(logitMod_beh_exp2_1) # Null deviance = 275184; Residual deviance = 257599; AIC = 257613
summary(logitMod_beh_exp2_2) # Null deviance = 275184; Residual deviance = 259831; AIC = 259839
# - ROC analysis
datCredit_valid[, prob_beh_exp2_1 := predict(logitMod_beh_exp2_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_beh_exp2_2 := predict(logitMod_beh_exp2_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp2_1) # 68.56%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp2_2) # 66.9%
### CONCLUSION:  Use [slc_pmnt_method] as it results in a model with a slightly higher AUC and a lower AIC.

# - Missing value indicators - From suggested sub-themes
# Model fitting
logitMod_beh_exp3_1 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_pre_lim_perc_imputed_med
                           , data=datCredit_train, family = "binomial")
logitMod_beh_exp3_2 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_pre_lim_perc_imputed_med/value_ind_slc_acct_pre_lim_perc # [slc_pmnt_method] already has a dedicated level for missing values, therefore do not include the missing value indicator
                         , data=datCredit_train, family = "binomial")
# Deviance and AIC
summary(logitMod_beh_exp3_1) # Null deviance = 275184; Residual deviance = 266557; AIC = 266561
summary(logitMod_beh_exp3_2) # Null deviance = 275184; Residual deviance = 266557; AIC = 266561
# - ROC analysis
datCredit_valid[, prob_beh_exp3_1 := predict(logitMod_beh_exp3_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_beh_exp3_2 := predict(logitMod_beh_exp3_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp3_1) # 64.46%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp3_2) # 64.46%
### RESULTS:    The coefficient estimate for the interaction term is "NA".
### CONCLUSION: Do not use missing value indicators for behavioral information variables

# - Clean up
rm(logitMod_beh_exp1_1, logitMod_beh_exp1_2, logitMod_beh_exp2_1, logitMod_beh_exp2_2, logitMod_beh_exp3_1, logitMod_beh_exp3_2); gc()
datCredit_valid[, `:=` (prob_beh_exp1_1=NULL, prob_beh_exp1_2=NULL, prob_beh_exp2_1=NULL, prob_beh_exp2_2=NULL, prob_beh_exp3_1=NULL, prob_beh_exp3_2=NULL)]


# --- 3.3 Final model (no need to do a best subset procedure since there are only two variables) | Full analysis
# - Full model
logitMod_beh1 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_pre_lim_perc_imputed_med + slc_pmnt_method
                           , data=datCredit_train, family = "binomial")
# - Deviance and AIC
summary(logitMod_beh1) # Null deviance = 275184; Residual deviance = 252143; AIC = 252159
# - ROC analysis
datCredit_valid[, prob_beh1 := predict(logitMod_beh1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh1)# 73.06%
# - Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_beh1) # 8.37%
# - Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_beh1), confint.default(logitMod_beh1))), 3)
### RESULTS: Both the numerical and the categorical, along with all associated levels of that variable have odds ratio that are "quite" larger/smaller than 1
# - Variable importance
varImport_logit(logitMod_beh1, method="absCoef", standardise=F, sig_level=0.1, plot=T) # Top three variables: [slc_acct_pre_lim_perc_imputed_med], [slc_pmnt_methodSuspense], and [slc_pmnt_methodStatement]

# --- 3.4 Final account-level information variables
# - Final variables
### CONCLUSION: Use [slc_acct_pre_lim_perc_imputed_med] and [slc_pmnt_method] as the account-level information variables.
# - Save variables
inputs_beh_fin <-  DefaultStatus1_lead_12_max ~ slc_acct_pre_lim_perc_imputed_med + slc_pmnt_method
pack.ffdf(paste0(genObjPath, "Beh_Formula"), inputs_beh_fin); gc()

# --- 3.5 Clean up
rm(cor_beh_spear, ind_row_spear, ind_col_spear, cor_beh_spear2, inputs_beh_fin, logitMod_beh1); gc()
datCredit_valid[,`:=`(prob_beh1=NULL)]




# ------ 4. Modelling & Feature Selection by combining all previous themes
# --- Create full model formula
# - Load in all thematic variables
unpack.ffdf(paste0(genObjPath, "ALI_Formula"), tempPath); unpack.ffdf(paste0(genObjPath, "Beh_Formula"), tempPath)
# - Create formula
inputs_full <- c(labels(terms(inputs_ali_fin)), labels(terms(inputs_beh_fin)))
form_com_full <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_full, collapse="+")))

# --- Full model
# - Full logit model with all combined thematically selected variables
logitMod_full1 <- glm(form_com_full, data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_full1) # Null deviance = 275184; Residual deviance = 250081; AIC = 250107
# Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_full1), confint.default(logitMod_full1))), 3)
# ROC analysis
datCredit_train[, prob_full1 := predict(logitMod_full1, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_full1 := predict(logitMod_full1, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_full1) # 74.6%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_full1) # 74.39%
# VIF analysis
car::vif(logitMod_full1)
### RESULTS:  Both [Principal] and [Balance] have VIFs that are larger than 10, but this is expected since they are both highly correlated
# Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_full1) # 9.12%
# Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_full1), confint.default(logitMod_full1))), 3)
### RESULTS: [Balance] and [Principal] have odds ratios that are very close to one, but this is due to the range of the variables. Run variable importance to get a clearer picture
# Variable importance
varImport_logit(logitMod_full1, method="absCoef", standardise=F, sig_level=0.1, plot=T) # Top three variables: [InterestRate_Margin_imputed_mean], [slc_acct_pre_lim_perc_imputed_med], and [slc_pmnt_methodSuspense]


### RESULTS:  All variables are significant and have reasonable standard errors.
###           Model is not overfitted as evidenced by the small change in AUC when a ROC analysis is conducted on the training- and validaiton datasets (74.6% vs 74.39%)
###           Number of variables is reduced from xx in the full model to xx in the best subset model
###           Some VIFs are over 10, but majority are not; thus no cause for concern.

### CONCLUSION: Use all the variables from the resulting account-level information and behavioral variables themes.

# --- Save model formula to disk
# - Final variables
### CONCLUSION: Use [ AgeToTerm], [Term], [Balance],, [Principal], [InterestRate_Margin_imputed_mean], [slc_acct_pre_lim_perc_imputed_med], [slc_pmnt_method]
# - Save variables
inputs_fin_bas <- formula(logitMod_full1)
pack.ffdf(paste0(genObjPath, "Basic_Com_Formula"), inputs_fin_bas); gc()


# --- Clean up
datCredit_train[,`:=`(prob_full1=NULL)]; datCredit_valid[,`:=`(prob_full1=NULL)]
rm(logitMod_full1)








### EO: Other variations of the account-level variables can now be grouped into sub-themes, e.g., log-transform variables, using the ratio-like variables rather than the "raw" variables, e.g., Balance vs BalanceToPrincipal or AgeToTerm vs Age and Term
### EO: Do not use the receipt variable (too many issues now)

# - Remove the insignificant variables from the applicable theme and analyse again
# "Raw" variables
# inputs_acc2 <- DefaultStatus1_lead_12_max ~ Age_Adj + Term + Receipt_Inf + Balance + InterestRate_Margin + BalanceToPrincipal
# logitMod_acc2 <- glm(inputs_acc2, data=datCredit_train, family="binomial")
# summary(logitMod_acc2)
### RESULTS: All variables are significant
### EO: If one or more of the models were insignificant, follow the same process and define inputs_acc3 and logitMod_acc3 etc. until all variables in the subtheme model are significant
### EO: You will end up with a range of models, all with significant variables, and now need to decide which one performs the best - use AUC as a proxy

# - Calculate the AUC to identify the most predictive set of variables in a model subtheme
# datCredit_train[, prob_acc2 := predict(logitMod_acc2, newdata = datCredit_train, type="response")]
# datCredit_valid[, prob_acc2 := predict(logitMod_acc2, newdata = datCredit_valid, type="response")]

# auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_acc2) # 60.60%
# auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_acc2) # 60.53%
### EO: You will then use the variables from the model that reports the highest AUC as the chosen variables from the theme
### EO: Diagnostics (in-sample): Think about using AIC, deviance-residuals (will help to infer the same thing), statistical significance (p-values with threshold of 5%, standard errors)
### EO: Out-of-sample: AUC
### EO: Final analysis of the final candidate model: AUC on train and test, as well as multicollinearity, and variable importance (Odds-ratio and Shapley-values)


# --- 4.2 Delinquency/performance-data
# Analyse the significance of the variables related to the delinquency/performance of the account
### EO: Think about including arrears (choose g0_delinq and arrears - possible sub-theme), time since last movement in delinquency-level
# inputs_del1 <- DefaultStatus1_lead_12_max ~ TimeInPerfSpell + g0_Delinq + PrevDefaults + slc_acct_arr_dir_3 + slc_acct_roll_ever_24
# logitMod_del1 <- glm(inputs_del1, data=datCredit_train, family="binomial")
# summary(logitMod_del1)
### RESULTS: Only one level of slc_acct_arr_dir_3 is insignificant (the missing group), but the rest of the groups are significant

### EO: Now similarly, you can analyse different subthemes if you'd like and compare them to create a "base" set of variables from this theme

# --- 4.3 Behavioral variables
# Analyse the significance of the variables related to the behavior of the account
# inputs_behav1 <- DefaultStatus1_lead_12_max ~ slc_pmnt_method + slc_acct_pre_lim_perc + slc_acct_prepaid_perc_dir_12

# --- 4.3 Portfolio variables

# --- 4.4 Macroeconomic information
# Analyse the significance per variable
# - Repo rate
### EO: Here, you can define all of the transformations/raw MVs per variable to identify the most significant ones
### EO: This step is the most time consuming, since you would probably have to remove a couple of variables at a time and re-run to identify whether the remaining ones are still significant
### EO: Once you have your set of base variables per MV theme/type of variable, you can combine all of the remaining significant variables and then follow the same elimination process



# - Effect size analysis | Maybe do this only for final model
# mean odds of event (unconditioned on any input): 0.038 | prob = odds / ( 1+ odds)
# odds of event given unit increase in InterestRate_Margin, exp(2.26322360946448 x 1) = 9.614 | Biggest effect size


### AB: Build graphing function (I'll help with this later, have an interesting idea to illustrate effect sizes of all inputs on
# a single graph, similar to Shapley-values' graph)
# https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
# https://www.geeksforgeeks.org/how-to-plot-odds-ratio-of-prediction-of-logistic-model-in-r/
# https://argoshare.is.ed.ac.uk/healthyr_book/odds-ratio-plot-1.html
# https://stackoverflow.com/questions/47085514/simple-way-to-visualise-odds-ratios-in-r
# probs <- seq(0,1,by=0.01)
# odds <- probs / (1-probs)
# plot(x=probs, y=odds, type="b")
# Put odds ratios as annotations on above graph (ggplot2-variant of course)

# See this link for plotting odds ratios on a log scale (which should always be done when comparing odds ratios in a graph):
# https://andrewpwheeler.com/2013/10/26/odds-ratios-need-to-be-graphed-on-log-scales/

