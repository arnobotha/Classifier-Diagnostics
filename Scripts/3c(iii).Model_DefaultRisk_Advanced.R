# ========================= MODEL DEFAULT RISK - COMBINED ===============================
# Develop several "advanced" logistic regression models. These models include portfolio-
# level variables along with the combined input space in selecting the "best" variables
# in the entire existing input space for predicting default.
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha, Marcel Muller

# DESCRIPTION:
# This script uses the previously prepared credit dataset fused with basic-, intermediary-
# and advanced variables. These include core-, delinquency-, forward looking-, and 
# portfolio level information variables.
# Logit models are fitted with the variables from each of the above mentioned segments,
# where the selection of the variables is considering different combinations of the
# segments. Larger models are also fitted where all segments are combined.
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 0a.CustomFunctions.R
#   - 3c(v).Model_DefaultRisk_Exp2
#
# -- Inputs:
#   - datCredit_real | Prepared credit data from script 2f
#
# -- Outputs:
#   - A graph showcasing the "individual" importance of each selected portfolio-level variable
#   - A formula for the final advanced model
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
# Intermediate variables
unpack.ffdf(paste0(genObjPath, "Int_Formula"), tempPath)




# ------ 2. Modelling & Feature Selection by theme "Delinquency information" - This excludes the basic variable [g0_Delinq]

# --- 2.1 Correlation analysis using spearman correlation
# - Correlation threshold
cor_thresh <- 0.6
# - Computing the correlation matrix
cor_del_spear <-cor(x=datCredit_train[!is.na(g0_Delinq_SD_6),list(PrevDefaults=as.numeric(PrevDefaults), TimeInPerfSpell,
                                                                  PerfSpell_Num, g0_Delinq_Num, g0_Delinq_SD_4, g0_Delinq_SD_6,
                                                                  PerfSpell_g0_Delinq_Num, # PerfSpell_g0_Delinq_SD is excluded since it is forward looking (it uses all information in a performance spell...predictions made before the end of that spell are thus using information of the future)
                                                                  slc_acct_roll_ever_24_imputed_mean, slc_past_due_amt_imputed_med)], # slc_acct_arr_dir_3 excluded since it is a categorical variable
                    method = "spearman")
# - Creating a correlalogram
corrplot(cor_del_spear, type="upper")
# - Setting the bottom halve of the correlation matrix to zero, since the matrix is symmetrical around the diagonal
cor_del_spear[lower.tri(cor_del_spear)] <- 0
# - Printing all correlations above the specified threshold
ind_row_spear <- which(abs(cor_del_spear)>cor_thresh & abs(cor_del_spear)<1)
ind_col_spear <- ind_row_spear %% ncol(cor_del_spear) + ncol(cor_del_spear)*(ind_row_spear %% ncol(cor_del_spear) == 0)
cor_del_spear2 <- data.table(x=rownames(cor_del_spear)[floor((ind_row_spear-1)/ncol(cor_del_spear)) + 1],
                             y=colnames(cor_del_spear)[ind_col_spear])
for(i in 1:(nrow(cor_del_spear2))) {cat("Absolute correlation above ", cor_thresh, " found for ", cor_del_spear2[i,1][[1]], " and ", cor_del_spear2[i,2][[1]], "\n")}
### RESULTS: High-correlation detected within [PerfSpell_Num], [Prev_Defaults], [g0_Delinq], [g0_Delinq_SD_4], [g0_Delinq_SD_6], [PerfSpell_g0_Delinq_Num], [g0_Delinq_Num], and [slc_acct_roll_ever_24_imputed_mean]

### Conclusion:
###   Remove either [PrevDefaults] or [PerfSpell_Num]
###   Remove either [g0_Delinq_Num] or [PerfSpell_g0_Delinq_Num]
###   Keep [g0_Delinq_SD_4], and [g0_Delinq_SD_6] as these variables are expected to have a high correlation
###   Keep [slc_acct_roll_ever_24_imputed_mean]; this variable gives similar information than [g0_Delinq_Num], but only for the first 24 months of an accounts lifetime.



# --- 2.2 Preliminary experimenting: Using insights from correlation analysis
# - [PrevDefaults] vs [PerfSpell_Num] - From the insights of the correlation analysis
# Model fitting
logitMod_del_exp1_1 <- glm(DefaultStatus1_lead_12_max ~ PrevDefaults
                           , data=datCredit_train, family="binomial")
logitMod_del_exp1_2 <- glm(DefaultStatus1_lead_12_max ~ PerfSpell_Num
                           , data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_del_exp1_1)
### RESULTS:  Null deviance = 275184; Residual deviance = 226130; AIC = 226134
summary(logitMod_del_exp1_2)
### RESULTS: Null deviance = 275184; Residual deviance = 263639; AIC = 263643
# ROC analysis
datCredit_valid[, prob_del_exp1_1 := predict(logitMod_del_exp1_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_del_exp1_2 := predict(logitMod_del_exp1_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del_exp1_1)
### RESULTS: 75.45%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del_exp1_2)
### RESULTS:    60.04%
### CONCLUSION: Use [PrevDefaults] in the modelling process as it results in a model with higher predictive power
###             In a PD modelling context, which is less dynamic than a SICR model, the less dynamic variable is more useful given the 12-month horizon of the typical PD model.

# - [g0_Delinq_Num] or [PerfSpell_g0_Delinq_Num] - From the insights of the correlation analysis
# Model fitting
logitMod_del_exp2_1 <- glm(DefaultStatus1_lead_12_max ~ g0_Delinq_Num
                           , data=datCredit_train, family="binomial")
logitMod_del_exp2_2 <- glm(DefaultStatus1_lead_12_max ~ PerfSpell_g0_Delinq_Num
                           , data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_del_exp2_1)
### RESULTS: Null deviance = 275184; Residual deviance = 258669; AIC = 258673
summary(logitMod_del_exp2_2)
### RESULTS: Null deviance = 275184; Residual deviance = 265999; AIC = 266003
# ROC analysis
datCredit_valid[, prob_del_exp2_1 := predict(logitMod_del_exp2_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_del_exp2_2 := predict(logitMod_del_exp2_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del_exp2_1)
### RESULTS: 77.24%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del_exp2_2)
### RESULTS:    73.89%
### CONCLUSION: Use [g0_Delinq_Num] as it results in a model with higher predictive power

# - Missing value indicator for [slc_acct_roll_ever_24_imputed_mean] - From suggested sub-themes
# Model fitting
logitMod_del_exp3_1 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_roll_ever_24_imputed_mean
                           , data=datCredit_train, family="binomial")

logitMod_del_exp3_2 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_roll_ever_24_imputed_mean/value_ind_slc_acct_roll_ever_24
                           , data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_del_exp3_1)
### RESULTS: Null deviance = 255631; Residual deviance = 234505; AIC = 234509
summary(logitMod_del_exp3_2)
### RESULTS: Null deviance = 255631; Residual deviance = 233237; AIC = 233243
# ROC analysis
datCredit_valid[, prob_del_exp3_1 := predict(logitMod_del_exp3_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_del_exp3_2 := predict(logitMod_del_exp3_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del_exp3_1)
### RESULTS: 78.61%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del_exp3_2)
### RESULTS:    78.14%
### COMPARISON: Model with missing value indicators has lower AIC (233243 vs 234509) but lower AUC (78.14% vs 78.14%) than the model without the missing value indicators (although the difference is very small)
### COMCLUSION: Use [slc_acct_roll_ever_24_imputed_mean] without its missing value indicators as it results in a model with higher predictive power

# - Missing value indicator for [slc_past_due_amount_imputed_med] - From suggested sub-themes
# Model fitting
logitMod_del_exp4_1 <- glm(DefaultStatus1_lead_12_max ~ slc_past_due_amt_imputed_med
                           , data=datCredit_train, family="binomial")
### WARNING       :   glm.fit: fitted probabilities numerically 0 or 1
### INVESTIGATION :   Model fit seems fine, predictions investigated below (those made on the validation set) and none are exactly 0 or 1.
logitMod_del_exp4_2 <- glm(DefaultStatus1_lead_12_max ~ slc_past_due_amt_imputed_med/value_ind_slc_past_due_amt
                           , data=datCredit_train, family="binomial")
### WARNING       :   glm.fit: fitted probabilities numerically 0 or 1
# Deviance and AIC
summary(logitMod_del_exp4_1)
### RESULTS: Null deviance = 255631; Residual deviance = 247218; AIC = 247222
summary(logitMod_del_exp4_2)
### RESULTS:    Null deviance = 255631; Residual deviance = 247218; AIC = 247222
###             Estimated coefficient for the interaction between the raw variable and the missing value indicator results in an estimated coefficient that is "NA"
### COMCLUSION: Use [slc_past_due_amount_imputed_med] only

# - Clean up
rm(logitMod_del_exp1_1, logitMod_del_exp1_2, logitMod_del_exp2_1, logitMod_del_exp2_2, logitMod_del_exp3_1, logitMod_del_exp3_2, logitMod_del_exp4_1, logitMod_del_exp4_2); gc()
datCredit_valid[, `:=` (prob_del_exp1_1=NULL, prob_del_exp1_2=NULL, prob_del_exp2_1=NULL, prob_del_exp2_2=NULL, prob_del_exp3_1=NULL, prob_del_exp3_2=NULL)]



# --- 2.3 Best subset selection
# - Full logit model with all account-level information - Exclude variables using insights from correlation analysis:  [PerfSpell_Num]
logitMod_del1 <- glm(inputs_del1 <- DefaultStatus1_lead_12_max ~ PrevDefaults + TimeInPerfSpell + g0_Delinq_Num + g0_Delinq_SD_4 + g0_Delinq_SD_6 + 
                       slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3 + slc_past_due_amt_imputed_med
                     , data=datCredit_train, family="binomial")
# - Assess full model
# Deviance and AIC
summary(logitMod_del1)
### RESULTS: Null deviance = 255631; Residual deviance = 174947; AIC = 174969
# Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_del1)
### RESULTS: 31.56%
# Odds Ratio analysis 
round(exp(cbind(OR = coef(logitMod_del1), confint.default(logitMod_del1))), 3)
### [slc_past_due_amt_imputed_med] has a ratio close to 1, but this might not give an accurate indication because of the range of this variable
# Residual deviance analysis
resid_deviance_glm(logitMod_del1)
### RESULTS: Model fit is somewhat strained (2 diagnostics gave warnings)
# Variable importance
varImport_logit(logitMod_del1, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top three variables: [PrevDefaultsTRUE], [slc_past_due_amt_imputed_med], and [g0_Delinq_SD_6]
# ROC analysis
datCredit_valid[, prob_del1 := predict(logitMod_del1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del1)
### RESULTS:    87.73%
### CONCLUSION: Proceed to using best subset selection on the input space.

# - Best subset selection
logitMod_del_best <- MASS::stepAIC(logitMod_del1, direction="both")
# Start AIC = 174968.9
# End AIC = 174968.9
### RESULTS:    Model inputed is returned as final model
### CONCLUSION: Use analysis of full model above.

### CONCLUSION: Save inputs to disk.
### NOTE      : [g0_Delinq_Num] has the wrong sign (negative); might be corrected in the final model with the combination of additional variables


# --- 2.4 Final delinquency-level information variables
# - Final variables
### CONCLUSION: Use [PrevDefaults], [TimeInPerfSpell], [g0_Delinq_Num], [g0_Delinq_SD_4], [g0_Delinq_SD_6],
###                 [slc_acct_roll_ever_24_imputed_mean], [slc_acct_arr_dir_3], [slc_past_due_amt_imputed_med]
# - Save variables
inputs_del_fin <- DefaultStatus1_lead_12_max ~ PrevDefaults + TimeInPerfSpell + g0_Delinq_Num + g0_Delinq_SD_4 + g0_Delinq_SD_6 + g0_Delinq_Num +
  slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3 + slc_past_due_amt_imputed_med
pack.ffdf(paste0(genObjPath, "Del_Formula"), inputs_del_fin); gc()

# --- 2.5 Clean up
rm(cor_del_spear, ind_row_spear, ind_col_spear, cor_del_spear2, logitMod_del_best, logitMod_del1, inputs_del1, inputs_del_fin); gc()




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
summary(logitMod_beh_exp1_1)
### RESULTS: Null deviance = 275184; Residual deviance = 275147; AIC = 275151
summary(logitMod_beh_exp1_2)
### RESULTS: Null deviance = 275184; Residual deviance = 266557; AIC = 266561
# ROC analysis
datCredit_valid[, prob_beh_exp1_1 := predict(logitMod_beh_exp1_1, newdata = datCredit_valid, type="response")]; # datCredit_valid[prob_beh_exp1_1==0,.N]; datCredit_valid[prob_beh_exp1_1==1,.N] # There are no probabilities that are exactly equal to 0 or 1
datCredit_valid[, prob_beh_exp1_2 := predict(logitMod_beh_exp1_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp1_1)
### RESULTS: 61.37%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp1_2)
### RESULTS: 64.46%
### CONCLUSION:   Use [slc_acct_pre_lim_perc_imputed] as it has results in a model with a lower AIC and higher AUC

# - [slc_pmnt_method] vs [pmnt_method_grp] - From suggested sub-themes
# Model fitting
logitMod_beh_exp2_1 <- glm(DefaultStatus1_lead_12_max ~ slc_pmnt_method
                           , data=datCredit_train, family = "binomial")
logitMod_beh_exp2_2 <- glm(DefaultStatus1_lead_12_max ~ pmnt_method_grp
                           , data=datCredit_train, family = "binomial")
# - Deviance and AIC
summary(logitMod_beh_exp2_1)
### RESULTS: Null deviance = 275184; Residual deviance = 257599; AIC = 257613
summary(logitMod_beh_exp2_2)
### RESULTS: Null deviance = 275184; Residual deviance = 259831; AIC = 259839
# - ROC analysis
datCredit_valid[, prob_beh_exp2_1 := predict(logitMod_beh_exp2_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_beh_exp2_2 := predict(logitMod_beh_exp2_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp2_1)
### RESULTS: 68.56%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp2_2)
### RESULTS: 66.9%
### CONCLUSION:  Use [slc_pmnt_method] as it results in a model with a slightly higher AUC and a lower AIC.

# - Missing value indicators - From suggested sub-themes
# Model fitting
logitMod_beh_exp3_1 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_pre_lim_perc_imputed_med
                           , data=datCredit_train, family = "binomial")
logitMod_beh_exp3_2 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_pre_lim_perc_imputed_med/value_ind_slc_acct_pre_lim_perc # [slc_pmnt_method] already has a dedicated level for missing values, therefore do not include the missing value indicator
                           , data=datCredit_train, family = "binomial")
# Deviance and AIC
summary(logitMod_beh_exp3_1)
### RESULTS: Null deviance = 275184; Residual deviance = 266557; AIC = 266561
summary(logitMod_beh_exp3_2)
### RESULTS: Null deviance = 275184; Residual deviance = 266557; AIC = 266561
# - ROC analysis
datCredit_valid[, prob_beh_exp3_1 := predict(logitMod_beh_exp3_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_beh_exp3_2 := predict(logitMod_beh_exp3_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp3_1)
### RESULTS: 64.46%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp3_2)
### RESULTS:    64.46%
###             The coefficient estimate for the interaction term is "NA".
### CONCLUSION: Do not use missing value indicators for behavioral information variables

# - Clean up
rm(logitMod_beh_exp1_1, logitMod_beh_exp1_2, logitMod_beh_exp2_1, logitMod_beh_exp2_2, logitMod_beh_exp3_1, logitMod_beh_exp3_2); gc()
datCredit_valid[, `:=` (prob_beh_exp1_1=NULL, prob_beh_exp1_2=NULL, prob_beh_exp2_1=NULL, prob_beh_exp2_2=NULL, prob_beh_exp3_1=NULL, prob_beh_exp3_2=NULL)]

# --- 3.3 Final model (no need to do a best subset procedure since there are only two variables) | Full analysis
# - Full model
logitMod_beh1 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_pre_lim_perc_imputed_med + slc_pmnt_method
                     , data=datCredit_train, family = "binomial")
# - Deviance and AIC
summary(logitMod_beh1)
### RESULTS: Null deviance = 275184; Residual deviance = 252143; AIC = 252159
# - Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_beh1)
### RESULTS: 8.37%
# - Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_beh1), confint.default(logitMod_beh1))), 3)
### RESULTS: Both the numerical and the categorical, along with all associated levels of that variable have odds ratio that are "quite" larger/smaller than 1
# - Residual deviance analysis
resid_deviance_glm(logitMod_beh1)
### RESULTS: Model fit is somewhat strained (2 diagnostics gave warnings)
# - Variable importance
varImport_logit(logitMod_beh1, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top three variables: [slc_acct_pre_lim_perc_imputed_med], [slc_pmnt_methodSuspense], and [slc_pmnt_methodStatement]
# - ROC analysis
datCredit_valid[, prob_beh1 := predict(logitMod_beh1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh1)
### RESTULS:    73.06%
### CONCLUSION: Do not run a best subset selection as there are only two variables in the model that are both significant

# --- 3.4 Final account-level information variables
# - Final variables
### CONCLUSION: Use [slc_acct_pre_lim_perc_imputed_med] and [slc_pmnt_method] as the account-level information variables.
# - Save variables
inputs_beh_fin <-  DefaultStatus1_lead_12_max ~ slc_acct_pre_lim_perc_imputed_med + slc_pmnt_method
pack.ffdf(paste0(genObjPath, "Beh_Formula"), inputs_beh_fin); gc()

# --- 3.5 Clean up
rm(cor_beh_spear, ind_row_spear, ind_col_spear, cor_beh_spear2, inputs_beh_fin, logitMod_beh1); gc()
datCredit_valid[,`:=`(prob_beh1=NULL)]




# ------ 4. Modelling & Feature Selection by theme "Portfolio-level information"
# --- 4.1 Correlation analysis using Spearman correlation
# - Correlation threshold
cor_thresh <- 0.6
# - Computing the correlation matrix
cor_por_spear <-cor(x=datCredit_train[!is.na(g0_Delinq_Any_Aggr_Prop_Lag_5),list(g0_Delinq_Any_Aggr_Prop, g0_Delinq_Any_Aggr_Prop_Lag_5, g0_Delinq_Ave, # Subsetting so that there are no missing values taken into account in the correlation matrix
                                                                                 InstalmentToBalance_Aggr_Prop, ArrearsToBalance_Aggr_Prop, CuringEvents_Aggr_Prop,
                                                                                 NewLoans_Aggr_Prop, NewLoans_Aggr_Prop_1, NewLoans_Aggr_Prop_3, NewLoans_Aggr_Prop_4, NewLoans_Aggr_Prop_5,
                                                                                 PerfSpell_Maturity_Aggr_Mean, AgeToTerm_Aggr_Mean,
                                                                                 InterestRate_Margin_Aggr_Med_1, InterestRate_Margin_Aggr_Med_2, InterestRate_Margin_Aggr_Med_3)],
                    method = "spearman")
# - Creating a correlalogram
corrplot(cor_por_spear, type="upper")
# - Setting the bottom halve of the correlation matrix to zero, since the matrix is symmetrical around the diagonal
cor_por_spear[lower.tri(cor_por_spear)] <- 0
# - Printing all correlations above the specified threshold
ind_row_spear <- which(abs(cor_por_spear)>cor_thresh & abs(cor_por_spear)<1)
ind_col_spear <- ind_row_spear %% ncol(cor_por_spear) + ncol(cor_por_spear)*(ind_row_spear %% ncol(cor_por_spear) == 0)
cor_por_spear2 <- data.table(x=rownames(cor_por_spear)[floor((ind_row_spear-1)/ncol(cor_por_spear)) + 1],
                             y=colnames(cor_por_spear)[ind_col_spear])
for(i in 1:(nrow(cor_por_spear2))) {cat("Absolute correlation above ", cor_thresh, " found for ", cor_por_spear2[i,1][[1]], " and ", cor_por_spear2[i,2][[1]], "\n")}
### RESULTS: High correlation detected within almost all lagged variables, among each other and other lagged variables.
### Conclusion:
###   These aggregated portfolio-level variables are expected to be highly correlated with each other, so the following conclusions may seek to overide many of these high correlations as to fit a model capable of capturing default contagion.
###     - The aggregated delinquency variables have high correlations among themselves and many other variables. Do not remove variables as they give vital information on default contagion.
###     - The aggregated new loans variables have high correlation among themselves (but not to other variables). Do not remove variables.
###     - The aggregated interest rate margin variables have high correlations among themselves and many other variables (and have negative correlation with the aggregated variables of delinquency...good sanity check). Do not remove as they may still give vital information of default contagion
###     - Remove [g0_Delinq_Ave] as it the information of this variable is inherently captured by the aggregated delinquency variables
###     - Remove either [AgeToTerm_Aggr_Mean] or [PerfSpell_Maturity_Aggr_Mean] as they are highly correlated and can give very similar information
###     - Remove [ArrearsToBalance_Aggr_Prop] as this variable is created with the problematic [Arrears] variable (which has zero values for all accounts for the last six months of the sampling window, i.e., July - December 2022)


# --- 4.2 Preliminary experimenting: Using insights from correlation analysis
# - [AgeToTerm_Aggr_Mean] vs [PerfSpell_Maturity_Aggr_Mean]
# Model fitting
logitMod_por_exp1_1 <- glm(DefaultStatus1_lead_12_max ~ AgeToTerm_Aggr_Mean
                           , data=datCredit_train, family="binomial")
logitMod_por_exp1_2 <- glm(DefaultStatus1_lead_12_max ~ PerfSpell_Maturity_Aggr_Mean
                           , data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_por_exp1_1)
### RESUTLS: Null deviance = 275184; Residual deviance = 272044; AIC = 272048
summary(logitMod_por_exp1_2)
### RESULTS: Null deviance = 275184; Residual deviance = 274819; AIC = 274823
# ROC analysis
datCredit_valid[, prob_por_exp1_1 := predict(logitMod_por_exp1_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_por_exp1_2 := predict(logitMod_por_exp1_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_por_exp1_1)
### RESULTS: 58.10%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_por_exp1_2)
### RESULTS:    51.95%
### CONCLUSION: Use [AgeToTerm_Aggr_Mean], as it has results in a model with a lower AIC, but  a slightly higher AUC

# - Clean up
rm(logitMod_por_exp1_1, logitMod_por_exp1_2); gc()
datCredit_valid[, `:=` (prob_por_exp1_1=NULL, prob_por_exp1_2=NULL)]


# --- 4.3 Best subset selection
# - Full logit model with all account-level information - Exclude variables using insights from correlation analysis:  [g0_Delinq_Ave] and [PerfSpell_Maturity_Aggr_Mean]
logitMod_por1 <- glm(inputs_por1 <- DefaultStatus1_lead_12_max ~ g0_Delinq_Any_Aggr_Prop + g0_Delinq_Any_Aggr_Prop_Lag_5 +
                       InstalmentToBalance_Aggr_Prop + CuringEvents_Aggr_Prop +
                       NewLoans_Aggr_Prop + NewLoans_Aggr_Prop_1 + NewLoans_Aggr_Prop_3 + NewLoans_Aggr_Prop_4 + NewLoans_Aggr_Prop_5 +
                       AgeToTerm_Aggr_Mean +
                       InterestRate_Margin_Aggr_Med_1 + InterestRate_Margin_Aggr_Med_2 + InterestRate_Margin_Aggr_Med_3
                     , data=datCredit_train, family="binomial")

# - Assess full model
# Deviance and AIC
summary(logitMod_por1)
### RESULTS: Null deviance = 266435; Residual deviance = 262851; AIC = 262879
# Odds Ratio analysis 
round(exp(cbind(OR = coef(logitMod_por1), confint.default(logitMod_por1))), 3)
### RESULTS: There is no variable with an odds ratio close to one, this is an indicatiion that the variables are significant in predicting default.
# Variable importance
varImport_logit(logitMod_por1, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top three variables: [g0_Delinq_Any_Aggr_Prop], [AgeToTerm_Aggr_Mean], and [NewLoans_Aggr_Prop_1]
# ROC analysis
datCredit_valid[, prob_por1 := predict(logitMod_por1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_por1)
### RESULTS:    58.31%
###             The estimated coefficients and standard errors of all variables related to [InterestRate_Margin_imputed_Aggr] and [CuringEvents_Aggr_Prop] are very large.
###             Some variables are insignificant: [InstalmentToBalance_Aggr_Prop], [CuringEvents_Aggr_Prop], [NewLoans_Aggr_Prop], [NewLoans_Aggr_Prop_4], and [InterestRate_Margin_Aggr_Med_1]
### CONCLUSION: Proceed to using best subset selection on the input space.


# - Best subset selection
logitMod_por_best <- MASS::stepAIC(logitMod_por1, direction="both")
# Start AIC = 262879.3
# End AIC = 262873.9
summary(logitMod_por_best)
### RESULTS: [NewLoans_Aggr_Prop_5] has a p-value of 0.13 - this is dubious, but we'll keep it in the model for now (it might be significant when combining with the broader input spcae)
###          Null deviance = 266435; Residual deviance = 262854; AIC = 262874
# Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_por_best)
### RESULTS: 1.34%
# Residual deviance logitMod_por_best
resid_deviance_glm(logitMod_por_best)
### RESULTS: Model fit is somewhat strained (1 diagnostics gave warnings)
# Variable importance
varImport_logit(logitMod_por_best, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top three variables: [g0_Delinq_Any_Aggr_Prop], [NewLoans_Aggr_Prop_1], and [AgeToTerm_Aggr_Mean]
# ROC Analysis
datCredit_valid[, prob_por_best := predict(logitMod_por_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_por_best)
### RESULTS: 58.3%
length(all.vars(getCall(logitMod_por1)$formula)); length(all.vars(getCall(logitMod_por_best)$formula))
### RESULTS:      The AIC of the model from the best subset selection is lower than the full model (262874 vs 262879).
###               The AUC of the model from the best subset selection is almost identical than the full model (58.3% vs 58.31%).
###               The number of variables in the model from the best subset selection has been reduced to 10 from the full model which has 15 variables.

### CONCLUSION:   Use the specified variables of the best subset selection in the dynamic variable selection process.


# --- 4.4 Final portfolio-level information variables
# - Final variables
### CONCLUSION: Use [g0_Delinq_Any_Aggr_Prop], [g0_Delinq_Any_Aggr_Prop_Lag_5], [NewLoans_Aggr_Prop_1], [NewLoans_Aggr_Prop_3], [NewLoans_Aggr_Prop_4], [NewLoans_Aggr_Prop_5], [AgeToTerm_Aggr_Mean],
###                 [InterestRate_Margin_Aggr_Med_2], [InterestRate_Margin_Aggr_Med_3]
# - Save variables
inputs_por_fin <- DefaultStatus1_lead_12_max ~ g0_Delinq_Any_Aggr_Prop + g0_Delinq_Any_Aggr_Prop_Lag_5 + NewLoans_Aggr_Prop_1 + NewLoans_Aggr_Prop_3 + NewLoans_Aggr_Prop_4 + NewLoans_Aggr_Prop_5 + AgeToTerm_Aggr_Mean +
  InterestRate_Margin_Aggr_Med_2 + InterestRate_Margin_Aggr_Med_3
pack.ffdf(paste0(genObjPath, "Por_Formula"), inputs_por_fin); gc()
### CONCLUSION:    Use [inputs_por_fin] as set of portfolio-level information variables


# --- 4.5 Clean up
datCredit_valid[, `:=` (prob_por1=NULL, prob_por_best=NULL)]
rm(cor_por_spear, ind_row_spear, ind_col_spear, cor_por_spear2,
   logitMod_por_best, logitMod_por1); gc()


# --- 4.6 Variable importance analysis of portfolio variables
# - Getting the final selection of portfolio level variables
vars_port <- labels(terms(inputs_por_fin))

# - Initialising plotting dataset
datPlot_port <- data.table(Variable=vars_port,
                           AUC=rep(0,length(vars_port)))

# - Looping and fitting a logit models containing only a single variable from which an ROC analysis is conducte in that model to get an AUC value as a measure of variable importance
for (i in 1:length(vars_port)){
  logitMod_por_i <- glm(as.formula(paste0("DefaultStatus1_lead_12_max~", vars_port[i])), data=datCredit_train, family="binomial")
  datCredit_valid[, prob_por_i := predict(logitMod_por_i, newdata = datCredit_valid, type="response")]
  datPlot_port[i, AUC:=auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_por_i)]
}

# - Plotting the variable importance
# Adjusting the dataset to enable easy annotations/labels
datPlot_port[, Label:=paste0(sprintf("%.2f",AUC*100),"%")]
# Plotting parameters
col.v <- brewer.pal(9, "Blues")[c(7)]
col.v2 <- rep(c(brewer.pal(9, "Blues")[c(6)]),9)
col.v3 <- rep("white", 9)
label.v <- c("Variable")
# Plot
(g_adv_port <- ggplot(datPlot_port, aes(x=Variable, y=AUC)) +
    theme_minimal() + theme(legend.position = "bottom", axis.text.x=element_text(angle=90)) + labs(x="Variable", y="AUC(%)") +
    geom_col(colour=col.v, fill=col.v2, position="dodge") +
    geom_label(aes(label=Label), fill = col.v2, colour = col.v3, position=position_dodge(0.9), vjust=1.2) +
    scale_colour_manual(name="Model:", values=col.v, labels=label.v) +
    scale_fill_manual(name="Model:", values=col.v, labels=label.v) +
    scale_y_continuous(breaks=pretty_breaks(), label=percent))
# Save plot
dpi<-240
ggsave(g_adv_port, file=paste0(genFigPath, "AdvVars_Portfolio_Vars_Select_Summary.png"), width=3000/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Clean up
rm(dpi, col.v, col.v2, col.v3, label.v, datPlot_port, g_adv_port); gc()




# ------ 5. Combing the basic, intermediate-, and advanced variables
# --- Loading the basic, intermediate-, and advanced variables
unpack.ffdf(paste0(genObjPath, "Basic_Com_Formula"), tempPath)
unpack.ffdf(paste0(genObjPath, "Int_Formula"), tempPath)
unpack.ffdf(paste0(genObjPath, "Del_Formula"), tempPath); unpack.ffdf(paste0(genObjPath, "Beh_Formula"), tempPath); unpack.ffdf(paste0(genObjPath, "Por_Formula"), tempPath)

# --- Formula compilation
inputs_adv1 <- as.formula(paste0("DefaultStatus1_lead_12_max~", paste(labels(terms(inputs_fin_bas)), collapse="+"), "+", paste(labels(terms(inputs_int)), collapse="+"), "+",
                                 paste(labels(terms(inputs_del_fin)), collapse="+"), "+", paste(labels(terms(inputs_beh_fin)), collapse="+"), "+", paste(labels(terms(inputs_por_fin)), collapse="+")))
# --- Fitting the full model
logitMod_adv1 <- glm(inputs_adv1, data=datCredit_train, family="binomial")
# --- Assess full model
summary(logitMod_adv1)
### RESULTS:    Insignificant variables: [AgeToTerm], [M_Repo_Rate_9], [M_Inflation_Growth_9], [M_RealIncome_Growth_12], [M_DTI_Growth_12], [slc_pmnt_method], [g0_Delinq_Any_Aggr_Prop], [g0_Delinq_Any_Aggr_Prop_Lag_5] [NewLoans_Aggr_Prop_4], and [InterestRate_Margin_Aggr_Med_2]
### CONCLUSION: Remove insignificant variables and refit model

# --- Formula compilation
inputs_adv2 <- as.formula(paste0("DefaultStatus1_lead_12_max~", paste(labels(terms(inputs_adv1))[-unlist(lapply(c("AgeToTerm", "M_Repo_Rate_9", "M_Inflation_Growth_9",
                                                                                                                  "M_RealIncome_Growth_12", "M_DTI_Growth_12", "slc_pmnt_method", 
                                                                                                                  "g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_5", "NewLoans_Aggr_Prop_4", "InterestRate_Margin_Aggr_Med_2"), function(X) which(X==labels(terms(inputs_adv1)))))], collapse="+")))
# --- Fitting the full model
logitMod_adv2 <- glm(inputs_adv2, data=datCredit_train, family="binomial")
# --- Assess full model
summary(logitMod_adv2)
### RESULTS:    Insignificant variables: [NewLoans_Aggr_Prop_3] has a p-value of 0.11, keep in model for now
### CONCLUSION: Continue with assessment

# --- Evaluating the full model
# - Deviance and AIC
summary(logitMod_adv2)
### RESULTS: Null deviance = 255631; Residual deviance = 168280; AIC = 168332
# - Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_adv2)
### RESULTS: 34.17%
# - Odds Ratio analysis 
round(exp(cbind(OR = coef(logitMod_adv2), confint.default(logitMod_adv2))), 3)
### [Balance], [Principal], and [slc_past_due_amt_imputed_med] have ratios close to 1, but this might not give an accurate indication because of the range of these variables
# - Residual deviance logitMod_por_best
resid_deviance_glm(logitMod_adv2)
### RESULTS: Model fit is somewhat strained (2 diagnostics gave warnings)
# - Variable importance
varImport_logit(logitMod_adv2, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top three variables: [PrevDefaultsTRUE], [g0_Delinq], and [TimeInPerfSpell]
# - ROC analysis
datCredit_train[, prob_adv2 := predict(logitMod_adv2, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_adv2 := predict(logitMod_adv2, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_adv2)
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_adv2)
### RESULTS: Training = 90.53%
###          Validation = 90.44%
# VIF analysis
car::vif(logitMod_adv2)
### RESULTS:  Most variables have low VIF values (<10), however most macroeconomic variables have large VIF values (>10)

# --- Clean up
rm(logitMod_adv1)
datCredit_train[,prob_adv2:=NULL]; datCredit_valid[,prob_adv2:=NULL]

# --- Results & Conclusion
### RESULTS:    All variables are significant and have reasonable standard errors; except for two portfolio-level variables
###             Model is not overfitted as evidenced by the small change in AUC when a ROC analysis is conducted on the training- and validation datasets (90.49% vs 90.04%)
###             The VIF values are as expected, with most macroeconomic variables, and one portfolio-level variable, having high VIF values (>10) compared to the other variables that have low VIF values (<10)

### CONCLUSION: Refit model on subsampled (training) dataset and re-evaluate.


# --- Save formulas
inputs_adv <- formula(logitMod_adv2)
pack.ffdf(paste0(genObjPath, "Adv_Formula"), inputs_adv); gc()




# ------ 6. Clean up
# --- Clean up
rm(logitMod_com1, logitMod_com2, logitMod_com3, logitMod_com4, form_com1, form_com2, form_com3, form_com4)
datCredit_train[, prob_com4:=NULL]; datCredit_valid[, prob_com4:=NULL]




# ------ 7. Subsampling and refitting the final model(s)
# --- 7.1 Preliminaries
# - Field names
stratifiers <- c("DefaultStatus1_lead_12_max", "Date") # Must at least include target variable used in graphing event rate
targetVar <- "DefaultStatus1_lead_12_max"

# - Subsampling & resampling parameters
smp_size <- 250000 # fixed size of downsampled set
smp_perc <- smp_size / ( datCredit_train[complete.cases(mget(stratifiers)), mget(stratifiers)][,.N] ) # Implied sampling fraction for downsampling step

# --- 7.2 Downsample data into a set with a fixed size (using stratified sampling) before implementing resampling scheme
set.seed(1)
datCredit_smp <- datCredit_train %>%group_by(across(all_of(stratifiers))) %>% slice_sample(prop=smp_perc) %>% as.data.table()
cat( (datCredit_smp[is.na(get(targetVar)), .N] == 0) %?% 'SAFE: No missingness in target variable.\n' %:% 
       'WARNING: Missingness detected in target variable.\n')
### RESULTS: Subasmpling is successful.

# --- 7.3 Refitting final model
# - Load in the models input spaces
unpack.ffdf(paste0(genObjPath, "Adv_Formula"), tempPath)

# - Refit models to subsampled dataset
logitMod_smp <- glm(inputs_adv, data=datCredit_smp, family="binomial")

# - Deviance and AIC
summary(logitMod_smp)
### RESULTS: Insignificant variables are: [Balance], [Principal], [M_DTI_Growth_6], [NewLoans_Aggr_Prop_3], and [NewLoans_Aggr_Prop_5]
###          Sign of [g0_Delinq_Num] is still negative, but this might be due to the unique composition of the input space.
# - Variable importance
varImport_logit(logitMod_smp, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Subsampled: Top three variables: [PrevDefaultsTRUE], [g0_Delinq], and [g0_Delinq_SD_6] ([TimeInPerfSpell] is the 4th "most important" variable)
###          Full:       Top three variables: [PrevDefaultsTRUE], [g0_Delinq], and [TimeInPerfSpell]
###          None of the insignificant variables of the model trained on the subsampled data are among the "most important" variables of the model trained on the full data.

### CONCLUSION: There are a few insignificant variables compared when refitting the model on the subsampled (training) dataset.
###             It might be worth removing these variables, especially since they aren't rank as being "important" in the model trained on the full data.

# --- 7.4 Clean up
rm(stratifiers, targetVar, smp_size, smp_perc, datCredit_smp, logitMod_smp); gc()



