# ========================== MODEL DEFAULT RISK - BASIC =================================
# Develop several "basic" logistic regression models ranging from models with few
# features to models with more features to predict default risk.
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha, Esmerelda Oberholzer

# DESCRIPTION:
# This script uses the previously prepared credit dataset  to create multiple logistic
# regression models for default per modelling themes. The focus of this
# script is non forward looking information (macroeconomic variables).
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

# GOAL: Series of candidate models, starting with simplest logit-models containing ONLY the most basic of information, e.g., delinquency-themed input spaces.
# Focus: default risk (PD-model) + most basic input space: delinquency-themed
# Approach: simple to complex: 1) model-by-input (expanding input space); 2) model-by-segment (expanding segmentation scheme); 3) modelling technique f
# Aspects to consider include: missing value treatments, interactions, 
# transformations (Tukey Power, Box-Cox, Yeo-Johnson); ask Marcel, also in sec. 5 in Std-PrinciplesForDataPrep




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

### AB: Look at a correlalogram - cFLI codebase | can be used as a callable function


# --- 2.1 Correlation analysis using Spearman correlation
# - Correlation threshold
cor_thresh <- 0.6
# - Computing a correlation matrix
cor_ali_spear <- cor(x=datCredit_train[,list(Age_Adj, TimeInPerfSpell, Term, Balance, InterestRate_Margin_imputed_mean,
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
### RESULTS: High-correlation detected within [Age_Adj], [TimeInPerfSpell], [AgeToTerm], [Balance], [BalanceToPrincipal], [Instalment], and [Principal]

### Conclusion:
###   Remove either [Age_Adj] or [TimeInPerfSpell]
###   Remove two of [Principal], [Balance] and [Instalment]
###   Remove [AgeToTerm]; Highly correlated with both [Age_Agj] and [TimeInPerfSpell]
###   Remove [BalanceToPrincipal]; Too many high-correlations with other variables



# --- 2.2 Prelimanry experimenting: Using insights from correlation analysis
# - [Age_Adj] vs [TimeInPerfSpell]
logitMod_ali_exp1_1 <- glm(DefaultStatus1_lead_12_max ~ TimeInPerfSpell
                           , data=datCredit_train, family="binomial")
logitMod_ali_exp1_2 <- glm(DefaultStatus1_lead_12_max ~ Age_Adj
                           , data=datCredit_train, family="binomial")
summary(logitMod_ali_exp1_1) # Null deviance = 275184; Residual deviance = 274001; AIC = 274005
summary(logitMod_ali_exp1_2) # Null deviance = 275184; Residual deviance = 274579; AIC = 274583
datCredit_valid[, prob_ali_exp1_1 := predict(logitMod_ali_exp1_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_ali_exp1_2 := predict(logitMod_ali_exp1_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp1_1) # 60.11%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp1_2) # 51.12%
### CONCLUSION:   Use [TimeInPerfSpell] as it is has a higher AUC value and has a lower AIC

# - [Balance] vs [Instalment] vs [Principal]
logitMod_ali_exp2_1 <- glm(DefaultStatus1_lead_12_max ~ Balance
                           , data=datCredit_train, family="binomial")
logitMod_ali_exp2_2 <- glm(DefaultStatus1_lead_12_max ~ Instalment
                           , data=datCredit_train, family="binomial")
logitMod_ali_exp2_3 <- glm(DefaultStatus1_lead_12_max ~ Principal
                           , data=datCredit_train, family="binomial")
summary(logitMod_ali_exp2_1) # Null deviance = 275184; Residual deviance = 275014; AIC = 275018
summary(logitMod_ali_exp2_2) # Null deviance = 275184; Residual deviance = 274908; AIC = 274912
summary(logitMod_ali_exp2_3) # Null deviance = 275184; Residual deviance = 274908; AIC = 273603
datCredit_valid[, prob_ali_exp2_1 := predict(logitMod_ali_exp2_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_ali_exp2_2 := predict(logitMod_ali_exp2_2, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_ali_exp2_3 := predict(logitMod_ali_exp2_3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp2_1) # 52.19%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp2_2) # 53.47%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp2_3) # 57.5%
### CONCLUSION:   Both [Balance] and [Instalment] are period-level variables, whilst [Principal] is an account-level variable. Therefore, keep [Principal] and decide between the other two period-level variables.
###               Keep [Balance] as it has a lower correlation (0.8045) with [Principal] compared to the correlation between [Instalment] and [Principal] (0.8971).

# - Clean up
rm(logitMod_ali_exp1_1, logitMod_ali_exp1_2, logitMod_ali_exp2_1, logitMod_ali_exp2_2, logitMod_ali_exp2_3); gc()
datCredit_valid[,`:=` (prob_ali_exp1_1=NULL, prob_ali_exp1_2=NULL, prob_ali_exp2_1=NULL, prob_ali_exp2_2=NULL, prob_ali_exp2_3=NULL)]



# --- 2.3 Best subset selection
# - Full logit model with all account-level information - Exclude variables using insights from above analysis: [Age_Adj]; [Instalment]; [AgeToTerm]; [BalanceToPrincipal]
logitMod_ali1 <- glm(DefaultStatus1_lead_12_max ~ TimeInPerfSpell + Term + Balance +
                       InterestRate_Margin_imputed_mean + Principal
                       , data=datCredit_train, family="binomial")
### WARNING       :   glm.fit: fitted probabilities numerically 0 or 1
### INVESTIGATION :   Model fit seems fine, predictions investigated below (those made on the validation set) and none are exactly 0 or 1.
# - Deviance and AIC
summary(logitMod_ali1) # Null deviance = 275184; Residual deviance = 268571; AIC = 268583
# - Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_ali1), confint.default(logitMod_ali1))), 3)
# - ROC analysis
datCredit_valid[, prob_ali1 := predict(logitMod_ali1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali1) # 63.64%

# - Best subset selection
logitMod_ali_best <- MASS::stepAIC(logitMod_ali1, direction="both")
### WARNING       :   glm.fit: fitted probabilities numerically 0 or 1
# Start AIC = 268582.9
# End AIC = 268582.9
### Model inputed is returned as final model
summary(logitMod_ali_best) # No insignificant variables
### CONCLUSION:   Use of all specified variables

# - Clean up
rm(logitMod_ali_best, logit_ali1)
datCredit_valid[,prob_ali1:=NULL]



# --- 2.4 Subtheme: [InterestRate_Margin_imputed_mean] vs [InterestRate_Margin_bin]
# - Fitting the model
logitMod_ali_exp_3 <- glm(DefaultStatus1_lead_12_max ~ TimeInPerfSpell + Term + Balance +
                          InterestRate_Margin_imputed_bin + Principal
                          , data=datCredit_train, family="binomial")
### WARNING : glm.fit: fitted probabilities numerically 0 or 1
# - Deviance and AIC
summary(logitMod_ali_exp_3) # Null deviance = 275184; Residual deviance = 268430; AIC = 268442
# - Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_ali_exp_3), confint.default(logitMod_ali_exp_3))), 3)
# - ROC analysis
datCredit_valid[, prob_ali_exp3 := predict(logitMod_ali_exp_3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_ali_exp3) # 63.39%
### RESULTS   : Model with binned variable has lower AIC (268442 vs 268583) and lower AUC (63.34% vs 63.64%) than the model with the raw variable.
### CONCLUSION: Use [InterestRate_Margin_imputed_mean].

# - Clean up
rm(logitMod_ali_exp_3)
datCredit_valid[,prob_ali_exp3:=NULL]



# --- 2.5 Final account-level information variables
inputs_ali_fin <-  DefaultStatus1_lead_12_max ~ TimeInPerfSpell + Term + Balance + InterestRate_Margin_imputed_mean + Principal
pack.ffdf(paste0(genObjPath, "ALI_Formula"), inputs_ali_fin); gc()
###  CONCLUSION:    Use [inputs_ali_fin] as set of account-level information variables



# --- 2.6 Clean up
rm(cor_ali_spear, ind_row_spear, ind_col_spear, cor_ali_spear2, inputs_ali_fin)




# ------ 3. Modelling & Feature Selection by theme "Delinquency information"
# --- 3.1 Correlation analysis using spearman correlation
# - Correlation threshold
cor_thresh <- 0.6
# - Computing the correlation matrix
cor_del_spear <-cor(x=datCredit_train[!is.na(g0_Delinq_SD_6),list(PrevDefaults=as.numeric(PrevDefaults),
                                                                  PerfSpell_Num, g0_Delinq, g0_Delinq_Num, g0_Delinq_SD_4, g0_Delinq_SD_6,
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
### RESULTS: High-correlation detected within [PerfSpell_Num], [Prev_Defaults], [g0_Delinq], [g0_Delinq_SD_4], [g0_Delinq_SD_6], [g0_Delinq_Num], and [slc_past_due_amt_imputed]

### Conclusion:
###   Remove either [PrevDefaults] or [PerfSpell_Num]
###   Keep [g0_Delinq], [g0_Delinq_SD_6], and [g0_Delinq_SD_4] as these variables are expected to have a high correlation
###   Keep both [PerfSpell_g0_Delinq_Num] and [PerfSpell_g0_Delinq_SD] as the former is on a period level and the latter on an spell level
###   Keep [slc_past_due_amt_imputed_med]; even though [g0_Delinq] has high correlation with [slc_past_due_amt_imputed_med] (0.8996), it is not as granular | This variable also serves as a proxy for [Arrears], which in of itself has missing values for all accounts over a 6 month period



# --- 3.2 Prelimanry experimenting: Using insights from correlation analysis
# - [PrevDefaults] vs [PerfSpell_Num]
logitMod_del_exp1_1 <- glm(DefaultStatus1_lead_12_max ~ PrevDefaults
                           , data=datCredit_train, family="binomial")
logitMod_del_exp1_2 <- glm(DefaultStatus1_lead_12_max ~ PerfSpell_Num
                           , data=datCredit_train, family="binomial")
summary(logitMod_del_exp1_1) # Null deviance = 275184; Residual deviance = 226130; AIC = 226134
summary(logitMod_del_exp1_2) # Null deviance = 275184; Residual deviance = 263639; AIC = 263643
datCredit_valid[, prob_del_exp1_1 := predict(logitMod_del_exp1_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_del_exp1_2 := predict(logitMod_del_exp1_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del_exp1_1) # 75.45%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del_exp1_2) # 60.04%
### CONCLUSION:   Use [PrevDefaults] in the modelling process and conduct an post-hoc experiment with the final variables and replace [PrevDefaults] with [PerfSpell_Num]
###               In a PD modelling context, which is less dynamic than a SICR model, the less dynamic variable is more useful given the 12-month horizon of the typical PD model.

# - Clean up
rm(logitMod_del_exp1_1, logitMod_del_exp1_2); gc()
datCredit_valid[, `:=` (prob_del_exp1_1=NULL, prob_del_exp1_2=NULL)]



# --- 3.3 Best subset selection
# - Full logit model with all account-level information - Exclude variables using insights from correlation analysis:  [PerfSpell_Num]
logitMod_del1 <- glm(inputs_del1 <- DefaultStatus1_lead_12_max ~ PrevDefaults + g0_Delinq + g0_Delinq_Num + g0_Delinq_SD_4 + g0_Delinq_SD_6 + 
                       PerfSpell_g0_Delinq_Num + slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3 + slc_past_due_amt_imputed_med
                     , data=datCredit_train, family="binomial")

# - Assess full model
# Deviance and AIC
summary(logitMod_del1) # Null deviance = 255631; Residual deviance = 171760; AIC = 171784
# Odds Ratio analysis 
round(exp(cbind(OR = coef(logitMod_del1), confint.default(logitMod_del1))), 3)
# ROC analysis
datCredit_valid[, prob_del1 := predict(logitMod_del1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del1) # 87.72%

# - Best subset selection
logitMod_del_best <- MASS::stepAIC(logitMod_del1, direction="both")
# Start AIC = 171783.9
# End AIC = 171783.9
### Model inputed is returned as final model
summary(logitMod_del_best)
### CONCLUSION: No insignificant variables, use all specified variables in dynamic variable selection process
### NOTE      : [g0_Delinq_Num] has the wrong sign (negative); might be corrected in the final model with the combination of additional variables



# --- 3.4 Subtheme: Non-linear relationships within [g0_Delinq]
# - Fitting the model
logitMod_del_exp2 <- glm(DefaultStatus1_lead_12_max ~ PrevDefaults + g0_Delinq_fac + g0_Delinq_Num + g0_Delinq_SD_4 + g0_Delinq_SD_6 +
                         PerfSpell_g0_Delinq_Num + slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3 + slc_past_due_amt_imputed_med
                         , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_del_exp2) # Null deviance = 255631; Residual deviance = 171747; AIC = 171773
# - ROC analysis
datCredit_valid[, prob_del_exp2 := predict(logitMod_del_exp2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del_exp2) # 87.61%
### RESULTS   : Model with factorised delinquency variable has lower AIC (123648 vs 122791) and a slightly higher AUC (95.93% vs 95.91%) than the model with the raw variable 
### COMCLUSION: Use [g0_Delinq_fac] as it has better predictive power and allows for non-linear relationships between the delinquency level and default

# - Clean up
rm(logitMod_del_exp2); gc()
datCredit_valid[, prob_del_exp2:=NULL]



# --- 3.5 Subtheme: Missing value indicators
# - Fitting the model
logitMod_del_exp3_1 <- glm(DefaultStatus1_lead_12_max ~ PrevDefaults + g0_Delinq_fac + g0_Delinq_Num + g0_Delinq_SD_4 + g0_Delinq_SD_6 + 
                            PerfSpell_g0_Delinq_Num + slc_acct_roll_ever_24_imputed_mean/value_ind_slc_acct_roll_ever_24 + slc_acct_arr_dir_3 + # [slc_acct_arr_dir_3] already has a dedicated level for missing values, therefore do not include the associated missing value indicator
                            slc_past_due_amt_imputed_med/value_ind_slc_past_due_amt
                           , data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_del_exp3_1) # Null deviance = 255631; Residual deviance = 171747; AIC = 171773
# - ROC analysis
datCredit_valid[, prob_del_exp3_1 := predict(logitMod_del_exp3_1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del_exp3_1) # 87.61%
### RESULTS   : Model with missing value indicators has lower AIC (171773 vs 171784) but lower AUC (87.72% vs 87.61%) than the model without the missing value indicators (although the difference is very small)
###             Both [slc_past_due_amt_imputed:value_ind_slc_past_due_amt] and [slc_past_due_amt_imputed_med:value_ind_slc_past_due_amt ] have estimated coefficients of "NA".
### COMCLUSION: Refit model without [value_ind_slc_past_due_amt].

# - Fitting the second model wihout [value_ind_slc_past_due_amt]
logitMod_del_exp3_2 <- glm(DefaultStatus1_lead_12_max ~ PrevDefaults + g0_Delinq_fac + g0_Delinq_Num + g0_Delinq_SD + 
                             PerfSpell_g0_Delinq_Num + slc_acct_roll_ever_24_imputed_mean/value_ind_slc_acct_roll_ever_24 + slc_acct_arr_dir_3 + # [slc_acct_arr_dir_3] already has a dedicated level for missing values, therefore do not include the associated missing value indicator
                             slc_past_due_amt_imputed_med
                           , data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_del_exp3_2) # Null deviance = 275184; Residual deviance = 143042; AIC = 143068
# ROC analysis
datCredit_valid[, prob_del_exp3_2 := predict(logitMod_del_exp3_1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del_exp3_2) # 87.61%
### RESULTS:    The interaction [slc_acct_roll_ever_24_imputed_mean:value_ind_slc_acct_roll_ever_24] is insignificant.
### CONCLUSION: Do not use missing value indicators in the delinquency themed variables.

# - Clean up
rm(logitMod_del_exp3_1, logitMod_del_exp3_2); gc()
datCredit_valid[, `:=`(prob_del_exp3_1=NULL, prob_del_exp3_2=NULL)]



# --- 3.6 Final delinquency information variables
inputs_del_fin <- DefaultStatus1_lead_12_max ~ PrevDefaults + g0_Delinq_fac + g0_Delinq_Num + g0_Delinq_SD + PerfSpell_g0_Delinq_Num +
  PerfSpell_g0_Delinq_SD + slc_acct_roll_ever_24_imputed/value_ind_slc_acct_roll_ever_24 + slc_acct_arr_dir_3 + slc_past_due_amt_imputed
pack.ffdf(paste0(genObjPath, "Del_Formula"), inputs_del_fin); gc()
###  CONCLUSION:    Use [inputs_del_fin] as set of account-level information variables



# --- 3.7 Clean up
rm(cor_del_spear, ind_row_spear, ind_col_spear, cor_del_spear2, logitMod_del_best, logitMod_del1, inputs_del_fin); gc()





# ------ 4. Modelling & Feature Selection by theme "Behavioural information"
# --- 4.1 Correlation analysis using Spearman correlation
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



# --- 4.2 Prelimanry experimenting: Using insights from correlation analysis
# - slc_acct_prepaid_perc_dir_12_imputed vs [slc_acct_pre_lim_perc_imputed]
logitMod_beh_exp1_1 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_prepaid_perc_dir_12_imputed_med
                           , data=datCredit_train, family="binomial")
### WARNING : glm.fit: fitted probabilities numerically 0 or 1
logitMod_beh_exp1_2 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_pre_lim_perc_imputed_med
                           , data=datCredit_train, family="binomial")
summary(logitMod_beh_exp1_1) # Null deviance = 275184; Residual deviance = 275147; AIC = 275151
summary(logitMod_beh_exp1_2) # Null deviance = 275184; Residual deviance = 266557; AIC = 266561
datCredit_valid[, prob_beh_exp1_1 := predict(logitMod_beh_exp1_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_beh_exp1_2 := predict(logitMod_beh_exp1_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp1_1) # 61.37%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp1_2) # 64.46%
### CONCLUSION:   Use [slc_acct_pre_lim_perc_imputed] as it has results in a model with a lower AIC and higher AUC

# - Clean up
rm(logitMod_beh_exp1_1, logitMod_beh_exp1_2); gc()
datCredit_valid[, `:=` (prob_beh_exp1_1, prob_beh_exp1_2)]



# --- 4.3 Subtheme: Payment method variables compared
# - Full model (as fitted above)
logitMod_beh_exp2_1 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_pre_lim_perc_imputed_med + slc_pmnt_method
                          , data=datCredit_train, family = "binomial")
logitMod_beh_exp2_2 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_pre_lim_perc_imputed_med + pmnt_method_grp
                           , data=datCredit_train, family = "binomial")
# - Deviance and AIC
summary(logitMod_beh_exp2_1) # Null deviance = 275184; Residual deviance = 252143; AIC = 252159
summary(logitMod_beh_exp2_2) # Null deviance = 275184; Residual deviance = 253995; AIC = 254005
# - ROC analysis
datCredit_valid[, prob_beh_exp2_1 := predict(logitMod_beh_exp2_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_beh_exp2_2 := predict(logitMod_beh_exp2_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp2_1) # 73.06%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp2_2) # 72.73%
### CONCLUSION:  Use [slc_pmnt_method] as it results in a model with a slightly higher AUC and a lower AIC.

# - Clean up
rm(logitMod_beh_exp2_1, logitMod_beh_exp2_2); gc()
datCredit_valid[, `:=` (prob_beh_exp2_1=NULL, prob_beh_exp2_2=NULL)]



# --- 4.4 Subtheme: Missing value indicators
# - Full model (as fitted above)
logitMod_beh_exp3 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_pre_lim_perc_imputed/value_ind_slc_acct_pre_lim_perc + slc_pmnt_method # [slc_pmnt_method] already has a dedicated level for missing values, therefore do not include the missing value indicator
                         , data=datCredit_train, family = "binomial")
# - Deviance and AIC
summary(logitMod_beh_exp3) # Null deviance = 275184; Residual deviance = 252143; AIC = 252159
# - ROC analysis
datCredit_valid[, prob_beh_exp3 := predict(logitMod_beh_exp3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp3) # 73.06%
### RESULTS:    The coefficient estimate for the interaction term is "NA".
### CONCLUSION: Do not use missing value indicators for behavioral information variables

# - Clean up
rm(logitMod_beh_exp3, logitMod_beh_exp3); gc()
datCredit_valid[, prob_beh_exp3:=NULL]



# --- 4.5 Final behavioral information variables
inputs_beh_fin <- DefaultStatus1_lead_12_max ~ slc_acct_pre_lim_perc_imputed + slc_pmnt_method
pack.ffdf(paste0(genObjPath, "Beh_Formula"), inputs_beh_fin); gc()
###  CONCLUSION:    Use [inputs_beh_fin] as set of behavioral information variables



# --- 4.6 Clean up
rm(cor_beh_spear, ind_row_spear, ind_col_spear, cor_beh_spear2, inputs_beh_fin); gc()




# ------ 5. Modelling & Feature Selection by theme "Portfolio-level information"
# --- 5.1 Correlation analysis using Spearman correlation
# - Correlation threshold
cor_thresh <- 0.6
# - Computing the correlation matrix
cor_por_spear <-cor(x=datCredit_train[!is.na(g0_Delinq_Any_Aggr_Lag_12),list(g0_Delinq_Any_Aggr, g0_Delinq_Any_Aggr_Lag_5, g0_Delinq_Any_Aggr_Lag_12, g0_Delinq_Ave,
                                                                             InstalmentToBalance_Aggr, CuringEvents_Aggr,
                                                                             NewLoans_Aggr, NewLoans_Aggr_1, NewLoans_Aggr_3, NewLoans_Aggr_4, NewLoans_Aggr_5, NewLoans_Aggr_12,
                                                                             BookMaturity_Aggr, PerfSpell_Maturity_Aggr, AgeToTerm_Aggr,
                                                                             InterestRate_Margin_imputed_Aggr, InterestRate_Margin_imputed_Aggr_1, InterestRate_Margin_imputed_Aggr_2,
                                                                             InterestRate_Margin_imputed_Aggr_12)],
                    method = "spearman")
# - Creating a correlalogram
corrplot(cor_por_spear, type="upper")
# - Setting the bottom halve of the correlation matrix to zero, since the matrix is symettrical around the diagonal
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
###   The aggregated delinquency variables have high correlations among themselves and many other variables. Do not remove variables as they give vital information on default contagion.
###   The aggregated new loans variables have high correlation among themselves (but not to other variables). Do not remove variables.
###   The aggregated interest rate margin variables have high correlations among themselves and many other variables. Test if these variables are worth keeping (either in the best subset procedure are in a subtheme)
###   Remove [g0_Delinq_Ave] as it the information of this variable is inherently captured by the aggregated delinquency variables
###   Remove either [BookMaturity_Aggr] or [PerfSpell_Maturity_Aggr]


# --- 5.2 Prelimanry experimenting: Using insights from correlation analysis
# - [BookMaturity_Aggr] vs [PerfSpell_Maturity_Aggr]
logitMod_por_exp1_1 <- glm(DefaultStatus1_lead_12_max ~ BookMaturity_Aggr
                           , data=datCredit_train, family="binomial")
logitMod_por_exp1_2 <- glm(DefaultStatus1_lead_12_max ~ PerfSpell_Maturity_Aggr
                           , data=datCredit_train, family="binomial")
summary(logitMod_por_exp1_1) # Null deviance = 275184; Residual deviance = 272044; AIC = 272048
summary(logitMod_por_exp1_2) # Null deviance = 275184; Residual deviance = 274819; AIC = 274823
datCredit_valid[, prob_por_exp1_1 := predict(logitMod_por_exp1_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_por_exp1_2 := predict(logitMod_por_exp1_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_por_exp1_1) # 58.10%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_por_exp1_2) # 51.95%
### CONCLUSION:   Use [BookMaturity_Aggr], as it has results in a model with a lower AIC and a higher AUC

# - Clean up
rm(logitMod_por_exp1_1, logitMod_por_exp1_2); gc()
datCredit_valid[, `:=` (prob_por_exp1_1=NULL, prob_por_exp1_2=NULL)]



# --- 5.3 Best subset selection
# - Full logit model with all account-level information - Exclude variables using insights from correlation analysis:  [g0_Delinq_Ave]; [PerfSpell_Maturity_Aggr]
logitMod_por1 <- glm(inputs_por1 <- DefaultStatus1_lead_12_max ~ g0_Delinq_Any_Aggr + g0_Delinq_Any_Aggr_Lag_5 + g0_Delinq_Any_Aggr_Lag_12 +
                     InstalmentToBalance_Aggr + CuringEvents_Aggr +
                     NewLoans_Aggr + NewLoans_Aggr_1 + NewLoans_Aggr_3 + NewLoans_Aggr_4 + NewLoans_Aggr_5 + NewLoans_Aggr_12 +
                     BookMaturity_Aggr + AgeToTerm_Aggr + 
                     InterestRate_Margin_imputed_Aggr + InterestRate_Margin_imputed_Aggr_1 + InterestRate_Margin_imputed_Aggr_2 +
                     InterestRate_Margin_imputed_Aggr_12
                     , data=datCredit_train, family="binomial")

# - Assess full model
# Deviance and AIC
summary(logitMod_por1) # Null deviance = 248769; Residual deviance = 245789; AIC = 245823
# Odds Ratio analysis 
round(exp(cbind(OR = coef(logitMod_por1), confint.default(logitMod_por1))), 3)
# ROC analysis
datCredit_valid[, prob_por1 := predict(logitMod_por1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_por1) # 58.15%
### RESULTS   : [AgeToTerm_Aggr] has a fitted coefficient of "NA", whilst the estimated coefficients and standard errors of all variables related to [InterestRate_Margin_imputed_Aggr] are very large.
###             Most variables are insignificant.
### CONCLUSION: Run 


# - Best subset selection
logitMod_por_best <- MASS::stepAIC(logitMod_por1, direction="both")
# Start AIC = 245823
# End AIC = 245810.3
summary(logitMod_por_best) # No insignificant variables left in model
datCredit_valid[, prob_por_best := predict(logitMod_por_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_por_best) # 58.16%
length(all.vars(getCall(logitMod_por1)$formula)); length(all.vars(getCall(logitMod_por_best)$formula))
### RESULTS:      The AIC of the model from the best subset selection is lower than the full model (245810 vs 245823).
###               The AUC of the model from the best subset selection is slightly higher than the full model (58.16% vs 58.15%).
###               The number of variables in the model from the best subset selection has been reduced to 9 from the full model which has 19 variables.

### CONCLUSION:   Use the specified variables of the best subset selection in the dynamic variable selection process.



# --- 5.5 Final portfolio-level information variables
inputs_por_fin <- DefaultStatus1_lead_12_max ~ g0_Delinq_Any_Aggr + CuringEvents_Aggr + NewLoans_Aggr_1 + NewLoans_Aggr_3 + NewLoans_Aggr_12 + BookMaturity_Aggr +
  InterestRate_Margin_imputed_Aggr + InterestRate_Margin_imputed_Aggr_12
pack.ffdf(paste0(genObjPath, "Por_Formula"), inputs_por_fin); gc()
###  CONCLUSION:    Use [inputs_por_fin] as set of portfolio-level information variables



# --- 5.6 Clean up
datCredit_valid[, `:=` (prob_por1=NULL, prob_por_best=NULL)]
rm(cor_por_spear, ind_row_spear, ind_col_spear, cor_por_spear2,
   logitMod_por_best, logitMod_por1, inputs_por_fin); gc()






















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












