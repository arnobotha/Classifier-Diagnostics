# ============================ DEFAULT RISK - ADVANCED ==================================
# Develops the input space of an advanced-complexity level PD-model
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Marcel Muller (MM), Roland Breedt (RB), Dr Arno Botha (AB)

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
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2f.Data_Fusion1.R
#   - 3b.Data_Subsampled_Fusion2.R
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
                                                                  g0_Delinq_Num, g0_Delinq_SD_4, g0_Delinq_SD_6,
                                                                  PerfSpell_g0_Delinq_Num,
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
### RESULTS: High-correlation detected within [g0_Delinq_SD_4], [g0_Delinq_SD_6], [PerfSpell_g0_Delinq_Num], [g0_Delinq_Num]

### Conclusion:
###   Remove either [g0_Delinq_Num] or [PerfSpell_g0_Delinq_Num]
###   Keep [g0_Delinq_SD_4], and [g0_Delinq_SD_6] as these variables are expected to have a high correlation


# - [g0_Delinq_Num] or [PerfSpell_g0_Delinq_Num] - From the insights of the correlation analysis
# Model fitting
logitMod_del_exp1_1 <- glm(DefaultStatus1_lead_12_max ~ g0_Delinq_Num
                           , data=datCredit_train, family="binomial")
logitMod_del_exp1_2 <- glm(DefaultStatus1_lead_12_max ~ PerfSpell_g0_Delinq_Num
                           , data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_del_exp1_1)
### RESULTS: Null deviance = 279124; Residual deviance = 262711; AIC = 262715
summary(logitMod_del_exp1_2)
### RESULTS: Null deviance = 279124; Residual deviance = 269570; AIC = 269574
# ROC analysis
datCredit_valid[, prob_del_exp1_1 := predict(logitMod_del_exp1_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_del_exp1_2 := predict(logitMod_del_exp1_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del_exp1_1)
### RESULTS: 76.97%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del_exp1_2)
### RESULTS:    73.86%
### CONCLUSION: Use [g0_Delinq_Num] as it results in a model with higher predictive power

# - Missing value indicator for [slc_acct_roll_ever_24_imputed_mean] - From suggested sub-themes
# Model fitting
logitMod_del_exp2_1 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_roll_ever_24_imputed_mean
                           , data=datCredit_train, family="binomial")

logitMod_del_exp2_2 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_roll_ever_24_imputed_mean/value_ind_slc_acct_roll_ever_24
                           , data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_del_exp2_1)
### RESULTS: Null deviance = 279124; Residual deviance = 238586; AIC = 238590
summary(logitMod_del_exp2_2)
### RESULTS: Null deviance = 279124; Residual deviance = 236690; AIC = 236696
# ROC analysis
datCredit_valid[, prob_del_exp2_1 := predict(logitMod_del_exp2_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_del_exp2_2 := predict(logitMod_del_exp2_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del_exp2_1)
### RESULTS: 79.04%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del_exp2_2)
### RESULTS:    78.85%
### COMPARISON: Model with missing value indicators has lower AIC (236696 vs 238590) but lower AUC (78.85% vs 79.04%) than the model without the missing value indicators (although the difference is very small)
### COMCLUSION: Use [slc_acct_roll_ever_24_imputed_mean] without its missing value indicators as it results in a model with higher predictive power

# - Missing value indicator for [slc_past_due_amount_imputed_med] - From suggested sub-themes
# Model fitting
logitMod_del_exp3_1 <- glm(DefaultStatus1_lead_12_max ~ slc_past_due_amt_imputed_med
                           , data=datCredit_train, family="binomial")
### WARNING       :   glm.fit: fitted probabilities numerically 0 or 1
### INVESTIGATION :   Model fit seems fine, predictions investigated below (those made on the validation set) and none are exactly 0 or 1.
logitMod_del_exp3_2 <- glm(DefaultStatus1_lead_12_max ~ slc_past_due_amt_imputed_med/value_ind_slc_past_due_amt
                           , data=datCredit_train, family="binomial")
### WARNING       :   glm.fit: fitted probabilities numerically 0 or 1
# Deviance and AIC
summary(logitMod_del_exp3_1)
### RESULTS: Null deviance = 279124; Residual deviance = 251118; AIC = 251122
summary(logitMod_del_exp3_2)
### RESULTS:    Null deviance = 279124; Residual deviance = 251118; AIC = 251122
###             Estimated coefficient for the interaction between the raw variable and the missing value indicator results in an estimated coefficient that is "NA"
### COMCLUSION: Use [slc_past_due_amount_imputed_med] only

# - Clean up
rm(logitMod_del_exp1_1, logitMod_del_exp1_2, logitMod_del_exp2_1, logitMod_del_exp2_2, logitMod_del_exp3_1, logitMod_del_exp3_2); gc()
datCredit_valid[, `:=` (prob_del_exp1_1=NULL, prob_del_exp1_2=NULL, prob_del_exp2_1=NULL, prob_del_exp2_2=NULL, prob_del_exp3_1=NULL, prob_del_exp3_2=NULL)]



# --- 2.3 Best subset selection
# - Full logit model with all account-level information - Exclude variables using insights from correlation analysis:  [PerfSpell_Num]
logitMod_del1 <- glm(inputs_del1 <- DefaultStatus1_lead_12_max ~ PrevDefaults + TimeInPerfSpell + g0_Delinq_Num + g0_Delinq_SD_4 + g0_Delinq_SD_6 + 
                       slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3 + slc_past_due_amt_imputed_med
                     , data=datCredit_train, family="binomial")
# - Assess full model
# Deviance and AIC
summary(logitMod_del1)
### RESULTS: Null deviance = 279124; Residual deviance = 195983; AIC = 196005
# Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_del1)
### RESULTS: 29.79%
# Odds Ratio analysis 
round(exp(cbind(OR = coef(logitMod_del1), confint.default(logitMod_del1))), 3)
### [slc_past_due_amt_imputed_med] has a ratio close to 1, but this might not give an accurate indication because of the range of this variable
# Residual deviance analysis
resid_deviance_glm(logitMod_del1)
### RESULTS: Model fit is somewhat strained (2 diagnostics gave warnings)
# Variable importance
varImport_logit(logitMod_del1, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top three variables: [PrevDefaultsTRUE], [slc_past_due_amt_imputed_med], and [slc_acct_roll_ever_24_imputed_mean]
# ROC analysis
datCredit_valid[, prob_del1 := predict(logitMod_del1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_del1)
### RESULTS:    87.66%
### CONCLUSION: Proceed to using best subset selection on the input space.

# - Best subset selection
logitMod_del_best <- MASS::stepAIC(logitMod_del1, direction="both")
# Start AIC = 196004.7
# End AIC = 196004.7
### RESULTS:    Model inputted is returned as final model
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
### RESULTS: Null deviance = 279124; Residual deviance = 279026; AIC = 279030
summary(logitMod_beh_exp1_2)
### RESULTS: Null deviance = 279124; Residual deviance = 270315; AIC = 270319
# ROC analysis
datCredit_valid[, prob_beh_exp1_1 := predict(logitMod_beh_exp1_1, newdata = datCredit_valid, type="response")]; # datCredit_valid[prob_beh_exp1_1==0,.N]; datCredit_valid[prob_beh_exp1_1==1,.N] # There are no probabilities that are exactly equal to 0 or 1
datCredit_valid[, prob_beh_exp1_2 := predict(logitMod_beh_exp1_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp1_1)
### RESULTS: 61.49%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp1_2)
### RESULTS: 64.72%
### CONCLUSION:   Use [slc_acct_pre_lim_perc_imputed_med] as it results in a model with a lower AIC (270319 vs 279030) and higher AUC (64.72% vs 61.49%)

# - [slc_pmnt_method] vs [pmnt_method_grp] - From suggested sub-themes
# Model fitting
logitMod_beh_exp2_1 <- glm(DefaultStatus1_lead_12_max ~ slc_pmnt_method
                           , data=datCredit_train, family = "binomial")
logitMod_beh_exp2_2 <- glm(DefaultStatus1_lead_12_max ~ pmnt_method_grp
                           , data=datCredit_train, family = "binomial")
# - Deviance and AIC
summary(logitMod_beh_exp2_1)
### RESULTS: Null deviance = 279124; Residual deviance = 261080; AIC = 261094
summary(logitMod_beh_exp2_2)
### RESULTS: Null deviance = 279124; Residual deviance = 263151; AIC = 263159
# - ROC analysis
datCredit_valid[, prob_beh_exp2_1 := predict(logitMod_beh_exp2_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_beh_exp2_2 := predict(logitMod_beh_exp2_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp2_1)
### RESULTS: 68.80%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp2_2)
### RESULTS: 67.26%
### CONCLUSION:  Use [slc_pmnt_method] as it results in a model with a slightly higher AUC (68.80% vs 67.26%) and has a lower AIC (261094 vs 263159).

# - Missing value indicators - From suggested sub-themes
# Model fitting
logitMod_beh_exp3_1 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_pre_lim_perc_imputed_med
                           , data=datCredit_train, family = "binomial")
logitMod_beh_exp3_2 <- glm(DefaultStatus1_lead_12_max ~ slc_acct_pre_lim_perc_imputed_med/value_ind_slc_acct_pre_lim_perc # [slc_pmnt_method] already has a dedicated level for missing values, therefore do not include the missing value indicator
                           , data=datCredit_train, family = "binomial")
# Deviance and AIC
summary(logitMod_beh_exp3_1)
### RESULTS: Null deviance = 279124; Residual deviance = 270315; AIC = 270319
summary(logitMod_beh_exp3_2)
### RESULTS: Null deviance = 279124; Residual deviance = 270315; AIC = 270319
# - ROC analysis
datCredit_valid[, prob_beh_exp3_1 := predict(logitMod_beh_exp3_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_beh_exp3_2 := predict(logitMod_beh_exp3_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp3_1)
### RESULTS: 64.72%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh_exp3_2)
### RESULTS:    64.72%
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
### RESULTS: Null deviance = 279124; Residual deviance = 255570; AIC = 255586
# - Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_beh1)
### RESULTS: 8.55%
# - Odds Ratio analysis
round(exp(cbind(OR = coef(logitMod_beh1), confint.default(logitMod_beh1))), 3)
### RESULTS: Both the numerical and the categorical, along with all associated levels of that variable have odds ratio that are "quite" larger/smaller than 1
# - Residual deviance analysis
resid_deviance_glm(logitMod_beh1)
### RESULTS: Model fit is strained (3 diagnostics gave warnings)
# - Variable importance
varImport_logit(logitMod_beh1, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top variable: [slc_acct_pre_lim_perc_imputed_med]
# - ROC analysis
datCredit_valid[, prob_beh1 := predict(logitMod_beh1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_beh1)
### RESTULS:    73.37%
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
### NOTE: See script 3c(v).Model_DefaultRisk_Exp2 for variables selected from each theme.
cor_por_spear <-cor(x=datCredit_train[,list(g0_Delinq_Any_Aggr_Prop, g0_Delinq_Any_Aggr_Prop_Lag_12, g0_Delinq_Any_Aggr_Prop_Lag_3,
                                            DefaultStatus1_Aggr_Prop, DefaultStatus1_Aggr_Prop_Lag_9, DefaultStatus1_Aggr_Prop_Lag_2,
                                            InstalmentToBalance_Aggr_Prop, ArrearsToBalance_Aggr_Prop, CuringEvents_Aggr_Prop,
                                            NewLoans_Aggr_Prop_5, NewLoans_Aggr_Prop, NewLoans_Aggr_Prop_4,
                                            PerfSpell_Maturity_Aggr_Mean, AgeToTerm_Aggr_Mean,
                                            InterestRate_Margin_Aggr_Med, InterestRate_Margin_Aggr_Med_1, InterestRate_Margin_Aggr_Med_9)],
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
###     - The aggregated default variables have high correlations among themselves and many other variables. Do not remove variables as they give vital information on default contagion.
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
### RESUTLS: Null deviance = 279124; Residual deviance = 276175; AIC = 276179
summary(logitMod_por_exp1_2)
### RESULTS: Null deviance = 279124; Residual deviance = 278993; AIC = 278997
# ROC analysis
datCredit_valid[, prob_por_exp1_1 := predict(logitMod_por_exp1_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_por_exp1_2 := predict(logitMod_por_exp1_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_por_exp1_1)
### RESULTS: 57.59%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_por_exp1_2)
### RESULTS:    50.88%
### CONCLUSION: Use [AgeToTerm_Aggr_Mean], as it has results in a model with a lower AIC (276179 vs v) and a higher AUC (57.59% vs 50.88%)

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
### RESULTS: Null deviance = 279124; Residual deviance = 275790; AIC = 275818
# Odds Ratio analysis 
round(exp(cbind(OR = coef(logitMod_por1), confint.default(logitMod_por1))), 3)
### RESULTS: There is no variable with an odds ratio close to one, this is an indication that the variables are significant in predicting default.
# Variable importance
varImport_logit(logitMod_por1, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top three variables: [g0_Delinq_Any_Aggr_Prop], [AgeToTerm_Aggr_Mean], and [g0_Delinq_Any_Aggr_Prop_Lag_5]
# ROC analysis
datCredit_valid[, prob_por1 := predict(logitMod_por1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_por1)
### RESULTS:    58.06%
###             The estimated coefficients and standard errors of all variables related to [InterestRate_Margin_imputed_Aggr] and [CuringEvents_Aggr_Prop] are very large.
###             Some variables are insignificant: [InstalmentToBalance_Aggr_Prop], [CuringEvents_Aggr_Prop], [NewLoans_Aggr_Prop], [NewLoans_Aggr_Prop_4], and [InterestRate_Margin_Aggr_Med_1]
### CONCLUSION: Proceed to using best subset selection on the input space.


# - Best subset selection
logitMod_por_best <- MASS::stepAIC(logitMod_por1, direction="both")
# Start AIC = 275818.4
# End AIC = 275816.5
summary(logitMod_por_best)
### RESULTS: [InterestRate_Margin_Aggr_Med_2] has a p-value of 0.1364 - this is dubious, but we'll keep it in the model for now (it might be significant when combining with the broader input spcae)
###          Null deviance = 279124; Residual deviance = 275793; AIC = 275817
# Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_por_best)
### RESULTS: 1.19%
# Residual deviance logitMod_por_best
resid_deviance_glm(logitMod_por_best)
### RESULTS: Model fit is somewhat strained (1 diagnostics gave warnings)
# Variable importance
varImport_logit(logitMod_por_best, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top three variables: [g0_Delinq_Any_Aggr_Prop], [AgeToTerm_Aggr_Mean], and [NewLoans_Aggr_Prop_1]
# ROC Analysis
datCredit_valid[, prob_por_best := predict(logitMod_por_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_por_best)
### RESULTS: 58.06%
length(all.vars(getCall(logitMod_por1)$formula)); length(all.vars(getCall(logitMod_por_best)$formula))
### RESULTS:      The AIC of the model from the best subset selection is lower than the full model (275817 vs 275818).
###               The AUC of the model from the best subset selection is identical to that of the full model (58.06% vs 58.06%).
###               The number of variables in the model from the best subset selection has been reduced to 15 from the full model which has 12 variables.

### CONCLUSION:   Use the specified variables of the best subset selection in the dynamic variable selection process.


# --- 4.4 Final portfolio-level information variables
# - Final variables
### CONCLUSION: Use [g0_Delinq_Any_Aggr_Prop], [AgeToTerm_Aggr_Mean], [NewLoans_Aggr_Prop_1], [g0_Delinq_Any_Aggr_Prop_Lag_5], [InterestRate_Margin_Aggr_Med_1], [NewLoans_Aggr_Prop], [NewLoans_Aggr_Prop_4],
###                 [InterestRate_Margin_Aggr_Med_3], [NewLoans_Aggr_Prop_5], [CuringEvents_Aggr_Prop]                 
# - Save variables
inputs_por_fin <- DefaultStatus1_lead_12_max ~ g0_Delinq_Any_Aggr_Prop + AgeToTerm_Aggr_Mean + NewLoans_Aggr_Prop_1 + g0_Delinq_Any_Aggr_Prop_Lag_5 + InterestRate_Margin_Aggr_Med_1 +
  NewLoans_Aggr_Prop + NewLoans_Aggr_Prop_4 + InterestRate_Margin_Aggr_Med_3 + NewLoans_Aggr_Prop_5 + CuringEvents_Aggr_Prop                 
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

# - Looping and fitting a logit models containing only a single variable from which an ROC analysis is conducted in that model to get an AUC value as a measure of variable importance
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
col.v2 <- rep(c(brewer.pal(9, "Blues")[c(6)]),length(vars_port))
col.v3 <- rep("white", length(vars_port))
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
# --- 5.1 Loading the basic, intermediate-, and advanced variables and compiling the combined model formula
# - Load model/theme formulas
unpack.ffdf(paste0(genObjPath, "Basic_Com_Formula"), tempPath)
unpack.ffdf(paste0(genObjPath, "Int_Formula"), tempPath)
unpack.ffdf(paste0(genObjPath, "Del_Formula"), tempPath); unpack.ffdf(paste0(genObjPath, "Beh_Formula"), tempPath); unpack.ffdf(paste0(genObjPath, "Por_Formula"), tempPath)

# - Formula compilation
inputs_adv1 <- as.formula(paste0("DefaultStatus1_lead_12_max~", paste(labels(terms(inputs_bas)), collapse="+"), "+", paste(labels(terms(inputs_int)), collapse="+"), "+",
                                 paste(labels(terms(inputs_del_fin)), collapse="+"), "+", paste(labels(terms(inputs_beh_fin)), collapse="+"), "+", paste(labels(terms(inputs_por_fin)), collapse="+")))

# - [SANITY CHECK] Custom function to check amount of missingness in each input variable
Amt_Missing(inputs_adv1,datCredit_train)
### Results: No missingness, continue to fit model


# --- 5.2 Fitting the full model and screening for insignificant variables
# - Fit full model
logitMod_adv1 <- glm(inputs_adv1, data=datCredit_train, family="binomial")
# Assess model
summary(logitMod_adv1)
### RESULTS:    Insignificant variables: [slc_pmnt_method], [g0_Delinq_Any_Aggr_Prop], [g0_Delinq_Any_Aggr_Prop_Lag_5], [InterestRate_Margin_Aggr_Med_1], [NewLoans_Aggr_Prop], and [NewLoans_Aggr_Prop_5]
### CONCLUSION: Remove insignificant variables and refit model

# - Refited model excluding insignificant variables
inputs_adv2 <- as.formula(paste0("DefaultStatus1_lead_12_max~", paste(labels(terms(inputs_adv1))[-unlist(lapply(c("slc_pmnt_method", "g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_5", "InterestRate_Margin_Aggr_Med_1", "NewLoans_Aggr_Prop", "NewLoans_Aggr_Prop_5"), function(X) which(X==labels(terms(inputs_adv1)))))], collapse="+")))
logitMod_adv2 <- glm(inputs_adv2, data=datCredit_train, family="binomial")
# Assess model
summary(logitMod_adv2)
### RESULTS:    Insignificant variables: No insignificant variables, but [NewLoans_Aggr_Prop_4] is just over the 0.05 threshold.
### CONCLUSION: Proceed with model assessment

# Save model formula
inputs_adv_pre <- inputs_adv2


# --- 5.3. Subsampling and refitting the final model(s)
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
logitMod_smp <- glm(inputs_adv_pre, data=datCredit_train_smp, family="binomial")
# Deviance and AIC
summary(logitMod_smp)
### RESULTS:    Insignificant variables are: [M_Repo_Rate_12], [M_RealGDP_Growth_12], [AgeToTerm_Aggr_Mean], [NewLoans_Aggr_Prop_4], and [CuringEvents_Aggr_Prop]
### CONCLUSION: Refit model on the full training dataset whilst excluding insignificant variables and assess the exclusion's impact on model performance

# - Refit model with new input space on full training dataset and subsampled training dataset
# Adjust formula
inputs_adv <- as.formula(paste0("DefaultStatus1_lead_12_max ~ ",paste(labels(terms(inputs_adv_pre))[-c(which(labels(terms(inputs_adv_pre)) %in% c("M_Repo_Rate_12", "M_RealGDP_Growth_12", "AgeToTerm_Aggr_Mean", "NewLoans_Aggr_Prop_4", "CuringEvents_Aggr_Prop")))], collapse=" + ")))
logitMod_post <- glm(inputs_adv, data=datCredit_train, family="binomial")
logitMod_smp_post <- glm(inputs_adv, data=datCredit_train_smp, family="binomial")
# Model assessment
summary(logitMod_post)
### RESULTS: Insignificant variable(s): None
summary(logitMod_smp_post)
### RESULTS: Insignificant variable(s): [M_Inflation_Growth_9]
### CONCLUSION: Refit model excluding insignificant variable(s)

# - Refit model with new input space
# Adjust Formula
inputs_adv2 <- as.formula(paste0("DefaultStatus1_lead_12_max ~ ",paste(labels(terms(inputs_adv))[-c(which(labels(terms(inputs_adv)) %in% c("M_Inflation_Growth_9")))], collapse=" + ")))
logitMod_post2 <- glm(inputs_adv2, data=datCredit_train, family="binomial")
logitMod_smp_post2 <- glm(inputs_adv2, data=datCredit_train_smp, family="binomial")
# Model assessment
summary(logitMod_post2)
### RESULTS: Insignificant variable(s): None
summary(logitMod_smp_post2)
### RESULTS: Insignificant variable(s): None
### CONCLUSION: Proceed assess model

# - AUC comparison
datCredit_valid[, prob_adv_full_pre := predict(logitMod_adv2, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_adv_full := predict(logitMod_post2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_adv_full_pre)
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_adv_full)
### RESULTS: AUC = 90.04% for model on full validation set
###          AUC = 90.02% for model on subsampled validation set
### CONCLUSION: Variable reduction has no results on model predictability, save new formula as intermediate model formula and proceed to final model assessment

# - Save formula
inputs_adv <- inputs_adv2
pack.ffdf(paste0(genObjPath, "Adv_Formula"), inputs_adv); gc()


# --- 5.4 Clean up
rm(stratifiers, targetVar, smp_size, smp_perc_train, smp_perc_valid, datCredit_train_smp, datCredit_valid_smp, inputs_adv1, inputs_adv2,
   inputs_adv_pre, logitMod_adv1, logitMod_adv2, logitMod_post, logitMod_post2, logitMod_smp_post, logitMod_smp_post2); gc()




# ------ 6. Assess final model
# --- 6.1 Fit model
# - Preliminaries
# Load data
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)
# Subset to exclude default spells
datCredit_train <- datCredit_train %>% subset(DefaultStatus1==0)
datCredit_valid <- datCredit_valid %>% subset(DefaultStatus1==0)
# Load formula
unpack.ffdf(paste0(genObjPath, "Adv_Formula"), tempPath)
if (!exists('inputs_adv')) {
  inputs_adv <- DefaultStatus1_lead_12_max ~ Age_Adj + Term + PerfSpell_Num + InterestRate_Margin_imputed_mean + Principal_Real + Balance_Real + 
    g0_Delinq + M_RealIncome_Growth_2 + M_RealIncome_Growth_9 + M_RealIncome_Growth_12 + M_DTI_Growth_3 + PrevDefaults + 
    TimeInPerfSpell + g0_Delinq_Num + g0_Delinq_SD_4 + g0_Delinq_SD_6 + slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3 + 
    slc_past_due_amt_imputed_med + slc_acct_pre_lim_perc_imputed_med + NewLoans_Aggr_Prop_1 + InterestRate_Margin_Aggr_Med_3
  # save formula
  pack.ffdf(paste0(genObjPath, "Adv_Formula"), inputs_adv)
}

# - Fit model
logitMod_Adv <- glm(inputs_adv, data=datCredit_train, family="binomial")


# --- 6.2 Assessment
# - Deviance and AIC
summary(logitMod_Adv)
### RESULTS: Null deviance = 279124; Residual deviance = 187833; AIC = 187883

# - Evaluate fit using R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_Adv)
### RESULTS: 32.71%

# - Odds Ratio analysis 
round(exp(cbind(OR = coef(logitMod_Adv), confint.default(logitMod_Adv))), 3)
### [Principal_Real], [Balance_Real], and [slc_past_due_amt_imputed_med] have ratios close to 1, but this might not give an accurate indication because of the range of these variables

# - Residual deviance logitMod_por_best
resid_deviance_glm(logitMod_Adv)
### RESULTS: Model fit is somewhat strained (2 diagnostics gave warnings)

# - Variable importance
varImport_logit(logitMod_Adv, method="stdCoef_Goodman", sig_level=0.1, impPlot=T, plotVersionName="Advanced")
### RESULTS: Top three variables: [PrevDefaultsTRUE], [g0_Delinq], and [slc_acct_roll_ever_24_imputed_mean]

# - ROC analysis
datCredit_train[, prob_adv := predict(logitMod_Adv, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_adv := predict(logitMod_Adv, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_adv)
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_adv)
### RESULTS: Training = 89.82%
###          Validation = 90.02%

# - VIF analysis
car::vif(logitMod_Adv)
### RESULTS:  Variables with high VIFs (>10):
###             [Principal_Real], [Balance_Real], [M_RealIncome_Growth_9], [M_RealIncome_Growth_12]
####          These variables are expected to have high correlation and are safe for use in the final model

# - Clean up
rm(logitMod_Adv)
datCredit_train[,prob_adv:=NULL]; datCredit_valid[,prob_adv:=NULL]

### RESULTS:    All variables are significant and have reasonable standard errors; except for two portfolio-level variables
###             Model is not overfitted as evidenced by the small change in AUC when a ROC analysis is conducted on the training- and validation datasets (89.82% vs 90.02%)
###             The VIF values are as expected with most variables having low VIFs (<10) compared to 4 varaibles with high VIFs (>10)
