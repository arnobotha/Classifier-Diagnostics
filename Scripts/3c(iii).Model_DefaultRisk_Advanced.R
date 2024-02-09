# ========================= MODEL DEFAULT RISK - COMBINED ===============================
# Develop several "advanced" logistic regression models. These models include portfolio-
# level variables along with the combined input space in selecting the "best" variables
# in the entire existing input space for predicting default.
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha, Esmerelda Oberholzer

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
# Intermediate variables
unpack.ffdf(paste0(genObjPath, "Int_Theme_Formula"), tempPath); unpack.ffdf(paste0(genObjPath, "Int_Full_Formula"), tempPath)




# ------ 2. Modelling & Feature Selection by theme "Portfolio-level information"
# --- 2.1 Correlation analysis using Spearman correlation
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


# --- 2.2 Prelimanry experimenting: Using insights from correlation analysis
# - [AgeToTerm_Aggr_Mean] vs [PerfSpell_Maturity_Aggr_Mean]
# Model fitting
logitMod_por_exp1_1 <- glm(DefaultStatus1_lead_12_max ~ AgeToTerm_Aggr_Mean
                           , data=datCredit_train, family="binomial")
logitMod_por_exp1_2 <- glm(DefaultStatus1_lead_12_max ~ PerfSpell_Maturity_Aggr_Mean
                           , data=datCredit_train, family="binomial")
# Deviance and AIC
summary(logitMod_por_exp1_1) # Null deviance = 275184; Residual deviance = 272044; AIC = 272048
summary(logitMod_por_exp1_2) # Null deviance = 275184; Residual deviance = 274819; AIC = 274823
# ROC analysis
datCredit_valid[, prob_por_exp1_1 := predict(logitMod_por_exp1_1, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_por_exp1_2 := predict(logitMod_por_exp1_2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_por_exp1_1) # 58.10%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_por_exp1_2) # 51.95%
### CONCLUSION:   Use [AgeToTerm_Aggr_Mean], as it has results in a model with a lower AIC, but  a slightly higher AUC

# - Clean up
rm(logitMod_por_exp1_1, logitMod_por_exp1_2); gc()
datCredit_valid[, `:=` (prob_por_exp1_1=NULL, prob_por_exp1_2=NULL)]


# --- 2.3 Best subset selection
# - Full logit model with all account-level information - Exclude variables using insights from correlation analysis:  [g0_Delinq_Ave]; [PerfSpell_Maturity_Aggr_Mean]
logitMod_por1 <- glm(inputs_por1 <- DefaultStatus1_lead_12_max ~ g0_Delinq_Any_Aggr_Prop + g0_Delinq_Any_Aggr_Prop_Lag_5 +
                       InstalmentToBalance_Aggr_Prop + CuringEvents_Aggr_Prop +
                       NewLoans_Aggr_Prop + NewLoans_Aggr_Prop_1 + NewLoans_Aggr_Prop_3 + NewLoans_Aggr_Prop_4 + NewLoans_Aggr_Prop_5 +
                       AgeToTerm_Aggr_Mean +
                       InterestRate_Margin_Aggr_Med_1 + InterestRate_Margin_Aggr_Med_2 + InterestRate_Margin_Aggr_Med_3
                     , data=datCredit_train, family="binomial")

# - Assess full model
# Deviance and AIC
summary(logitMod_por1) # Null deviance = 266435; Residual deviance = 262851; AIC = 262879
# Odds Ratio analysis 
round(exp(cbind(OR = coef(logitMod_por1), confint.default(logitMod_por1))), 3)
# ROC analysis
datCredit_valid[, prob_por1 := predict(logitMod_por1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_por1) # 58.31%
### RESULTS   : The estimated coefficients and standard errors of all variables related to [InterestRate_Margin_imputed_Aggr] and [CuringEvents_Aggr_Prop] are very large.
###             Some variables are insignificant: [InstalmentToBalance_Aggr_Prop], [CuringEvents_Aggr_Prop], [NewLoans_Aggr_Prop], [NewLoans_Aggr_Prop_4], and [InterestRate_Margin_Aggr_Med_1]
### CONCLUSION: Perform best subset selection.


# - Best subset selection
logitMod_por_best <- MASS::stepAIC(logitMod_por1, direction="both")
# Start AIC = 262879.3
# End AIC = 262873.9
summary(logitMod_por_best) # [NewLoans_Aggr_Prop_4] has a p-value of 0.13 - this is dubious, but we'll keep it in the model for now (it might be significant when combining with the broader input spcae)
# ROC Analysis
datCredit_valid[, prob_por_best := predict(logitMod_por_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_por_best) # 58.3%
length(all.vars(getCall(logitMod_por1)$formula)); length(all.vars(getCall(logitMod_por_best)$formula))
### RESULTS:      The AIC of the model from the best subset selection is lower than the full model (262874 vs 262879).
###               The AUC of the model from the best subset selection is slightly lower than the full model (58.3% vs 58.31%).
###               The number of variables in the model from the best subset selection has been reduced to 10 from the full model which has 15 variables.

### CONCLUSION:   Use the specified variables of the best subset selection in the dynamic variable selection process.


# --- 2.4 Final portfolio-level information variables
# - FIRM analysis on the final model (which in this case is the variables from the best subset model)
# Variable importance
varImport_logit(logitMod_por_best, method="ac", standardise=T, plot=T, sig_level=0.15) # Top 3 variables: [InteresstRate_Margin_Aggr_Med_3], [InteresstRate_Margin_Aggr_Med_2], and [NewLoans_Aggr_Prop_1]
# - Final variables
### CONCLUSION: Use [g0_Delinq_Any_Aggr_Prop], [g0_Delinq_Any_Aggr_Prop_Lag_5], [NewLoans_Aggr_Prop_1], [NewLoans_Aggr_Prop_3], [NewLoans_Aggr_Prop_4], [NewLoans_Aggr_Prop_5], [AgeToTerm_Aggr_Mean],
###                 [InterestRate_Margin_Aggr_Med_2], [InterestRate_Margin_Aggr_Med_3]
# - Save variables
inputs_por_fin <- DefaultStatus1_lead_12_max ~ g0_Delinq_Any_Aggr_Prop + g0_Delinq_Any_Aggr_Prop_Lag_5 + NewLoans_Aggr_Prop_1 + NewLoans_Aggr_Prop_3 + NewLoans_Aggr_Prop_4 + NewLoans_Aggr_Prop_5 + AgeToTerm_Aggr_Mean +
  InterestRate_Margin_Aggr_Med_2 + InterestRate_Margin_Aggr_Med_3
pack.ffdf(paste0(genObjPath, "Por_Formula"), inputs_por_fin); gc()
###  CONCLUSION:    Use [inputs_por_fin] as set of portfolio-level information variables


# --- 2.5 Clean up
datCredit_valid[, `:=` (prob_por1=NULL, prob_por_best=NULL)]
rm(cor_por_spear, ind_row_spear, ind_col_spear, cor_por_spear2,
   logitMod_por_best, logitMod_por1); gc()




# ------ 3. Combing the basic variables with the basic-, intermediate-, and advanced variables
unpack.ffdf(paste0(genObjPath, "Por_Formula"), tempPath)

# --- 3.1 Basic-, thematic intermediate-, and advanced variables 
# - Formula compilation
inputs_adv1 <- as.formula(paste0("DefaultStatus1_lead_12_max~", paste(labels(terms(inputs_fin_bas)), collapse="+"), "+", paste(labels(terms(inputs_int_theme)), collapse="+"), "+", paste(labels(terms(inputs_por_fin)), collapse="+")))
# - Fitting the full model
logitMod_adv1 <- glm(inputs_adv1, data=datCredit_train, family="binomial")
# - Assess full model
summary(logitMod_adv1)
### RESULTS:    Insignificant variables: [AgeToTerm], [M_DTI_Growth_2], [M_Repo_Rate_SD_9], [NewLoans_Aggr_Prop_1], [NewLoans_Aggr_Prop_3], [NewLoans_Aggr_Prop_5], and [InterestRate_Margin_Aggr_Med_2]
### CONCLUSION: Remove insignificant variables and refit model

# - Formula compilation
inputs_adv2 <- as.formula(paste0("DefaultStatus1_lead_12_max~", paste(labels(terms(inputs_adv1))[-unlist(lapply(c("AgeToTerm", "M_DTI_Growth_2", "M_Repo_Rate_SD_9", "NewLoans_Aggr_Prop_1", "NewLoans_Aggr_Prop_3", "NewLoans_Aggr_Prop_5", "InterestRate_Margin_Aggr_Med_2"), function(X) which(X==labels(terms(inputs_adv1)))))], collapse="+")))
# - Fitting the full model
logitMod_adv2 <- glm(inputs_adv2, data=datCredit_train, family="binomial")
# - Assess full model
summary(logitMod_adv2)
### RESULTS:    Insignificant variables: [M_DTI_Growth_SD_4]
### CONCLUSION: Remove insignificant variable and refit model

# - Formula compilation
inputs_adv3 <- as.formula(paste0("DefaultStatus1_lead_12_max~", paste(labels(terms(inputs_adv2))[-unlist(lapply(c("M_DTI_Growth_SD_4"), function(X) which(X==labels(terms(inputs_adv2)))))], collapse="+")))
# - Fitting the full model
logitMod_adv3 <- glm(inputs_adv3, data=datCredit_train, family="binomial")
# - Assess full model
summary(logitMod_adv3)
### RESULTS:    None
### CONCLUSION: Continue with assessment

# - Deviance and AIC
# Deviance and AIC
summary(logitMod_adv3) # Null deviance = 254945; Residual deviance = 167556; AIC = 167668
# Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_adv3) # 34.28%
# Odds Ratio analysis 
round(exp(cbind(OR = coef(logitMod_adv3), confint.default(logitMod_adv3))), 3)
### [Balance], [Principal], and [slc_past_due_amt_imputed_med] have ratios close to 1, but this might not give an accurate indication because of the range of these variables
# Variable importance
varImport_logit(logitMod_adv3, method="absCoef", standardise=T, sig_level=0.1, plot=T) # Top three variables: [PrevDefaultsTRUE], [M_RealGDP_Growth_SD_6], and [M_RealGDP_Growth_SD_9]
varImport_logit(logitMod_adv3, method="stdCoef", standardise=T, sig_level=0.1, plot=T) # Top three variables: [PrevDefaultsTRUE], [g0_Delinq], and [TimeInPerfSpell]
# ROC analysis
datCredit_train[, prob_adv3 := predict(logitMod_adv3, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_adv3 := predict(logitMod_adv3, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_adv3) # 90.58%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_adv3) # 90.47%
# VIF analysis
car::vif(logitMod_adv3)
### RESULTS:  Most variables have low VIF values (<10), however most macroeconomic variables have large VIF values (>10)

# - Clean up
rm(logitMod_adv1, logitMod_adv2)
datCredit_train[,prob_adv3:=NULL]; datCredit_valid[,prob_adv3:=NULL]

# - Results & Conclusion
### RESULTS:    All variables are significant and have reasonable standard errors; except for the SD variables, and one one portfolio-level variable, which have large standard errors
###             Model is not overfitted as evidenced by the small change in AUC when a ROC analysis is conducted on the training- and validation datasets (90.58% vs 90.47%)
###             The VIF values are as expected, with most macroeconomic variables, and one p[ortfolio-level variable, having high VIF values (>10) compared to the other variables that have low VIF values (<10)

### CONCLUSION: Compare model to the one with the intermediate variables that were selected from the full input space

# --- 3.2 Basic-, full intermediate-, and advanced variables 
# - Formula compilation
inputs_adv4 <- as.formula(paste0("DefaultStatus1_lead_12_max~", paste(labels(terms(inputs_fin_bas)), collapse="+"), "+", paste(labels(terms(inputs_int_full)), collapse="+"), "+", paste(labels(terms(inputs_por_fin)), collapse="+")))
# - Fitting the full model
logitMod_adv4 <- glm(inputs_adv4, data=datCredit_train, family="binomial")
# - Assess full model
summary(logitMod_adv4)
### RESULTS:    Insignificant variables: [AgeToTerm], [NewLoans_Aggr_Prop_1], [NewLoans_Aggr_Prop_3], [NewLoans_Aggr_Prop_5], and [InterestRate_Margin_Aggr_Med_2]
### CONCLUSION: Remove insignificant variables and refit model

# - Formula compilation
inputs_adv5 <- as.formula(paste0("DefaultStatus1_lead_12_max~", paste(labels(terms(inputs_adv4))[-unlist(lapply(c("AgeToTerm", "NewLoans_Aggr_Prop_1", "NewLoans_Aggr_Prop_3", "NewLoans_Aggr_Prop_5", "InterestRate_Margin_Aggr_Med_2"), function(X) which(X==labels(terms(inputs_adv4)))))], collapse="+")))
# - Fitting the full model
logitMod_adv5 <- glm(inputs_adv5, data=datCredit_train, family="binomial")
# - Assess full model
summary(logitMod_adv5)
### RESULTS:    None
### CONCLUSION: Continue with assessment

# - Deviance and AIC
# Deviance and AIC
summary(logitMod_adv5) # Null deviance = 254945; Residual deviance = 167554; AIC = 167668
# Evaluate fit using generic R^2 based on deviance vs null deviance
coefDeter_glm(logitMod_adv5) # 34.28%
# Odds Ratio analysis 
round(exp(cbind(OR = coef(logitMod_adv5), confint.default(logitMod_adv5))), 3)
### [Balance], [Principal], and [slc_past_due_amt_imputed_med] have ratios close to 1, but this might not give an accurate indication because of the range of these variables
# Variable importance
varImport_logit(logitMod_adv5, method="absCoef", standardise=T, sig_level=0.1, plot=T) # Top three variables: [PrevDefaultsTRUE], [M_RealGDP_Growth_SD_6], and [M_RealGDP_Growth_SD_9]
varImport_logit(logitMod_adv5, method="stdCoef", standardise=T, sig_level=0.1, plot=T) # Top three variables: [PrevDefaultsTRUE], [g0_Delinq], and [TimeInPerfSpell]
# ROC analysis
datCredit_train[, prob_adv5 := predict(logitMod_adv5, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_adv5 := predict(logitMod_adv5, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_adv5) # 90.58%
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_adv5) # 90.46%
# VIF analysis
car::vif(logitMod_adv5)
### RESULTS:  Most variables have low VIF values (<10), however most macroeconomic variables have large VIF values (>10)

# - Clean up
rm(logitMod_adv4)
datCredit_train[,prob_adv5:=NULL]; datCredit_valid[,prob_adv5:=NULL]

# - Results & Conclusion
### RESULTS:    All variables are significant and have reasonable standard errors; except for the SD variables, and one one portfolio-level variable, which have large standard errors
###             Model is not overfitted as evidenced by the small change in AUC when a ROC analysis is conducted on the training- and validation datasets (90.58% vs 90.47%)
###             The VIF values are as expected, with most macroeconomic variables, and one p[ortfolio-level variable, having high VIF values (>10) compared to the other variables that have low VIF values (<10)

### CONCLUSION: Compare model to the one with the intermediate variables that were selected thematically


# - Comparison
length(labels(terms(logitMod_adv3))); length(labels(terms(logitMod_adv5)))
### COMPARISON: Both models' SD variables have large standard errors.
###             The "full" model has the same AIC as the "thematic" model (167668 vs 167668).
###             The "full" model has a slightly higher AUC on the training dataset compared to the "thematic" model (90.58% vs 90.58%).
###             The "full" model has a slightly lower AUC on the validation dataset compared to the "thematic" model (90.46% vs 90.47%).
###             The "full" model has 48 variables compares to the "thematic" model which has 49 variables.

### CONCLUSION: Both models are similar in all considered metrics, either one can be used

# - Save formulas
inputs_adv_theme <- formula(logitMod_adv3)
inputs_adv_full <- formula(logitMod_adv5)
pack.ffdf(paste0(genObjPath, "Adv_Theme_Formula"), inputs_adv_theme); gc()
pack.ffdf(paste0(genObjPath, "Adv_Full_Formula"), inputs_adv_full); gc()

# - Clean up
rm(logitMod_adv3, logitMod_adv5)





# ------ 7. Clean up
# --- Clean up
rm(logitMod_com1, logitMod_com2, logitMod_com3, logitMod_com4, form_com1, form_com2, form_com3, form_com4)
datCredit_train[, prob_com4:=NULL]; datCredit_valid[, prob_com4:=NULL]




