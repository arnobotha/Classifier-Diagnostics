# ========================== MODEL DEFAULT RISK - INPUT DIAGNOSTICS ========================================
# This script performs input diagnostics on the three logit models. Mainly it assessess the variable importance
# of the three models using z-scores and standardised coefficients.
# -----------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Marcel Muller

# DESCRIPTION:
# This script uses the previously selected variables in fitting different logit models according to their
# level of complexity. The variable importance of these variables are then assessed.
# -----------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 0a.CustomFunctions.R
#   - 3b.Data_Subsample_Fusion2
#   - 3c(i).Model_DefaultRisk_Basic
#   - 3c(ii).Model_DefaultRisk_Intermediate
#   - 3c(iii).Model_DefaultRisk_Advanced

# -- Inputs:
#   - datCredit_train | Prepared credit data from script 3b
#   - datCredit_valid | Prepared credit data from script 3b
#   - Final model input variables
# -- Outputs:
#   - Some graphs showcasing variable importance
# ===========================================================================================================


# ------ 1. Preliminaries
ptm <- proc.time() # for runtime calculations (ignore)

# - Graphing Parameters
chosenFont <- "Cambria"
dpi <- 180

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)

# - Subset to exclude default spells
datCredit_train <- datCredit_train %>% subset(DefaultStatus1==0)
datCredit_valid <- datCredit_valid %>% subset(DefaultStatus1==0)

# - Load in all required formulas
# Basic variables
unpack.ffdf(paste0(genObjPath, "Basic_Com_Formula"), tempPath)
# Intermediate variables
unpack.ffdf(paste0(genObjPath, "Int_Formula"), tempPath)
# Advanced variables
unpack.ffdf(paste0(genObjPath, "Adv_Formula"), tempPath)




# ------ 2. Fitting the base/full model
# --- 2.1 Specify all variables used in thematic variable selection
# - Basic model
bas_vars <- labels(terms(inputs_bas))

# - Intermediary model
int_vars <- labels(terms(inputs_int))

# - Advanced model
# Delinquency Variables
del_vars <- c("PrevDefaults", "TimeInPerfSpell", "g0_Delinq_Num", "g0_Delinq_SD_4", "g0_Delinq_SD_6",
              "PerfSpell_g0_Delinq_Num", "slc_acct_roll_ever_24_imputed_mean", "slc_past_due_amt_imputed_med")
# Behavioral variables
beh_vars <- c("slc_acct_prepaid_perc_dir_12_imputed_med",
              "slc_acct_pre_lim_perc_imputed_med", "slc_pmnt_method")
# Portfolio level variables
port_vars <-  c("g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_12", "g0_Delinq_Any_Aggr_Prop_Lag_3",
                "DefaultStatus1_Aggr_Prop", "DefaultStatus1_Aggr_Prop_Lag_9", "DefaultStatus1_Aggr_Prop_Lag_2",
                "InstalmentToBalance_Aggr_Prop", "ArrearsToBalance_Aggr_Prop", "CuringEvents_Aggr_Prop",
                "NewLoans_Aggr_Prop_5", "NewLoans_Aggr_Prop", "NewLoans_Aggr_Prop_4",
                "PerfSpell_Maturity_Aggr_Mean", "AgeToTerm_Aggr_Mean",
                "InterestRate_Margin_Aggr_Med", "InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_9")

# - Combine themes
inputs_adv_full <- as.formula(paste0("DefaultStatus1_lead_12_max","~", paste0(c(bas_vars, int_vars, del_vars, beh_vars, port_vars), collapse="+")))


# --- 2.2 Fit full model
# - Fit full model
logitMod_adv_full <- glm(inputs_adv_full, data=datCredit_train, family="binomial")

# - High-level evaluation of model
summary(logitMod_adv_full)




# ------ 3. "Thematic" and "best subset" model
# --- 3.1 "Thematic" model
# - Fit model from predefined input space
logitMod_adv <- glm(inputs_adv, data=datCredit_train, family="binomial")


# --- 3.2 "Best subset" model
# - Running best subset selection on the full/base model
logitMod_adv_best <- MASS::stepAIC(logitMod_full, direction="both")
# Start AIC = 187399.8
# End AIC = 187386.7

# - Save input space to drive
inputs_adv_best <- formula(logitMod_adv_best)
pack.ffdf(paste0(genObjPath, "Adv_Best_Formula"), inputs_adv_best); gc()




# ------ 4. Comparison
# --- 4.1 Fitting "thematic" and "best subset" models if not in memory
# - "Thematic model"
if (!exists('logitMod_adv')) {
  unpack.ffdf(paste0(genObjPath, "Adv_Formula"), tempPath)
  logitMod_adv <- glm(inputs_adv, data=datCredit_train, family="binomial")
}

# - "Best subset model"
if (!exists('logitMod_adv_best')) {
  unpack.ffdf(paste0(genObjPath, "Adv_Best_Formula"), tempPath)
  logitMod_adv_best <- glm(inputs_adv_best, data=datCredit_train, family="binomial")
} 


# --- 4.2 Comparison
# - Composition of input space
paste0("Advanced (full) model variable:   ", labels(terms(inputs_adv_full)))
paste0("========================================================================================================================")
paste0("Advanced (thematic) model variable:   ", labels(terms(inputs_adv)))
paste0("========================================================================================================================")
paste0("Advanced (best subset) model variable:   ", labels(terms(inputs_adv_best)))


# - Total number of variables
# Get total number of variables in each model through an ANOVA analysis
sum_logitMod_adv_full <- summary(logitMod_adv_full) # Full model
sum_logitMod_adv_them <- summary(logitMod_adv) # Thematic model
sum_logitMod_adv_best <- summary(logitMod_adv_best) # Best subset model

# Displaying the total number of variables in each model
paste0("Total variables in Advanced (full) model:   ", nrow(sum_logitMod_adv_full$coefficients))
paste0("Total variables in Advanced (thematic) model:   ", nrow(sum_logitMod_adv_them$coefficients))
paste0("Total variables in Advanced (best subset) model:   ", nrow(sum_logitMod_adv_best$coefficients))
### RESULTS: Number of variables in Advanced full model = 48 variables
###          Number of variables in Advanced thematic model = 25 variables
###          Number of variables in Advanced best subset model = 39 variables

# - Model fit
# Basic model fit statistics
paste0("AIC of Advanced (full) model: ", sum_logitMod_adv_full$aic); paste0("Pseudo R-Squared of Advanced (full) model: ", coef_Deter_adv_full <- coefDeter_glm(logitMod_adv_full)[[1]])
paste0("AIC of Advanced (thematic) model: ", sum_logitMod_adv_them$aic); paste0("Pseudo R-Squared of Advanced (thematic) model: ", coef_Deter_adv_them <- coefDeter_glm(logitMod_adv)[[1]])
paste0("AIC of Advanced (best subset) model: ", sum_logitMod_adv_best$aic); paste0("Pseudo R-Squared of Advanced (best subset) model: ", coef_Deter_adv_best <- coefDeter_glm(logitMod_adv_best)[[1]])
### RESULTS: Advanced full model: AIC = 187399.77; Pseudo R-squared = 32.90%
###         Advanced thematic model: AIC = 187882.97; Pseudo R-squared = 32.71%
###         Advanced best subset model: AIC = 187386.7; Pseudo R-squared = 32.89%
### NOTE:   Not all variables in best subset model are significant

# Change in AIC with full model as reference
paste0("Difference in AIC between thematic and full (reference) model: ",  round(sum_logitMod_adv_them$aic/sum_logitMod_adv_full$aic,4)*100, "%")
paste0("Difference in AIC between best subset and full (reference) model: ",  round(sum_logitMod_adv_best$aic/sum_logitMod_adv_full$aic,4)*100, "%")
### RESULTS: Thematic model has an AIC that is 0.26% higher than the full model.
###          Best subset model has an AIC that is 0.01% lower than the full model.
###

# Change in Pseudo R-squared with full model as reference
paste0("Difference in Pseudo R-squared between thematic and full (reference) model: ",
       round(as.numeric(substr(coef_Deter_adv_them,1,nchar(coef_Deter_adv_them[[1]])-1))/as.numeric(substr(coef_Deter_adv_full,1,nchar(coef_Deter_adv_full[[1]])-1)),4)*100, "%")
paste0("Difference in Pseudo R-squared between best subset and full (reference) model: ",
       round(as.numeric(substr(coef_Deter_adv_best,1,nchar(coef_Deter_adv_best[[1]])-1))/as.numeric(substr(coef_Deter_adv_full,1,nchar(coef_Deter_adv_full[[1]])-1)),4)*100, "%")### RESULTS: 
### RESULTS: Thematic model has an Pseudo R-squared that is 0.58% lower than the full model.
###          Best subset model has an Pseudo R-squared that is 0.03% lower than the full model.


# - Variable importance
# High-level variable importance using Goodman's method
(varImport_logit_adv_full <- varImport_logit(logitMod_adv_full, method="stdCoef_Goodman", sig_level=0.1, impPlot=T, plotVersionName="Advanced"))
(varImport_logit_adv_them <- varImport_logit(logitMod_adv, method="stdCoef_Goodman", sig_level=0.1, impPlot=T, plotVersionName="Advanced"))
(varImport_logit_adv_best <- varImport_logit(logitMod_adv_best, method="stdCoef_Goodman", sig_level=0.1, impPlot=T, plotVersionName="Advanced"))
### RESULTS: Advanced full model: Top three variables: [PrevDefaultsTRUE], [g0_Delinq], and [slc_acct_roll_ever_24_imputed_mean]
###          Advanced thematic model: Top three variables: [PrevDefaultsTRUE], [g0_Delinq], and [slc_acct_roll_ever_24_imputed_mean]
###          Advanced best subset model: Top three variables: [PrevDefaultsTRUE], [g0_Delinq], and [slc_acct_roll_ever_24_imputed_mean]

# Comparing variable importance of variables in thematic model to best subset model
paste0("The average rank of variables that are in best subset model and not in the thematic model: ",  round(mean(varImport_logit_adv_best$data[!(varImport_logit_adv_best$data$Variable %in% varImport_logit_adv_them$data$Variable), Rank], na.rm=T),2))
paste0("Therefore, there are ", round(aveRank_logit_adv_best_them/nrow(varImport_logit_adv_best$data),4)*100,"% more important variables (on average) that are in both the best subset and thematic models.")
### RESULTS: The average rank of variables in the best subset model, that are not in the thematic model, is 22.77.
###          Therefore, there are 73.45% more important variable that are in both the best subset and thematic models.


# - Predictive performance
datCredit_valid[, prob_adv_full:=predict(logitMod_adv_full, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_adv:=predict(logitMod_adv, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_adv_best:=predict(logitMod_adv_best, newdata = datCredit_valid, type="response")]
paste0("AUC of Advanced (full) model:   ", auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_adv_full))
paste0("AUC of Advanced (thematic) model:   ", auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_adv))
paste0("AUC of Advanced (best subset) model:   ", auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_adv_best))
### RESULTS: Advanced full model: AUC = 90.109448%
###          Advanced thematic model: AUC = 90.022675%
###          Advanced best subset model: AUC = 90.108667%


# - VIF analysis
paste0("Number of variables with VIF > 10:   ", sum(car::vif(logitMod_adv)[,1]>10))
paste0("Number of variables with VIF > 10:   ", sum(car::vif(logitMod_adv_best)[,1]>10))
### RESULTS: Variables with VIF > 10 for Advanced thematic model: 4
### RESULTS: Variables with VIF > 10 for Advanced best subset model: 15

# - Comparison
### Compared to the advanced best subset model, the thematic advanced model:
###     (1) has less variables than the advanced model (22 vs 33),
###     (2) has variables that are all significant (3 insignificant variables in advanced best subset model),
###     (3) has a similar model fit (32.71% vs 32.89% pseudo R-squared)
###     (4) has the same top 3 variables ranked as important
###     (5) has the same predictive performance (in the margin of error; 90.02% vs 90.12% AUC)
###     (6) has less variables with VIF < 10 (4 vs 15)


# --- 2.4 Clean up
rm(logitMod_adv, logitMod_adv_best)
datCredit_valid[,prob_adv:=NULL]; datCredit_valid[,prob_adv_best:=NULL]

