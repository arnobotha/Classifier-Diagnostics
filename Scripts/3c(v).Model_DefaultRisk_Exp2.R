# ========================= MODEL DEFAULT RISK EXPERIMENT =================================
# Investigate the utility of different lags of certain variables using comparative studies
# -----------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Marcel Muller, Roland Breedt, Dr Arno Botha

# DESCRIPTION:
# This script uses the previously prepared credit dataset to fit a few logit models.
# Various variables and their derivatives are used within the models to find the set
# of significant derivatives for the associated variable.
# The experiments conducted are:
#     - Proportion of accounts with any delinquency vs the more granular/factorised version (g0_Delinq==0 & g0_Delinq==1)
#     - Mean aggregation of two version of interest rate margin; missing value imputation is done using mean- and median imputation
#     - Proportion of new loans where the aggregation is done using number- and balance weighting
#     - Account-level delinquency volatility/standard deviation over various "rolling windows"
#           - Since this variable requires the entire loan histories, feature engineering is done in script 2f.Data_Fusion1.
# -----------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 0a.CustomFunctions.R
#
# -- Inputs:
#   - datCredit_smp | Subsampled credit data from script 3b
#   - datCredit_train | Prepared credit data from script 3b
#   - datCredit_valid | Prepared credit data from script 3b
#
# -- Outputs:
#   - Various results and recommendations from experiments.
# =========================================================================================




# ------ 1. Preliminaries
# Load and resample the subsampled dataset, followed by some preliminary feature engineering

ptm <- proc.time() # for runtime calculations (ignore)

# --- Confirm prepared datasets are loaded into memory
if (!exists('datCredit_smp')) unpack.ffdf(paste0(genPath,"creditdata_smp"), tempPath)



# --- Feature engineering: Aggregated delinquency proportions
# - Creating an aggregated dataset with which to fuse to the full dataset
dat_g0_Delinq_Aggr <- data.table(datCredit_smp[DefaultStatus1==0, list(sum(g0_Delinq==0, na.rm=T)/.N), by=list(Date)],
                                 datCredit_smp[DefaultStatus1==0, list(sum(g0_Delinq==1, na.rm=T)/.N), by=list(Date)][,2])
colnames(dat_g0_Delinq_Aggr) <- c("Date", "g0_Delinq_0_Aggr_Prop", "g0_Delinq_1_Aggr_Prop")
# [SANITY CHECK] Check for illogical variables
cat( (!any(is.na(dat_g0_Delinq_Aggr[,-1]))) %?% "SAFE: No missingness, aggregated variables created successfully.\n" %:%
       "WARNING: Excessive missingness detected in the aggregated variables.\n")



# --- Feature engineering: Lagging aggregate delinquency variables
# - Specifying the lags (monthly) that should be applied
lags <- c(1,2,3,4,5,6,9,12)
# - Creating a dataset with which to check if the lags are applied correctly to the macroeconomic variables
dat_g0_Check1 <- data.table(Variable = NULL,
                            Check = NULL)
# - Getting the column names with which to apply the lags
ColNames <- colnames(dat_g0_Delinq_Aggr)[-1]
# - Looping over the specified lags and applying each to each of the specified columns
for (i in seq_along(lags)){
  for (j in seq_along(ColNames)){
    dat_g0_Delinq_Aggr[, (paste0(ColNames[j],"_",lags[i])) := fcoalesce(shift(get(ColNames[j]), n=lags[i], type="lag"),get(ColNames[j]))]
    dat_g0_Check1 <- rbind(dat_g0_Check1,
                           data.table(Variable=paste0(ColNames[j],"_",lags[i]),
                                      Check=sum(is.na(dat_g0_Delinq_Aggr[,get(paste0(ColNames[j],"_",lags[i]))]))==lags[i]))
  }
}
# - [SANITY CHECK] Check whether the lags were created correctly
cat( (!anyNA(dat_g0_Delinq_Aggr)) %?% "SAFE: Lags applied successfully to the aggregated delinquency variable [g0_Delinq_Aggr_Sum].\n" %:%
       "WARNING: Excessive missingness detected in the lagged aggregated delinquency variable [g0_Delinq_Aggr_Sum].\n")

# - Merging the aggregated delinquency information to the subsampled dataset
datCredit_smp <- merge(datCredit_smp, dat_g0_Delinq_Aggr, by = "Date", all.x = T)
# - Validate merging success )by checking for missingness (should be zero)
list_merge_variables <- list(colnames(dat_g0_Delinq_Aggr))
results_missingness <- list()
for (i in 1:length(list_merge_variables)){
  output <- sum(is.na(datCredit_smp$list_merge_variables[i]))
  results_missingness[[i]] <- output
}
cat( (length(which(results_missingness > 0)) == 0) %?% "SAFE: No missingness, fusion with aggregated delinquency data is successful.\n" %:%
       "WARNING: Missingness in certain aggregated delinquency fields detected, fusion compromised.\n")
# - Clean up
rm(ColNames, lags, dat_g0_Check1, dat_g0_Delinq_Aggr, list_merge_variables, results_missingness); gc()



# --- Feature engineering: Lagging aggregate interest rate margin variable
# - Distributional analysis
hist(datCredit_smp[Counter==1 & InterestRate_Margin > -0.05, InterestRate_Margin]); describe(datCredit_smp[Counter==1, InterestRate_Margin])
### RESULTS:    Mean = -0.005386 and Median = -0.0054; Can use either mean or median imputation as the two statistics are very close
### CONCLUSION: Test both versions and associated lags
# - Mean imputation for missing values within [InterestRate_Margin]
datCredit_smp[, InterestRate_Margin_imputed_mean := 
                ifelse(is.na(InterestRate_Margin) | InterestRate_Margin == "", 
                       mean(InterestRate_Margin, na.rm=TRUE), InterestRate_Margin)]
cat( ( datCredit_smp[is.na(InterestRate_Margin_imputed_mean), .N] == 0) %?% 
       'SAFE: Treatment successful for [InterestRate_Margin_imputed].\n' %:% 
       'ERROR: Treatment failed for [InterestRate_Margin_imputed] \n' )
# - Median imputation for missing values within [InterestRate_Margin]
datCredit_smp[, InterestRate_Margin_imputed_med := 
                ifelse(is.na(InterestRate_Margin) | InterestRate_Margin == "", 
                       median(InterestRate_Margin, na.rm=TRUE), InterestRate_Margin)]
cat( ( datCredit_smp[is.na(InterestRate_Margin_imputed_med), .N] == 0) %?% 
       'SAFE: Treatment successful for [InterestRate_Margin_imputed_med].\n' %:% 
       'ERROR: Treatment failed for [InterestRate_Margin_imputed_med] \n' )

# - Creating an aggregated dataset
dat_IRM_Aggr <- merge(datCredit_smp[, list(InterestRate_Margin_mean_Aggr_mean = mean(InterestRate_Margin_imputed_mean, na.rm=T)), by=list(Date)],
                      datCredit_smp[, list(InterestRate_Margin_mean_Aggr_med = median(InterestRate_Margin_imputed_med, na.rm=T)), by=list(Date)], all.x=T, by="Date")
# - Applying various lags
lags <- c(1,2,3,4,5,6,9,12) # Lags
dat_IRM_Aggr_Check1 <- data.table(Variable = NULL, # Dataset for conducting sanity checks
                                  Check = NULL)
ColNames <- colnames(dat_IRM_Aggr)[-1] # Names of the columns
for (i in seq_along(lags)){ # Looping over the specified lags and applying each to each of the specified columns
  for (j in seq_along(ColNames)){
    dat_IRM_Aggr[, (paste0(ColNames[j],"_",lags[i])) := fcoalesce(shift(get(ColNames[j]), n=lags[i], type="lag"),get(ColNames[j]))]
    dat_IRM_Aggr_Check1 <- rbind(dat_IRM_Aggr_Check1,
                                 data.table(Variable=paste0(ColNames[j],"_",lags[i]),
                                            Check=sum(is.na(dat_IRM_Aggr[,get(paste0(ColNames[j],"_",lags[i]))]))==lags[i]))
  }
}
# - [SANITY CHECK] Check whether the lags were created correctly
cat( (!anyNA(dat_IRM_Aggr)) %?% "SAFE: Lags applied successfully to the aggregated variable [InterestRate_Margin_imputed_Aggr].\n" %:%
       "WARNING: Excessive missingness detected in the lagged aggregated variable [InterestRate_Margin_imputed_Aggr] variables.\n")
# - Merging the credit dataset with the aggregated dataset
datCredit_smp <- merge(datCredit_smp, dat_IRM_Aggr, by="Date", all.x=T)
# - Validate merging success )by checking for missingness (should be zero)
list_merge_variables <- list(colnames(dat_IRM_Aggr))
results_missingness <- list()
for (i in 1:length(list_merge_variables)){
  output <- sum(is.na(datCredit_smp$list_merge_variables[i]))
  results_missingness[[i]] <- output
}
cat( (length(which(results_missingness > 0)) == 0) %?% "SAFE: No missingness, fusion with aggregated interest rate margin data is successful.\n" %:%
       "WARNING: Missingness in certain aggregated interest rate margin fields detected, fusion compromised.\n")
# - Clean up
rm(dat_IRM_Aggr, dat_IRM_Aggr_Check1, list_merge_variables, results_missingness, output, lags, ColNames)



# --- Feature engineering: Lagging aggregate new advances (as a proportion of the existing book balance) variable
# - Creating an aggregated dataset
dat_NewLoans_Aggr <- datCredit_smp[, list(NewLoans_Aggr_Prop_Bal = sum(ifelse(Age_Adj==1,Balance,0), na.rm=T)/sum(Balance, na.rm=T)), by=list(Date)]
# - Applying various lags
lags <- c(1,2,3,4,5,6,9,12) # Lags
datNewLoans_Aggr_Check1 <- data.table(Variable = NULL, # Dataset for conducting sanity checks
                                      Check = NULL)
ColNames <- colnames(dat_NewLoans_Aggr)[-1] # Names of the columns
for (i in seq_along(lags)){ # Looping over the specified lags and applying each to each of the specified columns
  for (j in seq_along(ColNames)){
    dat_NewLoans_Aggr[, (paste0(ColNames[j],"_",lags[i])) := fcoalesce(shift(get(ColNames[j]), n=lags[i], type="lag"),get(ColNames[j]))]
    datNewLoans_Aggr_Check1 <- rbind(datNewLoans_Aggr_Check1,
                                     data.table(Variable=paste0(ColNames[j],"_",lags[i]),
                                                Check=sum(is.na(dat_NewLoans_Aggr[,get(paste0(ColNames[j],"_",lags[i]))]))==lags[i]))
  }
}
# - [SANITY CHECK] Check whether the lags were created correctly
cat( (!anyNA(dat_NewLoans_Aggr)) %?% "SAFE: Lags applied successfully to the aggregated variable [NewLoans_Aggr].\n" %:%
       "WARNING: Excessive missingness detected in the lagged aggregated variable [NewLoans_Aggr] variables.\n")
# - Merging the credit dataset with the aggregated dataset
datCredit_smp <- merge(datCredit_smp, dat_NewLoans_Aggr, by="Date", all.x=T)
# - Validate merging success )by checking for missingness (should be zero)
list_merge_variables <- list(colnames(dat_NewLoans_Aggr))
results_missingness <- list()
for (i in 1:length(list_merge_variables)){
  output <- sum(is.na(datCredit_smp$list_merge_variables[i]))
  results_missingness[[i]] <- output
}
cat( (length(which(results_missingness > 0)) == 0) %?% "SAFE: No missingness, fusion with aggregated new loans data is successful.\n" %:%
       "WARNING: Missingness in certain aggregated new loans fields detected, fusion compromised.\n")
# - Clean up
rm(dat_NewLoans_Aggr, datNewLoans_Aggr_Check1, list_merge_variables, results_missingness, output, lags, ColNames)



# --- Applying a resampling scheme
# - Parameters
stratifiers <- c("DefaultStatus1_lead_12_max", "Date") # Must at least include target variable used in graphing event rate
targetVar <- "DefaultStatus1_lead_12_max"
currStatusVar <- "DefaultStatus1"
timeVar <- "Date"
train_prop <- 0.7 # sampling fraction for resampling scheme

# - Dataset prepatation
datCredit_smp[, Ind := 1:.N] # prepare for resampling scheme

# - Implement resampling scheme using given main sampling fraction
set.seed(1)
datCredit_train <- datCredit_smp %>% group_by(across(all_of(stratifiers))) %>% slice_sample(prop=train_prop) %>% as.data.table()
datCredit_valid <- subset(datCredit_smp, !(Ind %in% datCredit_train$Ind)) %>% as.data.table(); datCredit_train[,Ind:=NULL]; datCredit_valid[,Ind:=NULL]

# - Filter only for non-defaulted cases since the lead default indicators are already fused
datCredit_train<-datCredit_train[DefaultStatus1==0,]
datCredit_valid<-datCredit_valid[DefaultStatus1==0,]

# --- Clean up
rm(datCredit_smp)




# ------ 2. Delinquency aggregation techniques

# ---- 2.1 "Any delinquency" aggregated variable
# - Select relevant columns
ColNames <- colnames(datCredit_train); ColNames <- ColNames[which(grepl("g0_Delinq_",ColNames))]

# --- Full model
inputs_g0_Any1 <- ColNames[which(grepl("g0_Delinq_Any_",ColNames))]
form_g0_Any1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_g0_Any1, collapse="+")))
# - Fit the model
logitMod_g0_Any1 <- glm(form_g0_Any1, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_g0_Any1)
## RESULTS: Null deviance = 279124; Residual deviance = 275894; AIC = 275914
# - Variable importance
varImport_logit(logitMod_g0_Any1, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top 3 variables: [g0_Delinq_Any_Aggr_Prop], [g0_Delinq_Any_Aggr_Prop_12], and [g0_Delinq_Any_Aggr_Prop_3]
# - ROC analysis
datCredit_valid[, prob_g0_Any1 := predict(logitMod_g0_Any1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_Any1)
### RESULTS: AUC = 57.97%


# --- Best subset selection
# - Conducting the best subset procedure
logitMod_g0_Any_best <- MASS::stepAIC(logitMod_g0_Any1, direction="both")
# Start AIC = 275914.5
# End AIC = 275911
# Getting the variables within the model
inputs_g0_Fac_best <- labels(terms(logitMod_g0_Any_best))
# - Deviance and AIC
summary(logitMod_g0_Any_best)
## RESULTS: Null deviance = 279124; Residual deviance = 275895; AIC = 275911
# - Variable importance
varImport_logit(logitMod_g0_Any_best, method="stdCoef_Goodman", sig_level=0.1, impPlot=T) 
## RESULTS: Top 3 variables: [g0_Delinq_Any_Aggr_Prop], [g0_Delinq_Any_Aggr_Prop_12], and [g0_Delinq_Any_Aggr_Prop_3]
# - ROC analysis
datCredit_valid[, prob_g0_best := predict(logitMod_g0_Any_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_best)
### RESULTS: AUC = 57.99%
### RESULTS: The final set of variables are [g0_Delinq_Any_Aggr_Prop], [g0_Delinq_Any_Aggr_Prop_12], [g0_Delinq_Any_Aggr_Prop_3], [g0_Delinq_Any_Aggr_Prop_4], [g0_Delinq_Any_Aggr_Prop_5], and [g0_Delinq_Any_Aggr_Prop_9]


# --- Reduced model
inputs_g0_Any2 <- c("g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_12", "g0_Delinq_Any_Aggr_Prop_Lag_3")
form_g0_Any2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_g0_Any2, collapse="+")))
# - Fit the model
logitMod_g0_Any2 <- glm(form_g0_Any2, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_g0_Any2)
### RESULTS: Null deviance = 279124; Residual deviance = 275902; AIC = 275910
# - Variable importance
varImport_logit(logitMod_g0_Any2, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
## RESULTS: Top 3 variables: [g0_Delinq_Any_Aggr_Prop], [g0_Delinq_Any_Aggr_Prop_Lag_12], and [g0_Delinq_Any_Aggr_Prop_Lag_3]
# - ROC analysis
datCredit_valid[, prob_g0_Any2 := predict(logitMod_g0_Any2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_Any2)
### RESULTS:    57.99%
### CONCLUSION: The model with the top 3 variables from the best subset selection performs as well as the reduced model
###             Use the top three variables in this theme.


# --- Clean up
rm(logitMod_g0_Any1, logitMod_g0_Any2, logitMod_g0_Any_best, form_g0_Any1, form_g0_Any2, inputs_g0_Any1, inputs_g0_Any2, ColNames)
datCredit_valid[, `:=` (prob_g0_Any1=NULL, prob_g0_best=NULL, prob_g0_Any2=NULL)]



# ---- 2.2 Factorised aggregated delinquency variables
# - Select relevant columns
ColNames <- colnames(datCredit_train)

# --- Full model
inputs_g0_Fac1 <- ColNames[c(which(grepl("g0_Delinq_0_",ColNames)), which(grepl("g0_Delinq_1_",ColNames)))]
form_g0_Fac1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_g0_Fac1, collapse="+")))
# - Fit the model
logitMod_g0_Fac1 <- glm(form_g0_Fac1, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_g0_Fac1)
### RESULTS: Null deviance = 279124; Residual deviance = 275873; AIC = 275911
# - Variable importance
varImport_logit(logitMod_g0_Fac1, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top 3 variables: [g0_Delinq_0_Aggr_Prop], [g0_Delinq_1_Aggr_Prop], and [g0_Delinq_1_Aggr_Prop_12]
# - ROC analysis
datCredit_valid[, prob_g0_Fac1 := predict(logitMod_g0_Fac1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_Fac1)
### RESULTS: 57.96%


# --- Best subset selection
# - Conducting the best subset procedure
logitMod_g0_Fac_best <- MASS::stepAIC(logitMod_g0_Fac1, direction="both")
# Start AIC = 275910.8
# End AIC = 275901
# Getting the variables within the model
inputs_g0_Fac_best <- labels(terms(logitMod_g0_Fac_best))
# - Deviance and AIC
summary(logitMod_g0_Fac_best)
### RESULTS: Null deviance = 279124; Residual deviance = 275875; AIC = 275901
# - Variable importance
varImport_logit(logitMod_g0_Fac_best, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top 3 variables: [g0_Delinq_0_Aggr_Prop], [g0_Delinq_1_Aggr_Prop_1], and [g0_Delinq_1_Aggr_Prop_12]
# - ROC analysis
datCredit_valid[, prob_g0_Fac_best := predict(logitMod_g0_Fac_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_Fac_best) # 57.95%
### RESULTS:  The final set of variables are:
###             [g0_Delinq_0_Aggr_Prop], [g0_Delinq_1_Aggr_Prop_1], [g0_Delinq_1_Aggr_Prop_12], [g0_Delinq_1_Aggr_Prop],
###             [g0_Delinq_1_Aggr_Prop_6], [g0_Delinq_0_Aggr_Prop_1], [g0_Delinq_1_Aggr_Prop_3], [g0_Delinq_0_Aggr_Prop_12],
###             [g0_Delinq_1_Aggr_Prop_9]
### CONCLUSION: Rather use the "Any" version as it has less variables compared to this model whilst producing a very similar AUC value.




# ------ 3. Interest rate margin variable: varying techniques of imputation, aggregation, and

# ---- 3.1 Mean vs median imputation

# --- Mean imputation
# - Fit the model
logitMod_IRM_imputation_mean <- glm(DefaultStatus1_lead_12_max ~ InterestRate_Margin_imputed_mean, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_IRM_imputation_mean)
### RESULTS: Null deviance = 279124 ; Residual deviance = 277321; AIC = 277325
# - ROC analysis
datCredit_valid[, prob_IRM_imputation_mean:= predict(logitMod_IRM_imputation_mean, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_IRM_imputation_mean)
## RESULTS: 57.22%


# --- Median imputation
# - Fit the model
logitMod_IRM_imputation_med <- glm(DefaultStatus1_lead_12_max ~ InterestRate_Margin_imputed_med, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_IRM_imputation_med)
### RESULTS: Null deviance = 279124 ; Residual deviance = 277321; AIC = 277325
# - ROC analysis
datCredit_valid[, prob_IRM_imputation_med:= predict(logitMod_IRM_imputation_med, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_IRM_imputation_med)
### RESULTS:    AUC = 57.22%
### RESULTS:    The AUC value of the two models is exactly the same.
### CONCLUSION: Use either technique of imputation.


# --- Clean up
rm(logitMod_IRM_imputation_mean, logitMod_IRM_imputation_med)
datCredit_valid[, `:=` (prob_IRM_imputation_mean=NULL, prob_IRM_imputation_med=NULL)]



# ---- 3.2 Mean vs median aggregation

# ---- Mean-aggregated variable
# - Select relevant columns
ColNames <- colnames(datCredit_train); ColNames <- ColNames[which(grepl("InterestRate_Margin_mean_Aggr_mean",ColNames))]


# --- Full model
inputs_IRM1 <- ColNames
form_IRM1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_IRM1, collapse="+")))
# - Fit the model
logitMod_IRM1 <- glm(form_IRM1, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_IRM1)
### RESULTS: Null deviance = 279124; Residual deviance = 276774; AIC = 276794
# - Variable importance
(varImport_logitMod_IRM1 <- varImport_logit(logitMod_IRM1, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)) 
### RESULTS: Top 3 variables: [InterestRate_Margin_mean_Aggr_mean], [InterestRate_Margin_mean_Aggr_mean_12], and [InterestRate_Margin_mean_Aggr_mean_9]
# - ROC analysis
datCredit_valid[, prob_IRM1:= predict(logitMod_IRM1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_IRM1)
### RESULTS: 56.95%


# --- Best subset selection
# - Conducting the best subset procedure
logitMod_IRM_best <- MASS::stepAIC(logitMod_IRM1, direction="both")
# Start AIC = 276793.9
# End AIC = 276790
# - Deviance and AIC
summary(logitMod_IRM_best)
### RESULTS: Null deviance = 279124; Residual deviance = 276776; AIC = 276790
# - Variable importance
(varImport_logitMod_IRM_best <- varImport_logit(logitMod_IRM_best, method="stdCoef_Goodman", sig_level=0.1, impPlot=T))
### RESULTS: Top 3 variables: [InterestRate_Margin_mean_Aggr_mean], [InterestRate_Margin_mean_Aggr_mean_12], and [InterestRate_Margin_mean_Aggr_mean_9]
# - ROC analysis
datCredit_valid[, prob_IRM_best := predict(logitMod_IRM_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_IRM_best)
### RESULTS: 56.94%
### RESULTS:    The final set of variables are [InterestRate_Margin_imputed_Aggr_mean_12], [InterestRate_Margin_imputed_Aggr_mean], 
#                 [InterestRate_Margin_imputed_Aggr_mean_9], [InterestRate_Margin_imputed_Aggr_mean_5], 
#                 [InterestRate_Margin_imputed_Aggr_mean_1], and [InterestRate_Margin_imputed_Aggr_mean_2]
###             All coefficient estimates and the associated standard errors are very large.


# --- Clean up
rm(ColNames, logitMod_IRM1, logitMod_IRM_best, inputs_IRM1, form_IRM1, varImport_logitMod_IRM1, varImport_logitMod_IRM_best)
datCredit_valid[, `:=` (prob_IRM1=NULL, prob_IRM_best=NULL)]



# ---- 3.3 Lags for chosen aggregated variable (chosen as the median)
# - Select relevant columns
ColNames <- colnames(datCredit_train); ColNames <- ColNames[which(grepl("InterestRate_Margin_mean_Aggr_med",ColNames))]


# --- Full model
inputs_IRM2 <- ColNames
form_IRM2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_IRM2, collapse="+")))
# - Fit the model
logitMod_IRM2 <- glm(form_IRM2, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_IRM2)
### RESULTS: Null deviance = 279124; Residual deviance = 276413; AIC = 276433
# - Variable importance
(varImport_logitMod_IRM2 <- varImport_logit(logitMod_IRM2, method="stdCoef_Goodman", sig_level=0.1, impPlot=T))
### RESULTS: Top 3 variables: [InterestRate_Margin_mean_Aggr_med], [InterestRate_Margin_mean_Aggr_med_1], and [InterestRate_Margin_mean_Aggr_med_9]
# - ROC analysis
datCredit_valid[, prob_IRM2:= predict(logitMod_IRM2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_IRM2)
### RESULTS: 57.34%


# --- Best subset selection3
# - Conducting the best subset procedure
logitMod_IRM_best <- MASS::stepAIC(logitMod_IRM2, direction="both")
# Start AIC = 276432.8
# End AIC = 276429
# - Deviance and AIC
summary(logitMod_IRM_best)
## RESULTS: Null deviance = 279124; Residual deviance = 276413; AIC = 276429
# - Variable importance
(varImport_logitMod_IRM_best <- varImport_logit(logitMod_IRM_best, method="stdCoef_Goodman", sig_level=0.1, impPlot=T))
### RESULTS: Top 3 variables: [InterestRate_Margin_mean_Aggr_med], [InterestRate_Margin_mean_Aggr_med_1], and [InterestRate_Margin_mean_Aggr_med_9]
# - ROC analysis
datCredit_valid[, prob_IRM_best := predict(logitMod_IRM_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_IRM_best)
### RESULTS:    57.34%
### RESULTS:    The final set of variables are [InterestRate_Margin_mean_Aggr_med], [InterestRate_Margin_mean_Aggr_med_1], 
###           [InterestRate_Margin_mean_Aggr_med_9], [InterestRate_Margin_mean_Aggr_med_12], [InterestRate_Margin_mean_Aggr_med_6],
###           [InterestRate_Margin_mean_Aggr_med_2], and [InterestRate_Margin_mean_Aggr_med_5]
###             All coefficient estimates and the associated standard errors are very large.
### Conclusion: Median aggregation results in a superior model (AUC of 56.94% vs 57.34%).


# --- Reduced model
inputs_IRM3 <- c("InterestRate_Margin_mean_Aggr_med", "InterestRate_Margin_mean_Aggr_med_1", "InterestRate_Margin_mean_Aggr_med_9")
form_IRM3 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_IRM3, collapse="+")))
# - Fit the model
logitMod_IRM3 <- glm(form_IRM3, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_IRM3)
### RESULTS: Null deviance = 279124; Residual deviance = 276520; AIC = 276528
# - Variable importance
(varImport_logitMod_IRM3 <- varImport_logit(logitMod_IRM3, method="stdCoef_Goodman", sig_level=0.1, impPlot=T))
### RESULTS: Top 3 variables: [InterestRate_Margin_mean_Aggr_med_9], [InterestRate_Margin_mean_Aggr_med], [InterestRate_Margin_mean_Aggr_med_1]
# - ROC analysis
datCredit_valid[, prob_IRM3:= predict(logitMod_IRM3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_IRM3)
### RESULTS:    57.22%
### COMPARISON: AUC drops slightly from the best subset model (57.34% vs 57.22%), but is still larger than the mean aggregation model (56.94%).
### CONCLUSION: Use the top-three variables: [InterestRate_Margin_mean_Aggr_med_9], [InterestRate_Margin_mean_Aggr_med], and [InterestRate_Margin_mean_Aggr_med_1]


# --- Clean up
rm(ColNames, logitMod_IRM2,logitMod_IRM3, logitMod_IRM_best, inputs_IRM2, inputs_IRM3, form_IRM2, form_IRM3,
   varImport_logitMod_IRM2, varImport_logitMod_IRM_best, varImport_logitMod_IRM3)
datCredit_valid[, `:=` (prob_IRM2=NULL, prob_IRM_best=NULL, prob_IRM3=NULL)]




# ------ 4. Aggregated new loans variable over different lags

# ---- 1. Number weighted aggregation variable
# - Select relevant columns
ColNames <- colnames(datCredit_train); ColNames <- ColNames[which(grepl("NewLoans_Aggr_Prop",ColNames))]
ColNames <- ColNames[-which(grepl("NewLoans_Aggr_Prop_Bal",ColNames))]


# --- Full model
inputs_NL1 <- ColNames
form_NL1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_NL1, collapse="+")))
# - Fit the model
logitMod_NL1 <- glm(form_NL1, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_NL1)
### RESULTS: Null deviance = 279124; Residual deviance = 278375; AIC = 278387
# - Variable importance
varImport_logit(logitMod_NL1, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top 3 variables: [NewLoans_Aggr_Prop_5], [NewLoans_Aggr_Prop], and [NewLoans_Aggr_Prop_4]
# - ROC analysis
datCredit_valid[, prob_NL1:= predict(logitMod_NL1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_NL1)
### RESULTS: 54.08%


# --- Best subset selection
# - Conducting the best subset procedure
logitMod_NL_best <- MASS::stepAIC(logitMod_NL1, direction="both")
# Start AIC = 278387
# End AIC = 278385
# - Deviance and AIC
### RESULTS: The final set of variables are [NewLoans_Aggr_Prop_1], [NewLoans_Aggr_Prop_3], [NewLoans_Aggr_Prop], 
#             [NewLoans_Aggr_Prop_4], [NewLoans_Aggr_Prop_5]

# --- Clean up
rm(ColNames, logitMod_NL1, logitMod_NL_best, form_NL1, inputs_NL1)
datCredit_valid[, `:=` (prob_NL1=NULL)]



# ---- 2. Balance-weighted aggregation variables
# - Select relevant columns
ColNames <- colnames(datCredit_train); ColNames <- ColNames[which(grepl("NewLoans_Aggr",ColNames))]
ColNames <- ColNames[which(grepl("NewLoans_Aggr_Prop_Bal",ColNames))]

# --- Full model
inputs_NL2 <- ColNames
form_NL2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_NL2, collapse="+")))
# - Fit the model
logitMod_NL2 <- glm(form_NL2, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_NL2)
### RESULTS: Null deviance = 279124; Residual deviance = 277687; AIC = 277707
# - Variable importance
varImport_logit(logitMod_NL2, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top 3 variables: [NewLoans_Aggr_Prop_Bal_12], [NewLoans_Aggr_Prop_Bal], and [NewLoans_Aggr_Prop_Bal_9]
# - ROC analysis
datCredit_valid[, prob_NL2:= predict(logitMod_NL2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_NL2)
### RESULTS: 55.5%


# --- Best subset selection
# - Conducting the best subset procedure
logitMod_NL_best <- MASS::stepAIC(logitMod_NL2, direction="both")
# Start AIC = 277707
# End AIC = 277705
# - Deviance and AIC
summary(logitMod_NL_best)
### RESULTS: Null deviance = 279124; Residual deviance = 277689; AIC = 277705
# - Variable importance
varImport_logit(logitMod_NL_best, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top 3 variables: [NewLoans_Aggr_Prop_Bal_12], [NewLoans_Aggr_Prop_Bal], and [NewLoans_Aggr_Prop_Bal_9]
# - ROC analysis
datCredit_valid[, prob_NL_best := predict(logitMod_NL_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_NL_best)
### RESULTS:    55.5%
### CONCLUSION: The final set of variables are [NewLoans_Aggr_Prop_Bal_12], [NewLoans_Aggr_Prop_Bal], [NewLoans_Aggr_Prop_Bal_9],
#               [NewLoans_Aggr_Prop_Bal_6], [NewLoans_Aggr_Prop_Bal_2], [NewLoans_Aggr_Prop_Bal_1], and [NewLoans_Aggr_Prop_Bal_4]


# --- Reduced model using most important 3 variables (based on the number weighted aggregated variable)
inputs_NL3 <- c("NewLoans_Aggr_Prop", "NewLoans_Aggr_Prop_4", "NewLoans_Aggr_Prop_5")
form_NL3 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_NL3, collapse="+")))
# - Fit the model
logitMod_NL3 <- glm(form_NL3, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_NL3)
###  RESULTS: Null deviance = 279124; Residual deviance = 278403; AIC = 278411
# - Variable importance
varImport_logit(logitMod_NL3, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
### RESULTS: Top 3 variables: [NewLoans_Aggr_Prop_5], [NewLoans_Aggr_Prop_4], and [NewLoans_Aggr_Prop]
# - ROC analysis
datCredit_valid[, prob_NL3:= predict(logitMod_NL3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_NL3)
### RESULTS:    54.03%
### CONCLUSION: The model with all the selected variables from the best subset selection performs marginally better than the model with
###             only the top three variables form that selection (54.08% vs 54.03%)
###             Use the top three variables from the number weighted aggregated variables: [NewLoans_Aggr_Prop_5], 
###             [NewLoans_Aggr_Prop], and [NewLoans_Aggr_Prop_4]

# --- Clean up
rm(ColNames, logitMod_NL2, logitMod_NL3, logitMod_NL_best, form_NL2, form_NL3, inputs_NL2, inputs_NL3)
datCredit_valid[, `:=` (prob_NL2=NULL, prob_NL_best=NULL, prob_NL3=NULL)]




# ------ 5. Loan-level delinquency volatilities/standard deviations over different window lengths
# - Select relevant columns
ColNames <- colnames(datCredit_train); ColNames <- ColNames[which(grepl("g0_Delinq_SD_",ColNames) & !grepl("PerfSpell_",ColNames))]

# --- Full model
inputs_g0_SD1 <- ColNames
form_g0_SD1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_g0_SD1, collapse="+")))
# - Fit the model
logitMod_g0_SD1 <- glm(form_g0_SD1, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_g0_SD1) # Null deviance = 279124; Residual deviance = 225149; AIC = 225161
# - Variable importance
varImport_logit(logitMod_g0_SD1, method="stdCoef_Goodman", sig_level=0.1, impPlot=T) 
### RESULTS: Top 3 variables: [g0_Delinq_SD_12], [g0_Delinq_SD_4], and [g0_Delinq_SD_6]
# - ROC analysis
datCredit_valid[, prob_g0_SD1 := predict(logitMod_g0_SD1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_SD1) # 79.87%


# --- Best subset selection
# - Conducting the best subset procedure
logitMod_g0_SD_best <- MASS::stepAIC(logitMod_g0_SD1, direction="both")
# Start AIC = 225161
# End AIC = 225161
# - Deviance and AIC
summary(logitMod_g0_SD_best) # Null deviance = 279124; Residual deviance = 225149; AIC = 225161
# - Variable importance
varImport_logit(logitMod_g0_SD_best, method="stdCoef_Goodman", sig_level=0.1, impPlot=T) 
### RESULTS: Top 3 variables: [g0_Delinq_SD_12], [g0_Delinq_SD_4], and [g0_Delinq_SD_6]
# - ROC analysis
datCredit_valid[, prob_g0_best := predict(logitMod_g0_SD_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_best) # 79.87%
### RESULTS:  The final set of variables are [g0_Delinq_SD_12], [g0_Delinq_SD_4], 
#    [g0_Delinq_SD_6], [g0_Delinq_SD_9], and [g0_Delinq_SD_5]


# --- Refitting the logit model by keeping only the top 3 variables
inputs_g0_SD2 <- c("g0_Delinq_SD_12", "g0_Delinq_SD_4", "g0_Delinq_SD_6")
form_g0_SD2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_g0_SD2, collapse="+")))
# - Fit the model
logitMod_g0_SD2 <- glm(form_g0_SD2, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_g0_SD2) # Null deviance = 279124; Residual deviance = 225247; AIC = 225255
# - Variable importance
varImport_logit(logitMod_g0_SD2, method="stdCoef_Goodman", sig_level=0.1, impPlot=T) 
### RESULTS: Top 3 variables: [g0_Delinq_SD_12], [g0_Delinq_SD_4], and [g0_Delinq_SD_6]
# - ROC analysis
datCredit_valid[, prob_g0_SD2 := predict(logitMod_g0_SD2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_SD2) # 79.83%
### CONCLUSION: Use [g0_Delinq_SD_12], [g0_Delinq_SD_4], and [g0_Delinq_SD_6] given model parsimony and 
#               marginal drop in predictive performance


# --- Clean up
rm(logitMod_g0_SD1, logitMod_g0_SD2, logitMod_g0_SD_best, form_g0_SD1, form_g0_SD2, inputs_g0_SD1, 
   inputs_g0_SD2, ColNames)
datCredit_valid[, `:=` (prob_g0_SD1=NULL, prob_g0_best=NULL, prob_g0_SD2=NULL)]




# ------ 6. Portfolio-level default rates over different lags

# --- Preliminaries
# - Load previously resampled datasets since they already contain the relevant and previously-engineered 
# variables for testing purposes
rm(datCredit_train,datCredit_valid)
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)
# - Filter only for non-defaulted cases since the lead default indicators are already fused
datCredit_train <- datCredit_train %>% subset(DefaultStatus1==0)
datCredit_valid <- datCredit_valid %>% subset(DefaultStatus1==0)
# - Select relevant columns
ColNames <- colnames(datCredit_train); ColNames <- ColNames[which(grepl("DefaultStatus1_Aggr_Prop",ColNames))]


# --- Full model
form_Def_Rate <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(ColNames, collapse="+")))
logitMod_FULL <- glm(form_Def_Rate, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_FULL) # Null deviance = 279124; Residual deviance = 277061; AIC = 277081
# - Variable importance
varImport_logit(logitMod_FULL, method="stdCoef_Goodman", sig_level=0.1, impPlot=T) 
### RESULTS: Top 3 variables: [DefaultStatus1_Aggr_Prop], [DefaultStatus1_Aggr_Prop_Lag_9], and [DefaultStatus1_Aggr_Prop_Lag_1]
# - ROC analysis
datCredit_valid[, prob_DefRte := predict(logitMod_FULL, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_DefRte) # 56.42%


# --- Best subset selection
# - Conducting the best subset procedure
logitMod_FULL_Best <- MASS::stepAIC(logitMod_FULL, direction="both")
# Start AIC = 277080.7
# End AIC = 277074
# - Deviance and AIC
summary(logitMod_FULL_Best) # Null deviance = 279124; Residual deviance = 277062; AIC = 277074
# - Variable importance
varImport_logit(logitMod_FULL_Best, method="stdCoef_Goodman", sig_level=0.1, impPlot=T)
## RESULTS: Top 3 variables: [DefaultStatus1_Aggr_Prop], [DefaultStatus1_Aggr_Prop_Lag_1], and [DefaultStatus1_Aggr_Prop_Lag_9]
# - ROC analysis
datCredit_valid[, prob_DR_best := predict(logitMod_FULL_Best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_DR_best) # 56.44%


# --- Refitting the logit model by keeping only the top 3 variables
inputs_reduced <- c("DefaultStatus1_Aggr_Prop", "DefaultStatus1_Aggr_Prop_Lag_1", "DefaultStatus1_Aggr_Prop_Lag_9")
form_DR_red <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_reduced, collapse="+")))
# - Fit the model
logitMod_red <- glm(form_DR_red, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_red) # Null deviance = 279124; Residual deviance = 277097; AIC = 277105
# - Variable importance
varImport_logit(logitMod_red, method="stdCoef_Goodman", sig_level=0.1, impPlot=T) 
### RESULTS: Top 3 variables: [DefaultStatus1_Aggr_Prop], [DefaultStatus1_Aggr_Prop_Lag_9], and [DefaultStatus1_Aggr_Prop_Lag_1]
# - ROC analysis
datCredit_valid[, prob_red := predict(logitMod_red, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_red) # 56.4%
### CONCLUSION: Reduced model is preferred, given predictive performance and model parsimony
