# ========================= MODEL DEFAULT RISK EXPERIMENT =================================
# Compare logit models with different lags of aggregated variables.
# -----------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Marcel Muller

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
#   - datCredit_real | Prepared credit data from script 2f
#
# -- Outputs:
#   - 
# =========================================================================================




# ------ 1. Preliminaries

ptm <- proc.time() # for runtime calculations (ignore)

# --- Confirm prepared datasets are loaded into memory
if (!exists('datCredit_smp')) unpack.ffdf(paste0(genPath,"creditdata_final4c"), tempPath)

# --- Feature engineering: Aggregated delinquency proportions
# - Creating an aggregated dataset with which to fuse to the full dataset
dat_g0_Delinq_Aggr <- data.table(datCredit_smp[DefaultStatus1==0, list(sum(g0_Delinq>=1, na.rm=T)/.N), by=list(Date)],
                                 datCredit_smp[DefaultStatus1==0, list(sum(g0_Delinq==0, na.rm=T)/.N), by=list(Date)][,2],
                                 datCredit_smp[DefaultStatus1==0, list(sum(g0_Delinq==1, na.rm=T)/.N), by=list(Date)][,2])
colnames(dat_g0_Delinq_Aggr) <- c("Date", "g0_Delinq_Any_Aggr_Prop", "g0_Delinq_0_Aggr_Prop", "g0_Delinq_1_Aggr_Prop")
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
    dat_g0_Delinq_Aggr[, (paste0(ColNames[j],"_",lags[i])) := shift(get(ColNames[j]), n=lags[i], type="lag")]
    dat_g0_Check1 <- rbind(dat_g0_Check1,
                           data.table(Variable=paste0(ColNames[j],"_",lags[i]),
                                      Check=sum(is.na(dat_g0_Delinq_Aggr[,get(paste0(ColNames[j],"_",lags[i]))]))==lags[i]))
  }
}
# - [SANITY CHECK] Check whether the lags were created correctly
cat( (dat_g0_Check1[,.N]==sum(dat_g0_Check1$Check)) %?% "SAFE: Lags applied successfully to the aggregated delinquency variable [g0_Delinq_Aggr_Sum].\n" %:%
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
### RESULTS:    Mean = -0.005407 and Median = -0.0055; Can use either mean or median imputation as the two statistics are very close
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
dat_IRM_Aggr <- merge(datCredit_smp[, list(InterestRate_Margin_mean_Aggr_mean = mean(InterestRate_Margin_imputed_mean, na.rm=T)/.N), by=list(Date)],
                      datCredit_smp[, list(InterestRate_Margin_mean_Aggr_med = median(InterestRate_Margin_imputed_med, na.rm=T)/.N), by=list(Date)], all.x=T, by="Date")
# - Applying various lags
lags <- c(1,2,3,4,5,6,9,12) # Lags
dat_IRM_Aggr_Check1 <- data.table(Variable = NULL, # Dataset for conducting sanity checks
                                  Check = NULL)
ColNames <- colnames(dat_IRM_Aggr)[-1] # Names of the columns
for (i in seq_along(lags)){ # Looping over the specified lags and applying each to each of the specified columns
  for (j in seq_along(ColNames)){
    dat_IRM_Aggr[, (paste0(ColNames[j],"_",lags[i])) := shift(get(ColNames[j]), n=lags[i], type="lag")]
    dat_IRM_Aggr_Check1 <- rbind(dat_IRM_Aggr_Check1,
                                 data.table(Variable=paste0(ColNames[j],"_",lags[i]),
                                            Check=sum(is.na(dat_IRM_Aggr[,get(paste0(ColNames[j],"_",lags[i]))]))==lags[i]))
  }
}
# - [SANITY CHECK] Check whether the lags were created correctly
cat( (dat_IRM_Aggr_Check1[,.N]==sum(dat_IRM_Aggr_Check1$Check)) %?% "SAFE: Lags applied successfully to the aggregated variable [InterestRate_Margin_imputed_Aggr].\n" %:%
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


# --- Feature engineering: Lagging aggregate new loans variable
# - Creating an aggregated dataset
dat_NewLoans_Aggr <- merge(datCredit_smp[, list(NewLoans_Aggr_Prop = sum(Age_Adj==1, na.rm=T)/.N), by=list(Date)],
                           datCredit_smp[, list(NewLoans_Aggr_Prop_Bal = sum(ifelse(Age_Adj==1,Balance,0), na.rm=T)/sum(Balance, na.rm=T)), by=list(Date)])
# - Applying various lags
lags <- c(1,2,3,4,5,6,9,12) # Lags
datNewLoans_Aggr_Check1 <- data.table(Variable = NULL, # Dataset for conducting sanity checks
                                      Check = NULL)
ColNames <- colnames(dat_NewLoans_Aggr)[-1] # Names of the columns
for (i in seq_along(lags)){ # Looping over the specified lags and applying each to each of the specified columns
  for (j in seq_along(ColNames)){
    dat_NewLoans_Aggr[, (paste0(ColNames[j],"_",lags[i])) := shift(get(ColNames[j]), n=lags[i], type="lag")]
    datNewLoans_Aggr_Check1 <- rbind(datNewLoans_Aggr_Check1,
                                     data.table(Variable=paste0(ColNames[j],"_",lags[i]),
                                                Check=sum(is.na(dat_NewLoans_Aggr[,get(paste0(ColNames[j],"_",lags[i]))]))==lags[i]))
  }
}
# - [SANITY CHECK] Check whether the lags were created correctly
cat( (datNewLoans_Aggr_Check1[,.N]==sum(datNewLoans_Aggr_Check1$Check)) %?% "SAFE: Lags applied successfully to the aggregated variable [NewLoans_Aggr].\n" %:%
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


# --- Exploring the base (unlagged) aggregated delinquency variables
# describe(datCredit_smp$g0_Delinq_Any_Aggr_Prop); plot(unique(datCredit_smp$g0_Delinq_Any_Aggr_Prop))
# describe(datCredit_smp$g0_Delinq_0_Aggr_Prop); plot(unique(datCredit_smp$g0_Delinq_0_Aggr_Prop))
# describe(datCredit_smp$g0_Delinq_1_Aggr_Prop); plot(unique(datCredit_smp$g0_Delinq_1_Aggr_Prop))

# --- Exploring the base (unlagged) interest rate margin variable
# describe(datCredit_smp$InterestRate_Margin_imputed_mean); plot(unique(datCredit_smp$InterestRate_Margin_imputed_mean))

# --- Exploring the base (unlagged) new loans variable
# describe(datCredit_smp$NewLoans_Aggr_Prop); plot(unique(datCredit_smp$NewLoans_Aggr_Prop))
# describe(datCredit_smp$NewLoans_Aggr_Prop_Bal); plot(unique(datCredit_smp$NewLoans_Aggr_Prop_Bal))

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

# --- Clean up
rm(datCredit_smp)




# ------ 2. Delinquency aggregation techniques
# ---- 1. "Any delinquency" aggregated variable
# --- All column names of training dataset
ColNames <- colnames(datCredit_train); ColNames <- ColNames[which(grepl("g0_Delinq_",ColNames))]

# --- Full model
inputs_g0_Any1 <- ColNames[which(grepl("g0_Delinq_Any_",ColNames))]
form_g0_Any1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_g0_Any1, collapse="+")))
# - Fit the model
logitMod_g0_Any1 <- glm(form_g0_Any1, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_g0_Any1) # Null deviance = 552507; Residual deviance = 548305; AIC = 548325
# - Variable importance
varImport(logitMod_g0_Any1) # Top 3 variables: [g0_Delinq_Any_Aggr_Prop_12], [g0_Delinq_Any_Aggr_Prop], and [g0_Delinq_Any_Aggr_Prop_5]
# - ROC analysis
datCredit_valid[, prob_g0_Any1 := predict(logitMod_g0_Any1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_Any1) # 56.25%

# --- Best subset selection
# - Conducting the best subset procedure
logitMod_g0_Any_best <- MASS::stepAIC(logitMod_g0_Any1, direction="both")
# Start AIC = 548324.9
# End AIC = 548315.2
# Getting the variables within the model
inputs_g0_Fac_best <- labels(terms(logitMod_g0_Any_best))
# - Deviance and AIC
summary(logitMod_g0_Any_best) # Null deviance = 552507; Residual deviance = 548307; AIC = 548315
# - Variable importance
varImport(logitMod_g0_Any_best) # Top 3 variables: [g0_Delinq_Any_Aggr_Prop_12], [g0_Delinq_Any_Aggr_Prop_5], and [g0_Delinq_Any_Aggr_Prop]
# - ROC analysis
datCredit_valid[, prob_g0_best := predict(logitMod_g0_Any_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_best) # 56.25%
### RESULTS:  The final set of variables are [g0_Delinq_Any_Aggr], [g0_Delinq_Any_Aggr_5], and [g0_Delinq_Any_Aggr_12]


# --- Assessing the impact of missing values when fitting the logit model with these various windows from the best subset selection
sum(is.na(datCredit_train$g0_Delinq_Any_Aggr_Prop_5))/datCredit_train[,.N]; unique(datCredit_train[is.na(g0_Delinq_Any_Aggr_Prop_5),Date])
sum(is.na(datCredit_train$g0_Delinq_Any_Aggr_Prop_12))/datCredit_train[,.N]; unique(datCredit_train[is.na(g0_Delinq_Any_Aggr_Prop_12),Date])
### RESULTS:    The 5 month variable omits 2.58% of the observations; the 12 month variable omits 6.54% of the observations
### CONCLUSION: Refit the logit model and omit the 12 month aggregated variable; although the missing values aren't necessarily too large, the period in which there is missingness is troublesome since it is the start of an economic downturn (2007)


# --- Reduced model
inputs_g0_Any2 <- c("g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_5")
form_g0_Any2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_g0_Any2, collapse="+")))
# - Fit the model
logitMod_g0_Any2 <- glm(form_g0_Any2, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_g0_Any2) # Null deviance = 552507; Residual deviance = 548305; AIC = 548325
# - Variable importance
varImport(logitMod_g0_Any2) # Top 3 variables: [g0_Delinq_Any_Aggr_Prop_5], and [g0_Delinq_Any_Aggr_Prop]
# - ROC analysis
datCredit_valid[, prob_g0_Any2 := predict(logitMod_g0_Any2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_Any2) # 55.8%

# --- Clean up
rm(logitMod_g0_Any1, logitMod_g0_Any2, logitMod_g0_Any_best, form_g0_Any1, form_g0_Any2, inputs_g0_Any1, inputs_g0_Any2, ColNames)
datCredit_valid[, `:=` (prob_g0_Any1=NULL, prob_g0_best=NULL, prob_g0_Any2=NULL)]


# ---- 2. Factorised aggregated delinquency variables
# --- All column names of training dataset
ColNames <- colnames(datCredit_train)

# --- Full model
inputs_g0_Fac1 <- ColNames[c(which(grepl("g0_Delinq_0_",ColNames)), which(grepl("g0_Delinq_1_",ColNames)))]
form_g0_Fac1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_g0_Fac1, collapse="+")))
# - Fit the model
logitMod_g0_Fac1 <- glm(form_g0_Fac1, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_g0_Fac1) # Null deviance = 552507; Residual deviance = 548158; AIC = 548196
# - Variable importance
varImport(logitMod_g0_Fac1) # Top 3 variables: [g0_Delinq_0_Aggr_Prop_12], [g0_Delinq_1_Aggr_Prop_6], and [g0_Delinq_0_Aggr_Prop_6]
# - ROC analysis
datCredit_valid[, prob_g0_Fac1 := predict(logitMod_g0_Fac1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_Fac1) # 55.40%

# --- Best subset selection
# - Conducting the best subset procedure
logitMod_g0_Fac_best <- MASS::stepAIC(logitMod_g0_Fac1, direction="both")
# Start AIC = 548195.9
# End AIC = 548192.8
# Getting the variables within the model
inputs_g0_Fac_best <- labels(terms(logitMod_g0_Fac_best))
# - Deviance and AIC
summary(logitMod_g0_Fac_best) # Null deviance = 552507; Residual deviance = 548159; AIC = 548193
# - Variable importance
varImport(logitMod_g0_Fac_best) # Top 3 variables: [g0_Delinq_0_Aggr_Prop_12], [g0_Delinq_1_Aggr_Prop_6], and [g0_Delinq_0_Aggr_Prop_6]
# - ROC analysis
datCredit_valid[, prob_g0_Fac_best := predict(logitMod_g0_Fac_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_Fac_best) # 56.52%
### RESULTS:    The final set of variables are [g0_Delinq_0_Aggr], [g0_Delinq_0_Aggr_2], [g0_Delinq_0_Aggr_3], [g0_Delinq_0_Aggr_4],
###             [g0_Delinq_0_Aggr_5], [g0_Delinq_0_Aggr_6], [g0_Delinq_0_Aggr_9], [g0_Delinq_0_Aggr_12], [g0_Delinq_1_Aggr], [g0_Delinq_1_Aggr_2],
###             [g0_Delinq_1_Aggr_3], [g0_Delinq_1_Aggr_4], [g0_Delinq_1_Aggr_5], [g0_Delinq_1_Aggr_6], [g0_Delinq_1_Aggr_9], and [g0_Delinq_1_Aggr_12]

### CONCLUSION: Rather use the "Any"version as it has 4 variables compared to this model with its 16 variables whilst producing a very similar AUC value.


# ---- 3. Conclusion for aggregated delinquency variables
length(inputs_g0_Any_best); length(inputs_g0_Fac_best)
### The model with the "any delinquency" variables has an almost identical, but slightly lower, AUC value than the model with the more granular version of the delinquency variable (55.16% vs 55.39%).
### The model with the "any delinquency" variables has an higher AIC value compared to the model with the more granular version of the delinquency variable (548315 vs 548193).
### The model with the "any delinquency" variable is more parsimonious as it has 3 variables compared to 16 variables of the model with the more granular version of the delinquency variable.
### CONCLUSION: Use the "any delinquency" variables.

# --- Clean up
rm(logitMod_g0_Any_best, logitMod_g0_Fac_best, logitMod_g0_Fac1, inputs_g0_Any_best, inputs_g0_Fac_best, inputs_g0_Fac1, form_g0_Fac1);
datCredit_valid[, `:=` (prob_g0_Fac1, prob_g0_Fac_best)]




# ------ 3. Interest rate margin variable
# ---- 1. Mean vs median imputation
# --- Mean imputation
# - Fit the model
logitMod_IRM_imputation_mean <- glm(DefaultStatus1_lead_12_max ~ InterestRate_Margin_imputed_mean, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_IRM_imputation_mean) # Null deviance = 588256 ; Residual deviance = 588056; AIC = 588060
# - ROC analysis
datCredit_valid[, prob_IRM_imputation_mean:= predict(logitMod_IRM_imputation_mean, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_IRM_imputation_mean) # 55.77%

# --- Median imputation
# - Fit the model
logitMod_IRM_imputation_med <- glm(DefaultStatus1_lead_12_max ~ InterestRate_Margin_imputed_med, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_IRM_imputation_med) # Null deviance = 588256 ; Residual deviance = 588056; AIC = 588060
# - ROC analysis
datCredit_valid[, prob_IRM_imputation_med:= predict(logitMod_IRM_imputation_med, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_IRM_imputation_med) # 55.77%

### RESULTS:    The AUC value of the two models is exactly the same.
### CONCLUSION: Use either technique of imputation.

# --- Clean up
rm(logitMod_IRM_imputation_mean, logitMod_IRM_imputation_med)
datCredit_valid[, `:=` (prob_IRM_imputation_mean=NULL, prob_IRM_imputation_med=NULL)]


# ---- 2. Mean vs median aggregation
# ---- Mean aggregated variable
# --- All column names of training dataset
ColNames <- colnames(datCredit_train); ColNames <- ColNames[which(grepl("InterestRate_Margin_",ColNames))]
ColNames <- ColNames[-which(grepl("_med",ColNames))][-1]

# --- Full model
inputs_IRM1 <- ColNames
form_IRM1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_IRM1, collapse="+")))
# - Fit the model
logitMod_IRM1 <- glm(form_IRM1, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_IRM1) # Null deviance = 552507; Residual deviance = 549809; AIC = 549829
# - Variable importance
varImport(logitMod_IRM1) # Top 3 variables: [InterestRate_Margin_mean_Aggr_mean_1], [InterestRate_Margin_mean_Aggr_mean_12], and [InterestRate_Margin_mean_Aggr_mean_2]
# - ROC analysis
datCredit_valid[, prob_IRM1:= predict(logitMod_IRM1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_IRM1) # 55.54%

# --- Best subset selection
# - Conducting the best subset procedure
logitMod_IRM_best <- MASS::stepAIC(logitMod_IRM1, direction="both")
# Start AIC = 549828.8
# End AIC = 549820.6
# - Deviance and AIC
summary(logitMod_IRM_best) # Null deviance = 552507; Residual deviance = 549813; AIC = 549821
# - Variable importance
varImport(logitMod_IRM_best) # Top 3 variables: [InterestRate_Margin_mean_Aggr_mean_1], [InterestRate_Margin_mean_Aggr_mean_12], and [InterestRate_Margin_mean_Aggr_mean_2]
# - ROC analysis
datCredit_valid[, prob_IRM_best := predict(logitMod_IRM_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_IRM_best) # 55.53%
### RESULTS:    The final set of variables are [InterestRate_Margin_imputed_Aggr_1], [InterestRate_Margin_imputed_Aggr_2], and [InterestRate_Margin_imputed_Aggr_12]
###             All coefficient estimates and the associated standard errors are very large.

# --- Clean up
rm(ColNames, logitMod_IRM1, logitMod_IRM_best, inputs_IRM1, form_IRM1)
datCredit_valid[, `:=` (prob_IRM1=NULL, prob_IRM_best=NULL)]


# ---- Median aggregated variable
# --- All column names of training dataset
ColNames <- colnames(datCredit_train); ColNames <- ColNames[which(grepl("InterestRate_Margin_",ColNames))]
ColNames <- ColNames[which(grepl("_med",ColNames))][-1]

# --- Full model
inputs_IRM2 <- ColNames
form_IRM2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_IRM2, collapse="+")))
# - Fit the model
logitMod_IRM2 <- glm(form_IRM2, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_IRM2) # Null deviance = 552507; Residual deviance = 548828; AIC = 548848
# - Variable importance
varImport(logitMod_IRM2) # Top 3 variables: [InterestRate_Margin_mean_Aggr_med_12], [InterestRate_Margin_mean_Aggr_med_1], and [InterestRate_Margin_mean_Aggr_med_2]
# - ROC analysis
datCredit_valid[, prob_IRM2:= predict(logitMod_IRM2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_IRM2) # 56.22%

# --- Best subset selection3
# - Conducting the best subset procedure
logitMod_IRM_best <- MASS::stepAIC(logitMod_IRM2, direction="both")
# Start AIC = 548847.9
# End AIC = 548843.5
# - Deviance and AIC
summary(logitMod_IRM_best) # Null deviance = 552507; Residual deviance = 548830; AIC = 548844
# - Variable importance
varImport(logitMod_IRM_best) # Top 3 variables: [InterestRate_Margin_mean_Aggr_med_12], [InterestRate_Margin_mean_Aggr_med_1], and [InterestRate_Margin_mean_Aggr_med_2]
# - ROC analysis
datCredit_valid[, prob_IRM_best := predict(logitMod_IRM_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_IRM_best) # 56.21%
### RESULTS:    The final set of variables are [InterestRate_Margin_mean_Aggr_med], [InterestRate_Margin_mean_Aggr_med_1], [InterestRate_Margin_mean_Aggr_med_2], [InterestRate_Margin_mean_Aggr_med_3], [InterestRate_Margin_mean_Aggr_med_9], [InterestRate_Margin_mean_Aggr_med_12]
###             All coefficient estimates and the associated standard errors are very large.

### Conclusion: Aggregation using the median results in a superior model (AUC of 56.21% vs 55.53%)

# --- Assessing the impact of missing values when fitting the logit model with these various windows from the best subset selection
sum(is.na(datCredit_train$InterestRate_Margin_mean_Aggr_med_1))/datCredit_train[,.N]; unique(datCredit_train[is.na(InterestRate_Margin_mean_Aggr_med_1),Date])
sum(is.na(datCredit_train$InterestRate_Margin_mean_Aggr_med_2))/datCredit_train[,.N]; unique(datCredit_train[is.na(InterestRate_Margin_mean_Aggr_med_2),Date])
sum(is.na(datCredit_train$InterestRate_Margin_mean_Aggr_med_3))/datCredit_train[,.N]; unique(datCredit_train[is.na(InterestRate_Margin_mean_Aggr_med_3),Date])
sum(is.na(datCredit_train$InterestRate_Margin_mean_Aggr_med_9))/datCredit_train[,.N]; unique(datCredit_train[is.na(InterestRate_Margin_mean_Aggr_med_9),Date])
sum(is.na(datCredit_train$InterestRate_Margin_mean_Aggr_med_12))/datCredit_train[,.N]; unique(datCredit_train[is.na(InterestRate_Margin_mean_Aggr_med_12),Date])
### RESULTS:    The 1 month variable omits 0.5% of the observations; the 2 month variable omits 0.1% of the observations; the 3 month variable omits 1.5% of the observations; the 9 month variable omits 4.7% of the observations; the 12 month variable omits 6.5% of the observations
### CONCLUSION: Refit the logit model and omit the 9- and 12 month aggregated variables; although the missing values aren't necessarily too large, the period in which there is missingness is troublesome since it is the start of an economic downturn (2007)

# --- Reduced model
inputs_IRM3 <- c("InterestRate_Margin_mean_Aggr_med", "InterestRate_Margin_mean_Aggr_med_1", "InterestRate_Margin_mean_Aggr_med_2", "InterestRate_Margin_mean_Aggr_med_3")
form_IRM3 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_IRM3, collapse="+")))
# - Fit the model
logitMod_IRM3 <- glm(form_IRM3, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_IRM3) # Null deviance = 581581; Residual deviance = 578732; AIC = 578742
# - Variable importance
varImport(logitMod_IRM3) # Top 3 variables: [InterestRate_Margin_mean_Aggr_med_3], [InterestRate_Margin_mean_Aggr_med_1], and [InterestRate_Margin_mean_Aggr_med]
# - ROC analysis
datCredit_valid[, prob_IRM3:= predict(logitMod_IRM3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_IRM3) # 55.6%

# --- Clean up
rm(ColNames, logitMod_IRM2,logitMod_IRM3, logitMod_IRM_best, inputs_IRM2, inputs_IRM3, form_IRM2, form_IRM3)
datCredit_valid[, `:=` (prob_IRM2=NULL, prob_IRM_best=NULL, prob_IRM3=NULL)]




# ------ 4. Aggregated new loans variable
# ---- 1. Number weighted aggregation variable
# --- All column names of training dataset
ColNames <- colnames(datCredit_train); ColNames <- ColNames[which(grepl("NewLoans_Aggr",ColNames))]
ColNames <- ColNames[-which(grepl("NewLoans_Aggr_Prop_Bal",ColNames))]

# --- Full model
inputs_NL1 <- ColNames
form_NL1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_NL1, collapse="+")))
# - Fit the model
logitMod_NL1 <- glm(form_NL1, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_NL1) # Null deviance = 552507; Residual deviance = 551951; AIC = 551971
# - Variable importance
varImport(logitMod_NL1) # Top 3 variables: [NewLoans_Aggr_Prop], [NewLoans_Aggr_Prop_12], and [NewLoans_Aggr_Prop_1]
# - ROC analysis
datCredit_valid[, prob_NL1:= predict(logitMod_NL1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_NL1) # 53.52%

# --- Best subset selection
# - Conducting the best subset procedure
logitMod_NL_best <- MASS::stepAIC(logitMod_NL1, direction="both")
# Start AIC = 551971.4
# End AIC = 551968.1
# - Deviance and AIC
summary(logitMod_NL_best) # Null deviance = 552507; Residual deviance = 551954; AIC = 551968
# - Variable importance
varImport(logitMod_NL_best) # Top 3 variables: [NewLoans_Aggr_Prop], [NewLoans_Aggr_Prop_12], and [NewLoans_Aggr_Prop_1]
# - ROC analysis
datCredit_valid[, prob_NL_best := predict(logitMod_NL_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_NL_best) # 53.53%
### RESULTS:    The final set of variables are [NewLoans_Aggr_Prop], [NewLoans_Aggr_Prop_1], [NewLoans_Aggr_Prop_3], [NewLoans_Aggr_PRop_4], [NewLoans_Aggr_Prop_5], and [NewLoans_Aggr_Prop_12]

# --- Clean up
rm(ColNames, logitMod_NL1, logitMod_NL_best, form_NL1, inputs_NL1)
datCredit_valid[, `:=` (prob_NL1=NULL, prob_NL_best=NULL)]


# ---- 2. Balance weighted aggregation variable
# --- All column names of training dataset
ColNames <- colnames(datCredit_train); ColNames <- ColNames[which(grepl("NewLoans_Aggr",ColNames))]
ColNames <- ColNames[which(grepl("NewLoans_Aggr_Prop_Bal",ColNames))]

# --- Full model
inputs_NL2 <- ColNames
form_NL2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_NL2, collapse="+")))
# - Fit the model
logitMod_NL2 <- glm(form_NL2, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_NL2) # Null deviance = 552507; Residual deviance = 551971; AIC = 551991
# - Variable importance
varImport(logitMod_NL2) # Top 3 variables: [NewLoans_Aggr_Prop_Bal], [NewLoans_Aggr_Prop_Bal_12], and [NewLoans_Aggr_Prop_Bal_1]
# - ROC analysis
datCredit_valid[, prob_NL2:= predict(logitMod_NL2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_NL2) # 53.37%

# --- Best subset selection
# - Conducting the best subset procedure
logitMod_NL_best <- MASS::stepAIC(logitMod_NL2, direction="both")
# Start AIC = 551990.8
# End AIC = 551988.8
# - Deviance and AIC
summary(logitMod_NL_best) # Null deviance = 552507; Residual deviance = 551971; AIC = 551989
# - Variable importance
varImport(logitMod_NL_best) # Top 3 variables: [NewLoans_Aggr_Prop_Bal], [NewLoans_Aggr_Prop_Bal_12], and [NewLoans_Aggr_Prop_Bal_1]
# - ROC analysis
datCredit_valid[, prob_NL_best := predict(logitMod_NL_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_NL_best) # 53.38%
### RESULTS:    The final set of variables are [NewLoans_Aggr_Prop_Bal], [NewLoans_Aggr_Prop_Bal_1], [NewLoans_Aggr_Prop_Bal_3], [NewLoans_Aggr_Prop_Bal_4], [NewLoans_Aggr_Prop_Bal_5], and [NewLoans_Aggr_Prop_Bal_12]

### Conclusion: The number weighted version of the aggregated new loans results in a logit model with a slightly higher AUC value, i.e., 52.77% vs 52.64%.
###             Use the number weighted version of the aggregated new loans variable.

# --- Assessing the impact of missing values when fitting the logit model with these various windows from the best subset selection
sum(is.na(datCredit_train$NewLoans_Aggr_Prop_12))/datCredit_train[,.N]; unique(datCredit_train[is.na(NewLoans_Aggr_Prop_12),Date])
sum(is.na(datCredit_train$NewLoans_Aggr_Prop_5))/datCredit_train[,.N]; unique(datCredit_train[is.na(NewLoans_Aggr_Prop_5),Date])
sum(is.na(datCredit_train$NewLoans_Aggr_Prop_4))/datCredit_train[,.N]; unique(datCredit_train[is.na(NewLoans_Aggr_Prop_4),Date])
sum(is.na(datCredit_train$NewLoans_Aggr_Prop_3))/datCredit_train[,.N]; unique(datCredit_train[is.na(NewLoans_Aggr_Prop_3),Date])
sum(is.na(datCredit_train$NewLoans_Aggr_Prop_1))/datCredit_train[,.N]; unique(datCredit_train[is.na(NewLoans_Aggr_Prop_1),Date])

### CONCLUSION: Refit the logit model (the number weighted version) and omit the 12 month aggregated variable; although the missing values aren't necessarily too large, the period in which there is missingness is troublesome since it is the start of an economic downturn (2007)

# --- Reduced model (based on the number weighted aggregated variable)
inputs_NL3 <- c("NewLoans_Aggr_Prop_5", "NewLoans_Aggr_Prop_4", "NewLoans_Aggr_Prop_3", "NewLoans_Aggr_Prop_1", "NewLoans_Aggr_Prop")
form_NL3 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_NL3, collapse="+")))
# - Fit the model
logitMod_NL3 <- glm(form_NL3, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_NL3) # Null deviance = 576238; Residual deviance = 576000; AIC = 576012
# - Variable importance
varImport(logitMod_NL3) # Top 3 variables: [NewLoans_Aggr_Prop], [NewLoans_Aggr_Prop_1], and [NewLoans_Aggr_Prop_3]
# - ROC analysis
datCredit_valid[, prob_NL3:= predict(logitMod_NL3, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_NL3) # 52.59%


# --- Clean up
rm(ColNames, logitMod_NL2, logitMod_NL3, logitMod_NL_best, form_NL2, form_NL3, inputs_NL2, inputs_NL3)
datCredit_valid[, `:=` (prob_NL2=NULL, prob_NL_best=NULL, prob_NL3=NULL)]




# ------ 5. Loan-level delinquency volatilities/standard deviations
# --- All column names of training dataset
ColNames <- colnames(datCredit_train); ColNames <- ColNames[which(grepl("g0_Delinq_SD_",ColNames) & !grepl("PerfSpell_",ColNames))]
### NOTE: Exclude [g0_Delinq_SD] as this variable computes the volatility/standard deviation over the laon's entire history

# --- Full model
inputs_g0_SD1 <- ColNames
form_g0_SD1 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_g0_SD1, collapse="+")))
# - Fit the model
logitMod_g0_SD1 <- glm(form_g0_SD1, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_g0_SD1) # Null deviance = 527926; Residual deviance = 360032; AIC = 360044
# - Variable importance
varImport(logitMod_g0_SD1) # Top 3 variables: [g0_Delinq_SD_12], [g0_Delinq_SD_6], and [g0_Delinq_SD_9]
# - ROC analysis
datCredit_valid[, prob_g0_SD1 := predict(logitMod_g0_SD1, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_SD1) # 79.66%

# --- Best subset selection
# - Conducting the best subset procedure
logitMod_g0_SD_best <- MASS::stepAIC(logitMod_g0_SD1, direction="both")
# Start AIC = 360044.2
# End AIC = 360042.3
# - Deviance and AIC
summary(logitMod_g0_SD_best) # Null deviance = 527926; Residual deviance = 360032; AIC = 360042
# - Variable importance
varImport(logitMod_g0_SD_best) # Top 3 variables: [g0_Delinq_SD_12], [g0_Delinq_SD_6], and [g0_Delinq_SD_9]
# - ROC analysis
datCredit_valid[, prob_g0_best := predict(logitMod_g0_SD_best, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_best) # 79.66%
### RESULTS:  The final set of variables are [g0_Delinq_SD_12], [g0_Delinq_SD_6], [g0_Delinq_SD_9], and [g0_Delinq_SD_4]

# --- Assessing the impact of missing values when fitting the logit model with these various windows from the best subset selection
sum(datCredit_train[,is.na(g0_Delinq_SD_4)])/datCredit_train[,.N]; sum(datCredit_train[,is.na(g0_Delinq_SD_6)])/datCredit_train[,.N]
sum(datCredit_train[,is.na(g0_Delinq_SD_9)])/datCredit_train[,.N]; sum(datCredit_train[,is.na(g0_Delinq_SD_12)])/datCredit_train[,.N]
### RESULTS:    The 4 month SD variable omits 3.5685% of the observations; the 6 month variable omits 5.966% of the observations;
###             the 9 month variable omits 9.55% of the observations; the 12 month variable omits 13.1518% of the observations.
### CONCLUSION: Refit the logit model and omit the 9- and 12 month SD variables

# --- Refitting the logit model by omitting the 9- and 12 month SD variables
inputs_g0_SD2 <- c("g0_Delinq_SD_6", "g0_Delinq_SD_4")
form_g0_SD2 <- as.formula(paste("DefaultStatus1_lead_12_max~", paste(inputs_g0_SD2, collapse="+")))
# - Fit the model
logitMod_g0_SD2 <- glm(form_g0_SD2, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_g0_SD2) # Null deviance = 563705; Residual deviance = 430500; AIC = 430506
# - Variable importance
varImport(logitMod_g0_SD2) # Top 2 variables: [g0_Delinq_SD_6], [g0_Delinq_SD_4]
# - ROC analysis
datCredit_valid[, prob_g0_SD2 := predict(logitMod_g0_SD2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_SD2) # 73.03%


# --- Clean up
rm(logitMod_g0_SD1, logitMod_g0_SD2, logitMod_g0_SD_best, form_g0_SD1, form_g0_SD2, inputs_g0_SD1, inputs_g0_SD2, ColNames)
datCredit_valid[, `:=` (prob_g0_SD1=NULL, prob_g0_best=NULL, prob_g0_SD2=NULL)]



