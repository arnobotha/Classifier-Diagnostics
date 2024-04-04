# =================================== MODEL SEGMENTATION COMPARISON ==========================================
# Exploring the influence of inflation on variables sensitive to the time value of money, i.e., [Principal]
# and [Balance].
# ------------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Marcel Muller

# DESCRIPTION:
# This script explores the influence of inflation on [Principal] and [Balance], which are both sensitive to
# the time value of money, in terms of their statistical significance in predicting default in a logit model.
# ------------------------------------------------------------------------------------------------------------
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
#   - Some graphs illustrating the differences in the raw variables vs their defalted counterparts
# ============================================================================================================





# ------ 1. Preliminaries
ptm <- proc.time() # for runtime calculations (ignore)

# --- Confirm prepared datasets are loaded into memory
# - Macroeconomic dataset (used for getting inflation/deflation factors)
if (!exists('datMV')) unpack.ffdf(paste0(genPath,"datMV"), tempPath)
# - Resampled credit datasets
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)

# --- Subset to exclude default spells
datCredit_train <- datCredit_train %>% subset(DefaultStatus1==0)
datCredit_valid <- datCredit_valid %>% subset(DefaultStatus1==0)




# ------ 2. Applying deflation factor to [Principal] and [Balance]
# --- Getting a range of inflation factors for each date in the sampling window
date_range <- ceiling_date(unique(datCredit_train$Date), unit="month")-days(1)
datInflation <- data.table(Date=date_range)
datInflation[,Inf_Factor:=adjInflation(datMacro=datMV, time="Date", Inflation_Growth="M_Inflation_Growth", g_start=Date, g_stop = date_range[length(date_range)]), by=Date]
datCredit_train <- merge(datCredit_train, datInflation, all.x=T, by="Date")
datCredit_valid <- merge(datCredit_valid, datInflation, all.x=T, by="Date")

# --- Deflate the relevant variables
# - [Principal]
datCredit_train[, Principal_Real := Principal*Inf_Factor]
datCredit_valid[, Principal_Real := Principal*Inf_Factor]
# - [Balance]
datCredit_train[, Balance_Real := Balance*Inf_Factor]
datCredit_valid[, Balance_Real := Balance*Inf_Factor]

# --- Distribution Comparison
# - [Principal] vs [Principal_Real]
# Aggregating over the sampling window
port.aggr_Principal <- datCredit_train[,list(Principal_Aggr_Mean = mean(Principal),
                                             Principal_Real_Aggr_Mean = mean(Principal_Real)), by=list(Date)]
port.aggr_Principal_long <- pivot_longer(port.aggr_Principal, cols=c(Principal_Aggr_Mean, Principal_Real_Aggr_Mean)) %>% as.data.table()
colnames(port.aggr_Principal_long) <- c("Date", "Variable", "Value")
# Reordering the levels in [Variable] to facilitate better overlays
port.aggr_Principal_long[,Variable:=factor(Variable, levels=c("Principal_Real_Aggr_Mean", "Principal_Aggr_Mean"))]
# Plotting parameters
chosenFont<-"Cambria"
# Plotting the distributions
(g_Principal <- ggplot(data=port.aggr_Principal_long, aes(x=Date, y=Value, group=Variable)) +
  geom_point(aes(colour=Variable, shape=Variable)) +
  geom_line(aes(colour=Variable, linetype=Variable)) +
  theme(text=element_text(family=chosenFont),legend.position = "bottom",
        axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
        scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))
# Save plot
dpi<-240
ggsave(g_Principal, file=paste0(genFigPath, "Principal_vs_Principal_Real.png"), width=3000/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - [Principal] vs [Principal_Real]
# Aggregating over the sampling window
port.aggr_Balance <- datCredit_train[,list(Balance_Aggr_Mean = mean(Balance),
                                           Balance_Real_Aggr_Mean = mean(Balance_Real)), by=list(Date)]
port.aggr_Balance_long <- pivot_longer(port.aggr_Balance, cols=c(Balance_Aggr_Mean, Balance_Real_Aggr_Mean)) %>% as.data.table()
colnames(port.aggr_Balance_long) <- c("Date", "Variable", "Value")
# Plotting the distributions
(g_Balance <- ggplot(data=port.aggr_Balance_long, aes(x=Date, y=Value, group=Variable)) +
  geom_point(aes(colour=Variable, shape=Variable)) +
  geom_line(aes(colour=Variable, linetype=Variable)) +
  theme(text=element_text(family=chosenFont),legend.position = "bottom",
        axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
  scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))
# Save plot
dpi<-240
ggsave(g_Balance, file=paste0(genFigPath, "Balance_vs_Balance_Real.png"), width=3000/dpi, height=1000/dpi, dpi=dpi, bg="white")

# --- Clean up
rm(date_range, datInflation, port.aggr_Principal, port.aggr_Principal_long,
   port.aggr_Balance, port.aggr_Balance_long, g_Principal, g_Balance); gc()




# ------ 3. Testing the effects of inflation on the relevant variables in predicting the probability of default by a logit model | Full subsampled dataset(s)
# ---- 3.1 Isolating each variable
# --- 3.1.1 [Principal] vs [Principal_Real]
# - Fitting a model with [Principal]
# Model fit
logitMod_Principal <- glm(DefaultStatus1_lead_12_max ~ Principal, family="binomial", data=datCredit_train)
# Deviance and AIC
summary(logitMod_Principal)
### RESULTS: Null deviance = 275184; Residual Deviance = 273599; AIC = 273603
###         [Principal] is significant
# Coefficient of determination
coefDeter_glm(logitMod_Principal)
### RESULTS: 0.058%
# ROC Analysis
datCredit_valid[,prob_logitMod_Principal:=predict(logitMod_Principal, newdata = datCredit_valid, type = "response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_logitMod_Principal)
### RESULTS: 57.5%

# - Fitting a model with [Principal_Real]
# Model fit
logitMod_Principal_Real <- glm(DefaultStatus1_lead_12_max ~ Principal_Real, family="binomial", data=datCredit_train)
# Deviance and AIC
summary(logitMod_Principal_Real)
### RESULTS: Null deviance = 275184; Residual Deviance = 274780; AIC = 274784
###          [Principal_Real] is significant
# Coefficient of determination
coefDeter_glm(logitMod_Principal_Real)
### RESULTS: 0.0015%
# ROC Analysis
datCredit_valid[,prob_logitMod_Principal_Real:=predict(logitMod_Principal_Real, newdata = datCredit_valid, type = "response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_logitMod_Principal_Real)
### RESULTS: 55.19%

# - COMPARISON
### Model with [Principal] has a better fit than the model with [Principal_Real]; AIC of 273603 vs 274784 and Coefficient of Determination of 0.058% vs 0.0015%
### Model with [Principal] results in better prediction than the model with [Principal_Real]; AUC of 57.5% vs 55.19%
### Both [Principal] and [Principal_Real] are significant in their respective models


# --- 3.1.2 [Balance] vs [Balance_Real]
# - Fitting a model with [Balance]
# Model fit
logitMod_Balance <- glm(DefaultStatus1_lead_12_max ~ Balance, family="binomial", data=datCredit_train)
# Deviance and AIC
summary(logitMod_Balance)
### RESULTS: Null deviance = 275184; Residual Deviance = 275014; AIC = 275018
###          [Balance] is significant
# Coefficient of determination
coefDeter_glm(logitMod_Balance)
### RESULTS: 0.06%
# ROC Analysis
datCredit_valid[,prob_logitMod_Balance:=predict(logitMod_Balance, newdata = datCredit_valid, type = "response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_logitMod_Balance)
### RESULTS: 52.19%

# - Fitting a model with [Balance_Real]
# Model fit
logitMod_Balance_Real <- glm(DefaultStatus1_lead_12_max ~ Balance_Real, family="binomial", data=datCredit_train)
# Deviance and AIC
summary(logitMod_Balance_Real)
### RESULTS: Null deviance = 275184; Residual Deviance = 275158; AIC = 275162
###          [Balance_Real] is significant
# Coefficient of determination
coefDeter_glm(logitMod_Balance_Real)
### RESULTS: 0.009%
# ROC Analysis
datCredit_valid[,prob_logitMod_Balance_Real:=predict(logitMod_Balance_Real, newdata = datCredit_valid, type = "response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_logitMod_Balance_Real)
### RESULTS: 49.73%
### Look at section 2.5 to fix this
###          Model is reverse ranking, will need to adjust [Balance_Real] and refit

# - COMPARISON
### Model with [Balance] has a better fir than the model with [Balance_Real]; AIC of 275018 vs 275162 and Coefficient of Determination of 0.06% vs 0.009%
### Model with [Balance] results in better prediction than the model with [Balance_Real]; AUC of 52.19% vs 49.73%
### Both [Balance] and [Balance_Real] are significant in their respective models



# ---- 3.2 Full input space
# --- 3.2.1 Loading in the full input space and adjusting it accordingly
# - Conditionally loading in the input space
if (!exists('inputs_adv')) unpack.ffdf(paste0(genObjPath,"Adv_Formula"), tempPath)
# - Adjusting the input space for [Principal]
inputs_adv_principal_real <- as.formula(paste0("DefaultStatus1_lead_12_max~", paste0(labels(terms(inputs_adv))[-which(labels(terms(inputs_adv))=="Principal")],collapse="+"),"+ Principal_Real"))
# - Adjusting the input space for [Balance]
inputs_adv_balance_real <-  as.formula(paste0("DefaultStatus1_lead_12_max~", paste0(labels(terms(inputs_adv))[-which(labels(terms(inputs_adv))=="Balance")], collapse="+"), "+ Balance_Real"))
# - Adjusting the input space for both [Principal] and [Balance]
inputs_adv_real <-  as.formula(paste0("DefaultStatus1_lead_12_max~", paste0(labels(terms(inputs_adv))[-which(labels(terms(inputs_adv)) %in% c("Principal", "Balance"))], collapse="+"), "+ Principal_Real + Balance_Real"))


# --- 3.2.2 Full (original) model
# - Fitting the full model
logitMod_adv <- glm(inputs_adv, family="binomial", data=datCredit_train)
# - Deviance and AIC
summary(logitMod_adv)
### RESULTS: Null deviance = 255631; Residual Deviance = 168261; AIC = 168321
###          Variables are significant
# - Coefficient of determination
coefDeter_glm(logitMod_adv)
### RESULTS: 34.18%
# - ROC Analysis
datCredit_valid[,prob_logitMod_Principal_Real_Full:=predict(logitMod_Principal_Real_Full, newdata = datCredit_valid, type = "response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_logitMod_Principal_Real_Full)
### RESULTS: 90.4%


# --- 3.2.3 [Principal_Real]
# - Model fit
logitMod_Principal_Real_Full <- glm(inputs_adv_principal_real, family="binomial", data=datCredit_train)
# - Deviance and AIC
summary(logitMod_Principal_Real_Full)
### RESULTS: Null deviance = 255631; Residual Deviance = 168222; AIC = 168282
###          [Principal_Real] is significant
# - Coefficient of determination
coefDeter_glm(logitMod_Principal_Real_Full)
### RESULTS: 34.19%
# - ROC Analysis
datCredit_valid[,prob_logitMod_Principal_Real_Full:=predict(logitMod_Principal_Real_Full, newdata = datCredit_valid, type = "response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_logitMod_Principal_Real_Full)
### RESULTS: 90.4%


# --- 3.2.4 [Balance_Real]
# - Fitting a model with [Balance]
# Model fit
logitMod_Balance_Real_Full <- glm(inputs_adv_balance_real, family="binomial", data=datCredit_train)
# - Deviance and AIC
summary(logitMod_Balance_Real_Full)
### RESULTS: Null deviance = 255631; Residual Deviance = 167928; AIC = 167988
###          [Balance_Real] is significant
# - Coefficient of determination
coefDeter_glm(logitMod_Balance_Real_Full)
### RESULTS: 34.31%
# - ROC Analysis
datCredit_valid[,prob_logitMod_Balance_Real_Full:=predict(logitMod_Balance_Real_Full, newdata = datCredit_valid, type = "response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_logitMod_Balance_Real_Full)
### RESULTS: 90.56%


# --- 3.2.5 [Principal_Real] and [Balance_Real]
# - Fitting a model with [Balance]
# Model fit
logitMod_Real_Full <- glm(inputs_adv_real, family="binomial", data=datCredit_train)
# - Deviance and AIC
summary(logitMod_Real_Full)
### RESULTS: Null deviance = 255631; Residual Deviance = 168184; AIC = 168244
###          [Principal_Real] and [Balance_Real] are significant
# - Coefficient of determination
coefDeter_glm(logitMod_Real_Full)
### RESULTS: 34.21%
# - ROC Analysis
datCredit_valid[,prob_logitMod_Real_Full:=predict(logitMod_Real_Full, newdata = datCredit_valid, type = "response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_logitMod_Real_Full)
### RESULTS: 90.47%




# ------ 4. Testing the effects of inflation on the relevant variables in the probability of default predicted by a logit model | Super-subsampled dataset(s)
# --- 4.1 Subsampling the subsampled (training) dataset
# - Field names
stratifiers <- c("DefaultStatus1_lead_12_max", "Date") # Must at least include target variable used in graphing event rate
targetVar <- "DefaultStatus1_lead_12_max"
# - Subsampling & resampling parameters
smp_size <- 250000 # fixed size of downsampled set
smp_perc <- smp_size / ( datCredit_train[complete.cases(mget(stratifiers)), mget(stratifiers)][,.N] ) # Implied sampling fraction for downsampling step
# - Downsample data into a set with a fixed size (using stratified sampling) before implementing resampling scheme
set.seed(1)
datCredit_train_smp <- datCredit_train %>%group_by(across(all_of(stratifiers))) %>% slice_sample(prop=smp_perc) %>% as.data.table()
datCredit_valid_smp <- datCredit_valid %>%group_by(across(all_of(stratifiers))) %>% slice_sample(prop=smp_perc) %>% as.data.table()
cat( (datCredit_train_smp[is.na(get(targetVar)), .N] == 0 & datCredit_valid_smp[is.na(get(targetVar)), .N] == 0) %?% 'SAFE: No missingness in target variable.\n' %:% 
       'WARNING: Missingness detected in target variable.\n')
### RESULTS: Subasmpling is successful.
# - Clean up
rm(stratifiers, targetVar, smp_size, smp_perc); gc()


# ---- 4.2 Isolating each variable
# --- 4.2.1 [Principal] vs [Principal_Real]
# - Fitting a model with [Principal]
# Model fit
logitMod_Principal_smp <- glm(DefaultStatus1_lead_12_max ~ Principal, family="binomial", data=datCredit_train_smp)
# Deviance and AIC
summary(logitMod_Principal_smp)
### RESULTS: Null deviance = 68370; Residual Deviance = 67978; AIC = 67982
###          Variable is significant

# - Fitting a model with [Principal_Real]
# Model fit
logitMod_Principal_Real_smp <- glm(DefaultStatus1_lead_12_max ~ Principal_Real, family="binomial", data=datCredit_train_smp)
# Deviance and AIC
summary(logitMod_Principal_Real_smp)
### RESULTS: Null deviance = 68370; Residual Deviance = 68325; AIC = 68329
###          [Principal_Real] is significant


# - COMPARISON
### Both [Principal] and [Principal_Real] are significant in their respective models


# --- 4.2.2 [Balance] vs [Balance_Real]
# - Fitting a model with [Balance]
# Model fit
logitMod_Balance_smp <- glm(DefaultStatus1_lead_12_max ~ Balance, family="binomial", data=datCredit_train_smp)
# Deviance and AIC
summary(logitMod_Balance_smp)
### RESULTS: Null deviance = 68370; Residual Deviance = 42043; AIC = 42095
###          [Balance_Real] is significant

# - Fitting a model with [Balance_Real]
# Model fit
logitMod_Balance_Real_smp <- glm(DefaultStatus1_lead_12_max ~ Balance_Real, family="binomial", data=datCredit_train_smp)
# Deviance and AIC
summary(logitMod_Balance_Real_smp)
### RESULTS: Null deviance = 68370; Residual Deviance = 42043; AIC = 42095
###          [Balance_Real] is significant

# - COMPARISON
### Both [Balance] and [Balance_Real] are significant in their respective models



# ---- 4.3 Full input space
# --- 4.3.1 Full (original) model
# - Model fit
logitMod_adv_smp <- glm(inputs_adv, family="binomial", data=datCredit_train_smp)
# - Deviance and AIC
summary(logitMod_adv_smp)
### RESULTS: Null deviance = 63506 ; Residual Deviance = 42109; AIC = 42161
###          Insignificant variables: [Principal], [Balance]
# - Coefficient of determination
coefDeter_glm(logitMod_adv_smp)
### RESULTS: 33.69%
# - ROC Analysis
datCredit_valid_smp[,prob_logitMod_adv_smp:=predict(logitMod_adv_smp, newdata = datCredit_valid_smp, type = "response")]
auc(datCredit_valid_smp$DefaultStatus1_lead_12_max, datCredit_valid_smp$prob_logitMod_adv_smp)
### RESULTS: 90.54%

# --- 4.3.2 [Principal_Real]
# - Model fit
logitMod_Principal_Real_smp <- glm(inputs_adv_principal_real, family="binomial", data=datCredit_train_smp)
# Deviance and AIC
summary(logitMod_Principal_Real_smp)
### RESULTS: Null deviance = 63506; Residual Deviance = 42071; AIC = 42123
###          [Balance] and [Principal_Real] are significant
# - Coefficient of determination
coefDeter_glm(logitMod_Principal_Real_smp)
### RESULTS: 33.75%
# - ROC Analysis
datCredit_valid_smp[,prob_logitMod_Principal_Real_smp:=predict(logitMod_Principal_Real_smp, newdata = datCredit_valid_smp, type = "response")]
auc(datCredit_valid_smp$DefaultStatus1_lead_12_max, datCredit_valid_smp$prob_logitMod_Principal_Real_smp)
### RESULTS: 90.53%

# --- 4.3.2 [Balance_Real]
# - Model fit
logitMod_Balance_Real_smp <- glm(inputs_adv_balance_real, family="binomial", data=datCredit_train_smp)
# - Deviance and AIC
summary(logitMod_Balance_Real_smp)
### RESULTS: Null deviance = 63506; Residual Deviance = 42043; AIC = 42095
###          [Principal] and [Balance_Real] are significant
# - Coefficient of determination
coefDeter_glm(logitMod_Balance_Real_smp)
### RESULTS: 33.80%
# - ROC Analysis
datCredit_valid_smp[,prob_logitMod_Balance_Real_smp:=predict(logitMod_Balance_Real_smp, newdata = datCredit_valid_smp, type = "response")]
auc(datCredit_valid_smp$DefaultStatus1_lead_12_max, datCredit_valid_smp$prob_logitMod_Balance_Real_smp)
### RESULTS: 90.68%

# --- 4.3.3 [Principal_Real] and [Balance_Real]
# - Model fit
logitMod_Real_smp <- glm(inputs_adv_real, family="binomial", data=datCredit_train_smp)
# - Deviance and AIC
summary(logitMod_Real_smp)
### RESULTS: Null deviance = 63506; Residual Deviance = 42086; AIC = 42138
###          Insignificant variables: [Principal_Real] and [Balance_Real]
# - Coefficient of determination
coefDeter_glm(logitMod_Real_smp)
### RESULTS: 33.73%
# - ROC Analysis
datCredit_valid_smp[,prob_logitMod_Real_smp:=predict(logitMod_Real_smp, newdata = datCredit_valid_smp, type = "response")]
auc(datCredit_valid_smp$DefaultStatus1_lead_12_max, datCredit_valid_smp$prob_logitMod_Real_smp)
### RESULTS: 90.57%


# ---- 4.4 Conclusion
###   Full subsample, isolated variables:
###     - [Principal] and [Principal_Real] are significant in their respective models.
###     - [Balance] and [Balance_Real] are significant in their respective models.
###   Full subsample, full input space:
###     - [Principal] and [Balance] are significant.
###     - [Principal_Real] and [Balance] are significant.
###     - [Principal] and [Balance_Real] are significant.
###     - [Principal_Real] and [Balance_Real] are significant.
###   Sub subsample, isolated variables:
###     - [Principal] and [Principal_Real] are significant in their respective models.
###     - [Balance] and [Balance_Real] are significant.
###   Sub subsample, full input space
###     - [Principal] and [Balance] are insignificant.
###     - [Principal_Real] and [Balance] are significant.
###     - [Principal] and [Balance_Real] are significant.
###     - [Principal_Real] and [Balance_Real] are significant.

###   Use either [Balance] or [Principal] (or their derivatives in the final model), but not both in the final model.
###     [Principal] is preferred. [Balance] has a component of loan age within since balances decrease over time.
###                               [Principal] has no such component and may give a better indication of the underlying account.









