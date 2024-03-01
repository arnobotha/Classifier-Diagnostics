# ========================= MODEL DEFAULT RISK EXPERIMENT =================================
# Compare logit models with raw variables against their transformed counterparts.
# -----------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Marcel Muller

# DESCRIPTION:
# This script uses the previously prepared credit dataset to fit a few logit models
# in comparing raw variables to their transformed counterparts.
# -----------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 0a.CustomFunctions.R
#
# -- Inputs:
#   - datCredit_real | Prepared credit data from script 2f
#
# -- Outputs:
#   - Graphs to compare different models with.
# =========================================================================================




# ------ 1. Preliminaries

ptm <- proc.time() # for runtime calculations (ignore)

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)

# - Subset to exclude default spells (NOT SURE IF THIS SHOULD BE DONE, BUT CAN'T THINK OF WHY YOU WOULD WANT TO PREDICT DEFAULT WHEN IN DEFAULT)
datCredit_train <- datCredit_train %>% subset(DefaultStatus1==0)
datCredit_valid <- datCredit_valid %>% subset(DefaultStatus1==0)




# ------------ 1. Non FLI
# ------ 1. Distributional Analysis
# --- Feature engineering
# - Log transformation
datCredit_train[, BalanceLog := log(ifelse(Balance>0,Balance,0.0001))]; datCredit_valid[, BalanceLog := log(ifelse(Balance>0,Balance,0.0001))]
# - Yeo-Johnson optimal transformation
datCredit_train$BalanceYJ <- transform_yj(datCredit_train$Balance, lambda_inc = 0.1) # Optimal lambda = 0.3
datCredit_valid$BalanceYJ <- transform_yj(datCredit_valid$Balance, lambda_inc = 0.1) # Optimal lambda = 0.3

# --- Creating a long dataset to facilitate faceting
datPlot <- datCredit_train[, c("Date", "Balance", "BalanceLog", "BalanceYJ")] %>% pivot_longer(cols = c("Balance", "BalanceLog", "BalanceYJ")) %>% as.data.table()

# --- Subsetting to exclude outliers (solely for graphing purposes)
quant_99_Bal <- quantile(datPlot[name=="Balance", value], probs = 0.99, na.rm=T)
quant_99_BalLog <- quantile(datPlot[name=="BalanceLog", value], probs = 0.99, na.rm=T)
quant_99_BalYJ <- quantile(datPlot[name=="BalanceYJ", value], probs = 0.99, na.rm=T)

datPlot <- rbind(datPlot[name=="Balance" & value<quant_99_Bal, ], datPlot[name=="BalanceLog" & value<quant_99_BalLog, ], datPlot[name=="BalanceYJ" & value<quant_99_BalYJ, ])

# --- Graph
col.v <- brewer.pal(8, name="Set1")
fill.v <- brewer.pal(8, name="Pastel1")
  
ggplot(datPlot, aes(x=value)) + theme_minimal() +
  geom_histogram(aes(y=after_stat(density), col=name, fill=name)) + facet_wrap(name ~ ., scales = "free") +
  scale_colour_manual(values=col.v) + scale_fill_manual(values=fill.v)


# ------ 2. Single covariate models
# --- Model fitting
# - Raw variable
logitMod_Test_Bal <- glm(inputs_Test_Bal <- DefaultStatus1_lead_12_max ~ Balance,
                         data=datCredit_train, family="binomial")
# - Log-transformed variable
logitMod_Test_log_Bal <- glm(inputs_Test_log_Bal <- DefaultStatus1_lead_12_max ~ BalanceLog,
                             data=datCredit_train, family="binomial")
# - Yeo-Johnson optimal normality transformation
logitMod_Test_yj_Bal <- glm(inputs_Test_log_Bal <- DefaultStatus1_lead_12_max ~ BalanceYJ,
                            data=datCredit_train, family="binomial")

# --- Basic model statistics
# - Raw variable
summary(logitMod_Test_Bal)
### RESULTS: Null deviance = 275184; Residual deviance = 275014; AIC = 275018
###          Standard Error = 0.00000001235
# - Log-transformed variable
summary(logitMod_Test_log_Bal)
### RESULTS: Null deviance = 275184; Residual deviance = 275005; AIC = 275009
###          Standard Error = 0.001124
# - Yeo-Johnson optimal normality transformation
summary(logitMod_Test_yj_Bal)
### RESULTS: Null deviance = 275184; Residual deviance = 274723; AIC = 274727
###          Standard error = 0.00009032

# --- Odds ratio analysis
# - Raw variable
round(exp(cbind(OR = coef(logitMod_Test_Bal), confint.default(logitMod_Test_Bal))), 3)
### RESULTS: OR = 1; 2.5% OR = 1; 97.5% OR = 1
# - Log-transformed variable
round(exp(cbind(OR = coef(logitMod_Test_log_Bal), confint.default(logitMod_Test_log_Bal))), 3)
### RESULTS: OR = 0.984; 2.5% OR = 0.9682; 97.5% OR = 0.987
# - Yeo-Johnson optimal normality transformation
round(exp(cbind(OR = coef(logitMod_Test_yj_Bal), confint.default(logitMod_Test_yj_Bal))), 3)
### RESULTS: OR = 0.998; 2.5% OR = 0.998; 97.5% OR = 0.998

# --- ROC analysis
# - Raw variable
datCredit_valid[, prob_Test_Bal := predict(logitMod_Test_Bal, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_Test_Bal)
### RESULTS:~ 52.19%
# - Log-transformed variable
datCredit_valid[, prob_Test_log_Bal := predict(logitMod_Test_log_Bal, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_Test_log_Bal)
### RESULTS:~ 52.19%
# - Yeo-Johnson optimal transformation for normality
datCredit_valid[, prob_Test_yj_Bal := predict(logitMod_Test_yj_Bal, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_Test_yj_Bal)
### RESULTS:~ 52.19%

# --- Graph for comparison of model statistics
# - Getting the summary statistics for each model
sum_logitMod_Test_Bal <- summary(logitMod_Test_Bal)
sum_logitMod_Test_log_Bal <- summary(logitMod_Test_log_Bal)
sum_logitMod_Test_yj_Bal <- summary(logitMod_Test_yj_Bal)
# - Creating a plotting dataset
datPlot2 <- data.table(Model = c("Raw", "Log-transformed", "YJ-transformed"),
                       Standard_Error = c(signif(sum_logitMod_Test_Bal$coefficients[,2][-1],2),
                                          signif(sum_logitMod_Test_log_Bal$coefficients[,2][-1],2),
                                          signif(sum_logitMod_Test_yj_Bal$coefficients[,2][-1],2)),
                       AIC = c(signif(sum_logitMod_Test_Bal$aic,6),
                               signif(sum_logitMod_Test_log_Bal$aic,6),
                               signif(sum_logitMod_Test_yj_Bal$aic,6)),
                       Deviance = c(signif(sum_logitMod_Test_Bal$deviance,6),
                                    signif(sum_logitMod_Test_log_Bal$deviance,6),
                                    signif(sum_logitMod_Test_yj_Bal$deviance,6)))
# - Pivoting in a long format to facilitate faceting
datPlot2_long <- datPlot2 %>%pivot_longer(cols = c(Standard_Error, AIC, Deviance)) %>% data.table()
colnames(datPlot2_long) <- c("Model", "Statistic", "Value")
# - Graphing parameters
col.v <- brewer.pal(9, "Blues")[c(4,5,9)]
col.v2 <- c("white","white", "white")
label.v <- c("Raw"="Raw", "Log-transformed"="Log", "YJ-transformed"="YJ")
# - Graph
(g_comp1 <-  ggplot(datPlot2_long, aes(x=Model, y=Value)) +
   theme_minimal() +
   geom_col(aes(colour=Model, fill=Model), position="dodge") +
   facet_wrap(Statistic~., scales = "free_y") +
   geom_label(aes(label=Value), fill = rep(col.v,3), colour = rep(col.v2,3), position=position_dodge(0.9)) +
   scale_colour_manual(name="Model", values=col.v, labels=label.v) +
   scale_fill_manual(name="Model", values=col.v, labels=label.v) +
   scale_x_discrete(labels = label.v) + 
   theme(legend.position = "bottom",
         strip.text = element_text(size=20),
         panel.border = element_rect(colour = "black", fill = NA, size = 0.5)))
    

# --- Clean up
rm(logitMod_Test_Bal, logitMod_Test_log_Bal, logitMod_Test_yj_Bal, sum_logitMod_Test_Bal, sum_logitMod_Test_log_Bal, sum_logitMod_Test_yj_Bal,
   col.v, col.v2, label.v, datPlot2, datPlot2_long, g_comp1)
datCredit_valid[, `:=` (prob_Test_Bal=NULL, prob_Test_log_Bal=NULL, prob_Test_yj_Bal=NULL)]


# --- CONCLUSION
### No difference between the AUC values of the raw- or transformed variables. The deviance, and AIC values do, however, differ.
### The standard errors of the estimated coefficients increase significantly when transformations are used
### Do not used transformed version of variables.




# ------ 3. Two covariate models
# --- Model fitting
# - Raw variable [Balance]
logitMod_Test_Bal2 <- glm(inputs_Test_Bal2 <- DefaultStatus1_lead_12_max ~ Balance + g0_Delinq,
                         data=datCredit_train, family="binomial")
# - Transformed variable [BalanceLog]
logitMod_Test_log_Bal2 <- glm(inputs_Test_log_Bal2 <- DefaultStatus1_lead_12_max ~ BalanceLog + g0_Delinq,
                              data=datCredit_train, family="binomial")
# - Yeo-Johnson transformed variable [BalanceYJ]
logitMod_Test_yj_Bal2 <- glm(inputs_Test_yj_Bal2 <- DefaultStatus1_lead_12_max ~ BalanceYJ + g0_Delinq,
                             data=datCredit_train, family="binomial")

# --- Basic model statistics
# - Raw variable [Balance]
summary(logitMod_Test_Bal2)
### RESULTS: Null deviance = 275184; Residual deviance = 215917; AIC = 215923
###          Standard error [Balance] = 0.00000001342; Standard error [g0_Delinq] = 0.01113485479
# - Transformed variable [BalanceLog]
summary(logitMod_Test_log_Bal2)
### RESULTS: Null deviance = 275184; Residual deviance = 214993; AIC = 214999
###          Standard error [BalanceLog] = 0.001225; Standard error [g0_Delinq] = 0.011331
# - Yeo-Johnson transformed variable [BalanceYJ]
summary(logitMod_Test_yj_Bal2)
### RESULTS: Null deviance = 215431; Residual deviance = 215437; AIC = 215437
###          Standard error [BalanceLog] = 0.0001057; Standard error [g0_Delinq] = 0.0111498

# --- Odds ration analysis
# - Raw variable [Balance]
round(exp(cbind(OR = coef(logitMod_Test_Bal2), confint.default(logitMod_Test_Bal2))), 3)
### RESULTS: Balance:   OR = 1;      2.5% OR = 1;      97.5% OR = 1
###          g0_Delinq: OR = 15.011; 2.5% OR = 14.687; 97.5% OR = 15.342
# - Transformed variable [BalanceLog]
round(exp(cbind(OR = coef(logitMod_Test_log_Bal2), confint.default(logitMod_Test_log_Bal2))), 3)
### RESULTS: Balance:   OR = 0.960;  2.5% OR = 0.957;  97.5% OR = 0.962
###          g0_Delinq: OR = 16.681; 2.5% OR = 15.336; 97.5% OR = 16.033
# - Yeo-Johnson transformed variable [BalanceYJ]
round(exp(cbind(OR = coef(logitMod_Test_yj_Bal2), confint.default(logitMod_Test_yj_Bal2))), 3)
### RESULTS: Balance:   OR = 0.998;  2.5% OR = 0.997;  97.5% OR = 0.998
###          g0_Delinq: OR = 15.112; 2.5% OR = 14.785; 97.5% OR = 15.446

# --- ROC analysis
# - Raw variable [Balance]
datCredit_valid[, prob_Test_Bal2 := predict(logitMod_Test_Bal2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_Test_Bal2)
### RESULTS:~ 75.97%
# - Transformed variable [BalanceLog]
datCredit_valid[, prob_Test_log_Bal2 := predict(logitMod_Test_log_Bal2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_Test_log_Bal2)
### RESULTS:~ 75.97%
# - Yeo-Johnson transformed variable [BalanceYJ]
datCredit_valid[, prob_Test_yj_Bal2 := predict(logitMod_Test_yj_Bal2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_Test_yj_Bal2)
### RESULTS:~ 75.97%

# --- Graph for comparison of model statistics
# - Getting the summary statistics for each model
sum_logitMod_Test_Bal2 <- summary(logitMod_Test_Bal2)
sum_logitMod_Test_log_Bal2 <- summary(logitMod_Test_log_Bal2)
sum_logitMod_Test_yj_Bal2 <- summary(logitMod_Test_yj_Bal2)
# - Creating a plotting dataset
datPlot3 <- data.table(Model = c("Raw", "Log-transformed", "YJ-transformed"),
                       Standard_Error = c(signif(sum_logitMod_Test_Bal2$coefficients[2,2],2),
                                          signif(sum_logitMod_Test_log_Bal2$coefficients[2,2],2),
                                          signif(sum_logitMod_Test_yj_Bal2$coefficients[2,2],2)),
                       AIC = c(signif(sum_logitMod_Test_Bal2$aic,6),
                               signif(sum_logitMod_Test_log_Bal2$aic,6),
                               signif(sum_logitMod_Test_yj_Bal2$aic,6)),
                       Deviance = c(signif(sum_logitMod_Test_Bal2$deviance,6),
                                    signif(sum_logitMod_Test_log_Bal2$deviance,6),
                                    signif(sum_logitMod_Test_yj_Bal2$deviance,6)))
# - Pivoting in a long format to facilitate faceting
datPlot3_long <- datPlot3 %>%pivot_longer(cols = c(Standard_Error, AIC, Deviance)) %>% data.table()
colnames(datPlot3_long) <- c("Model", "Statistic", "Value")
# - Graphing parameters
col.v <- brewer.pal(9, "Blues")[c(4,5,9)]
col.v2 <- c("white","white", "white")
label.v <- c("Raw"="Raw", "Log-transformed"="Log", "YJ-transformed"="YJ")
# - Graph
(g_comp2 <-  ggplot(datPlot3_long, aes(x=Model, y=Value)) +
    theme_minimal() +
    geom_col(aes(colour=Model, fill=Model), position="dodge") +
    facet_wrap(Statistic~., scales = "free_y") +
    geom_label(aes(label=Value), fill = rep(col.v,3), colour = rep(col.v2,3), position=position_dodge(0.9)) +
    scale_colour_manual(name="Model", values=col.v, labels=label.v) +
    scale_fill_manual(name="Model", values=col.v, labels=label.v) +
    scale_x_discrete(labels = label.v) + 
    theme(legend.position = "bottom",
          strip.text = element_text(size=20),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5)))


# --- Clean up
rm(logitMod_Test_Bal2, logitMod_Test_log_Bal2, logitMod_Test_yj_Bal2, sum_logitMod_Test_Bal2, sum_logitMod_Test_log_Bal2, sum_logitMod_Test_yj_Bal2,
   col.v, col.v2, label.v, datPlot3, datPlot3_long, g_comp2)
datCredit_valid[, `:=` (prob_Test_Bal2=NULL, prob_Test_log_Bal2=NULL, prob_Test_yj_Bal2=NULL)]


# --- CONCLUSION
### No difference between the AUC values of the raw- or transformed variables. The deviance, and AIC values do, however, differ.
### The standard errors of the estimated coefficients increase significantly when transformations are used
### Do not used transformed version of variables.




# ------------ 2. FLI
# ------ 1. Distributional Analysis
# --- Applying transformations
# - Squared (power) transformation
datCredit_train[, M_Repo_RateSqaured := (M_Repo_Rate*100)^2] # Multiplying with 100 to increase the scale...enables the power transformation to be applied monotonically over the entire range
datCredit_valid[, M_Repo_RateSqaured := (M_Repo_Rate*100)^2] 
# - Yeo-Johnson optimal transformation for normality
datCredit_train[!is.na(M_Repo_Rate), M_Repo_RateYJ := transform_yj(x=M_Repo_Rate*100, bound_lower=-2, bound_upper=2, lambda_inc=0.1)] # Lambda = 0.4  |Subsetting to exclude missing repo rate values (missing values for the last month in the sampling window - 2December 2022)
datCredit_valid[!is.na(M_Repo_Rate), M_Repo_RateYJ := transform_yj(x=M_Repo_Rate*100, bound_lower=-2, bound_upper=2, lambda_inc=0.1)] # Lambda = 0.4

# --- Creating a long dataset to facilitate faceting
datPlot_M <- unique(datCredit_train[, list(Date, M_Repo_Rate, M_Repo_RateSqaured, M_Repo_RateYJ)]) %>% pivot_longer(cols = c("M_Repo_Rate", "M_Repo_RateSqaured", "M_Repo_RateYJ")) %>% as.data.table()

# --- Graph
col.v <- brewer.pal(8, name="Set1")
fill.v <- brewer.pal(8, name="Pastel1")

ggplot(datPlot_M, aes(x=value)) + theme_minimal() +
  geom_histogram(aes(y=after_stat(density), col=name, fill=name)) + facet_wrap(name ~ ., scales = "free") +
  scale_colour_manual(values=col.v) + scale_fill_manual(values=fill.v)




# ------ 2. Single covariate models
# --- Model fitting
# - Raw variable [M_Repo_Rate]
logitMod_Test_RepoRate <- glm(DefaultStatus1_lead_12_max ~ M_Repo_Rate,
                              data=datCredit_train, family="binomial")
# - Transformed variable [M_Repo_RateSquared]
logitMod_Test_log_RepoRate_Sqaured <- glm(DefaultStatus1_lead_12_max ~ M_Repo_RateSqaured,
                                          data=datCredit_train, family="binomial")
# - Yeo-Johnson transformed variable [M_Repo_RateYJ]
logitMod_Test_RepoRate_YJ <- glm(DefaultStatus1_lead_12_max ~ M_Repo_RateYJ,
                                 data=datCredit_train, family="binomial")

# --- Basic model statistics
# - Raw variable [M_Repo_Rate]
summary(logitMod_Test_RepoRate)
### RESULTS: Null deviance = 274496; Residual deviance = 271654; AIC = 271658
###          Standard error = 0.24275
# - Transformed variable [M_Repo_RateSquared]
summary(logitMod_Test_log_RepoRate_Sqaured)
### RESULTS: Null deviance = 274496; Residual deviance = 271471; AIC = 271475
###          Standard error = 0.0001461
# - Yeo-Johnson transformed variable [M_Repo_RateYJ]
summary(logitMod_Test_RepoRate_YJ)
### RESULTS: Null deviance = 274496; Residual deviance = 271841; AIC = 271845
###          Standard error = 0.008971

# --- Odds ration analysis
# - Raw variable [M_Repo_Rate]
round(exp(cbind(OR = coef(logitMod_Test_RepoRate), confint.default(logitMod_Test_RepoRate))), 3)
### RESULTS: OR = 675800.709; 2.5% OR = 419941.928; 97.5% OR = 1087547.036
# - Transformed variable [M_Repo_RateSquared]
round(exp(cbind(OR = coef(logitMod_Test_log_RepoRate_Sqaured), confint.default(logitMod_Test_log_RepoRate_Sqaured))), 3)
### RESULTS: OR = 1.009; 2.5% OR = 1.008; 97.5% OR = 1.009
# - Yeo-Johnson transformed variable [M_Repo_RateYJ]
round(exp(cbind(OR = coef(logitMod_Test_RepoRate_YJ), confint.default(logitMod_Test_RepoRate_YJ))), 3)
### RESULTS: OR = 1.604; 2.5% OR = 1.576; 97.5% OR = 1.633

# --- ROC analysis
# - Raw variable [M_Repo_Rate]
datCredit_valid[, prob_Test_M_Repo_Rate := predict(logitMod_Test_RepoRate, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_Test_M_Repo_Rate)
### RESULTS:~ 56.75%
# - Transformed variable [M_Repo_RateSquared]
datCredit_valid[, prob_Test_M_Repo_RateSquared := predict(logitMod_Test_log_RepoRate_Sqaured, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_Test_M_Repo_RateSquared)
### RESULTS:~ 56.75%
# - Yeo-Johnson transformed variable [M_Repo_RateYJ]
datCredit_valid[, prob_Test_M_Repo_RateYJ := predict(logitMod_Test_RepoRate_YJ, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_Test_M_Repo_RateYJ)
### RESULTS:~ 56.75%


# --- Graph for comparison of model statistics
# - Getting the summary statistics for each model
sum_logitMod_Test_RepoRate <- summary(logitMod_Test_RepoRate)
sum_logitMod_Test_log_RepoRate_Sqaured <- summary(logitMod_Test_log_RepoRate_Sqaured)
sum_logitMod_Test_RepoRate_YJ <- summary(logitMod_Test_RepoRate_YJ)
# - Creating a plotting dataset
datPlot4 <- data.table(Model = c("Raw", "Squared", "YJ-transformed"),
                       Standard_Error = c(signif(sum_logitMod_Test_RepoRate$coefficients[2,2],2),
                                          signif(sum_logitMod_Test_log_RepoRate_Sqaured$coefficients[2,2],2),
                                          signif(sum_logitMod_Test_RepoRate_YJ$coefficients[2,2],2)),
                       AIC = c(signif(sum_logitMod_Test_RepoRate$aic,6),
                               signif(sum_logitMod_Test_log_RepoRate_Sqaured$aic,6),
                               signif(sum_logitMod_Test_RepoRate_YJ$aic,6)),
                       Deviance = c(signif(sum_logitMod_Test_RepoRate$deviance,6),
                                    signif(sum_logitMod_Test_log_RepoRate_Sqaured$deviance,6),
                                    signif(sum_logitMod_Test_RepoRate_YJ$deviance,6)))
# - Pivoting in a long format to facilitate faceting
datPlot4_long <- datPlot4 %>%pivot_longer(cols = c(Standard_Error, AIC, Deviance)) %>% data.table()
colnames(datPlot4_long) <- c("Model", "Statistic", "Value")
# - Graphing parameters
col.v <- brewer.pal(9, "Blues")[c(4,5,9)]
col.v2 <- c("white","white", "white")
label.v <- c("Raw"="Raw", "Squared"="Squared", "YJ-transformed"="YJ")
# - Graph
(g_comp3 <-  ggplot(datPlot4_long, aes(x=Model, y=Value)) +
    theme_minimal() +
    geom_col(aes(colour=Model, fill=Model), position="dodge") +
    facet_wrap(Statistic~., scales = "free_y") +
    geom_label(aes(label=Value), fill = rep(col.v,3), colour = rep(col.v2,3), position=position_dodge(0.9)) +
    scale_colour_manual(name="Model", values=col.v, labels=label.v) +
    scale_fill_manual(name="Model", values=col.v, labels=label.v) +
    scale_x_discrete(labels = label.v) + 
    theme(legend.position = "bottom",
          strip.text = element_text(size=20),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5)))


# --- Clean up
rm(logitMod_Test_RepoRate, logitMod_Test_log_RepoRate_Sqaured,  logitMod_Test_RepoRate_YJ2, sum_logitMod_Test_RepoRate2, sum_logitMod_Test_log_RepoRate_Sqaured2, sum_logitMod_Test_RepoRate_YJ2,
   col.v, col.v2, label.v, datPlot4, datPlot4_long, g_comp3)
datCredit_valid[, `:=` (prob_Test_M_Repo_Rate2=NULL, prob_Test_M_Repo_RateSquared2=NULL, prob_Test_M_Repo_RateYJ2=NULL)]


# --- CONCLUSION
### No difference between the AUC values of the raw- or transformed variables. The deviance, and AIC values do, however, differ.
### The standard errors of the estimated coefficients increase significantly when transformations are used
### Do not used transformed version of variables.




# ------ 3. Two covariate models
# --- Model fitting
# - Raw variable [M_Repo_Rate]
logitMod_Test_RepoRate2 <- glm(DefaultStatus1_lead_12_max ~ M_Repo_Rate + g0_Delinq,
                              data=datCredit_train, family="binomial")
# - Transformed variable [M_Repo_RateSquared]
logitMod_Test_log_RepoRate_Sqaured2 <- glm(DefaultStatus1_lead_12_max ~ M_Repo_RateSqaured + g0_Delinq,
                                          data=datCredit_train, family="binomial")
# - Yeo-Johnson transformed variable [M_Repo_RateYJ]
logitMod_Test_RepoRate_YJ2 <- glm(DefaultStatus1_lead_12_max ~ M_Repo_RateYJ + g0_Delinq,
                                 data=datCredit_train, family="binomial")

# --- Basic model statistics
# - Raw variable [M_Repo_Rate]
summary(logitMod_Test_RepoRate2)
### RESULTS: Null deviance = 274496; Residual deviance = 214551; AIC = 214557
###          Standard error [M_Repo_Rate] = 0.26818; Standard error [g0_Delinq] = 0.01116
# - Transformed variable [M_Repo_RateSquared]
summary(logitMod_Test_log_RepoRate_Sqaured2)
### RESULTS: Null deviance = 274496; Residual deviance = 214497; AIC = 214503
###          Standard error [M_Repo_RateSqaured] = 0.0117934; Standard error [g0_Delinq] = 0.0111570
# - Yeo-Johnson transformed variable [M_Repo_RateYJ]
summary(logitMod_Test_RepoRate_YJ2)
### RESULTS: Null deviance = 274496; Residual deviance = 214604; AIC = 214610
###          Standard error [M_Repo_RateYJ] = 0.009772; Standard error [g0_Delinq] = 0.011154

# --- Odds ration analysis
# - Raw variable [M_Repo_Rate]
round(exp(cbind(OR = coef(logitMod_Test_RepoRate2), confint.default(logitMod_Test_RepoRate2))), 3)
### RESULTS: M_Repo_Rate:   OR = 2098.065;  2.5% OR = 1240.349;  97.5% OR = 3548.900
###          g0_Delinq:     OR = 14.450; 2.5% OR = 14.137; 97.5% OR = 14.769
# - Transformed variable [M_Repo_RateSquared]
round(exp(cbind(OR = coef(logitMod_Test_log_RepoRate_Sqaured2), confint.default(logitMod_Test_log_RepoRate_Sqaured2))), 3)
### RESULTS: M_Repo_RateSquared:   OR = 1.005;  2.5% OR = 1.005;  97.5% OR = 1.005
###          g0_Delinq:            OR = 14.415; 2.5% OR = 14.103; 97.5% OR = 14.733
# - Yeo-Johnson transformed variable [M_Repo_RateYJ]
round(exp(cbind(OR = coef(logitMod_Test_RepoRate_YJ2), confint.default(logitMod_Test_RepoRate_YJ2))), 3)
### RESULTS: M_Repo_RateYJ:   OR = 1.306;  2.5% OR = 1.281;  97.5% OR = 1.332
###          g0_Delinq:       OR = 14.487; 2.5% OR = 14.174; 97.5% OR = 14.807

# --- ROC analysis
# - Raw variable [M_Repo_Rate]
datCredit_valid[, prob_Test_M_Repo_Rate2 := predict(logitMod_Test_RepoRate2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_Test_M_Repo_Rate2)
### RESULTS:~ 77.01%
# - Transformed variable [M_Repo_RateSquared]
datCredit_valid[, prob_Test_M_Repo_RateSquared2 := predict(logitMod_Test_log_RepoRate_Sqaured2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_Test_M_Repo_RateSquared2)
### RESULTS:~ 77.01%
# - Yeo-Johnson transformed variable [M_Repo_RateYJ]
datCredit_valid[, prob_Test_M_Repo_RateYJ2 := predict(logitMod_Test_RepoRate_YJ2, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_Test_M_Repo_RateYJ2)
### RESULTS:~ 77.01%


# --- Graph for comparison of model statistics
# - Getting the summary statistics for each model
sum_logitMod_Test_RepoRate2 <- summary(logitMod_Test_RepoRate2)
sum_logitMod_Test_log_RepoRate_Sqaured2 <- summary(logitMod_Test_log_RepoRate_Sqaured2)
sum_logitMod_Test_RepoRate_YJ2 <- summary(logitMod_Test_RepoRate_YJ2)
# - Creating a plotting dataset
datPlot5 <- data.table(Model = c("Raw", "Squared", "YJ-transformed"),
                       Standard_Error = c(signif(sum_logitMod_Test_RepoRate2$coefficients[2,2],2),
                                          signif(sum_logitMod_Test_log_RepoRate_Sqaured2$coefficients[2,2],2),
                                          signif(sum_logitMod_Test_RepoRate_YJ2$coefficients[2,2],2)),
                       AIC = c(signif(sum_logitMod_Test_RepoRate2$aic,6),
                               signif(sum_logitMod_Test_log_RepoRate_Sqaured2$aic,6),
                               signif(sum_logitMod_Test_RepoRate_YJ2$aic,6)),
                       Deviance = c(signif(sum_logitMod_Test_RepoRate2$deviance,6),
                                    signif(sum_logitMod_Test_log_RepoRate_Sqaured2$deviance,6),
                                    signif(sum_logitMod_Test_RepoRate_YJ2$deviance,6)))
# - Pivoting in a long format to facilitate faceting
datPlot5_long <- datPlot5 %>%pivot_longer(cols = c(Standard_Error, AIC, Deviance)) %>% data.table()
colnames(datPlot5_long) <- c("Model", "Statistic", "Value")
# - Graphing parameters
col.v <- brewer.pal(9, "Blues")[c(4,5,9)]
col.v2 <- c("white","white", "white")
label.v <- c("Raw"="Raw", "Squared"="Squared", "YJ-transformed"="YJ")
# - Graph
(g_comp4 <-  ggplot(datPlot5_long, aes(x=Model, y=Value)) +
    theme_minimal() +
    geom_col(aes(colour=Model, fill=Model), position="dodge") +
    facet_wrap(Statistic~., scales = "free_y") +
    geom_label(aes(label=Value), fill = rep(col.v,3), colour = rep(col.v2,3), position=position_dodge(0.9)) +
    scale_colour_manual(name="Model", values=col.v, labels=label.v) +
    scale_fill_manual(name="Model", values=col.v, labels=label.v) +
    scale_x_discrete(labels = label.v) + 
    theme(legend.position = "bottom",
          strip.text = element_text(size=20),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5)))


# --- Clean up
rm(logitMod_Test_RepoRate2, logitMod_Test_log_RepoRate_Sqaured2,  logitMod_Test_RepoRate_YJ2, sum_logitMod_Test_RepoRate2, sum_logitMod_Test_log_RepoRate_Sqaured2, sum_logitMod_Test_RepoRate_YJ2,
   col.v, col.v2, label.v, datPlot5, datPlot5_long, g_comp4)
datCredit_valid[, `:=` (prob_Test_M_Repo_Rate2=NULL, prob_Test_M_Repo_RateSquared2=NULL, prob_Test_M_Repo_RateYJ2=NULL)]


# --- CONCLUSION
### No difference between the AUC values of the raw- or transformed variables. The deviance, and AIC values do, however, differ.
### The standard errors of the estimated coefficients increase significantly when transformations are used
### Do not used transformed version of variables.


### AB: Conclude section for now, can explore the transformations further in the future (suggested that EO will investigate)
