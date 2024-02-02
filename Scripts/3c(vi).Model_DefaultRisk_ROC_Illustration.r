# ========================== MODEL DEFAULT RISK - ROC ANALYSIS ILLUSTRATION =================================
# Showcasing the use of ROC curves in evalutaing the predictive power of logit models.
# -----------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Marcel Muller

# DESCRIPTION:
# This script uses the previously selected variables in fitting different logit models according to their
# level of complexity. ROC analysis are conducted on the models and the results are overlaid as to produce
# a single graph. This graph is itself used within the binary classification standard.
# -----------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 0a.CustomFunctions.R
#   - 3b.Data_Subsample_Fusion2
#   - 3c(i).Model_DefaultRisk_Basic
#   - 3c(ii).MOdel_DefaultRisk_Macro
#
# -- Inputs:
#   - datCredit_train | Prepared credit data from script 3b
#   - datCredit_valid | Prepared credit data from script 3b
#
# -- Outputs:
#   - Some graphs showcasing ROC analysis conducted with various logit models.
# ===========================================================================================================


# ------ 1. Preliminaries

ptm <- proc.time() # for runtime calculations (ignore)

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)

# - Subset to exclude default spells
datCredit_train <- datCredit_train %>% subset(DefaultStatus1==0)
datCredit_valid <- datCredit_valid %>% subset(DefaultStatus1==0)




# ------ 2. Most basic logit model (only basic/core variables)
logitMod_1 <- glm(DefaultStatus1_lead_12_max ~ TimeInPerfSpell + Balance + Principal + InterestRate_Margin_imputed_mean
                           , data=datCredit_train, family="binomial")
summary(logitMod_1) # Null deviance = 275184; Residual deviance = 268598; AIC = 268608
# - Variable importance
(varImport_logitMod1 <- varImport_logit(logit_model=logitMod_1, method="ac", plot=T, sig_level=0.1, standardise=T)) # Top 3 variables: [TimeInPerfSpell], [Balance], and [InterestRate_Margin_imputed_mean]
# - ROC analysis
datCredit_train[, prob_1 := predict(logitMod_1, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_1 := predict(logitMod_1, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_1) # 63.65%
(auc_logitMod_1 <- auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_1)) # 63.64%
### RESULTS:  All estimated coefficients and their associated standard errors seem reasonable
###           The model is not overfitted as evidenced by the AUCs computed on the training- and validation datasets being close to one another (63.65% vs 63.64)

### CONCLUSION: Model is safe to use in further analysis and/or comparisons




# ------ 3. Semi-advanced logit model (some core and feature engineered delinquency variables)
logitMod_2 <- glm(DefaultStatus1_lead_12_max ~ PerfSpell_Num + g0_Delinq + g0_Delinq_SD_4 + g0_Delinq_SD_6
                  , data=datCredit_train, family="binomial")
summary(logitMod_2) # Null deviance = 255631; Residual deviance = 190886; AIC = 190896
# - FIRM analysis
(varImport_logitMod2 <- varImport_logit(logitMod_2, method="ac", plot=T, sig_level=0.1, standardise=T)) # Top 3 variables: [PerfSpell_Num], [g0_Delinq_SD_4], and [g0_Delinq]
# - ROC analysis
datCredit_train[, prob_2 := predict(logitMod_2, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_2 := predict(logitMod_2, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_2) # 81.67%
(auc_logitMod_2 <- auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_2)) # 81.37%
### RESULTS:  All estimated coefficients and their associated standard errors seem reasonable
###           The model is not overfitted as evidenced by the AUCs computed on the training- and validation datasets being close to one another (81.67% vs 81.37%)

### CONCLUSION: Model is safe to use in further analysis and/or comparisons




# ------ 4. Advanced logit model (combining the input space of the above models with some macroeconomic- and portfolio level variables)
logitMod_3 <- glm(DefaultStatus1_lead_12_max ~ TimeInPerfSpell + Balance + Principal + InterestRate_Margin_imputed_mean + # Core variables
                    PerfSpell_Num + g0_Delinq + g0_Delinq_SD_4 + g0_Delinq_SD_6 + # Delinquency variables
                    M_RealGDP_Growth_12 + M_RealIncome_Growth_12 + M_Repo_Rate + M_Repo_Rate_12 + # Macroeconomic variables
                    g0_Delinq_Any_Aggr_Prop + AgeToTerm_Aggr_Mean + NewLoans_Aggr_Prop_1 # Portfolio level variables
                  , data=datCredit_train, family="binomial")
summary(logitMod_3) # Null deviance = 254945; Residual deviance = 187734; AIC = 187766
# - Variable importance
(varImport_logitMod3 <- varImport_logit(logit_model=logitMod_3, method="ac", plot=T, sig_level=0.1, standardise=T)) # Top 3 variables: [Balance], [g0_Delinq], and [M_Repo_Rate]
# - ROC analysis
datCredit_train[, prob_3 := predict(logitMod_3, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_3 := predict(logitMod_3, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_3) # 85.84%
(auc_logitMod_3 <- auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_3)) # 85.61%
### RESULTS:  All estimated coefficients and their associated standard errors seem reasonable
###           The model is not overfitted as evidenced by the AUCs computed on the training- and validation datasets being close to one another (85.84% vs 85.61%)

### CONCLUSION: Model is safe to use in further analysis and/or comparisons




# ------ 5. ROC analysis
# --- Creating ROC objects
roc_obj_1 <- roc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_1)
roc_obj_2 <- roc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_2)
roc_obj_3 <- roc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_3)

# --- Creating the individual ROC plots
# - Basic model
ggroc(roc_obj_1)
# - Semi-advanced model
ggroc(roc_obj_2)
# - Advanced model
ggroc(roc_obj_3)

# --- Creating the combined (overlaid) plot
# - Preparing the data for plotting from each ROC object
datPlot <- rbind(data.table(TPR=roc_obj_1$sensitivities,
                            FPR=1-roc_obj_1$specificities,
                            Model="a_Basic"),
                 data.table(TPR=roc_obj_2$sensitivities,
                            FPR=1-roc_obj_2$specificities,
                            Model="b_Intermediate"),
                 data.table(TPR=roc_obj_3$sensitivities,
                            FPR=1-roc_obj_3$specificities,
                            Model="c_Advanced"))
# - Getting the AUCs of each model (so that the values can be used as labels)
dat_anno <- data.table(Model=c("a_Basic","b_Intermediate","c_Advanced"),
                       AUC=c(auc_logitMod_1, auc_logitMod_2, auc_logitMod_3),
                       x=c(0.5,0.5,0.5), y=c(0.68,0.83,0.9))
dat_anno[,Label:=paste0("AUC=",sprintf("%.2f",AUC*100),"%")]
# - Plotting parameters
chosenFont <- "Cambria"; dpi<-180
col.v <- brewer.pal(10, "Paired")[c(10,8,6)]
fill.v <- brewer.pal(10, "Paired")[c(9,7,5)]
linetype.v <- c(2,3,4)
label.v <- list("a_Basic"="Basic",
                "b_Intermediate"="Intermediate",
                "c_Advanced"="Advanced")
# - Overlaying the ROC plots
(g_ROC_compar <- ggplot(datPlot) + theme_minimal() +
  labs(x=bquote("False positive rate "*italic(F^{"+"})~" = "*italic(1-S^{"-"})), y=bquote("True positive rate "*italic(T^{"+"})~" = "*italic(S^{"+"}))) +
  theme(text=element_text(family=chosenFont), legend.position="bottom",
        axis.text.x=element_text(angle=90), legend.text=element_text(family=chosenFont), 
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
  geom_line(aes(x=FPR, y=TPR, colour=Model)) +
  # geom_area(aes(x=FPR, y=TPR, colour=Model, alpha=0.4)) +
  geom_abline(intercept=0, slope=1, linetype=1, linewidth=0.3) +
  geom_label(data=dat_anno, aes(x=x, label=Label, y=y, colour=Model), show.legend=F, size=2) +
  scale_colour_manual(name="Model", values=col.v, label=label.v) +
  scale_linetype_manual(name="Model", values=linetype.v, label=label.v) +
  scale_x_continuous(breaks=pretty_breaks(), label=percent) +
  scale_y_continuous(breaks=pretty_breaks(), label=percent))

# --- Saving the combined (overlaid) graph
ggsave(g_ROC_compar, file=paste0(genFigPath, "ROC_Curves_Comparison.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# --- Clean up
datCredit_train[, `:=`(prob_1=NULL, prob_2=NULL, prob_3=NULL)]
datCredit_valid[, `:=`(prob_1=NULL, prob_2=NULL, prob_3=NULL)]
rm(auc_logitMod_1, auc_logitMod_2, auc_logitMod_3, varImport_logitMod1, varImport_logitMod2, varImport_logitMod3,
   logitMod_1, logitMod_2, logitMod_3, roc_obj_1, roc_obj_2, roc_obj_3,
   chosenFont, col.v, fill.v, linetype.v, label.v,
   datPlot, dat_anno, g_ROC_compar); gc()

