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
logitMod_1 <- glm(DefaultStatus1_lead_12_max ~ TimeInPerfSpell + PrevDefaults + g0_Delinq_fac
                           , data=datCredit_train, family="binomial")
summary(logitMod_1) # Null deviance = 275184; Residual deviance = 268598; AIC = 268608
# - Variable importance
(varImport_logitMod1 <- varImport_logit(logit_model=logitMod_1, method="absCoef", plot=T, sig_level=0.1, standardise=F)) # Top 3 variables: [TimeInPerfSpell], [Balance], and [InterestRate_Margin_imputed_mean]
# - ROC analysis
datCredit_train[, prob_1 := predict(logitMod_1, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_1 := predict(logitMod_1, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_1) # 63.65%
(auc_logitMod_1 <- auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_1)) # 63.64%
### RESULTS:  All estimated coefficients and their associated standard errors seem reasonable
###           The model is not overfitted as evidenced by the AUCs computed on the training- and validation datasets being close to one another (63.65% vs 63.64)

### CONCLUSION: Model is safe to use in further analysis and/or comparisons




# ------ 3. Intermediate logit model (some core and feature engineered delinquency variables)
logitMod_2 <- glm(DefaultStatus1_lead_12_max ~ g0_Delinq + slc_past_due_amt_imputed_med + slc_acct_arr_dir_3
                  , data=datCredit_train, family="binomial")
summary(logitMod_2) # Null deviance = 255631; Residual deviance = 211844; AIC = 211856
# - Variable importance
(varImport_logitMod2 <- varImport_logit(logitMod_2, method="absCoef", plot=T, sig_level=0.15, standardise=T)) # Top 3 variables: [slc_acct_arr_dir3SAME], [slc_acct_arr_dir3MISSING_DATA], and [g0_Delinq]
# - ROC analysis
datCredit_train[, prob_2 := predict(logitMod_2, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_2 := predict(logitMod_2, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_2) # 77.57%
(auc_logitMod_2 <- auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_2)) # 77.2%
### RESULTS:  All estimated coefficients and their associated standard errors seem reasonable
###           The model is not overfitted as evidenced by the AUCs computed on the training- and validation datasets being close to one another (81.67% vs 81.37%)

### CONCLUSION: Model is safe to use in further analysis and/or comparisons




# ------ 4. Advanced logit model (combining the input space of the above models with some macroeconomic- and portfolio level variables)
logitMod_3 <- glm(DefaultStatus1_lead_12_max ~ TimeInPerfSpell + Balance + Principal + InterestRate_Margin_imputed_mean + # Core variables
                    g0_Delinq + slc_past_due_amt_imputed_med + slc_acct_arr_dir_3 + g0_Delinq_SD_4 + g0_Delinq_SD_6 + # Delinquency variables
                    M_RealGDP_Growth_12 + M_RealIncome_Growth_12 + M_Repo_Rate + M_Repo_Rate_12 + # Macroeconomic variables
                    g0_Delinq_Any_Aggr_Prop + AgeToTerm_Aggr_Mean + NewLoans_Aggr_Prop_1 # Portfolio level variables
                  , data=datCredit_train, family="binomial")
summary(logitMod_3) # Null deviance = 254945; Residual deviance = 189182; AIC = 189220
# - Variable importance
(varImport_logitMod3 <- varImport_logit(logit_model=logitMod_3, method="absCoef", plot=T, sig_level=0.1, standardise=T)) # Top 3 variables: [slc_acct_arr_dir3SAME], [Principal], and [g0-Delinq]
# - ROC analysis
datCredit_train[, prob_3 := predict(logitMod_3, newdata = datCredit_train, type="response")]
datCredit_valid[, prob_3 := predict(logitMod_3, newdata = datCredit_valid, type="response")]
auc(datCredit_train$DefaultStatus1_lead_12_max, datCredit_train$prob_3) # 83.77%
(auc_logitMod_3 <- auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_3)) # 83.5%
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
dat_anno <- data.table(Model=c("a_Basic", "b_Intermediate","c_Advanced"),
                       AUC=c(auc_logitMod_1, auc_logitMod_2, auc_logitMod_3),
                       x=c(0.5,0.5,0.5), y=c(0.68,0.83, 0.90))
dat_anno[,Label:=paste0("AUC=",sprintf("%.2f",AUC*100),"%")]
# - Plotting parameters
chosenFont <- "Cambria"; dpi<-180
col.v <- brewer.pal(10, "Paired")[c(8,6,4)]
fill.v <- brewer.pal(10, "Paired")[c(7,5,3)]
linetype.v <- c(3,4)
label.v <- list("a_Baisc"="Basic",
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








### Model PSI - from 01-01-2022 to 31-12-2022
# install.packages("PDtoolkit")
# require(PDtoolkit)

# dates <- unique(datCredit_valid[Date>"2021-12-31",Date])
# vars <- c("Balance", "PerfSpell_Num", "Principal", "Term", "TimeInPerfSpell", "slc_pmnt_method")
# datPlot <- data.table(Variable=rep(vars,length(dates)), PSI=0) %>% arrange(Variable)
# datPlot[, Date:=rep(dates,length(vars))]
# for (i in 1:length(vars)){
#   for (j in 1:length(dates)) {
#     PSI <- psi(base=datCredit_train[Date=="2021-12-31", ][[vars[i]]],
#                target=datCredit_valid[Date==dates[j],][[vars[i]]], bin=10, alpha=0.05)$res[3]
#     datPlot[12*(i-1)+j, ]$PSI <- PSI
#   }
# }
# datPlot[Variable=="slc_pmnt_method",][7:12,]$PSI <- c(0.0045,0.0056,0.0034,0.0087,0.0084,0.0075)

# ggplot(datPlot, aes(x=Date, y=PSI, group=Variable)) + geom_line(aes(colour=Variable))




### Actual vs Expected Default Rates
# using a cutoff of cut, calculate sensitivity, specificity, and classification rate
# perf = function(cut, mod, y)
# {
#   yhat = (mod$fit>cut)
#   w = which(y==1)
#   sensitivity = mean( yhat[w] == 1 ) 
#   specificity = mean( yhat[-w] == 0 ) 
#   c.rate = mean( y==yhat ) 
#   d = cbind(sensitivity,specificity)-c(1,1)
#   d = sqrt( d[1]^2 + d[2]^2 ) 
#   out = t(as.matrix(c(sensitivity, specificity, c.rate,d)))
#   colnames(out) = c("sensitivity", "specificity", "c.rate", "distance")
#   return(out)
# }

# Model 1
# s = seq(.01,.99,length=1000)
# OUT = matrix(0,1000,4)
# for(i in 1:1000) OUT[i,]=perf(s[i],logitMod_2,datCredit_valid$DefaultStatus1_lead_12_max)
# plot(s,OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
# axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
# axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
# lines(s,OUT[,2],col="darkgreen",lwd=2)
# lines(s,OUT[,3],col=4,lwd=2)
# lines(s,OUT[,4],col="darkred",lwd=2)
# box()
# legend(0,.25,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Classification Rate","Distance"))
# Choose 0.065

# Model 2
# s = seq(.01,.99,length=1000)
# OUT = matrix(0,1000,4)
# for(i in 1:1000) OUT[i,]=perf(s[i],logitMod_3,datCredit_valid$DefaultStatus1_lead_12_max)
# plot(s,OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
# axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
# axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
# lines(s,OUT[,2],col="darkgreen",lwd=2)
# lines(s,OUT[,3],col=4,lwd=2)
# lines(s,OUT[,4],col="darkred",lwd=2)
# box()
# legend(0,.25,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Classification Rate","Distance"))

# Plotting data
# targetVar1<-"prob_2"
# targetVar2<-"prob_3"

# datGraph <- rbind(datCredit_valid[Date>"2014-12-31", list(Time=get(timeVar), Status=get(currStatusVar), Target=get(targetVar), Sample = "a_Valid_Act")],
#                   datCredit_valid[Date>"2014-12-31", list(Time=get(timeVar), Status=get(currStatusVar), Target=get(targetVar1)>0.045, Sample = "b_Valid_Exp1")],
#                   datCredit_valid[Date>"2014-12-31", list(Time=get(timeVar), Status=get(currStatusVar), Target=get(targetVar1)>0.065, Sample = "c_Valid_Exp2")])

# - Setting some aggregation parameters, purely to facilitate graphing aesthetics
# def_StartDte <- min(datCredit[,get(timeVar)], na.rm=T)
# def_EndDte <- max(datCredit[,get(timeVar)], na.rm=T)
# maxDate <- def_EndDte - year(1) # A post-hoc filter, used for graphing purposes, given a 12-month outcome window

# - Aggregate to monthly level and observe up to given point
# port.aggr <- datGraph[Status==0, list(EventRate = sum(Target, na.rm=T)/.N),
#                       by=list(Sample, Time)][Time >= def_StartDte & Time <= maxDate,] %>% setkey(Time)

# - Aesthetics engineering
# port.aggr[, Facet_label := "Worst-ever aggregation approach"]

# - calculate TTC event rate and confidence interval for one sample, dichotomous outcome (population proportion)
# mean_EventRate <- port.aggr[Sample == "a_Valid_Act", mean(EventRate, na.rm=T)]
# stdError_EventRate <- port.aggr[Sample == "a_Valid_Act", sd(EventRate, na.rm=T)] / sqrt(port.aggr[Sample == "a_Valid_Act", .N])
# margin_EventRate <- qnorm(1-(1-confLevel)/2) * stdError_EventRate
# cat("\nMean event rate with 95% confidence intervals in training sample: ", sprintf("%.2f", mean_EventRate*100) , "% +-", sprintf("%.3f", margin_EventRate*100), "%")

# - Calculate MAE over time by sample
# port.aggr2 <- port.aggr %>% pivot_wider(id_cols = c(Time), names_from = c(Sample), values_from = c(EventRate))
# (diag.samplingRep.act <- mean(abs(port.aggr2$a_Valid_Act - port.aggr2$b_Valid_Exp1)) * 100)
# (diag.samplingRep.exp1 <- mean(abs(port.aggr2$a_Valid_Act - port.aggr2$c_Valid_Exp2)) * 100)
# (diag.samplingRep.epx1exp2 <- mean(abs(port.aggr2$b_Valid_Exp1 - port.aggr2$c_Valid_Exp2)) * 100)
### RESULTS: Sample-size dependent
# 100k-sample: Train: 0.57%; Validation: 0.87%
# 1m-sample: Train: 0.18%; Validation: 0.28%
# 1.5m-sample: Train: 0.16%; Validation: 0.22%
# 2m-sample: Train: 0.12%; Validation: 0.2%
# 4m-sample: Train: 0.08%; Validation: 0.14%

# - Graphing parameters
# chosenFont <- "Cambria"; dpi <- 170
# col.v <- brewer.pal(9, "Set1")[c(1,5,2,4)]; size.v <- c(0.5,0.3,0.3,0.3)
# label.v <- c("a_Valid_Act"=expression(italic(A)[t]*": Actuals "*italic(D)),
#              "b_Valid_Exp1"=bquote(italic(B)[t]*": Expected 1 "*italic(D)[italic(T)]~"("*.(round(train_prop*smp_size/1000))*"k)"),
#              "c_Valid_Exp2"=bquote(italic(C)[t]*": Expected 2 "*italic(D)[italic(V)]~"("*.(round((1-train_prop)*smp_size/1000))*"k)"))

# - Create graph 1 (all sets)
# a<-as.Date("2020-12-31")
# (g2 <- ggplot(port.aggr[Sample %in% c("a_Valid_Act", "b_Valid_Exp1")], aes(x=Time, y=EventRate, group=Sample)) + theme_minimal() + 
#     labs(x="Reporting date (months)", y=bquote("Conditional 12-month default rate (%) across sample "*italic(bar(D)))) + 
#     theme(text=element_text(family=chosenFont),legend.position = "bottom",
#           axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
#           strip.background=element_rect(fill="snow2", colour="snow2"),
#           strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
#    # main line graph with overlaid points
#     geom_line(aes(colour=Sample, linetype=Sample, linewidth=Sample)) + 
#     geom_vline(xintercept=as.numeric(a), linetype=4) +
#     geom_point(aes(colour=Sample, shape=Sample), size=1) + 
#     #annotations
#     annotate("text", x=as.Date("2018-12-31"), y=port.aggr[Time > "2014-12-31", mean(EventRate)]*1.15, size=3, family=chosenFont,
#              label=paste0("'TTC-mean '*E(italic(B[t]))*': ", sprintf("%1.2f", mean_EventRate*100), "% ± ", 
#                           sprintf("%.2f", margin_EventRate*100),"%'"), parse=T) +     
#     annotate(geom="text", x=as.Date("2018-12-31"), y=port.aggr[Time > "2014-12-31", mean(EventRate)]*1.05,
#              label=paste0("'MAE between '*italic(A)[t]*' and '*italic(B)[t]*': ", sprintf("%.2f", diag.samplingRep.act),"%'"),
#              family=chosenFont, size=3, parse=T) +     
#     annotate(geom="text", x=as.Date("2018-12-31"), y=port.aggr[Time > "2014-12-31", mean(EventRate)]*1,
#              label=paste0("'MAE between '*italic(A)[t]*' and '*italic(C)[t]*': ", sprintf("%.2f", diag.samplingRep.exp1),"%'"),
#              family=chosenFont, size=3, parse=T) +      
#     annotate(geom="text", x=as.Date("2018-12-31"), y=port.aggr[Time > "2014-12-31", mean(EventRate)]*0.95,
#              label=paste0("'MAE between '*italic(B)[t]*' and '*italic(C)[t]*': ", sprintf("%.2f", diag.samplingRep.epx1exp2),"%'"),
#              family=chosenFont, size=3, parse=T) +     
#     # facets & scale options
#     facet_grid(Facet_label ~ .) + 
#     scale_colour_manual(name=bquote("Sample "*italic(bar(D))), values=col.v, labels=label.v) + 
#     scale_linewidth_manual(name=bquote("Sample "*italic(bar(D))), values=size.v, labels=label.v) + 
#     scale_shape_discrete(name=bquote("Sample "*italic(bar(D))), labels=label.v) + scale_linetype_discrete(name=bquote("Sample "*italic(bar(D))), labels=label.v) + 
#     scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
#     scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))



