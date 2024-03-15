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

# - Load in basic, intermediate, and advanced model formulas
# Basic model(s)
unpack.ffdf(paste0(genObjPath, "Basic_Com_Formula"), tempPath)
# Intermediate model(s)
unpack.ffdf(paste0(genObjPath, "Int_Formula"), tempPath)
# Advanced model(s)
unpack.ffdf(paste0(genObjPath, "Adv_Formula"), tempPath)





# ------ 2. Model comparison
# --- Fitting the models
# - Basic modle(s)
logitMod_Basic <- glm(inputs_fin_bas, data=datCredit_train, family="binomial")
# - Basic modle(s)
logitMod_Int <- glm(inputs_int, data=datCredit_train, family="binomial")
# - Advanced modle(s)
logitMod_Adv <- glm(inputs_adv, data=datCredit_train, family="binomial")

# --- Coefficient of determination
# - Basic model
(coefDeter_Basic <- coefDeter_glm(logitMod_Basic))
### RESULTS: 2.09%
# - Intermediate model
(coefDeter_Int <- coefDeter_glm(logitMod_Int))
### RESULTS: 21.95%
# - Advanced model
(coefDeter_Adv <- coefDeter_glm(logitMod_Adv))
### RESULTS: 34.16%

# --- ROC analysis
# - Set confidende interval level
alpha <- 0.05
# - Basic model
datCredit_valid[, prob_basic := predict(logitMod_Basic, newdata = datCredit_valid, type="response")]
roc_obj_basic <- pROC::roc(response=datCredit_valid$DefaultStatus1_lead_12_max, predictor=datCredit_valid$prob_basic, ci.method="bootstrap", ci=T, conf.level = 1-alpha, percent=T)
roc_obj_basic$auc; paste0(sprintf("%.2f",(roc_obj_basic$ci[3]-roc_obj_basic$ci[1])/2),"%")
### RESULTS: 63.22% +- 0.46%
# - Intermediate model
datCredit_valid[, prob_int := predict(logitMod_Int, newdata = datCredit_valid, type="response")]
roc_obj_int <- roc(response=datCredit_valid$DefaultStatus1_lead_12_max, predictor=datCredit_valid$prob_int, ci.method="bootstrap", ci=T, conf.level = 1-alpha, percent=T)
roc_obj_int$auc; paste0(sprintf("%.2f",(roc_obj_int$ci[3]-roc_obj_int$ci[1])/2),"%")
### RESULTS: 77.62% +- 48%
# - Advanced model
datCredit_valid[, prob_adv := predict(logitMod_Adv, newdata = datCredit_valid, type="response")]
roc_obj_adv <- roc(response=datCredit_valid$DefaultStatus1_lead_12_max, predictor=datCredit_valid$prob_adv, ci.method="bootstrap", ci=T, conf.level = 1-alpha, percent=T)
roc_obj_adv$auc; paste0(sprintf("%.2f",(roc_obj_adv$ci[3]-roc_obj_adv$ci[1])/2),"%")
### RESULTS: 90.04% +- 0.29%

### CONCLUSION: Use the basic model, the thematic intermediate model, and the thematic advanced model (the choice between the thematic- and full models are completely subjective as the predictive performance is near identical)


# --- Plotting model diagnostics
# - Creating the plotting dataset
datPlot_diag <- rbind(data.table(Statistic=c("Coef_Deter", "AUC"),
                                 Value = c(coefDeter_Basic, roc_obj_basic$auc),
                                 Model=rep("a_Basic",2)),
                      data.table(Statistic=c("Coef_Deter","AUC"),
                                 Value = c(coefDeter_Int, roc_obj_int$auc),
                                 Model=rep("b_Intermediate",2)),
                      data.table(Statistic=c("Coef_Deter","AUC"),
                                 Value = c(coefDeter_Adv,roc_obj_adv$auc),
                                 Model=rep("c_Advanced",2)))
datPlot_diag[, Label:=paste0(sprintf("%.2f", Value),"%")]
datPlot_diag[Statistic=="AUC" & Model=="a_Basic",Label:=paste0(sprintf("%.2f", Value),"% ± ", sprintf("%.2f", (roc_obj_basic$ci[3]-roc_obj_basic$ci[1])/2), "%")]
datPlot_diag[Statistic=="AUC" & Model=="b_Intermediate",Label:=paste0(sprintf("%.2f", Value),"% ± ", sprintf("%.2f", (roc_obj_int$ci[3]-roc_obj_int$ci[1])/2), "%")]
datPlot_diag[Statistic=="AUC" & Model=="c_Advanced",Label:=paste0(sprintf("%.2f", Value),"% ± ", sprintf("%.2f", (roc_obj_adv$ci[3]-roc_obj_adv$ci[1])/2), "%")]
# - Plotting parameters
chosenFont <- "Cambria"; dpi<-180
col.v <- c("a_Basic"=brewer.pal(9, "Blues")[4], "b_Intermediate"=brewer.pal(9, "Blues")[7], "c_Advanced"=brewer.pal(9, "Blues")[9])
col.v2 <- c(rep(col.v[1],2), rep(col.v[2],2), rep(col.v[3],2))
col.v3 <- rep("white", 3*2)
linetype.v <- c(3,4)
label.v <- list("a_Basic"="Basic",
                "b_Intermediate"="Intermediate",
                "c_Advanced"="Advanced")

# - Creating the clustered column chart
(g_model_diag_compar <- ggplot(datPlot_diag, aes(x=Statistic, y=Value, group=Model)) +
    theme_minimal() + theme(legend.position = "bottom", text=element_text(family=chosenFont)) + labs(x="Statistic", y="Value") +
    geom_col(aes(colour=Model, fill=Model), position="dodge") +
    geom_label(aes(label=Label), fill = col.v2, colour = col.v3, position=position_dodge(0.9)) +
    scale_colour_manual(name="Model:", values=col.v, labels=label.v) +
    scale_fill_manual(name="Model:", values=col.v, labels=label.v) +
    scale_x_discrete(labels=c("AUC"="AUC","Coef_Deter"="Coeffcient of Determination")) +
    scale_y_continuous(breaks=pretty_breaks(), label=percent))

# - Saving the graph
ggsave(g_model_diag_compar, file=paste0(genFigPath, "Diagnostics_Comparison.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Clean up
rm(col.v, col.v2, col.v3, linetype.v, label.v, datPlot_diag, g_model_diag_compar); gc()

# --- Plotting the ROC curves
# - Creating the plotting dataset
datPlot_ROC <- rbind(data.table(TPR=roc_obj_basic$sensitivities/100,
                                FPR=1-roc_obj_basic$specificities/100,
                                Model="a_Basic"),
                     data.table(TPR=roc_obj_int$sensitivities/100,
                                FPR=1-roc_obj_int$specificities/100,
                                Model="b_Intermediate"),
                     data.table(TPR=roc_obj_adv$sensitivities/100,
                                FPR=1-roc_obj_adv$specificities/100,
                                Model="c_Advanced"))
# - Getting the AUCs of each model (so that the values can be used as labels)
dat_anno <- data.table(Model=c("a_Basic", "b_Intermediate","c_Advanced"),
                       AUC=c(roc_obj_basic$auc, roc_obj_int$auc, roc_obj_adv$auc),
                       x=c(0.5,0.5,0.5), y=c(0.68,0.80, 0.95))
dat_anno[Model=="a_Basic",Label:=paste0("AUC=",sprintf("%.2f",AUC),"% ± ", sprintf("%.2f", (roc_obj_basic$ci[3]-roc_obj_basic$ci[1])/2), "%")]
dat_anno[Model=="b_Intermediate",Label:=paste0("AUC=",sprintf("%.2f",AUC),"% ± ", sprintf("%.2f", (roc_obj_int$ci[3]-roc_obj_int$ci[1])/2), "%")]
dat_anno[Model=="c_Advanced",Label:=paste0("AUC=",sprintf("%.2f",AUC),"% ± ", sprintf("%.2f", (roc_obj_adv$ci[3]-roc_obj_adv$ci[1])/2), "%")]
# - Plotting parameters
chosenFont <- "Cambria"; dpi<-180
col.v <- brewer.pal(10, "Paired")[c(8,6,4)]
fill.v <- brewer.pal(10, "Paired")[c(7,5,3)]
linetype.v <- c(3,4)
label.v <- list("a_Basic"="Basic",
                "b_Intermediate"="Intermediate",
                "c_Advanced"="Advanced")
# - Overlaying the ROC plots
(g_ROC_compar <- ggplot(datPlot_ROC) + theme_minimal() +
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
datCredit_valid[, `:=`(prob_basic=NULL, prob_int=NULL, prob_adv=NULL)]
rm(logitMod_Basic, logitMod_Int, logitMod_Adv, roc_obj_basic, roc_obj_int, roc_obj_adv,
   chosenFont, col.v, fill.v, linetype.v, label.v, datPlot_ROC, dat_anno, g_ROC_compar); gc()




























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



