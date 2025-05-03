# ========================== MODEL DEFAULT RISK - ROC ANALYSIS ILLUSTRATION =================================
# Showcasing the use of ROC curves in evaluating the predictive power of logit models.
# -----------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Marcel Muller, Roland Breedt, Dr Arno Botha

# DESCRIPTION:
# This script uses the previously selected variables in fitting different logit models according to their
# level of complexity. ROC analysis are conducted on the models and the results are overlaid as to produce
# a single graph. This graph is itself used within the binary classification standard.
# -----------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup
#   - 0a.CustomFunctions
#   - 3b.Data_Subsample_Fusion2
#   - 3c(i).Model_DefaultRisk_Basic
#   - 3c(ii).Model_DefaultRisk_Intermediate
#   - 3c(iii).Model_DefaultRisk_Advanced
#
# -- Inputs:
#   - datCredit_train | Prepared credit data from script 3b
#   - datCredit_valid | Prepared credit data from script 3b
#   - Basic_Com_Formula | Model formula for basic PD-model
#   - Int_Formula | Model formula for intermediate PD-model
#   - Adv_Formula | Model formula for advanced PD-model
#
# -- Outputs:
#   - <analytics> | Graphs showing various model-level diagnostics
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
# Basic model
unpack.ffdf(paste0(genObjPath, "Basic_Com_Formula"), tempPath)
# Intermediate model
unpack.ffdf(paste0(genObjPath, "Int_Formula"), tempPath)
# Advanced model
unpack.ffdf(paste0(genObjPath, "Adv_Formula"), tempPath)





# ------ 2. Model fitting and probability scoring 

# --- 2.1 Fitting the models
# - Basic model
logitMod_Basic <- glm(inputs_bas, data=datCredit_train, family="binomial")
# - Intermediate model
logitMod_Int <- glm(inputs_int, data=datCredit_train, family="binomial")
# - Advanced model
logitMod_Adv <- glm(inputs_adv, data=datCredit_train, family="binomial")


# -- 2.2 Probability scoring
# - Training Set
datCredit_train[, prob_basic := predict(logitMod_Basic, newdata = datCredit_train, type="response")]
datCredit_train[, prob_int := predict(logitMod_Int, newdata = datCredit_train, type="response")]
datCredit_train[, prob_adv := predict(logitMod_Adv, newdata = datCredit_train, type="response")]

# - Validation Set
datCredit_valid[, prob_basic := predict(logitMod_Basic, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_int := predict(logitMod_Int, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_adv := predict(logitMod_Adv, newdata = datCredit_valid, type="response")]

# [SANITY CHECKS] Check for no missingness in probability scores in validation set
cat((anyNA(datCredit_valid[,prob_basic])) %?% "WARNING: Missingness detected in predicted probabilities of the Validation Set.\n" %:%
      "SAFE: No missingness in predicted probabilities.\n")
cat((anyNA(datCredit_valid[,prob_int])) %?% "WARNING: Missingness detected in predicted probabilities of the Validation Set.\n" %:%
      "SAFE: No missingness in predicted probabilities.\n")
cat((anyNA(datCredit_valid[,prob_adv])) %?% "WARNING: Missingness detected in predicted probabilities of the Validation Set.\n" %:%
      "SAFE: No missingness in predicted probabilities.\n")


# --- 2.3 Create ROC objects
alpha <- 0.05 # Set confidence interval level

# - Basic model
roc_obj_basic <- pROC::roc(response=datCredit_valid$DefaultStatus1_lead_12_max, predictor=datCredit_valid$prob_basic, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
cat(paste0("AUC: ", percent(as.numeric(roc_obj_basic$auc)/100, accuracy=0.01), " ± ", sprintf("%.2f",(roc_obj_basic$ci[3]-roc_obj_basic$ci[1])/2),"%"))
### RESULTS: 69.96% +- 0.45%

# - Intermediate model
roc_obj_int <- roc(response=datCredit_valid$DefaultStatus1_lead_12_max, predictor=datCredit_valid$prob_int, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
cat(paste0("AUC: ", percent(as.numeric(roc_obj_int$auc)/100, accuracy=0.01), " ± ", sprintf("%.2f",(roc_obj_int$ci[3]-roc_obj_int$ci[1])/2),"%"))
### RESULTS: 77.6% +- 0.50%

# - Advanced model
roc_obj_adv <- roc(response=datCredit_valid$DefaultStatus1_lead_12_max, predictor=datCredit_valid$prob_adv, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
cat(paste0("AUC: ", percent(as.numeric(roc_obj_adv$auc)/100, accuracy=0.01), " ± ", sprintf("%.2f",(roc_obj_adv$ci[3]-roc_obj_adv$ci[1])/2),"%"))
### RESULTS: 90.02% +- 0.29%
### CONCLUSION: Use the advanced model as it has strongest predictive power




# ------ 3. Model-level diagnostics and associated analytics

# --- 3.1 Coefficient of determination | Pseudo R^2-measures

# - Computing the coefficient of determination
# Basic model
(coefDeter_Basic <- coefDeter_glm(logitMod_Basic))
### RESULTS: McFadden = 5.85%
###          Cox Snell = 1.63%
###          Nagelkerke = 6.65%
# Intermediate model
(coefDeter_Int <- coefDeter_glm(logitMod_Int))
### RESULTS: McFadden = 21.30%
###          Cox Snell = 5.80%
###          Nagelkerke = 23.71%
# Advanced model
(coefDeter_Adv <- coefDeter_glm(logitMod_Adv))
### RESULTS: McFadden = 32.71%
###          Cox Snell = 8.76%
###          Nagelkerke = 35.84%

# - Create a single table containing the three R^2 measures for each of the models
(PseudoR2_Table<-data.table(Model=c("Basic","Intermediate","Advance"),CoxSnell=c(coefDeter_Basic$CoxSnell,coefDeter_Int$CoxSnell,coefDeter_Adv$CoxSnell),McFadden=c(coefDeter_Basic$McFadden,coefDeter_Int$McFadden,coefDeter_Adv$McFadden),
                            Nagelkerke=c(coefDeter_Basic$Nagelkerke,coefDeter_Int$Nagelkerke,coefDeter_Adv$Nagelkerke)))

# - Save table to specified path
pack.ffdf(paste0(genObjPath,"PseudoR2_Table"), PseudoR2_Table)


# -- Graph these measures together
# - Create a dataset to feed into ggplot2 (also change character R^2 values to numeric)
datPlot<-data.table(Statistic=rep(c("McFadden", "Nagelkerke"),
                                     each=3),Model=rep(c("a_Basic","b_Intermediate", "c_Advanced"),times=2),Value=
                         as.numeric(sub("%","",c(PseudoR2_Table$McFadden,PseudoR2_Table$Nagelkerke)))/100)

# - Aesthetic engineering
datPlot[, Label:=paste0(sprintf("%.2f", Value*100),"%")]

# - Plotting parameters
chosenFont <- "Cambria"; dpi<-180
vCol1 <- c("a_Basic"=brewer.pal(9, "BuGn")[5], "b_Intermediate"=brewer.pal(9, "BuGn")[7], "c_Advanced"=brewer.pal(9, "BuGn")[9])
vCol2 <- rep(c(vCol1[1],vCol1[2],vCol1[3]),2)
vCol3 <- rep("white", 3*2)
vLabel <- list("a_Basic"="Basic", "b_Intermediate"="Intermediate", "c_Advanced"="Advanced")

# - Create the plot
(gPlot<-ggplot(datPlot, aes(group=Model, y=Value, x=Statistic)) + 
    theme_minimal() + theme(legend.position = "bottom", text=element_text(family=chosenFont), axis.title.x = element_text(margin = margin(t = 5))) + labs(x=bquote("Pseudo"~italic(R^{2})*"-measure"), y="Value", family=chosenFont) +
    geom_col(aes(colour=Model, fill=Model), position="dodge") +
    geom_label(aes(label=Label), fill = vCol2, colour = vCol3, position=position_dodge(0.90), size=2.75,label.padding = unit(0.15, "lines")) +
    scale_colour_manual(name="Model:", values=vCol1, labels=vLabel) +
    scale_fill_manual(name="Model:", values=vCol1, labels=vLabel) +
    scale_x_discrete(labels=c("McFadden"="McFadden","Nagelkerke"="Nagelkerke")) +
    scale_y_continuous(limits = c(0, 0.4), breaks = seq(0, 1, by = 0.1),label=percent))

# Saving the graph to specified path
ggsave(gPlot, file=paste0(genFigPath, "R2Plot_V2.png"), width=1200/dpi, height=1000/dpi, dpi=400, bg="white")

# - Cleanup
rm(datPlot, gPlot, PseudoR2_Table)



# --- 3.2 Plotting the ROC curves

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
chosenFont <- "Cambria"; dpi<-200
vCol1 <- brewer.pal(10, "Paired")[c(8,6,4)]
vFill <- brewer.pal(10, "Paired")[c(7,5,3)]
vLineType <- c(3,4)
vLabel <- list("a_Basic"="Basic",
                "b_Intermediate"="Intermediate",
                "c_Advanced"="Advanced")

# - Create main ROC graph by overlaying competing ROC-curves
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
    scale_colour_manual(name="Model", values=vCol1, label=vLabel) +
    scale_linetype_manual(name="Model", values=vLineType, label=vLabel) +
    scale_x_continuous(breaks=pretty_breaks(), label=percent) +
    scale_y_continuous(breaks=pretty_breaks(), label=percent))

# - Saving the graph
ggsave(g_ROC_compar, file=paste0(genFigPath, "ROC_Curves_Comparison.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Cleanup
rm(datPlot_ROC, dat_anno, g_ROC_compar)



# --- 3.3 Plotting model diagnostics: AUC vs pseudo R^2-measures

# - Creating the plotting dataset
datPlot_diag <- rbind(data.table(Statistic=c("Coef_Deter", "AUC"),
                                 Value = c(as.numeric(sub("%","",coefDeter_Basic[[1]])), roc_obj_basic$auc),
                                 Model=rep("a_Basic",2)),
                      data.table(Statistic=c("Coef_Deter","AUC"),
                                 Value = c(as.numeric(sub("%","",coefDeter_Int[[1]])), roc_obj_int$auc),
                                 Model=rep("b_Intermediate",2)),
                      data.table(Statistic=c("Coef_Deter","AUC"),
                                 Value = c(as.numeric(sub("%","",coefDeter_Adv[[1]])),roc_obj_adv$auc),
                                 Model=rep("c_Advanced",2)))
datPlot_diag[, Label:=paste0(as.character(sprintf("%.2f", Value)),"%")]
datPlot_diag[Statistic=="AUC" & Model=="a_Basic",Label:=paste0(sprintf("%.2f", Value),"% ± ", sprintf("%.2f", (roc_obj_basic$ci[3]-roc_obj_basic$ci[1])/2), "%")]
datPlot_diag[Statistic=="AUC" & Model=="b_Intermediate",Label:=paste0(sprintf("%.2f", Value),"% ± ", sprintf("%.2f", (roc_obj_int$ci[3]-roc_obj_int$ci[1])/2), "%")]
datPlot_diag[Statistic=="AUC" & Model=="c_Advanced",Label:=paste0(sprintf("%.2f", Value),"% ± ", sprintf("%.2f", (roc_obj_adv$ci[3]-roc_obj_adv$ci[1])/2), "%")]

# - Plotting parameters
chosenFont <- "Cambria"; dpi<-180
vCol1 <- c("a_Basic"=brewer.pal(9, "Blues")[4], "b_Intermediate"=brewer.pal(9, "Blues")[7], "c_Advanced"=brewer.pal(9, "Blues")[9])
vCol2 <- c(rep(vCol1[1],2), rep(vCol1[2],2), rep(vCol1[3],2))
vCol3 <- rep("white", 3*2)
vLineType <- c(3,4)
vLabel <- list("a_Basic"="Basic",
                "b_Intermediate"="Intermediate",
                "c_Advanced"="Advanced")

# - Creating the clustered column chart
(g_model_diag_compar <- ggplot(datPlot_diag, aes(x=Statistic, y=Value, group=Model)) +
    theme_minimal() + theme(legend.position = "bottom", text=element_text(family=chosenFont)) + labs(x="Statistic", y="Value") +
    geom_col(aes(colour=Model, fill=Model), position="dodge") +
    geom_label(aes(label=Label), fill = vCol2, colour = vCol3, position=position_dodge(0.9)) +
    scale_colour_manual(name="Model:", values=vCol1, labels=vLabel) +
    scale_fill_manual(name="Model:", values=vCol1, labels=vLabel) +
    scale_x_discrete(labels=c("AUC"="AUC","Coef_Deter"="Coeffcient of Determination")) +
    scale_y_continuous(breaks=pretty_breaks(), label=percent))

# - Saving the graph
ggsave(g_model_diag_compar, file=paste0(genFigPath, "Diagnostics_Comparison.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Clean up
rm(datPlot_diag, g_model_diag_compar,coefDeter_Adv, coefDeter_Basic, coefDeter_Int); gc()



# --- 3.4 Matthews Correlation Coefficient for measuring prediction accuracy
# NOTE: A robust alternative to the AUC, though MCC requires a cut-off for probabilistic classifiers

# -- Enumerate MCC-values by varying the cutoff
# - Set sequence of cutoffs
cutoff_seq<-seq(0,1, by = 0.0025)

# - Initialise data table to store results
datMCC_Basic<- data.table(Cutoff=cutoff_seq,Model="a_Bas",MCC=0)
datMCC_Int<- data.table(Cutoff=cutoff_seq,Model="b_Int",MCC=0)
datMCC_Adv<- data.table(Cutoff=cutoff_seq,Model="c_Adv",MCC=0)

# - Fill data table with MCC values
counter<-1
for(k in cutoff_seq){
  datMCC_Basic[counter,"MCC"]<-Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_basic,k)
  datMCC_Int[counter,"MCC"]<-Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_int,k)
  datMCC_Adv[counter,"MCC"]<-Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_adv,k)
  counter<-counter+1
}

# - Obtain highest MCCs for each of the models for annotation purposes
HighestMCC<-rbind(datMCC_Basic[which.max(as.matrix(datMCC_Basic[,"MCC"])),],datMCC_Int[which.max(as.matrix(datMCC_Int[,"MCC"])),],datMCC_Adv[which.max(as.matrix(datMCC_Adv[,"MCC"])),])
(MCC_B<-Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_basic,HighestMCC[1,Cutoff])) # MCC = 0.150
(MCC_I<-Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_int,HighestMCC[2,Cutoff]))   # MCC = 0.355
(MCC_A<-Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_adv,HighestMCC[3,Cutoff]))   # MCC = 0.399


# -- Graph MCC and scaled MCC across three candidate classifiers
# - Create graphing dataset
datPlot<-data.table(MCC_Version=rep(c(" ", "  "),each=3),Model=rep(c("a_Basic","b_Intermediate", "c_Advanced"),times=2),Value=round(c(MCC_B,MCC_I,MCC_A,(MCC_B+1)/2,(MCC_I+1)/2,(MCC_A+1)/2),3))
datPlot[, Label:=Value]

# - Set aesthetic parameters
dpi <- 200
vCol1 <- brewer.pal(9, "Reds")[c(6,8,9)]
vCol3 <- rep("white", 3*2)
vLabel <- c("a_Basic"=bquote("Basic ("*italic(p[c])*" = "*.(round(HighestMCC[1,Cutoff],3))*")"),
             "b_Intermediate"=bquote("Intermediate ("*italic(p[c])*" = "*.(round(HighestMCC[2,Cutoff],3))*")"),
             "c_Advanced"=bquote("Advanced ("*italic(p[c])*" = "*.(round(HighestMCC[3,Cutoff],3))*")"))

# - Create the plot
(gMCC<-ggplot(datPlot, aes(group=Model, y=Value, x=MCC_Version)) + 
    theme_minimal() + theme(legend.position = "bottom", text=element_text(family=chosenFont), axis.title.x = element_text(margin = margin(t = 5))) + labs(x="Matthews Correlation Coefficients", y="Value", family=chosenFont) +
    geom_col(aes(colour=Model, fill=Model), position="dodge") +
    facet_wrap(~MCC_Version, scales="free") +
    geom_label(aes(label=Label,fill=Model),colour="white", position=position_dodge(0.90), size=2.75,label.padding = unit(0.15, "lines"),show.legend = F) +
    scale_colour_manual(name="Model:", values=vCol1, labels=vLabel) +
    scale_fill_manual(name="Model:", values=vCol1, labels=vLabel) +
    scale_x_discrete(labels=c(" "=bquote("MCC "*rho[M]),"  "=bquote("Scaled MCC "*rho*"'"[M]))))

# Saving the graph to specified path
ggsave(gMCC, file=paste0(genFigPath, "MCC_Scaled_vs_Unscaled.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")


# -- Comparison to discrete AUC, having imposed the same cut-offs

# - Dichotomise classifiers
datCredit_valid[, pred_basic := ifelse(prob_basic > HighestMCC[Model=="a_Bas", Cutoff], 1, 0)]
datCredit_valid[, pred_int := ifelse(prob_int > HighestMCC[Model=="b_Int", Cutoff], 1, 0)]
datCredit_valid[, pred_adv := ifelse(prob_adv > HighestMCC[Model=="c_Adv", Cutoff], 1, 0)]

# - Create pROC objects
roc_obj_basic_disc <- pROC::roc(response=datCredit_valid$DefaultStatus1_lead_12_max, predictor=datCredit_valid$pred_basic, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
roc_obj_int_disc <- pROC::roc(response=datCredit_valid$DefaultStatus1_lead_12_max, predictor=datCredit_valid$pred_int, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
roc_obj_adv_disc <- pROC::roc(response=datCredit_valid$DefaultStatus1_lead_12_max, predictor=datCredit_valid$pred_adv, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)

# - Discrete AUC results
roc_obj_basic_disc$auc; paste0(sprintf("%.2f",(roc_obj_basic_disc$ci[3]-roc_obj_basic_disc$ci[1])/2),"%")
### RESULTS: 58.24% (original AUC: 69.96%) +- 0.34%
roc_obj_int_disc$auc; paste0(sprintf("%.2f",(roc_obj_int_disc$ci[3]-roc_obj_int_disc$ci[1])/2),"%")
### RESULTS: 73.84% (original AUC: 77.6%) +- 0.42%
roc_obj_adv_disc$auc; paste0(sprintf("%.2f",(roc_obj_adv_disc$ci[3]-roc_obj_adv_disc$ci[1])/2),"%")
### RESULTS: 70.2% (original AUC: 90.02%) +- 0.29%

# - Compare improvement in AUC & MCC from basic to intermediate
AUC_delta_BasInt <- (roc_obj_int_disc$auc / roc_obj_basic_disc$auc) - 1
MCC_delta_BasInt <- (MCC_I/ MCC_B) - 1
cat("NOTE: Improvement in discrete AUC of basic to intermediate classifier:", percent(AUC_delta_BasInt))
cat("NOTE: Improvement in MCC of basic to intermediate classifier:", percent(MCC_delta_BasInt))


# -- Graph MCC across all cut-offs for three candidate classifiers

# - Create gCombined MCC dataset
datMCC <- rbind(datMCC_Basic, datMCC_Int, datMCC_Adv)

# - Set graphing parameters
dpi <- 200
vLabel <- c("a_Bas"="Basic","b_Int"="Intermediate","c_Adv"="Advanced")
vLineType <- c("solid","solid","solid")
		  
# - Create annotation object
datAnnotate_max <- data.table(Set=c("a_Bas", "b_Int","c_Adv"),HighestMCC[,"Cutoff"], HighestMCC[,"MCC"],
                              Label=c(paste0("' '*rho[M]*' = ", as.character(round(HighestMCC[Model=="a_Bas","MCC"],3)),";  '*italic(p[c])*' = ",round(HighestMCC[1,Cutoff],3),"'"),
                                      paste0("' '*rho[M]*' = ", as.character(round(HighestMCC[Model=="b_Int","MCC"],3)),";  '*italic(p[c])*' = ",round(HighestMCC[2,Cutoff],3),"'"),
                                      paste0("' '*rho[M]*' = ", as.character(round(HighestMCC[Model=="c_Adv","MCC"],3)),";  '*italic(p[c])*' = ",round(HighestMCC[3,Cutoff],3),"'")))

# - Create graph
(gMCC_cutoffs <- ggplot(datMCC, aes(x=Cutoff, y=MCC)) + theme_minimal() +
  labs(x=bquote("Cut-off "*italic(p[c])), y=bquote("Matthews Correlation Coefficient "*rho[M]), family=chosenFont) + 
  theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-10, 0, 0, 0),
        axis.text.x=element_text(angle=90), 
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=11, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
  # main line graph with overlaid points
  geom_line(aes(colour=Model, linetype=Model), linewidth=0.6) +
																				   
  # annotations at max points
  geom_point(data=datAnnotate_max, aes(x=Cutoff, y=MCC, colour=Set), shape=17, size=2.5, show.legend=F) +
  geom_label(data=datAnnotate_max, aes(x=Cutoff, label=Label, y=MCC, colour=Set), show.legend = F, 
               nudge_y=0.015, nudge_x=0.06, size=2.5,label.padding = unit(0.2, "lines"), parse = T) +
  # facets & scale options
  scale_colour_manual(name="Model:", values=vCol1, labels=vLabel) + 
  #scale_shape_manual(name="Model:", values=vShape, labels=vLabel) + 
  scale_linetype_manual(name="Model:", values=vLineType, labels=vLabel) + 
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks()))

# Saving the graph to specified path
ggsave(gMCC_cutoffs, file=paste0(genFigPath, "MCC_Cut-Offs.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Clean up
rm(gMCC_cutoffs, datPlot, datMCC, datMCC_Adv, datMCC_Basic, datMCC_Int, gMCC, datAnnotate_max,
   HighestMCC,  roc_obj_adv_disc, roc_obj_basic_disc, roc_obj_int_disc); gc()




# ---  3.5 Generalised Youden Index
# Create a plot displaying time graphs of actual vs expected default rate as a function of the cost multiple (a)

# - Function to create the time graph given a particular TPR:TNR cost ratio a
DefRte_Plotter<-function(a, datGiven, modLogit, chosenFont="Cambria", fldTarget, fldProb){
  # - Testing conditions
  # a=1; datGiven=datCredit_smp; modLogit=modSICR_logit; fldTarget="SICR_target"; fldProb="Prob_account"
  
  # - Obtain Generalised Youden Index cut-off pc for the cost ratio a
  (Algo_Results<-Gen_Youd_Ind(modLogit, datGiven, fldTarget, a))
  pc<-Algo_Results$cutoff
  
  # - Dichotomise the probability scores according to the cutoff pc
  datCredit_valid[, class_vals := ifelse(get(fldProb) <= pc, 0,1)]
  
  # - Create plotting dataset
  ActRte_Dset <- datCredit_valid[,list(DefRate=mean(DefaultStatus1_lead_12_max,na.rm=T), Rate="Act"),by=list(Date)]
  ExpRte_Dset <- datCredit_valid[,list(DefRate=mean(class_vals,na.rm=T), Rate="Exp"),by=list(Date)]
  PlotSet <- rbind(ActRte_Dset,ExpRte_Dset)
  
  # - Creating annotation datasets for easier annotations
  dat_anno1 <- data.table(MAE = NULL, Label = paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*'"),
                          x = as.Date("2016-01-31"),
                          y = max(PlotSet$DefRate,na.rm=T)*0.65)
  
  dat_anno2 <- data.table(Label =  paste0("' Cut-off  '*italic(p[c])*' = ", as.character(round(pc,4)),"'"),
                          x = as.Date("2016-01-31"),
                          y = max(PlotSet$DefRate,na.rm=T)*0.6)
  
  # - MAE Calculation between actual and expected rate
  dat_anno1[1, MAE := mean(abs(PlotSet[Rate=="Act", DefRate] - PlotSet[Rate=="Exp", DefRate]), na.rm = T)]
  
  # - Making the label more readable
  dat_anno1[, Label := paste0(Label, " = ", sprintf("%.4f",MAE*100), "%'")]
  
  # - More graphing parameters
  dpi <- 180
  vCol1<-brewer.pal(9, "Set1")[c(2,1)]
  vShape <- c(18,20) 
  vLineType <- c("dashed","solid")
  
  vLabel <- c("Act"=bquote(italic(A)[t]*": Actual"),
              "Exp"=bquote(italic(B)[t]*": Expected"))
  
  # - Aesthetics engineering
  PlotSet[, Facet_label := paste0("' Cost ratio  '*italic(a)*' = ", as.character(a),"'")]
  
  # - Actual Plot
  gPlot <- ggplot(PlotSet, aes(x=Date, y=DefRate)) + 
    theme_minimal() +
    labs(x="Calendar date (months)", y=bquote(" Conditional 12-month default rate (%)"), family=chosenFont) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-10, 0, 0, 0),
          axis.text.x=element_text(angle=90), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=15, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
    # Main line graph with overlaid points
    geom_line(aes(colour=Rate, linetype=Rate), linewidth=0.6) +
    geom_point(aes(colour=Rate, shape=Rate),size=1.7) + 
    # Facet and Scale options
    facet_grid(Facet_label ~ .,labeller = label_parsed) + 
    geom_text(data=dat_anno1, aes(x=x, y=y, label = Label), family=chosenFont, size=4.5, parse=T) +
    geom_text(data=dat_anno2, aes(x=x, y=y, label = Label), family=chosenFont, size=4.5, parse=T) +
    scale_colour_manual(name=bquote("Event Rate: "), values=vCol1, labels=vLabel) + 
    scale_shape_manual(name=bquote("Event Rate: "), values=vShape, labels=vLabel) + 
    scale_linetype_manual(name=bquote("Event Rate: "), values=vLineType, labels=vLabel) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
    scale_y_continuous(breaks=pretty_breaks(), label=percent)
  
  # - Return plot 
  return(gPlot)
}

# - Call function for various "a" values
(a_1<-DefRte_Plotter(a=1, datGiven=datCredit_train, modLogit=logitMod_Adv, fldTarget="DefaultStatus1_lead_12_max", fldProb="prob_adv"))
(a_3<-DefRte_Plotter(a=3, datGiven=datCredit_train, modLogit=logitMod_Adv, fldTarget="DefaultStatus1_lead_12_max", fldProb="prob_adv"))
(a_4<-DefRte_Plotter(a=4, datGiven=datCredit_train, modLogit=logitMod_Adv, fldTarget="DefaultStatus1_lead_12_max", fldProb="prob_adv"))
(a_6<-DefRte_Plotter(a=6, datGiven=datCredit_train, modLogit=logitMod_Adv, fldTarget="DefaultStatus1_lead_12_max", fldProb="prob_adv"))

# - Bind graphs together
(gCombined<-grid.arrange(a_1, a_3 ,a_4 ,a_6 ,ncol=2))

# Saving the graph to a specified path
dpi <- 180
ggsave(gCombined, file=paste0(genFigPath, "ACTvsEXP_DefRate.png"), width=2200/dpi, height=1800/dpi, dpi=dpi, bg="white")

# - Cleanup
rm(a_1, a_3,a_4,a_6, gCombined)



# ---  3.6 Comparison of probability score densities by class

# - Dichotomise probability scores using Generalised Youden Index
cutoff_basic <- Gen_Youd_Ind(logitMod_Basic, datCredit_train, "DefaultStatus1_lead_12_max", a=4)
cutoff_int <- Gen_Youd_Ind(logitMod_Int, datCredit_train, "DefaultStatus1_lead_12_max", a=4)
cutoff_adv <- Gen_Youd_Ind(logitMod_Adv, datCredit_train, "DefaultStatus1_lead_12_max", a=4)

# - Obtain prevalences and divergence measures
phi <- mean(datCredit_valid$DefaultStatus1_lead_12_max) # actual prevalence
div_basic <- divergences_binary(datCredit_valid, Target="DefaultStatus1_lead_12_max", Prediction="prob_basic", cutOff=cutoff_basic$cutoff)
div_int <- divergences_binary(datCredit_valid, Target="DefaultStatus1_lead_12_max", Prediction="prob_int", cutOff=cutoff_basic$cutoff)
div_adv <- divergences_binary(datCredit_valid, Target="DefaultStatus1_lead_12_max", Prediction="prob_adv", cutOff=cutoff_basic$cutoff)

# - Two-sample Kolmogorov-Smirnov test of score CDFs between class subpopulations
KS_results_basic <- KS_discimination(datCredit_valid[DefaultStatus1_lead_12_max==0, prob_basic],
                                     datCredit_valid[DefaultStatus1_lead_12_max==1, prob_basic])
KS_results_int <- KS_discimination(datCredit_valid[DefaultStatus1_lead_12_max==0, prob_int],
                                     datCredit_valid[DefaultStatus1_lead_12_max==1, prob_int])
KS_results_adv <- KS_discimination(datCredit_valid[DefaultStatus1_lead_12_max==0, prob_adv],
                                   datCredit_valid[DefaultStatus1_lead_12_max==1, prob_adv])


# --- Basic model
hist(datCredit_valid$prob_basic, breaks="FD")

# - Find extreme percentile of each class probability distribution for graphing purposes
percX <- quantile(datCredit_valid$prob_basic, 0.997)

# - Aesthetic engineering: Statistical Summaries
maxDensPhi_1 <- max(density(datCredit_valid[DefaultStatus1_lead_12_max==1, prob_basic])$y)
maxDensPhi_0 <- max(density(datCredit_valid[DefaultStatus1_lead_12_max==0, prob_basic])$y)

# - Aesthetic engineering: general
datCredit_valid[, Facet_label := "Basic PD-model"]

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 250
vCol1<-brewer.pal(8, "Dark2")[c(1,2)]
vFill<-brewer.pal(8, "Set2")[c(1,2)]
vLabel <- c(bquote(italic(C)[0]), 
              bquote(italic(C)[1]))
binNum <- round(2*datCredit_valid[,.N]^(1/3)*0.5) # using Rice's rule

# - Plot double density across both classes
(gPlot <- ggplot( data=datCredit_valid, aes(x=prob_basic)) + theme_bw() + 
  labs(x=bquote("Expected class probability "*italic(p[y](bold(x)))*" in "*italic(D[V])), 
       y="Histogram (density)") + 
  theme(legend.position="bottom", text=element_text(family=chosenFont),
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
  #stat_ecdf(geom="step",linewidth=0.3, aes(linetype=factor(DefaultStatus1_lead_12_max), 
   #                                        colour=factor(DefaultStatus1_lead_12_max))) + 
  geom_histogram(aes(y=after_stat(density), colour=factor(DefaultStatus1_lead_12_max), 
                     fill=factor(DefaultStatus1_lead_12_max)), alpha=0.5, bins=binNum, position="identity") + 
  #geom_density(aes(colour=factor(DefaultStatus1_lead_12_max), fill=factor(DefaultStatus1_lead_12_max)), 
  #             linewidth=0.3, alpha=0.4) + 
  # Annotations: actual prevalence
  geom_vline(xintercept=phi, linewidth=0.5, colour="black") + 
  annotate(geom="text", x=phi*0.82, y=max(maxDensPhi_1, maxDensPhi_0)*0.25, 
           label=paste0("'Actual prevalence '*phi[A]==",sprintf("%.3f", phi*100), "*'%'"),
           family=chosenFont, size=3, colour="black", angle=90, parse=T) +
  # Annotations: expected prevalences
  annotate(geom="text", x=phi*4.4, y=max(maxDensPhi_1, maxDensPhi_0)*0.6, 
           label=paste0("'Expected prevalence (overall) '*phi[E]*' = ",
                        percent(div_basic$Prevalence_Expected, accuracy=0.001),"'"),
           family=chosenFont, size=3, colour="black", parse=T) +    
  annotate(geom="text", x=phi*4.2, y=max(maxDensPhi_1, maxDensPhi_0)*0.55, 
       label=paste0("'Expected prevalence in '*italic(C)[1]*': '*phi[E1]*' = ",
                    percent(div_basic$Prevalence_Expected_1, accuracy=0.001),"'"),
       family=chosenFont, size=3, colour=vCol1[2], parse=T) +    
  annotate(geom="text", x=phi*4.2, y=max(maxDensPhi_1, maxDensPhi_0)*0.50, 
           label=paste0("'Expected prevalence in '*italic(C)[0]*': '*phi[E0]*' = ",
                        percent(div_basic$Prevalence_Expected_0, accuracy=0.001),"'"),
           family=chosenFont, size=3, colour=vCol1[1], parse=T) +    
  # Annotations: KS-test of discrimination
  annotate(geom="text", x=phi*4.5, y=max(maxDensPhi_1, maxDensPhi_0)*0.4, 
           label=paste0("'KS-statistic '*italic(K)*' = ",
                        percent(KS_results_basic$KS_statistic, accuracy=0.1),"; discriminiation level: ",
                        KS_results_basic$KS_discrimation, "'"),
           family=chosenFont, size=2.5, colour="black", parse=T) +   
  annotate(geom="text", x=phi*3.35, y=max(maxDensPhi_1, maxDensPhi_0)*0.37, 
           label=paste0("Two-sample ",KS_results_basic$KS_decision),
           family=chosenFont, size=2.5, colour="black") +   
  # facets & scale options
  facet_grid(Facet_label ~ .) + 
  scale_color_manual(name=bquote("Class "*italic(Y)), labels=vLabel, values=vCol1) + 
  scale_fill_manual(name=bquote("Class "*italic(Y)), labels=vLabel, values=vFill) + 
  scale_x_continuous(breaks=pretty_breaks(), label=percent, limits=c(0,percX)))

# Saving the graph to a specified path
ggsave(gPlot, file=paste0(genFigPath, "ProbScoreDensity_1Basic.png"), width=1100/dpi, height=1000/dpi, dpi=dpi, bg="white")



# --- Intermediate model
hist(datCredit_valid$prob_int, breaks="FD")

# - Find extreme percentile of each class probability distribution for graphing purposes
percX <- quantile(datCredit_valid$prob_int, 0.999)

# - Aesthetic engineering: general
datCredit_valid[, Facet_label := "Intermediate PD-model"]

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 250
vCol1<-brewer.pal(8, "Dark2")[c(1,2)]
vFill<-brewer.pal(8, "Set2")[c(1,2)]
vLabel <- c(bquote(italic(C)[0]), 
             bquote(italic(C)[1]))
binNum <- 32 # manually tweaked given extreme bimodality in the distributions

# - Plot double density across both classes
(gPlot <- ggplot( data=datCredit_valid, aes(x=prob_int)) + theme_bw() + 
    labs(x=bquote("Expected class probability "*italic(p[y](bold(x)))*" in "*italic(D[V])), 
         y="Histogram (density)") + 
    theme(legend.position="bottom", text=element_text(family=chosenFont),
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    geom_histogram(aes(y=after_stat(density), colour=factor(DefaultStatus1_lead_12_max), 
                       fill=factor(DefaultStatus1_lead_12_max)), alpha=0.5, bins=binNum, position="identity") + 
    #geom_density(aes(colour=factor(DefaultStatus1_lead_12_max), fill=factor(DefaultStatus1_lead_12_max)), 
                 #linewidth=0.3, alpha=0.4, position="identity", trim=F) + 
    # Annotations: actual prevalence
    annotate(geom="text", x=phi*1.7, y=12, 
             label=paste0("'Actual prevalence '*phi[A]==",sprintf("%.3f", phi*100), "*'%'"),
             family=chosenFont, size=3, colour="black", angle=90, parse=T) +
    # Annotations: expected prevalences
    annotate(geom="text", x=0.4, y=28, 
             label=paste0("'Expected prevalence (overall) '*phi[E]*' = ",
                          percent(div_int$Prevalence_Expected, accuracy=0.001),"'"),
             family=chosenFont, size=3, colour="black", parse=T) +    
    annotate(geom="text", x=0.395, y=26, 
             label=paste0("'Expected prevalence in '*italic(C)[1]*': '*phi[E1]*' = ",
                          percent(div_int$Prevalence_Expected_1, accuracy=0.001),"'"),
             family=chosenFont, size=3, colour=vCol1[2], parse=T) +    
    annotate(geom="text", x=0.4, y=24, 
             label=paste0("'Expected prevalence in '*italic(C)[0]*': '*iphi[E0]*' = ",
                          percent(div_int$Prevalence_Expected_0, accuracy=0.001),"'"),
             family=chosenFont, size=3, colour=vCol1[1], parse=T) +    
    # Annotations: KS-test of discrimination
    annotate(geom="text", x=0.38, y=20, 
             label=paste0("'KS-statistic '*italic(K)*' = ",
                          percent(KS_results_int$KS_statistic, accuracy=0.1),"; discriminiation level: ",
                          KS_results_int$KS_discrimation, "'"),
             family=chosenFont, size=2.5, colour="black", parse=T) +   
    annotate(geom="text", x=0.29, y=18, 
             label=paste0("Two-sample ",KS_results_int$KS_decision),
             family=chosenFont, size=2.5, colour="black") +      
    # facets & scale options
    facet_grid(Facet_label ~ .) + 
    geom_vline(xintercept=phi, linewidth=0.5, colour="black") + 
    scale_color_manual(name=bquote("Class "*italic(Y)), labels=vLabel, values=vCol1) + 
    scale_fill_manual(name=bquote("Class "*italic(Y)), labels=vLabel, values=vFill) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent))

# Saving the graph to a specified path
ggsave(gPlot, file=paste0(genFigPath, "ProbScoreDensity_2Intermediate.png"), width=1100/dpi, height=1000/dpi, dpi=dpi, bg="white")



# --- Advanced model
hist(datCredit_valid$prob_adv, breaks="FD")

# - Find extreme percentile of each class probability distribution for graphing purposes
percX <- quantile(datCredit_valid$prob_int, 0.999)

# - Aesthetic engineering: general
datCredit_valid[, Facet_label := "Advanced PD-model"]

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 250
vCol1<-brewer.pal(8, "Dark2")[c(1,2)]
vFill<-brewer.pal(8, "Set2")[c(1,2)]
vLabel <- c(bquote(italic(C)[0]), 
             bquote(italic(C)[1]))
binNum <- 36 # manually tweaked given extreme bimodality in the distributions

# - Plot double density across both classes
(gPlot <- ggplot( data=datCredit_valid, aes(x=prob_adv)) + theme_bw() + 
    labs(x=bquote("Expected class probability "*italic(p[y](bold(x)))*" in "*italic(D[V])), 
         y="Histogram (density)") + 
    theme(legend.position="bottom", text=element_text(family=chosenFont),
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    geom_histogram(aes(y=after_stat(density), colour=factor(DefaultStatus1_lead_12_max), 
                       fill=factor(DefaultStatus1_lead_12_max)), alpha=0.5, bins=binNum, position="identity") + 
    #geom_density(aes(colour=factor(DefaultStatus1_lead_12_max), fill=factor(DefaultStatus1_lead_12_max)), 
    #linewidth=0.3, alpha=0.4, position="identity", trim=F) + 
    # Annotations: actual prevalence
    annotate(geom="text", x=phi*1.7, y=14, 
             label=paste0("'Actual prevalence '*phi[A]==",sprintf("%.3f", phi*100), "*'%'"),
             family=chosenFont, size=3, colour="black", angle=90, parse=T) +
    # Annotations: expected prevalences
    annotate(geom="text", x=0.5, y=25, 
             label=paste0("'Expected prevalence (overall) '*phi[E]*' = ",
                          percent(div_adv$Prevalence_Expected, accuracy=0.001),"'"),
             family=chosenFont, size=3, colour="black", parse=T) +    
    annotate(geom="text", x=0.49, y=23, 
             label=paste0("'Expected prevalence in '*italic(C)[1]*': '*phi[E1]*' = ",
                          percent(div_adv$Prevalence_Expected_1, accuracy=0.001),"'"),
             family=chosenFont, size=3, colour=vCol1[2], parse=T) +    
    annotate(geom="text", x=0.48, y=21, 
             label=paste0("'Expected prevalence in '*italic(C)[0]*': '*phi[E0]*' = ",
                          percent(div_adv$Prevalence_Expected_0, accuracy=0.001),"'"),
             family=chosenFont, size=3, colour=vCol1[1], parse=T) +    
    # Annotations: KS-test of discrimination
    annotate(geom="text", x=0.48, y=17, 
             label=paste0("'KS-statistic '*italic(K)*' = ",
                          percent(KS_results_adv$KS_statistic, accuracy=0.1),"; discriminiation level: ",
                          KS_results_adv$KS_discrimation, "'"),
             family=chosenFont, size=2.5, colour="black", parse=T) +   
    annotate(geom="text", x=0.375, y=16, 
             label=paste0("Two-sample ",KS_results_int$KS_decision),
             family=chosenFont, size=2.5, colour="black") +      
    geom_vline(xintercept=phi, linewidth=0.5, colour="black") + 
    # facets & scale options
    facet_grid(Facet_label ~ .) + 
    scale_color_manual(name=bquote("Class "*italic(Y)), labels=vLabel, values=vCol1) + 
    scale_fill_manual(name=bquote("Class "*italic(Y)), labels=vLabel, values=vFill) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent))

# Saving the graph to a specified path
ggsave(gPlot, file=paste0(genFigPath, "ProbScoreDensity_3Advanced.png"), width=1100/dpi, height=1000/dpi, dpi=dpi, bg="white")


# - Section cleanup
rm(KS_results_basic, KS_results_int, KS_results_adv); gc()



# --- 3.7 Divergence/Information measures
# - Shannon Entropy
div_basic$ShannonEntropy
div_int$ShannonEntropy
div_adv$ShannonEntropy
### RESULTS: Shannon entropy is the same for all 3 classifiers (0.2040216), given that the classifiers are trained on the same data.

# Focusing on %-differences in cross-entropy when drawing inference
(CE_Improv_Int<-div_int$CrossEntropy/div_basic$CrossEntropy -1) # 17% decrease / "improvement" of intermediate over baseline/basic
(CE_Improv_Adv<-div_adv$CrossEntropy/div_basic$CrossEntropy -1) # 29% decrease / "improvement" of advance over baseline/basic

# Focusing on %-differences in KL-divergence when drawing inference
(KL_Improv_Int<-div_int$KullbackLeibler_divergence/div_basic$KullbackLeibler_divergence -1) # 48% decrease / "improvement" of intermediate over basic?
(KL_Improv_Adv<-div_adv$KullbackLeibler_divergence/div_basic$KullbackLeibler_divergence -1) # 94% decrease / "improvement" of intermediate over basic?

# - Create graphing dataset to visualize information measures
datPlotz<-data.table(Info_M=rep(c(" ", "  "),each=2),Model=rep(c("b_Intermediate", "c_Advanced"),times=2),Value=-1*c(CE_Improv_Int,CE_Improv_Adv,KL_Improv_Int,KL_Improv_Adv))
# - More adjustments before plotting
datPlotz[, Label := paste0(sprintf("%.2f",Value*100),"%")]

# - Set aesthetic parameters
dpi <- 200
vCol1 <- brewer.pal(9, "Purples")[c(5,7)]
vCol3 <- rep("white", 2*2)
vLabel <- c("b_Intermediate"=bquote("Intermediate vs Basic"),
            "c_Advanced"=bquote("Advanced vs Basic"))

# - Create the plot
(Info_M<-ggplot(datPlotz, aes(group=Model, y=Value, x=Info_M)) + 
    theme_minimal() + theme(legend.position = "bottom", text=element_text(family=chosenFont), axis.title.x = element_text(margin = margin(t = 5))) + labs(x="Information measure", y="Relative improvement (%)", family=chosenFont) +
    geom_col(aes(colour=Model, fill=Model), position="dodge") +
    # - Annotation
    annotate(geom="text", hjust=0, x=0.55, y=0.825, family=chosenFont, size=4, parse=T,
             label=paste0("'Shannon entropy '*italic(H(q))=='", sprintf("%#.3f", div_basic$ShannonEntropy),"'")) +
    annotate(geom="text", hjust=0, x=0.55, y=0.75, family=chosenFont, size=4, parse=T,
             label=paste0("'Basic model: Cross-entropy '*italic(H[q](p))=='", sprintf("%#.3f", div_basic$CrossEntropy),"'")) +
    annotate(geom="text", hjust=0, x=0.55, y=0.695, family=chosenFont, size=4, parse=T,
             label=paste0("'Intermediate model: Cross-entropy '*italic(H[q](p))=='", sprintf("%#.3f", div_int$CrossEntropy),"'")) +
    annotate(geom="text", hjust=0, x=0.55, y=0.64, family=chosenFont, size=4, parse=T,
             label=paste0("'Advanced model: Cross-entropy '*italic(H[q](p))=='", sprintf("%#.3f", div_adv$CrossEntropy),"'")) +
    # - General graph specifications
    geom_label(aes(label=Label,fill=Model),colour="white", position=position_dodge(0.90), size=2.75,label.padding = unit(0.15, "lines"),show.legend = F) +
    scale_colour_manual(name="Model:", values=vCol1, labels=vLabel) +
    scale_fill_manual(name="Model:", values=vCol1, labels=vLabel) +
    scale_x_discrete(labels=c(" "=bquote("Cross-entropy "*italic(H[q](p))),"  "=bquote("KL-divergence "*italic(D[q](p)))))+
    scale_y_continuous(breaks=pretty_breaks(),limits=c(0,1), label=percent))

# Saving the graph to specified path
ggsave(Info_M, file=paste0(genFigPath, "Information_Measures.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# --- Final Clean up
rm(logitMod_Basic, logitMod_Int, logitMod_Adv, datCredit_train, datCredit_valid, 
   cutoff_basic, cutoff_int, cutoff_adv, div_basic, div_int, div_adv,
   KS_results_basic, KS_results_int, KS_results_adv,
   roc_obj_basic, roc_obj_int, roc_obj_adv,
   vCol1, vCol2, vCol3, vLineType, vLabel, datPlot, gPlot, dat_anno1, Info_M, datPlotz); gc()
