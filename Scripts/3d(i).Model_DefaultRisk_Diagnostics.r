# ========================== MODEL DEFAULT RISK - ROC ANALYSIS ILLUSTRATION =================================
# Showcasing the use of ROC curves in evalutaing the predictive power of logit models.
# -----------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Marcel Muller, Roland Breedt

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
#   - 3c(ii).Model_DefaultRisk_Macro
#
# -- Inputs:
#   - datCredit_train | Prepared credit data from script 3b
#   - datCredit_valid | Prepared credit data from script 3b
#
# -- Outputs:
#   - Some graphs showcasing ROC analysis conducted with various logit models.
# ===========================================================================================================


### MM: Check for unnecessary duplication in the code...

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
# --- 2.1 Fitting the models
# - Basic model
logitMod_Basic <- glm(inputs_bas, data=datCredit_train, family="binomial")
# - Basic model
logitMod_Int <- glm(inputs_int, data=datCredit_train, family="binomial")
# - Advanced model
logitMod_Adv <- glm(inputs_adv, data=datCredit_train, family="binomial")


# --- 2.2 Coefficient of determination
# - Computing the coefficient of determination
# Basic model
(coefDeter_Basic <- coefDeter_glm(logitMod_Basic))
### RESULTS: McFadden = 5.76%
###          Cox Snell = 1.60%
###          Nagelkerke = 6.55%
# Intermediate model
(coefDeter_Int <- coefDeter_glm(logitMod_Int))
### RESULTS: McFadden = 21.30%
###          Cox Snell = 5.80%
###          Nagelkerke = 23.71%
# Advanced model
(coefDeter_Adv <- coefDeter_glm(logitMod_Adv))
### RESULTS: McFadden = 32.76%
###          Cox Snell = 8.78%
###          Nagelkerke = 35.89%

# - Create a single table containing the three R^2 measures for each of the models
(PseudoR2_Table<-data.table(Model=c("Basic","Intermediate","Advance"),CoxSnell=c(coefDeter_Basic$CoxSnell,coefDeter_Int$CoxSnell,coefDeter_Adv$CoxSnell),McFadden=c(coefDeter_Basic$McFadden,coefDeter_Int$McFadden,coefDeter_Adv$McFadden),
                            Nagelkerke=c(coefDeter_Basic$Nagelkerke,coefDeter_Int$Nagelkerke,coefDeter_Adv$Nagelkerke)))

# - Save table to specified path
pack.ffdf(paste0(genObjPath,"PseudoR2_Table"), PseudoR2_Table)

# - Creating a graph to visualy display these three measures
# Create a dataset to feed into ggplot2 (also change character R^2 values to numeric)
R2_PlotSet<-data.table(Statistic=rep(c("McFadden", "Nagelkerke"),
                                     each=3),Model=rep(c("a_Basic","b_Intermediate", "c_Advanced"),times=2),Value=
                         as.numeric(sub("%","",c(PseudoR2_Table$McFadden,PseudoR2_Table$Nagelkerke)))/100)
# Create labels to overlay the bar charts 
R2_PlotSet[, Label:=paste0(sprintf("%.2f", Value*100),"%")]
# Plotting parameters
chosenFont <- "Cambria"; dpi<-180
col.v <- c("a_Basic"=brewer.pal(9, "BuGn")[5], "b_Intermediate"=brewer.pal(9, "BuGn")[7], "c_Advanced"=brewer.pal(9, "BuGn")[9])
col.v2 <- rep(c(col.v[1],col.v[2],col.v[3]),2)
col.v3 <- rep("white", 3*2)
label.v <- list("a_Basic"="Basic",
                "b_Intermediate"="Intermediate",
                "c_Advanced"="Advanced")
# Create the plot
(R2Plot<-ggplot(R2_PlotSet, aes(group=Model, y=Value, x=Statistic)) + 
    theme_minimal() + theme(legend.position = "bottom", text=element_text(family=chosenFont), axis.title.x = element_text(margin = margin(t = 5))) + labs(x=bquote("Pseudo"~italic(R^{2})*"-measure"), y="Value", family=chosenFont) +
    geom_col(aes(colour=Model, fill=Model), position="dodge") +
    geom_label(aes(label=Label), fill = col.v2, colour = col.v3, position=position_dodge(0.90), size=2.75,label.padding = unit(0.15, "lines")) +
    scale_colour_manual(name="Model:", values=col.v, labels=label.v) +
    scale_fill_manual(name="Model:", values=col.v, labels=label.v) +
    scale_x_discrete(labels=c("McFadden"="McFadden","Nagelkerke"="Nagelkerke")) +
    scale_y_continuous(limits = c(0, 0.4), breaks = seq(0, 1, by = 0.1),label=percent))
# Saving the graph to specified path
ggsave(R2Plot, file=paste0(genFigPath, "R2Plot_V2.png"), width=1200/dpi, height=1000/dpi, dpi=400, bg="white")


# --- 2.3 Create ROC objects (no plotting)
# - Set confidence interval level
alpha <- 0.05
# - Basic model
datCredit_valid[, prob_basic := predict(logitMod_Basic, newdata = datCredit_valid, type="response")]
# [SANITY CHECK] Check for no missingness in probability scores
cat((anyNA(datCredit_valid[,prob_basic])) %?% "WARNING: Missingness detected in predicted probabilities of the Validation Set.\n" %:%
     "SAFE: No missingness in predicted probabilities.\n")
roc_obj_basic <- pROC::roc(response=datCredit_valid$DefaultStatus1_lead_12_max, predictor=datCredit_valid$prob_basic, ci.method="bootstrap", ci=T, conf.level = 1-alpha, percent=T)
roc_obj_basic$auc; paste0(sprintf("%.2f",(roc_obj_basic$ci[3]-roc_obj_basic$ci[1])/2),"%")
### RESULTS: 69.87% +- 0.44%
# - Intermediate model
datCredit_valid[, prob_int := predict(logitMod_Int, newdata = datCredit_valid, type="response")]
# [SANITY CHECK] Check for no missingness in probability scores
cat((anyNA(datCredit_valid[,prob_int])) %?% "WARNING: Missingness detected in predicted probabilities of the Validation Set.\n" %:%
      "SAFE: No missingness in predicted probabilities.\n")
roc_obj_int <- roc(response=datCredit_valid$DefaultStatus1_lead_12_max, predictor=datCredit_valid$prob_int, ci.method="bootstrap", ci=T, conf.level = 1-alpha, percent=T)
roc_obj_int$auc; paste0(sprintf("%.2f",(roc_obj_int$ci[3]-roc_obj_int$ci[1])/2),"%")
### RESULTS: 77.6% +- 0.49%
# - Advanced model
datCredit_valid[, prob_adv := predict(logitMod_Adv, newdata = datCredit_valid, type="response")]
# [SANITY CHECK] Check for no missingness in probability scores
cat((anyNA(datCredit_valid[,prob_adv])) %?% "WARNING: Missingness detected in predicted probabilities of the Validation Set.\n" %:%
      "SAFE: No missingness in predicted probabilities.\n")
roc_obj_adv <- roc(response=datCredit_valid$DefaultStatus1_lead_12_max, predictor=datCredit_valid$prob_adv, ci.method="bootstrap", ci=T, conf.level = 1-alpha, percent=T)
roc_obj_adv$auc; paste0(sprintf("%.2f",(roc_obj_adv$ci[3]-roc_obj_adv$ci[1])/2),"%")
### RESULTS: 89.99% +- 0.28%
### CONCLUSION: Use the advanced model as it has strongest predictive power


# --- 2.4 Plotting model diagnostics
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


# --- 2.5 Plotting the ROC curves
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


# --- 2.6 Saving the combined (overlaid) graph and clean up
# - Saving the graph
ggsave(g_ROC_compar, file=paste0(genFigPath, "ROC_Curves_Comparison.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")


# --- 2.7 Matthews Correlation Coefficient
# - Training Set
# Get cutoff value (set equal to mean of DefaultStatus1_lead_12_max)
(cutoff_T<-mean(datCredit_train$DefaultStatus1_lead_12_max))

datCredit_train[, prob_basic := predict(logitMod_Basic, newdata = datCredit_train, type="response")]
datCredit_train[, prob_int := predict(logitMod_Int, newdata = datCredit_train, type="response")]
datCredit_train[, prob_adv := predict(logitMod_Adv, newdata = datCredit_train, type="response")]

Get_MCC(datCredit_train$DefaultStatus1_lead_12_max,datCredit_train$prob_basic,cutoff_T) # MCC = 0.097
Get_MCC(datCredit_train$DefaultStatus1_lead_12_max,datCredit_train$prob_int,cutoff_T)   # MCC = 0.351
Get_MCC(datCredit_train$DefaultStatus1_lead_12_max,datCredit_train$prob_adv,cutoff_T)   # MCC = 0.339

# - Validation Set
# Get cutoff value (set equal to mean of DefaultStatus1_lead_12_max)
(cutoff_V<-mean(datCredit_valid$DefaultStatus1_lead_12_max))

datCredit_valid[, prob_basic := predict(logitMod_Basic, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_int := predict(logitMod_Int, newdata = datCredit_valid, type="response")]
datCredit_valid[, prob_adv := predict(logitMod_Adv, newdata = datCredit_valid, type="response")]

(MCC_V_Bas<-Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_basic,cutoff_V)) # MCC = 0.100
Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_int,cutoff_V)   # MCC = 0.355
Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_adv,cutoff_V)   # MCC = 0.343

# --- Create a plot of the MCC's for the 3 models by varying the cutoff
# - Set sequence of cutoffs
cutoff_seq<-seq(0,1, by = 0.0025)

# - Initialise data table to store results
BASIC_MCCs<- data.table(Cutoff=cutoff_seq,Model="a_Bas",MCC=0)
INT_MCCs<- data.table(Cutoff=cutoff_seq,Model="b_Int",MCC=0)
ADV_MCCs<- data.table(Cutoff=cutoff_seq,Model="c_Adv",MCC=0)

# - Fill data table with MCC values
counter<-1
for(k in cutoff_seq){
  BASIC_MCCs[counter,"MCC"]<-Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_basic,k)
  INT_MCCs[counter,"MCC"]<-Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_int,k)
  ADV_MCCs[counter,"MCC"]<-Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_adv,k)
  counter<-counter+1
}

# - Create a dataset in long format for ggplot2
PlottingSet<-rbind(BASIC_MCCs,INT_MCCs,ADV_MCCs)
# - Obtain highest MCCs for each of the models for annotation purposes
HighestMCC<-rbind(BASIC_MCCs[which.max(as.matrix(BASIC_MCCs[,"MCC"])),],INT_MCCs[which.max(as.matrix(INT_MCCs[,"MCC"])),],ADV_MCCs[which.max(as.matrix(ADV_MCCs[,"MCC"])),])

# - Unscaled vs Scaled Graph
# Unscaled vs Scaled at each optimal cutoff
(MCC_B<-Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_basic,HighestMCC[1,Cutoff])) # MCC = 0.150
(MCC_I<-Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_int,HighestMCC[2,Cutoff]))   # MCC = 0.355
(MCC_A<-Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_adv,HighestMCC[3,Cutoff]))   # MCC = 0.398


MCC_PlotSet<-data.table(MCC_Version=rep(c(" ", "  "),each=3),Model=rep(c("a_Basic","b_Intermediate", "c_Advanced"),times=2),Value=round(c(MCC_B,MCC_I,MCC_A,(MCC_B+1)/2,(MCC_I+1)/2,(MCC_A+1)/2),3))
MCC_PlotSet[,Label:=Value]

col.v <- brewer.pal(9, "Reds")[c(6,8,9)]
col.v3 <- rep("white", 3*2)

label.v <- c("a_Basic"=bquote("Basic ("*italic(p[c])*" = "*.(round(HighestMCC[1,Cutoff],3))*")"),
             "b_Intermediate"=bquote("Intermediate ("*italic(p[c])*" = "*.(round(HighestMCC[2,Cutoff],3))*")"),
             "c_Advanced"=bquote("Advanced ("*italic(p[c])*" = "*.(round(HighestMCC[3,Cutoff],3))*")"))

# Create the plot
(MCCPlot<-ggplot(MCC_PlotSet, aes(group=Model, y=Value, x=MCC_Version)) + 
    theme_minimal() + theme(legend.position = "bottom", text=element_text(family=chosenFont), axis.title.x = element_text(margin = margin(t = 5))) + labs(x="Matthews Correlation Coefficients", y="Value", family=chosenFont) +
    geom_col(aes(colour=Model, fill=Model), position="dodge") +
    facet_wrap(~MCC_Version, scales="free") +
    geom_label(aes(label=Label,fill=Model),colour="white", position=position_dodge(0.90), size=2.75,label.padding = unit(0.15, "lines"),show.legend = F) +
    scale_colour_manual(name="Model:", values=col.v, labels=label.v) +
    scale_fill_manual(name="Model:", values=col.v, labels=label.v) +
    scale_x_discrete(labels=c(" "=bquote("MCC "*rho[M]),"  "=bquote("Scaled MCC "*rho*"'"[M]))))
# Saving the graph to specified path
ggsave(MCCPlot, file=paste0(genFigPath, "MCC_Scaled_vs_Unscaled.png"), width=1200/dpi, height=1000/dpi, dpi=400, bg="white")

# - Cut-off graph
# - Set graphing parameters
									  
label.v <- c("a_Bas"="Basic","b_Int"="Intermediate","c_Adv"="Advanced")
linetype.v <- c("solid","solid","solid")
		  
# - Create annotation object
datAnnotate_max <- data.table(Set=c("a_Bas", "b_Int","c_Adv"),HighestMCC[,"Cutoff"], HighestMCC[,"MCC"],
                              Label=c(paste0("' '*rho[M]*' = ", as.character(round(HighestMCC[Model=="a_Bas","MCC"],3)),";  '*italic(p[c])*' = ",round(HighestMCC[1,Cutoff],3),"'"),
                                      paste0("' '*rho[M]*' = ", as.character(round(HighestMCC[Model=="b_Int","MCC"],3)),";  '*italic(p[c])*' = ",round(HighestMCC[2,Cutoff],3),"'"),
                                      paste0("' '*rho[M]*' = ", as.character(round(HighestMCC[Model=="c_Adv","MCC"],3)),";  '*italic(p[c])*' = ",round(HighestMCC[3,Cutoff],3),"'")))


(gg_TS <- ggplot(PlottingSet, aes(x=Cutoff, y=MCC)) + 
  theme_minimal() +
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
  scale_colour_manual(name="Model:", values=col.v, labels=label.v) + 
  #scale_shape_manual(name="Model:", values=shape.v, labels=label.v) + 
  scale_linetype_manual(name="Model:", values=linetype.v, labels=label.v) + 
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks()))

# Saving the graph to specified path
ggsave(gg_TS, file=paste0(genFigPath, "MCC_Cut-Offs.png"), width=1200/dpi, height=1000/dpi, dpi=400, bg="white")

# - Clean up
datCredit_valid[, `:=`(prob_basic=NULL, prob_int=NULL, prob_adv=NULL)]
rm(roc_obj_basic, roc_obj_int, roc_obj_adv,
   chosenFont, col.v, fill.v, linetype.v, label.v, datPlot_ROC, dat_anno, g_ROC_compar); gc()




# ---  2.8 Generalised Youden Index
# - Call GYI Function for advanced model with a=4
Gen_Youd_Ind(logitMod_Adv,datCredit_train,"DefaultStatus1_lead_12_max",4)
### RESULTS: pc = 0.1758


# --- Create a plot displaying default rate as a function of a
# - Add probability scores to the validation set
datCredit_valid[, prob_adv := predict(logitMod_Adv, newdata = datCredit_valid, type="response")]

# - Graphing Parameters
chosenFont <- "Cambria"
dpi <- 180

# - Function to create a actual vs expected 12-month conditional default rate as a function of the TPR:TNR cost ratio a
DefRte_Plotter<-function(a){
  
  # - Obtain Generalised Youden Index cut-off pc for the cost ratio a
  (Algo_Results<-Gen_Youd_Ind(logitMod_Adv, datCredit_train, "DefaultStatus1_lead_12_max", a))
  pc<-Algo_Results$par
  
  # - Dichotomise the probability scores according to the cutoff pc
  datCredit_valid[, class_vals := ifelse(prob_adv <= pc, 0,1)]
  
  # - Create plotting dataset
  ActRte_Dset <- datCredit_valid[,list(DefRate=mean(DefaultStatus1_lead_12_max,na.rm=T), Rate="Act"),by=list(Date)]
  ExpRte_Dset <- datCredit_valid[,list(DefRate=mean(class_vals,na.rm=T), Rate="Exp"),by=list(Date)]
  PlotSet <- rbind(ActRte_Dset,ExpRte_Dset)
  
  # - Creating annotation datasets for easier annotations
  dat_anno1 <- data.table(MAE = NULL, Label = paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*'"),
                          x = rep(as.Date("2016-01-31"),1),
                          y = ifelse(a>5,0.09,0.055)) # little adjustment such that the text and graph do not overlap
  
  dat_anno2 <- data.table(Label =  paste0("' Cut-off  '*italic(p[c])*' = ", as.character(round(pc,4)),"'"),
                          x = rep(as.Date("2016-01-31"),1),
                          y = ifelse(a>5,0.08,0.05))
  
  # - MAE Calculation between actual and expected rate
  dat_anno1[1, MAE := mean(abs(PlotSet[Rate=="Act", DefRate] - PlotSet[Rate=="Exp", DefRate]), na.rm = T)]
  
  # - Making the label more readable
  dat_anno1[, Label := paste0(Label, " = ", sprintf("%.4f",MAE*100), "%'")]
  
  # - More graphing parameters
  col.v<-brewer.pal(9, "Set1")[c(2,1)]
  shape.v <- c(18,20) 
  linetype.v <- c("dashed","solid")
  
  label.v <- c("Act"=bquote(italic(A)[t]*": Actual"),
               "Exp"=bquote(italic(B)[t]*": Expected"))
  
  # - Aesthetics engineering
  PlotSet[, Facet_label := paste0("' Cost ratio  '*italic(a)*' = ", as.character(a),"'")]
  # - Actual Plot
  gg_TS <- ggplot(PlotSet, aes(x=Date, y=DefRate)) + 
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
    scale_colour_manual(name=bquote("Event Rate: "), values=col.v, labels=label.v) + 
    scale_shape_manual(name=bquote("Event Rate: "), values=shape.v, labels=label.v) + 
    scale_linetype_manual(name=bquote("Event Rate: "), values=linetype.v, labels=label.v) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
    scale_y_continuous(breaks=pretty_breaks(), label=percent)
  
  # - Return plot 
  return(gg_TS)
}

# - Call function for various "a" values
(a_1<-DefRte_Plotter(1))
(a_3<-DefRte_Plotter(3))
(a_4<-DefRte_Plotter(4))
(a_6<-DefRte_Plotter(6))
#(a_5<-DefRte_Plotter(5))

# --- Bind Graphs
(combined<-grid.arrange(a_1, a_3 ,a_4 ,a_6 ,ncol=2))

# Saving the graph to a specified path
ggsave(combined, file=paste0(genFigPath, "ACTvsEXP_DefRate.png"), width=2200/dpi, height=1800/dpi, dpi="retina", bg="white")


# --- Clean up
rm(logitMod_Basic, logitMod_Int, logitMod_Adv); gc()


