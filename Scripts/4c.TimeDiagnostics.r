# =============================== DEFAULT RISK - TIME DIAGNOSTICS ===========================================
# Benchmarking the predictions of competing classifiers over time by calculating various time diagnostic
# -----------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Roland Breedt (RB), Dr Arno Botha (AB)

# DESCRIPTION:
# This script uses the previously selected variables in fitting different logit models according to their
# level of complexity. These models are then analysed over time to ensure the models are able to perform well
# against various macroeconomic scenarios. 
# Time diagnostics include: 1) actual-expected pairs of event rates with MAEs in measuring the divergence; 
#     2) as AUC-values over time; 3) Risk prudence degree (VaR-plot)
# -----------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2f.Data_Fusion1.R
#   - 3b.Data_Subsampled_Fusion2.R
#   - 3c(i).Model_DefaultRisk_Basic
#   - 3c(ii).Model_DefaultRisk_Intermediate
#   - 3c(iii).Model_DefaultRisk_Advanced

# -- Inputs:
#   - datCredit_train | Prepared credit data from script 3b
#   - datCredit_valid | Prepared credit data from script 3b
#   - datCredit_smp   | Prepared credit data from script 3b
#   - Final model input variables
#
# -- Outputs:
#   - <analytics> | Graphs showing various time-level diagnostics
# ===========================================================================================================



# ------ 1. Preliminaries
ptm <- proc.time() # for runtime calculations (ignore)

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)
if (!exists('datCredit_smp')) unpack.ffdf(paste0(genPath,"creditdata_smp"), tempPath)

# - Subset to exclude default spells
datCredit_train <- datCredit_train %>% subset(DefaultStatus1==0)
datCredit_valid <- datCredit_valid %>% subset(DefaultStatus1==0)
datCredit_smp <- datCredit_smp %>% subset(DefaultStatus1==0)

# - Load in basic, intermediate, and advanced model formulas
# Basic model
unpack.ffdf(paste0(genObjPath, "Basic_Com_Formula"), tempPath)
# Intermediate model
unpack.ffdf(paste0(genObjPath, "Int_Formula"), tempPath)
# Advanced model
unpack.ffdf(paste0(genObjPath, "Adv_Formula"), tempPath)


# - 2.1 Fitting the models
# Basic model
logitMod_Basic <- glm(inputs_bas, data=datCredit_train, family="binomial")
# Intermediate model
logitMod_Int <- glm(inputs_int, data=datCredit_train, family="binomial")
# Advanced model
logitMod_Adv <- glm(inputs_adv, data=datCredit_train, family="binomial")






# ------ 2. Time diagnostics


# --- 2.1 Time graph of actual vs expected 12-month default rates | Advanced PD-model

# - Add probability scores to the sub sampled set
datCredit_smp[, prob_adv := predict(logitMod_Adv, newdata = datCredit_smp, type="response")]
datCredit_smp[, prob_bas := predict(logitMod_Basic, newdata = datCredit_smp, type="response")]
datCredit_smp[, prob_int := predict(logitMod_Int, newdata = datCredit_smp, type="response")]  

# - Create plotting dataset
datActRte <- datCredit_smp[,list(DefRate=mean(DefaultStatus1_lead_12_max,na.rm=T), Rate="Act"),by=list(Date)]
datExpRte <- datCredit_smp[,list(DefRate=mean(prob_adv,na.rm=T), Rate="Exp"),by=list(Date)]
datPlot <- rbind(datActRte,datExpRte)
  
# - Creating annotation datasets for easier annotations
datAnnotate <- data.table(MAE = NULL, Label = paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*'"),
                          x = as.Date("2016-01-31"),
                          y = 0.055)
  
# - MAE Calculation between actual and expected rate
datAnnotate[1, MAE := mean(abs(datPlot[Rate=="Act", DefRate] - datPlot[Rate=="Exp", DefRate]), na.rm = T)]
  
# - Making the label more readable
datAnnotate[, Label := paste0(Label, " = ", sprintf("%.4f",MAE*100), "%'")]
  
# - Aesthetic engineering and graphing parameters
chosenFont <- "Cambria"; dpi <- 180
vCol<-brewer.pal(9, "Set1")[c(2,1)]
vShape <- c(18,20) 
vLineType <- c("dashed","solid")
vLabel <- c("Act"=bquote(italic(A)[t]*": Actual event rate"),
               "Exp"=bquote(italic(B)[t]*": Expected event rate"))
  
# - Create graph
(gg_ActExp <- ggplot(datPlot, aes(x=Date, y=DefRate)) + 
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
    geom_text(data=datAnnotate, aes(x=x, y=y, label = Label), family=chosenFont, size=4.5, parse=T) +
    scale_colour_manual(name=bquote(" "), values=vCol, labels=vLabel) + 
    scale_shape_manual(name=bquote(" "), values=vShape, labels=vLabel) + 
    scale_linetype_manual(name=bquote(" "), values=vLineType, labels=vLabel) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
    scale_y_continuous(breaks=pretty_breaks(), label=percent))

# - Save graph
ggsave(gg_ActExp, file=paste0(genFigPath, "DefaultRate_ActExp_Adv.png"), width=1200/dpi, height=1000/dpi, dpi=400, bg="white")

# - Clean up
rm(datActRte, datExpRte, gg_ActExp, datPlot)




# --- 2.2 Time graph of actual vs expected 12-month default rates | All PD-models

# - Create plotting dataset by obtaining the actual and expected event rates over time
# Aggregation is performed by taking the mean of the probability scores at each date
datActRte <- datCredit_smp[,list(DefRate=mean(DefaultStatus1_lead_12_max,na.rm=T), Dataset="A"),by=list(Date)]
datExpRte_Bas <- datCredit_smp[,list(DefRate=mean(prob_bas,na.rm=T), Dataset="B"),by=list(Date)]
datExpRte_Int <- datCredit_smp[,list(DefRate=mean(prob_int,na.rm=T), Dataset="C"),by=list(Date)]
datExpRte_Adv <- datCredit_smp[,list(DefRate=mean(prob_adv,na.rm=T), Dataset="D"),by=list(Date)]
# Bind rates into a single data object for graphing purposes
datPlot <- rbind(datActRte,datExpRte_Bas,datExpRte_Int,datExpRte_Adv)

# - Standard deviations of event rates over time
(sd(datActRte$DefRate,na.rm = TRUE)) # 0.01104
(sd(datExpRte_Bas$DefRate,na.rm = TRUE)) # 0.00232
(sd(datExpRte_Int$DefRate,na.rm = TRUE)) # 0.01066
(sd(datExpRte_Adv$DefRate,na.rm = TRUE)) # 0.01041

# - Wilcoxon Signed Rank Test
(Bas_WSR <- Wilcoxon_SR_Test(datActRte[,DefRate], datExpRte_Bas[,DefRate], Alpha = 0.05)) #basic
### RESULTS: p-value ~ 0; i.e., WSR-Test: rejected
(IntWSR <- Wilcoxon_SR_Test(datActRte[,DefRate], datExpRte_Int[,DefRate], Alpha = 0.05)) #intermediate
### RESULTS: p-value = 0.6136; i.e., WSR-Test: not rejected
(AdvWSR <- Wilcoxon_SR_Test(datActRte[,DefRate], datExpRte_Adv[,DefRate], Alpha = 0.05)) #advanced
### RESULTS: p-value = 0.3392; i.e., WSR-Test: not rejected

# - Aesthetic engineering: annotations
# Location of annotations
start_y <- 0.06
space <- 0.00375
y_vals <- c(start_y, start_y-space, start_y-space*2)
  
# - Aesthetic engineering: annotation table
datAnnotate <- data.table(MAE = NULL,
                          Dataset = c("A-B","A-C","A-D"),
                          Label = c(paste0("'MAE  '*italic(bar(r)(A[t],B[t]))*'"),
                                    paste0("'MAE  '*italic(bar(r)(A[t],C[t]))*'"),
                                    paste0("'MAE  '*italic(bar(r)(A[t],D[t]))*'")),
                          outcome = c(Bas_WSR$outcome, IntWSR$outcome, AdvWSR$outcome), # WSR-Test p-values
                          x = rep(as.Date("2010-05-31"),3), # Text x coordinates
                          y = y_vals)

# - MAE Calculations and labelling
datAnnotate[1, MAE := mean(abs(datPlot[Dataset=="A", DefRate] - datPlot[Dataset=="B", DefRate]), na.rm = T)]
datAnnotate[2, MAE := mean(abs(datPlot[Dataset=="A", DefRate] - datPlot[Dataset=="C", DefRate]), na.rm = T)]
datAnnotate[3, MAE := mean(abs(datPlot[Dataset=="A", DefRate] - datPlot[Dataset=="D", DefRate]), na.rm = T)]
datAnnotate[, Label := paste0(Label, " = ", sprintf("%.4f",MAE*100), "%;", ifelse(outcome=="WSR-Test: rejected", paste0("  WSR-Test: '*italic(H[0])*' rejected'"), paste0("  WSR-Test: '*italic(H[0])*' not rejected'")) )]
  
# - Aesthetic engineering & graphing parameters
chosenFont <- "Cambria"; dpi <- 180
vCol <- brewer.pal(9, "Set1")[c(2,1,4,3)]
vLabel <- c("A"=bquote(italic(A)[t]*": Actual"),
             "B"=bquote(italic(B)[t]*": Basic"),
             "C"=bquote(italic(C)[t]*": Intermediate"),
             "D"=bquote(italic(D)[t]*": Advanced"))
vShape <- c(18,20,16,17); 
vLineType <- c("dashed","solid","solid","solid")
  
# - Create graph
(gg_ActExp <- ggplot(datPlot, aes(x=Date, y=DefRate)) + 
    theme_minimal() +
    labs(x="Calendar date (months)", y=bquote(" Conditional 12-month default rate (%)"), family=chosenFont) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-10, 0, 0, 0),
          axis.text.x=element_text(angle=90), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=11, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
    # main line graph with overlaid points
    geom_line(aes(colour=Dataset, linetype=Dataset), linewidth=0.6) +
    geom_point(aes(colour=Dataset, shape=Dataset),size=1.7) + 
    # facets & scale options
    geom_text(data=datAnnotate, aes(x=x, y=y, label = Label), family=chosenFont, size=3.5, parse=T, hjust=0) +
    scale_colour_manual(name=bquote("Event rate: "),  values=vCol, labels=vLabel) + 
    scale_shape_manual(name=bquote("Event rate: "),  values=vShape, labels=vLabel) + 
    scale_linetype_manual(name=bquote("Event rate: "),  values=vLineType, labels=vLabel) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
    scale_y_continuous(breaks=pretty_breaks(), label=percent))
### RESULTS:
# MAE between Actuals and Basic Expected = 0.9714%
# MAE between Actuals and Intermediate Expected = 0.2146%
# MAE between Actuals and Advanced Expected = 0.2486%

# - Save graph
ggsave(gg_ActExp, file=paste0(genFigPath, "DefaultRate_ActExp_AllModels.png"), width=1200/dpi, height=1000/dpi, dpi=400, bg="white")

# - Clean up
rm(datActRte, datExpRte_Bas, datExpRte_Int, datExpRte_Adv, gg_ActExp, datPlot, datAnnotate,
   AdvWSR, IntWSR, Bas_WSR)




# --- 2.4 AUC over time | All PD-models

# - Call custom AUC_overTime() function for each of the three PD-models
BasAUC <- AUC_overTime(datCredit_smp,"Date","DefaultStatus1_lead_12_max","prob_bas")
IntAUC <- AUC_overTime(datCredit_smp,"Date","DefaultStatus1_lead_12_max","prob_int")
AdvAUC <- AUC_overTime(datCredit_smp,"Date","DefaultStatus1_lead_12_max","prob_adv")

# - Differentiation for plotting
BasAUC[,Dataset := "A"]
IntAUC[,Dataset := "B"]
AdvAUC[,Dataset := "C"]

# - Create final dataset for ggplot
datPlot <- rbind(BasAUC,IntAUC,AdvAUC)

# - Aesthetic engineering: annotations
# Location of annotations
start_y <- 0.625
space <- 0.025
y_vals <- c(start_y,start_y-space,start_y-space*2)

# - Creating an annotation dataset for easier annotations
datAnnotate <- data.table(MeanAUC = NULL, Dataset = c("A-B","A-C","A-D"),
                          x = rep(as.Date("2013-05-31"),3), # Text x coordinates
                          y = y_vals )

# - TTC-mean & confidence interval calculations
confLevel <- 0.95
vEventRates_Mean <- c(mean(BasAUC$AUC_Val, na.rm = T), mean(IntAUC$AUC_Val, na.rm = T), mean(AdvAUC$AUC_Val, na.rm = T))
vEventRates_stErr <- c(sd(BasAUC$AUC_Val, na.rm=T) / sqrt(BasAUC[, .N]),
                       sd(IntAUC$AUC_Val, na.rm=T) / sqrt(IntAUC[, .N]),
                       sd(AdvAUC$AUC_Val, na.rm=T) / sqrt(AdvAUC[, .N]) )
vMargin <- qnorm(1-(1-confLevel)/2) * vEventRates_stErr
vLabel <- c(paste0("'TTC-mean over '*italic(A[X](t))*' for '*italic(A[t])*' : ", sprintf("%.2f",vEventRates_Mean[1]*100),
                   "% ± ", sprintf("%1.3f", vMargin[1]*100),"%'"),
          paste0("'TTC-mean over '*italic(A[X](t))*' for '*italic(B[t])*' : ", sprintf("%.2f",vEventRates_Mean[2]*100),
                 "% ± ", sprintf("%1.3f", vMargin[2]*100),"%'"),
          paste0("'TTC-mean over '*italic(A[X](t))*' for '*italic(C[t])*' : ", sprintf("%.2f",vEventRates_Mean[3]*100),
                 "% ± ", sprintf("%1.3f", vMargin[3]*100),"%'") )
datAnnotate[, Label := vLabel]

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 180
vCol <- brewer.pal(8, "Dark2")[c(2,1,3)]
vLabel <- c("A"=bquote(italic(A[t])~": Basic"), "B"=bquote(italic(B[t])~": Intermediate"), 
            "C"=bquote(italic(C[t])~": Advanced"))
vShape <- c(17,20,4) 

# - Create graph
(g3 <- ggplot(datPlot, aes(x=Date, y=AUC_Val)) + theme_minimal() + 
    labs(y=bquote("Prediction Accuracy: AUC (%) "*italic(A[X])), x=bquote("Reporting time "*italic(t)* " (months)")) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-10, 0, 0, 0),
          axis.text.x=element_text(angle=90), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=11, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # Main graph
    geom_ribbon(aes(fill=Dataset, ymin=AUC_LowerCI, ymax=AUC_UpperCI), alpha=0.2,show.legend = FALSE) + 
    geom_line(aes(colour=Dataset, linetype=Dataset), linewidth=0.3) +    
    geom_point(aes(colour=Dataset, shape=Dataset), size=1.8) + 
    geom_hline(yintercept = 0.7, linewidth=0.75) +
    geom_text(data=datAnnotate, aes(x=x, y=y, label = Label), family=chosenFont, size=3.5, hjust=0, parse=TRUE) +
    # Facets & scale options
    scale_colour_manual(name="Model", values=vCol, labels=vLabel) + 
    scale_fill_manual(name="Model", values=vCol, labels=vLabel) +
    scale_shape_manual(name=bquote("Model"), values=vShape, labels=vLabel) + 
    scale_linetype_discrete(name=bquote("Model"), labels=vLabel) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
    scale_y_continuous(breaks=pretty_breaks(), label=percent)
)

# - Save graph
ggsave(g3, file=paste0(genFigPath, "AUC-time.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Cleanup
rm(datAnnotate, g3, datPlot, BasAUC, IntAUC, AdvAUC)




# --- 2.5 Risk Prudence Degree
datRiskPrud <- datCredit_smp[,list(Actuals=mean(DefaultStatus1_lead_12_max,na.rm=T), ExpBas=mean(prob_bas,na.rm=T), ExpInt=mean(prob_int,na.rm=T), ExpAdv=mean(prob_adv,na.rm=T) ), by=list(Date)]

# - 95% VaR for under predictions
# - Basic Model
# - Calculate the extent of the under predicted cases
UnderPreds_B<- as.matrix(datRiskPrud[Actuals>ExpBas, list(difference=(Actuals-ExpBas))])
# - Obtain the 95th percentile
(Bas_VaR<-quantile(UnderPreds_B, 0.95))*100
### RESULTS: 95% Empirical VaR = 3.72%

# - Intermediate Model
# - Calculate the under differences for the under predicted cases
UnderPreds_I<- as.matrix(datRiskPrud[Actuals>ExpInt, list(difference=(Actuals-ExpInt))])
# - Obtain the 95th percentile
(Int_VaR<-quantile(UnderPreds_I, 0.95))*100
### RESULTS: 95% Empirical VaR = 0.55%

# - Advanced Model
# - Calculate the under differences for the under predicted cases
UnderPreds_A<- as.matrix(datRiskPrud[Actuals>ExpAdv, list(difference=(Actuals-ExpAdv))])
# - Obtain the 95th percentile
(Adv_VaR<-quantile(UnderPreds_A, 0.95))*100
### RESULTS: 95% Empirical VaR = 0.72%

# - Create data to feed into ggplot
Dat_Plot<-as.data.table(rbind(cbind(UnderPreds_B, Rate="A_Bas"), cbind(UnderPreds_I, Rate="B_Int"), cbind(UnderPreds_A, Rate="C_Adv")))
Dat_Plot[,difference:=as.numeric(difference)]

# - Plot differences in histogram format
# - Annotations
start_y<-150
space<-10
y_vals<-c(start_y,start_y-space,start_y-2*space)

# - Aesthetic engineering & graphing parameters
vCol<-brewer.pal(9, "Set1")[c(2,1,4)]

vLabel <- c("A_Bas"=bquote(italic(A)[t]-italic(B)[t]*"  "),
             "B_Int"=bquote(italic(A)[t]-italic(C)[t]*"  "),
             "C_Adv"=bquote(italic(A)[t]-italic(D)[t]*"  "))
vLineType <- c("solid","dashed","solid")

datAnnotate <- data.table(Label = c(paste0("'95% VaR of positive '*italic(A[t]-B[t])*' cases"),
                                  paste0("'95% VaR of positive '*italic(A[t]-C[t])*' cases"),
                                  paste0("'95% VaR of positive '*italic(A[t]-D[t])*' cases")),
                        VaR = c(Bas_VaR,Int_VaR,Adv_VaR),
                        x = rep(0.02,3),
                        y = y_vals)

# - Last adjustments before plotting
datAnnotate[, Label := paste0(Label, " = ", sprintf("%.3f",VaR*100), "%'")]

# - Create graph
(ggVaR <- ggplot(Dat_Plot, aes(x=difference, fill=Rate, color=Rate)) + theme_minimal() +
    geom_histogram(alpha=0.8, position="identity", aes(y = after_stat(!!str2lang("density"))), color="black") +
    geom_vline(xintercept=Bas_VaR, color=vCol[1], linetype="dashed", alpha=0.8) +
    geom_vline(xintercept=Int_VaR, color=vCol[2], linetype="dashed", alpha=0.8) +
    geom_vline(xintercept=Adv_VaR, color=vCol[3], linetype="dashed", alpha=0.8) +
    geom_density(alpha=0.6, linetype="dashed", linewidth = 0.8, show.legend = FALSE) +
    labs(x="Under prediction (%)", y = "Density") +
    theme(legend.key = element_blank(),text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=0), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=9, colour="gray50"), strip.text.y.right=element_text(angle=90), legend.margin = margin(t=-5)) +
    annotate(geom="text", x=Bas_VaR-0.0008, y=240, label=paste0("95% VaR"),
             family=chosenFont, size=2.5, colour=vCol[1], angle=90, alpha=0.9) +
    annotate(geom="text", x=Int_VaR-0.0008, y=240, label=paste0("95% VaR"),
             family=chosenFont, size=2.5, colour=vCol[2], angle=90, alpha=0.9) +
    annotate(geom="text", x=Adv_VaR-0.0008, y=240, label=paste0("95% VaR"),
             family=chosenFont, size=2.5, colour=vCol[3], angle=90, alpha=0.9) +
    geom_text(data=datAnnotate, aes(x=x, y=y, label = Label), family=chosenFont, size=4, parse=T, inherit.aes=FALSE) +
    scale_fill_manual(name=bquote("Difference: "),  values=vCol, labels=vLabel)+
    scale_colour_manual(name=bquote("Difference: "),  values=vCol, labels=vLabel) +
    scale_x_continuous(breaks=pretty_breaks(), label=percent) +
    scale_y_continuous(breaks=pretty_breaks()))

# - Save graph
ggsave(ggVaR, file=paste0(genFigPath, "ggVaR.png"), width=1200/dpi, height=1000/dpi, dpi=600, bg="white")




# - Final Clean up
rm(g3,BasAUC, IntAUC, AdvAUC, vLabel, logitMod_Adv, logitMod_Basic, logitMod_Int, IntWSR, Bas_WSR, AdvWSR, gg_ActExp,
   datPlot, datPlot, datExpRte_Bas, datExpRte_Int, datExpRte_Adv, datAnnotate, datActRte, Dat_Plot,datRiskPrud, ggVaR,UnderPreds_A,
   UnderPreds_B,UnderPreds_I); gc()
