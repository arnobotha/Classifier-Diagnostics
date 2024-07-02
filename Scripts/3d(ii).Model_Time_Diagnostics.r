# ========================== MODEL DEFAULT RISK - TIME DIAGNOSTICS ========================================
# This script performs time diagnostics on the three logit models. Mainly it tests the realised default rate 
# against the predictions thereof, by using the MAE.
# -----------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Roland Breedt

# DESCRIPTION:
# This script uses the previously selected variables in fitting different logit models according to their
# level of complexity. These models are then analysed over time to ensure the models are able to perform well
# against various macroeconomic scenarios.
# -----------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 0a.CustomFunctions.R
#   - 3b.Data_Subsample_Fusion2
#   - 3c(i).Model_DefaultRisk_Basic
#   - 3c(ii).Model_DefaultRisk_Intermediate
#   - 3c(iii).Model_DefaultRisk_Advanced

# -- Inputs:
#   - datCredit_train | Prepared credit data from script 3b
#   - datCredit_valid | Prepared credit data from script 3b
#   - Final model input variables
# -- Outputs:
#   - Some graphs showcasing model performance over time
# ===========================================================================================================

# ------ 1. Preliminaries
ptm <- proc.time() # for runtime calculations (ignore)

# - Graphing Parameters
chosenFont <- "Cambria"
dpi <- 180

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




# ------ 2. Model comparison
# --- 2.1 Fitting the models
# - Basic model
logitMod_Basic <- glm(inputs_bas, data=datCredit_train, family="binomial")
# - Basic model
logitMod_Int <- glm(inputs_int, data=datCredit_train, family="binomial")
# - Advanced model
logitMod_Adv <- glm(inputs_adv, data=datCredit_train, family="binomial")


# --- 2.2 Simplistic plot, using only the advanced model
# --- Create a plot displaying default rate
# - Add probability scores to the sub sampled set
datCredit_smp[, prob_adv := predict(logitMod_Adv, newdata = datCredit_smp, type="response")]
datCredit_smp[, prob_bas := predict(logitMod_Basic, newdata = datCredit_smp, type="response")]
datCredit_smp[, prob_int := predict(logitMod_Int, newdata = datCredit_smp, type="response")]  

# - Create plotting dataset
ActRte_Dset <- datCredit_smp[,list(DefRate=mean(DefaultStatus1_lead_12_max,na.rm=T), Rate="Act"),by=list(Date)]
ExpRte_Adv <- datCredit_smp[,list(DefRate=mean(prob_adv,na.rm=T), Rate="Exp"),by=list(Date)]
PlotSet <- rbind(ActRte_Dset,ExpRte_Adv)
  
# - Creating annotation datasets for easier annotations
dat_anno1 <- data.table(MAE = NULL, Label = paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*'"),
                          x = as.Date("2016-01-31"),
                          y = 0.055)
  
# - MAE Calculation between actual and expected rate
dat_anno1[1, MAE := mean(abs(PlotSet[Rate=="Act", DefRate] - PlotSet[Rate=="Exp", DefRate]), na.rm = T)]
  
# - Making the label more readable
dat_anno1[, Label := paste0(Label, " = ", sprintf("%.4f",MAE*100), "%'")]
  
# - More graphing parameters
col.v<-brewer.pal(9, "Set1")[c(2,1)]
shape.v <- c(18,20) 
linetype.v <- c("dashed","solid")
  
label.v <- c("Act"=bquote(italic(A)[t]*": Actual event rate"),
               "Exp"=bquote(italic(B)[t]*": Expected event rate"))
  
# - Actual Plot
  (gg_TimeDiag <- ggplot(PlotSet, aes(x=Date, y=DefRate)) + 
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
    geom_text(data=dat_anno1, aes(x=x, y=y, label = Label), family=chosenFont, size=4.5, parse=T) +
    scale_colour_manual(name=bquote(" "), values=col.v, labels=label.v) + 
    scale_shape_manual(name=bquote(" "), values=shape.v, labels=label.v) + 
    scale_linetype_manual(name=bquote(" "), values=linetype.v, labels=label.v) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
    scale_y_continuous(breaks=pretty_breaks(), label=percent))
# - Pack away graph
ggsave(gg_TimeDiag, file=paste0(genFigPath, "Adv_Model_TimeDiagnostics.png"), width=1200/dpi, height=1000/dpi, dpi=400, bg="white")
  
# - Clean up
rm(ActRte_Dset, ExpRte_Adv)

# --- 2.3 Plot for all 3 models
# - Create plotting dataset
Actual <- datCredit_smp[,list(DefRate=mean(DefaultStatus1_lead_12_max,na.rm=T), Dataset="A"),by=list(Date)]
ExpRte_Bas <- datCredit_smp[,list(DefRate=mean(prob_bas,na.rm=T), Dataset="B"),by=list(Date)]
ExpRte_Int <- datCredit_smp[,list(DefRate=mean(prob_int,na.rm=T), Dataset="C"),by=list(Date)]
ExpRte_Adv <- datCredit_smp[,list(DefRate=mean(prob_adv,na.rm=T), Dataset="D"),by=list(Date)]
  
# - Bind rates
PlotSet <- rbind(Actual,ExpRte_Bas,ExpRte_Int,ExpRte_Adv)

# - Location of annotations
start_y<-0.06
space<-0.00375
y_vals<-c(start_y,start_y-space,start_y-space*2)
  
# - Creating an annotation dataset for easier annotations
dat_anno1 <- data.table(MAE = NULL,
                          Dataset = c("A-B","A-C","A-D"),
                          Label = c(paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*'"),
                                    paste0("'MAE between '*italic(A[t])*' and '*italic(C[t])*'"),
                                    paste0("'MAE between '*italic(A[t])*' and '*italic(D[t])*'")),
                          x = rep(as.Date("2016-01-31"),3), # Text x coordinates
                          y = y_vals)
# - MAE Calculations
dat_anno1[1, MAE := mean(abs(PlotSet[Dataset=="A", DefRate] - PlotSet[Dataset=="B", DefRate]), na.rm = T)]
dat_anno1[2, MAE := mean(abs(PlotSet[Dataset=="A", DefRate] - PlotSet[Dataset=="C", DefRate]), na.rm = T)]
dat_anno1[3, MAE := mean(abs(PlotSet[Dataset=="A", DefRate] - PlotSet[Dataset=="D", DefRate]), na.rm = T)]
  
# - Last adjustments before plotting
dat_anno1[, Label := paste0(Label, " = ", sprintf("%.4f",MAE*100), "%'")]
  
# - Graphing parameters
col.v<-brewer.pal(9, "Set1")[c(2,1,4,3)]
  
label.v <- c("A"=bquote(italic(A)[t]*": Actual"),
               "B"=bquote(italic(B)[t]*": Basic"),
               "C"=bquote(italic(C)[t]*": Intermediate"),
               "D"=bquote(italic(D)[t]*": Advanced"))
shape.v <- c(18,20,16,17); 
linetype.v <- c("dashed","solid","solid","solid")
  
# - Facet names
facet_names_full <- c("DefRate"="Tester")
  
# - Aesthetics engineering
(Models_TimeDiag <- ggplot(PlotSet, aes(x=Date, y=DefRate)) + 
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
    geom_text(data=dat_anno1, aes(x=x, y=y, label = Label), family=chosenFont, size=3.5, parse=T) +
    scale_colour_manual(name=bquote("Event rate: "),  values=col.v, labels=label.v) + 
    scale_shape_manual(name=bquote("Event rate: "),  values=shape.v, labels=label.v) + 
    scale_linetype_manual(name=bquote("Event rate: "),  values=linetype.v, labels=label.v) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
    scale_y_continuous(breaks=pretty_breaks(), label=percent))
  
# - Pack away graph
ggsave(Models_TimeDiag, file=paste0(genFigPath, "ModelsTimeDiagnostics.png"), width=1200/dpi, height=1000/dpi, dpi=400, bg="white")
  
# --- 2.3. Wilcoxon Signed Rank Test
wilcox.test(Actual[,DefRate],ExpRte_Bas[,DefRate], alternative = "two.sided", conf.level = 0.95) #basic
### RESULTS: p-value ~ 0
wilcox.test(Actual[,DefRate],ExpRte_Int[,DefRate], alternative = "two.sided", conf.level = 0.95) #intermediate
### RESULTS: p-value = 0.6136
wilcox.test(Actual[,DefRate],ExpRte_Adv[,DefRate], alternative = "two.sided", conf.level = 0.95) #advanced
### RESULTS: p-value = 0.3392

# --- 2.4 AUC over time
# - Call AUC.Over.Time Function for each of the three PD-models
BasAUC<-AUC.Over.Time(datCredit_smp,"Date","DefaultStatus1_lead_12_max","prob_bas")
IntAUC<-AUC.Over.Time(datCredit_smp,"Date","DefaultStatus1_lead_12_max","prob_int")
AdvAUC<-AUC.Over.Time(datCredit_smp,"Date","DefaultStatus1_lead_12_max","prob_adv")

# - Differentiation for plotting
BasAUC[,Dataset:="A"]
IntAUC[,Dataset:="B"]
AdvAUC[,Dataset:="C"]

# - Create final dataset for ggplot
PlottingSet<-rbind(BasAUC,IntAUC,AdvAUC)

# - Graphing parameters
vCol<-brewer.pal(9, "Set1")[c(2,1,4)]

vCol <- brewer.pal(8, "Dark2")[c(2,1,3)]
label.v <- c("A"=": Basic",
             "B"=": Intermediate",
             "C"=": Advanced")
shape.v <- c(17,20,4) 
linetype.v <- rep("solid",3)

# - Graph results
(g3 <- ggplot(PlottingSet, aes(x=Date, y=AUC_Val)) + theme_minimal() + 
    labs(y="Area Under the Curve (%)", x="Calendar date (months)") + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-10, 0, 0, 0),
          axis.text.x=element_text(angle=90), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=11, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # Main graph
    geom_ribbon(aes(fill=Dataset, ymin=AUC_LowerCI, ymax=AUC_UpperCI), alpha=0.2,show.legend = FALSE) + 
    geom_line(aes(colour=Dataset, linetype=Dataset), linewidth=0.5) +    
    geom_point(aes(colour=Dataset, shape=Dataset), size=2) + 
    # Facets & scale options
    scale_colour_manual(name="Model", values=vCol, labels=label.v) + 
    scale_fill_manual(name="Model", values=vCol, labels=label.v) +
    scale_shape_manual(name=bquote("Model"), values=shape.v, labels=label.v) + 
    scale_linetype_manual(name=bquote("Model"), values=linetype.v, labels=label.v) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
    scale_y_continuous(breaks=pretty_breaks(), label=percent, limits=c(0.5,1))
)
# - Pack away graph
ggsave(g3, file=paste0(genFigPath, "AUC.Over.Time.png"), width=1400/dpi, height=1000/dpi, dpi="retina", bg="white")
