# ========================== MODEL DEFAULT RISK - Risk Prudence Experimentation =============================
# This script experiments with a few different ideas towards formulating a risk prudence degree on the event rate
# and a trained classifiers expectation thereof
# -----------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Roland Breedt

# DESCRIPTION:
# Experiments on risk prudence using 4 ideas. 1) Proportion of under vs over prediction; 2) Cost function; 
# 3) Proportion of error by under prediction; 4) Underprediction VaR.
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
#   - datCredit_smp   | Prepared credit data from script 3b
#   -  3 final PD-models
#   - Final model input variables
# -- Outputs:
#   - Doesnt save anything, only experiments.
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
# - Intermediate model
logitMod_Int <- glm(inputs_int, data=datCredit_train, family="binomial")
# - Advanced model
logitMod_Adv <- glm(inputs_adv, data=datCredit_train, family="binomial")

# --- 2.2 Simplistic plot, using only the advanced model
# --- Create a plot displaying default rate
# - Add probability scores to the sub sampled set
datCredit_smp[, prob_adv := predict(logitMod_Adv, newdata = datCredit_smp, type="response")]
datCredit_smp[, prob_bas := predict(logitMod_Basic, newdata = datCredit_smp, type="response")]
datCredit_smp[, prob_int := predict(logitMod_Int, newdata = datCredit_smp, type="response")]  

# --- 2.3 Risk Prudence Analysis
RiskPrud_Set <- datCredit_smp[,list(Actuals=mean(DefaultStatus1_lead_12_max,na.rm=T), ExpBas=mean(prob_bas,na.rm=T), ExpInt=mean(prob_int,na.rm=T), ExpAdv=mean(prob_adv,na.rm=T) ), by=list(Date)]

# - Proportion of Under predictions
cat("Proportion of dates where the default rate was under predicted by the Basic model = ", round(RiskPrud_Set[Actuals>ExpBas,.N]/RiskPrud_Set[,.N]*100,2), "%", sep="", "\n") # 26.67%
cat("Proportion of dates where the default rate was under predicted by the Intermediate model = ", round(RiskPrud_Set[Actuals>ExpInt,.N]/RiskPrud_Set[,.N]*100,2), "%", sep="", "\n") # 45%
cat("Proportion of dates where the default rate was under predicted by the Advanced model = ", round(RiskPrud_Set[Actuals>ExpAdv,.N]/RiskPrud_Set[,.N]*100,2), "%", sep="", "\n") # 44.44%

# - Cost function, by setting a cost to the degree of under prediction and over prediction respectively
UnderCost<-2
OverCost<-1
# - Basic
(sum(RiskPrud_Set[Actuals>ExpBas,Actuals-ExpBas])*UnderCost+
    sum(RiskPrud_Set[Actuals<ExpBas,ExpBas-Actuals])*OverCost) # 2.57
# - Intermediate
(sum(RiskPrud_Set[Actuals>ExpInt,Actuals-ExpInt])*UnderCost+
    sum(RiskPrud_Set[Actuals<ExpInt,ExpInt-Actuals])*OverCost) # 0.58
# - Advance
(sum(RiskPrud_Set[Actuals>ExpAdv,Actuals-ExpAdv])*UnderCost+
    sum(RiskPrud_Set[Actuals<ExpAdv,ExpAdv-Actuals])*OverCost) # 0.67

# - Proportion of error due to overprediction
# - Basic
(sum(RiskPrud_Set[Actuals>ExpBas,Actuals-ExpBas]))/sum(RiskPrud_Set[,abs(Actuals-ExpBas)])*100 # 47.18%
# - Intermediate
(sum(RiskPrud_Set[Actuals>ExpInt,Actuals-ExpInt]))/sum(RiskPrud_Set[,abs(Actuals-ExpInt)])*100 # 51.32%
# - Advance
(sum(RiskPrud_Set[Actuals>ExpAdv,Actuals-ExpAdv]))/sum(RiskPrud_Set[,abs(Actuals-ExpAdv)])*100 # 49.77%

# - 95% VaR for under predictions
# - Basic Model
# - Calculate the extent of the under predicted cases
UnderPreds_B<- as.matrix(RiskPrud_Set[Actuals>ExpBas, list(difference=(Actuals-ExpBas))])
# - Obtain the 95th percentile
(Bas_VaR<-quantile(UnderPreds_B, 0.95))*100
### RESULTS: 95% Empirical VaR = 3.72%

# - Intermediate Model
# - Calculate the under differences for the under predicted cases
UnderPreds_I<- as.matrix(RiskPrud_Set[Actuals>ExpInt, list(difference=(Actuals-ExpInt))])
# - Obtain the 95th percentile
(Int_VaR<-quantile(UnderPreds_I, 0.95))*100
### RESULTS: 95% Empirical VaR = 0.55%

# - Advanced Model
# - Calculate the under differences for the under predicted cases
UnderPreds_A<- as.matrix(RiskPrud_Set[Actuals>ExpAdv, list(difference=(Actuals-ExpAdv))])
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

# - Graphing parameters
col.v<-brewer.pal(9, "Set1")[c(2,1,4)]

label.v <- c("A_Bas"=bquote(italic(A)[t]-italic(B)[t]*"  "),
             "B_Int"=bquote(italic(A)[t]-italic(C)[t]*"  "),
             "C_Adv"=bquote(italic(A)[t]-italic(D)[t]*"  "))
linetype.v <- c("solid","dashed","solid")

dat_anno1 <- data.table(Label = c(paste0("'95% VaR of positive '*italic(A[t]-B[t])*' cases"),
                                  paste0("'95% VaR of positive '*italic(A[t]-C[t])*' cases"),
                                  paste0("'95% VaR of positive '*italic(A[t]-D[t])*' cases")),
                        VaR = c(Bas_VaR,Int_VaR,Adv_VaR),
                        x = rep(0.02,3),
                        y = y_vals)

# - Last adjustments before plotting
dat_anno1[, Label := paste0(Label, " = ", sprintf("%.3f",VaR*100), "%'")]

(VaR_plot <- ggplot(Dat_Plot, aes(x=difference, fill=Rate, color=Rate)) + theme_minimal() +
    geom_histogram(alpha=0.8, position="identity", aes(y = after_stat(!!str2lang("density"))), color="black") +
    geom_vline(xintercept=Bas_VaR, color=col.v[1], linetype="dashed", alpha=0.8) +
    geom_vline(xintercept=Int_VaR, color=col.v[2], linetype="dashed", alpha=0.8) +
    geom_vline(xintercept=Adv_VaR, color=col.v[3], linetype="dashed", alpha=0.8) +
    geom_density(alpha=0.6, linetype="dashed", linewidth = 0.8, show.legend = FALSE) +
    labs(x="Under prediction (%)", y = "Density") +
    theme(legend.key = element_blank(),text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=0), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=9, colour="gray50"), strip.text.y.right=element_text(angle=90), legend.margin = margin(t=-5)) +
    annotate(geom="text", x=Bas_VaR-0.001, y=240, label=paste0("95% VaR"),
             family=chosenFont, size=2.5, colour=col.v[1], angle=90, alpha=0.9) +
    annotate(geom="text", x=Int_VaR-0.001, y=240, label=paste0("95% VaR"),
             family=chosenFont, size=2.5, colour=col.v[2], angle=90, alpha=0.9) +
    annotate(geom="text", x=Adv_VaR-0.001, y=240, label=paste0("95% VaR"),
             family=chosenFont, size=2.5, colour=col.v[3], angle=90, alpha=0.9) +
    geom_text(data=dat_anno1, aes(x=x, y=y, label = Label), family=chosenFont, size=4, parse=T, inherit.aes=FALSE) +
    scale_fill_manual(name=bquote("Difference: "),  values=col.v, labels=label.v)+
    scale_colour_manual(name=bquote("Difference: "),  values=col.v, labels=label.v) +
    scale_x_continuous(breaks=pretty_breaks(), label=percent) +
    scale_y_continuous(breaks=pretty_breaks()))
