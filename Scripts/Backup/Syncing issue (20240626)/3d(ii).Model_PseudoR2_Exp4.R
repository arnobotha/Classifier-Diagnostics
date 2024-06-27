# ========================== MODEL DEFAULT RISK - PSUEDO R^2 Experiments =================================
# Showcasing the use of Pseudo R^2's in evalutaing the goodness of fit for logit models.
# -----------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Roland Breedt

# DESCRIPTION:
# Pseudo R^2 experiments to generate rule of thumbs
# -----------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 0a.CustomFunctions.R
#   - 3b.Data_Subsample_Fusion2
#   - 3c(i).Model_DefaultRisk_Basic
#   - 3c(ii).Model_DefaultRisk_Intermediate
#   - 3c(iii).Model_DefaultRisk_Advanced
#
# -- Inputs:
#   - datCredit_train | Prepared credit data from script 3b
#   - datCredit_valid | Prepared credit data from script 3b
#
# -- Outputs:
#   - A Pseudo R2 table with results together with a graph to visualise this information.
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

# ------ 2. R^2 Analysis of final models
# --- Fitting the models
# - Basic model
logitMod_Basic <- glm(inputs_fin_bas, data=datCredit_train, family="binomial")
# - Basic model
logitMod_Int <- glm(inputs_int, data=datCredit_train, family="binomial")
# - Advanced model
logitMod_Adv <- glm(inputs_adv, data=datCredit_train, family="binomial")

# --- Coefficient of determination
# - Basic model
(coefDeter_Basic <- coefDeter_glm(logitMod_Basic))
### RESULTS:
# McFadden = 2.08%
# Cox Snell = 0.57%
# Nagelkerke = 2.38%

# - Intermediate model
(coefDeter_Int <- coefDeter_glm(logitMod_Int))
### RESULTS:
# McFadden = 21.93%
# Cox Snell = 5.88%
# Nagelkerke = 24.35%

# - Advanced model
(coefDeter_Adv <- coefDeter_glm(logitMod_Adv))
### RESULTS:
# McFadden = 34.17%
# Cox Snell = 8.93%
# Nagelkerke = 37.29%

# --- Create a single table containing the three R^2 measures for each of the models
(PseudoR2_Table<-data.table(Model=c("Basic","Intermediate","Advance"),CoxSnell=c(coefDeter_Basic$CoxSnell,coefDeter_Int$CoxSnell,coefDeter_Adv$CoxSnell),McFadden=c(coefDeter_Basic$McFadden,coefDeter_Int$McFadden,coefDeter_Adv$McFadden),
                Nagelkerke=c(coefDeter_Basic$Nagelkerke,coefDeter_Int$Nagelkerke,coefDeter_Adv$Nagelkerke)))

# - Save 
pack.ffdf(paste0(genObjPath,"PseudoR2_Table"), PseudoR2_Table)

# --- Creating a graph to visualy display these three measures
# - Create a dataset to feed into ggplot2 (also change character R^2 values to numeric)
R2_PlotSet<-data.table(Statistic=rep(c("Cox Snell", "McFadden", "Nagelkerke"),
                                     each=3),Model=rep(c("a_Basic","b_Intermediate", "c_Advanced"),times=3),Value=
                         as.numeric(sub("%","",c(PseudoR2_Table$CoxSnell,PseudoR2_Table$McFadden,PseudoR2_Table$Nagelkerke)))/100)

# - Create labels to overlay the bar charts 
R2_PlotSet[, Label:=paste0(sprintf("%.2f", Value*100),"%")]

# - Plotting parameters
chosenFont <- "Cambria"; dpi<-180

col.v <- c("a_Basic"=brewer.pal(9, "BuGn")[5], "b_Intermediate"=brewer.pal(9, "BuGn")[7], "c_Advanced"=brewer.pal(9, "BuGn")[9])
col.v2 <- rep(c(col.v[1],col.v[2],col.v[3]),3)
col.v3 <- rep("white", 3*3)

label.v <- list("a_Basic"="Basic",
                "b_Intermediate"="Intermediate",
                "c_Advanced"="Advanced")

# - Create the plot
(R2Plot<-ggplot(R2_PlotSet, aes(group=Model, y=Value, x=Statistic)) + 
  theme_minimal() + theme(legend.position = "bottom", text=element_text(family=chosenFont), axis.title.x = element_text(margin = margin(t = 5))) + labs(x=bquote(italic(R^{2})*"-measure"), y="Value", family=chosenFont) +
  geom_col(aes(colour=Model, fill=Model), position="dodge") +
  geom_label(aes(label=Label), fill = col.v2, colour = col.v3, position=position_dodge(0.90), size=2.75,label.padding = unit(0.15, "lines")) +
  scale_colour_manual(name="Model:", values=col.v, labels=label.v) +
  scale_fill_manual(name="Model:", values=col.v, labels=label.v) +
  scale_x_discrete(labels=c("Cox Snell"="Cox Snell","McFadden"="McFadden","Nagelkerke"="Nagelkerke")) +
  scale_y_continuous(limits = c(0, 0.4), breaks = seq(0, 1, by = 0.1),label=percent))
  
# - Saving the graph to specified path
ggsave(R2Plot, file=paste0(genFigPath, "R2Plot.png"), width=1200/dpi, height=1000/dpi, dpi=400, bg="white")

# ------ 3. R^2 experiments to generate rule of thumbs

# - Investigate if sample size plays a part in the pseudo R^2 measures via a bootstrap procedure
# - Field names for subsampling
stratifiers <- c("DefaultStatus1_lead_12_max", "Date") 
targetVar <- "DefaultStatus1_lead_12_max"

# - Create Matrices to store bootstrap results in
Result_Mat_MCF<-matrix(NA,nrow = 100, ncol=4)
Result_Mat_NAG<-matrix(NA,nrow = 100, ncol=4)

counter<-1
for(sub_sz in c(10000,50000,100000,400000)){
  for(k in 1:100){
# - Subsampling & resampling parameters
smp_size <- sub_sz # fixed size of downsampled set
smp_perc <- smp_size / ( datCredit_train[complete.cases(mget(stratifiers)), mget(stratifiers)][,.N] ) # Implied sampling fraction for downsampling step

# --- Downsample data into a set with a fixed size (using stratified sampling) building models and calculating pseudo R^2
set.seed(k)
datCredit_smp <- datCredit_train %>%group_by(across(all_of(stratifiers))) %>% slice_sample(prop=smp_perc) %>% as.data.table()

# --- Refit model on subsampled data
Advanced_Subsampled<-glm(inputs_adv, data=datCredit_smp, family="binomial")
Result_Mat_MCF[k,counter]<-as.numeric(sub("%","",coefDeter_glm(Advanced_Subsampled)$McFadden))/100 #Save the two Pseudo R^2's
Result_Mat_NAG[k,counter]<-as.numeric(sub("%","",coefDeter_glm(Advanced_Subsampled)$Nagelkerke))/100}
    counter<-counter+1
}

# - Inspect means of bootstrap procedure
MeansR2<-data.frame(McFadden_R2_Mean=colMeans(Result_Mat_MCF),Nagelkerke_R2_Mean=colMeans(Result_Mat_NAG))
rownames(MeansR2)<-c("n = 10 000","n = 50 000","n = 100 000","n = 400 000")
MeansR2

### Results
###             McFadden_R2_Mean Nagelkerke_R2_Mean
# n = 10 000          0.345321           0.370541
# n = 50 000          0.341069           0.370702
# n = 100 000         0.342601           0.373082
# n = 400 000         0.341544           0.372544

### Results
## Conclude that sample size does not have a significant influence on pseudo R^2 measures, since
## the value does not deviate from the "original" too much.

# --- Graphing Histograms
# - Set colours for histograms
col.v<-brewer.pal(9, "BuGn")[c(3,5,7,9)]

# - Save histogram of McFadden R^2's to disc
png(file=paste0(genFigPath, "McF_R2Hist.png"))
par(mfrow=c(2,2))
hist(Result_Mat_MCF[,1],breaks="FD",probability = TRUE,col = col.v[1],xlab="R^2",main="McFadden R^2's for n=10 000")
abline(v = mean(Result_Mat_MCF[,1]), col = "red", lwd = 3)
hist(Result_Mat_MCF[,2],breaks="FD",probability = TRUE,col = col.v[2],xlab="R^2",main="McFadden R^2's for n=50 000")
abline(v = mean(Result_Mat_MCF[,2]), col = "red", lwd = 3)
hist(Result_Mat_MCF[,3],breaks="FD",probability = TRUE,col = col.v[3],xlab="R^2",main="McFadden R^2's for n=100 000")
abline(v = mean(Result_Mat_MCF[,3]), col = "red", lwd = 3)
hist(Result_Mat_MCF[,4],breaks="FD",probability = TRUE,col = col.v[4],xlab="R^2",main="McFadden R^2's for n=400 000")
abline(v = mean(Result_Mat_MCF[,4]), col = "red", lwd = 3)
dev.off()

# - Save histogram of Nagelkerke's R^2's to disc
png(file=paste0(genFigPath, "Nag_R2Hist.png"))
par(mfrow=c(2,2))
hist(Result_Mat_NAG[,1],breaks="FD",probability = TRUE, col = col.v[1],xlab="R^2",main="Nagelkerke R^2's for n=10 000")
abline(v = mean(Result_Mat_NAG[,1]), col = "red", lwd = 3)
hist(Result_Mat_NAG[,2],breaks="FD",probability = TRUE, col = col.v[2],xlab="R^2",main="Nagelkerke R^2's for n=50 000")
abline(v = mean(Result_Mat_NAG[,2]), col = "red", lwd = 3)
hist(Result_Mat_NAG[,3],breaks="FD",probability = TRUE, col = col.v[3],xlab="R^2",main="Nagelkerke R^2's for n=100 000")
abline(v = mean(Result_Mat_NAG[,3]), col = "red", lwd = 3)
hist(Result_Mat_NAG[,4],breaks="FD",probability = TRUE, col = col.v[4],xlab="R^2",main="Nagelkerke R^2's for n=400 000")
abline(v = mean(Result_Mat_NAG[,4]), col = "red", lwd = 3)
dev.off()

# --- Pseudo R^2's for single input variables (small amount of information)
cat("AgeToTerm:\n")
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ AgeToTerm, data=datCredit_train, family="binomial"))
### Results:
##  R^2 for Mcfadden, Cox Snell, and Nagelkerke respectively:
#   0.09%; 0.02%; 0.10%
cat("Term:\n")
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term, data=datCredit_train, family="binomial"))
### Results:
##  R^2 for Mcfadden, Cox Snell, and Nagelkerke respectively:
#   0.00%; 0.00%; 0.00%
cat("Balance:\n")
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Balance, data=datCredit_train, family="binomial"))
### Results:
##  R^2 for Mcfadden, Cox Snell, and Nagelkerke respectively:
#   0.06%; 0.02%; 0.07%
cat("Principal:\n")
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Principal, data=datCredit_train, family="binomial"))
### Results:
##  R^2 for Mcfadden, Cox Snell, and Nagelkerke respectively:
#   0.58%; 0.16%; 0.66%
cat("InterestRate_Margin_imputed_mean:\n")
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ InterestRate_Margin_imputed_mean, data=datCredit_train, family="binomial"))
### Results:
##  R^2 for Mcfadden, Cox Snell, and Nagelkerke respectively:
#   0.64%; 0.18%; 0.73%

### Conclusion (McFadden): Provided that single input variables obtained pseudo R^2's smaller than 1% and the final
###                        basic model had a pseudo R^2 less than 5%, we conclude that a pseudo R^2 less than 5% is a bad fit.

# --- Subsequently add input variables from final advanced model (Focus on McFadden)
# - Add Term
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term , data=datCredit_train, family="binomial"))
### Results: R^2 = 0.00%

# - Add Balance
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance , data=datCredit_train, family="binomial"))
### Results: R^2 = 0.07%

# - Add Principal
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal , data=datCredit_train, family="binomial"))
### Results: R^2 = 1.70%

# - Add InterestRate_Margin_imputed_mean
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean , data=datCredit_train, family="binomial"))
### Results: R^2 = 1.96%

# - Add M_DTI_Growth_6
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                  M_DTI_Growth_6 , data=datCredit_train, family="binomial"))
### Results: R^2 = 2.49%

# - Add M_DTI_Growth_9
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9, data=datCredit_train, family="binomial"))
### Results: R^2 = 2.49%

# - Add M_Emp_Growth_9
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9, data=datCredit_train, family="binomial"))
### Results: R^2 = 2.61%

# - Add M_RealGDP_Growth_12
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12, data=datCredit_train, family="binomial"))
### Results: R^2 = 2.61%

# - Add TimeInPerfSpell
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12 + TimeInPerfSpell , data=datCredit_train, family="binomial"))
### Results: R^2 = 2.89%

# - Add g0_Delinq_SD_4
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12 + TimeInPerfSpell +
                    g0_Delinq_SD_4, data=datCredit_train, family="binomial"))
### Results: R^2 = 17.33%

# - Add g0_Delinq_SD_6
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12 + TimeInPerfSpell +
                    g0_Delinq_SD_4 + g0_Delinq_SD_6, data=datCredit_train, family="binomial"))
### Results: R^2 = 19.89%

# - Add slc_acct_roll_ever_24_imputed_mean
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12 + TimeInPerfSpell +
                    g0_Delinq_SD_4 + g0_Delinq_SD_6 + slc_acct_roll_ever_24_imputed_mean, data=datCredit_train, family="binomial"))
### Results: R^2 = 22.92%

# - Add slc_acct_arr_dir_3
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12 + TimeInPerfSpell +
                    g0_Delinq_SD_4 + g0_Delinq_SD_6 + slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3, data=datCredit_train, family="binomial"))
### Results: R^2 = 24.98%

# - Add slc_past_due_amt_imputed_med
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12 + TimeInPerfSpell +
                    g0_Delinq_SD_4 + g0_Delinq_SD_6 + slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3 +
                    slc_past_due_amt_imputed_med, data=datCredit_train, family="binomial"))
### Results: R^2 = 25.97%

# - Add slc_acct_pre_lim_perc_imputed_med
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12 + TimeInPerfSpell +
                    g0_Delinq_SD_4 + g0_Delinq_SD_6 + slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3 +
                    slc_past_due_amt_imputed_med + slc_acct_pre_lim_perc_imputed_med, data=datCredit_train, family="binomial"))
### Results: R^2 = 26.56%

# - Add NewLoans_Aggr_Prop_1 
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12 + TimeInPerfSpell +
                    g0_Delinq_SD_4 + g0_Delinq_SD_6 + slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3 +
                    slc_past_due_amt_imputed_med + slc_acct_pre_lim_perc_imputed_med + NewLoans_Aggr_Prop_1  
                    , data=datCredit_train, family="binomial"))
### Results: R^2 = 26.67%

# - Add NewLoans_Aggr_Prop_3
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12 + TimeInPerfSpell +
                    g0_Delinq_SD_4 + g0_Delinq_SD_6 + slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3 +
                    slc_past_due_amt_imputed_med + slc_acct_pre_lim_perc_imputed_med + NewLoans_Aggr_Prop_1 + 
                    NewLoans_Aggr_Prop_3, data=datCredit_train, family="binomial"))
### Results: R^2 = 26.69%

# - Add NewLoans_Aggr_Prop_5
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12 + TimeInPerfSpell +
                    g0_Delinq_SD_4 + g0_Delinq_SD_6 + slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3 +
                    slc_past_due_amt_imputed_med + slc_acct_pre_lim_perc_imputed_med + NewLoans_Aggr_Prop_1 + 
                    NewLoans_Aggr_Prop_3 + NewLoans_Aggr_Prop_5, data=datCredit_train, family="binomial"))
### Results: R^2 = 26.69%

# - Add AgeToTerm_Aggr_Mean
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12 + TimeInPerfSpell +
                    g0_Delinq_SD_4 + g0_Delinq_SD_6 + slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3 +
                    slc_past_due_amt_imputed_med + slc_acct_pre_lim_perc_imputed_med + NewLoans_Aggr_Prop_1 + 
                    NewLoans_Aggr_Prop_3 + NewLoans_Aggr_Prop_5 + AgeToTerm_Aggr_Mean
                    , data=datCredit_train, family="binomial"))
### Results: R^2 = 26.76%

# - Add InterestRate_Margin_Aggr_Med_3
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12 + TimeInPerfSpell +
                    g0_Delinq_SD_4 + g0_Delinq_SD_6 + slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3 +
                    slc_past_due_amt_imputed_med + slc_acct_pre_lim_perc_imputed_med + NewLoans_Aggr_Prop_1 + 
                    NewLoans_Aggr_Prop_3 + NewLoans_Aggr_Prop_5 + AgeToTerm_Aggr_Mean + InterestRate_Margin_Aggr_Med_3 
                    , data=datCredit_train, family="binomial"))
### Results: R^2 = 26.82%

# - Add g0_Delinq
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12 + TimeInPerfSpell +
                    g0_Delinq_SD_4 + g0_Delinq_SD_6 + slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3 +
                    slc_past_due_amt_imputed_med + slc_acct_pre_lim_perc_imputed_med + NewLoans_Aggr_Prop_1 + 
                    NewLoans_Aggr_Prop_3 + NewLoans_Aggr_Prop_5 + AgeToTerm_Aggr_Mean + InterestRate_Margin_Aggr_Med_3 + 
                    g0_Delinq, data=datCredit_train, family="binomial"))
### Results: R^2 = 28.20%

# - Add g0_Delinq_Num
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12 + TimeInPerfSpell +
                    g0_Delinq_SD_4 + g0_Delinq_SD_6 + slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3 +
                    slc_past_due_amt_imputed_med + slc_acct_pre_lim_perc_imputed_med + NewLoans_Aggr_Prop_1 + 
                    NewLoans_Aggr_Prop_3 + NewLoans_Aggr_Prop_5 + AgeToTerm_Aggr_Mean + InterestRate_Margin_Aggr_Med_3 + 
                    g0_Delinq + g0_Delinq_Num, data=datCredit_train, family="binomial"))
### Results: R^2 = 28.28%


# - Add PrevDefaults
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12 + TimeInPerfSpell +
                    g0_Delinq_SD_4 + g0_Delinq_SD_6 + slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3 +
                    slc_past_due_amt_imputed_med + slc_acct_pre_lim_perc_imputed_med + NewLoans_Aggr_Prop_1 + 
                    NewLoans_Aggr_Prop_3 + NewLoans_Aggr_Prop_5 + AgeToTerm_Aggr_Mean + InterestRate_Margin_Aggr_Med_3 + 
                    g0_Delinq + g0_Delinq_Num + PrevDefaults, data=datCredit_train, family="binomial"))
### Results: R^2 = 37.29%

# --- Now remove the variables which significantly increased the pseudo R2
# - Remove all g0 input variables and PrevDefaults
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12 + TimeInPerfSpell +
                    slc_acct_roll_ever_24_imputed_mean + slc_acct_arr_dir_3 +
                    slc_past_due_amt_imputed_med + slc_acct_pre_lim_perc_imputed_med + NewLoans_Aggr_Prop_1 + 
                    NewLoans_Aggr_Prop_3 + NewLoans_Aggr_Prop_5 + AgeToTerm_Aggr_Mean + InterestRate_Margin_Aggr_Med_3 
                    , data=datCredit_train, family="binomial"))
### Results: R^2 = 24.76%

# - Remove all slc_acct_roll_ever_24_imputed_mean and slc_acct_arr_dir_3
coefDeter_glm(glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean +
                    M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + M_RealGDP_Growth_12 + TimeInPerfSpell +
                    slc_past_due_amt_imputed_med + slc_acct_pre_lim_perc_imputed_med + NewLoans_Aggr_Prop_1 + 
                    NewLoans_Aggr_Prop_3 + NewLoans_Aggr_Prop_5 + AgeToTerm_Aggr_Mean + InterestRate_Margin_Aggr_Med_3 
                  , data=datCredit_train, family="binomial"))
### Results: R^2 = 16.03%

### Conclusion (McFadden): Given that the best pseudo R^2 is 34.17%, which is in McFaddens [20%,40%] range (together with various other good fits),
###                        we conclude that this is indeed the range for excellent models. Moreover, as seen above it seems as if the range
###                        [10%,20%] still represents a good fit.

