# ========================== Matthews Correlation Coefficient =================================
# Showcasing the use of ROC curves in evalutaing the predictive power of logit models.
# -----------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Roland Breedt
# ...



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
logitMod_Basic <- glm(inputs_fin_bas, data=datCredit_train, family="binomial")
# - Basic model
logitMod_Int <- glm(inputs_int, data=datCredit_train, family="binomial")
# - Advanced model
logitMod_Adv <- glm(inputs_adv, data=datCredit_train, family="binomial")

# --- 2.3 Create ROC objects (no plotting)
# - Set confidence interval level
alpha <- 0.05
# - Basic model
datCredit_valid[, prob_basic := predict(logitMod_Basic, newdata = datCredit_valid, type="response")]

# - Intermediate model
datCredit_valid[, prob_int := predict(logitMod_Int, newdata = datCredit_valid, type="response")]

# - Advanced model
datCredit_valid[, prob_adv := predict(logitMod_Adv, newdata = datCredit_valid, type="response")]


# - Get cutoff value (set equal to mean)
(cutoff<-mean(datCredit_valid$DefaultStatus1_lead_12_max))

Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_basic,cutoff) # MCC = 0.065
Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_int,cutoff)   # MCC = 0.352
Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_adv,cutoff)   # MCC = ?

# --- Investigate Problem with Advanced Model
describe(datCredit_valid$prob_adv) # Some NA probability scores
describe(logitMod_Adv$fitted.values)
LookAt<-datCredit_valid[1:10,] # Seems as if the problem is with the input fields having NA's
LookAt2<-datCredit_train[1:10,]

# --- Remove g0_Delinq_SD_4, g0_Delinq_SD_6, NewLoans_Aggr_Prop_1, NewLoans_Aggr_Prop_3, NewLoans_Aggr_Prop_5, InterestRate_Margin_Aggr_Med_3
logitMod_Adv2 <- glm(DefaultStatus1_lead_12_max ~ Term + Balance + Principal + InterestRate_Margin_imputed_mean + 
                       g0_Delinq + M_DTI_Growth_6 + M_DTI_Growth_9 + M_Emp_Growth_9 + 
                       M_RealGDP_Growth_12 + PrevDefaults + TimeInPerfSpell + g0_Delinq_Num + 
                       slc_acct_roll_ever_24_imputed_mean + 
                       slc_acct_arr_dir_3 + slc_past_due_amt_imputed_med + slc_acct_pre_lim_perc_imputed_med + 
                       AgeToTerm_Aggr_Mean, data=datCredit_train, family="binomial")
datCredit_valid[, prob_adv2 := predict(logitMod_Adv2, newdata = datCredit_valid, type="response")]
describe(datCredit_valid$prob_adv2) # No NA probability scores










# Observe MCC for different cutoffs
Sequence<-seq(0,1, by = 0.005)
MCC_Matrix<-matrix(NA,nrow=length(Sequence),ncol=3)
counter<-1
for(k in Sequence){
  MCC_Matrix[counter,1]<-Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_basic,k)
  MCC_Matrix[counter,2]<-Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_int,k)
  MCC_Matrix[counter,3]<-Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_adv2,k)
  counter<-counter+1
}

plot(Sequence,MCC_Matrix[,1],type="l",ylim=c(-0.1,0.45), xlab="Cutoff", ylab="MCC", main="MCC for various cutoffs")
lines(Sequence,MCC_Matrix[,2],col="red")
lines(Sequence,MCC_Matrix[,3], col="green")

legend(x = "topright",legend = c("Basic", "Intermediate", "Advance"), col = c("black","red","green"),lty=c(1,1,1))

# - Check at cutoff=0.3
Get_MCC(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_adv2,0.3) # MCC = 0.386

ConfM<-confusionMatrix(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_adv2,0.3)
rownames(ConfM)<-c("Predicted-","Predicted+")
colnames(ConfM)<-c("Actual-","Actual+")
ConfM

# - Test if function is correct
mcc(datCredit_valid$DefaultStatus1_lead_12_max,datCredit_valid$prob_adv2,0.3) # MCC = 0.386
