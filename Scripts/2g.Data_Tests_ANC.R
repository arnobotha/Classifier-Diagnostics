### Check Issue With Age_Adj Variable
		   

##### datCredit_real[, list(NewLoans_Aggr_Prop = sum(Age_Adj==1, na.rm=T)/.N), by=list(Date)] #####

# - Load in Dataset that was packed away in script 2b (line 375)
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final1c"), tempPath)

# - Analyze the Age_Adj variable
describe(datCredit_real[,Age_Adj])
### RESULTS: 1057772 of the 50938360 observations (2.077%) has missing Age_Adj

# - Analyze the New_Ind variable
describe(datCredit_real[,New_Ind])
### RESULTS 693811 of the 50938360 observations (1.36%) has missing New_Ind

# - Calculate the minimum Age_Adj by Date
MinAge_ByDate<-data.table(datCredit_real[ExclusionID==0,min(Age_Adj,na.rm = T),by=list(Date)])
colnames(MinAge_ByDate)<-c("Date","MinAgeAdj")

# - Get the number of dates that does not have a minimum Age_Adj of 1
cat("Nr of Dates with no loans having Age_Adj equal to one: ",sum(MinAge_ByDate[,MinAgeAdj]!=1),"\n")
### RESULTS: All dates has minimum Age_Adj of 1
### CONCLUSION: This implies that the loans having Age_Adj = 1 are excluded in further data preperation steps

# - Visually inspect
plot(MinAge_ByDate[,Date],MinAge_ByDate[,MinAgeAdj],type="l",xlab="Date",ylab="Minimum Age Adjusted", main="Minimum Age Adjusted By Date")

# - Load in Dataset that was packed away in script 3b (line 160)
if (!exists('datCredit_smp')) unpack.ffdf(paste0(genPath,"creditdata_final4c"), tempPath)

describe(datCredit_smp[,Age_Adj])
# No missing Age_Adj
describe(datCredit_smp[,New_Ind])
# No missing New_Ind

# - Calculate the minimum Age_Adj by Date
MinAge_ByDate2<-data.table(datCredit_smp[,min(Age_Adj,na.rm = T),by=list(Date)])
colnames(MinAge_ByDate2)<-c("Date","MinAgeAdj")
cat("Nr of Dates with no loans having Age_Adj equal to one: ",sum(MinAge_ByDate2[,MinAgeAdj]!=1),"\n")

# - Look at the months with MinAgeAdj not equal to 1
MinAge_ByDate2[MinAgeAdj!=1,]
# - Visually inspect
plot(MinAge_ByDate2[,Date],MinAge_ByDate2[,MinAgeAdj],type="l",xlab="Date",ylab="Minimum Age Adjusted", main="Minimum Age Adjusted By Date",ylim=c(1,12))
### RESULTS: 13 dates has minimum Age_Adj not equal to 1. These are the last 12-months and 2020-04-30
### CONCLUSION: Last 12 months excluded since we can't look into the future.
###             But why do we then have other other observations during these dates, where Age_Adj ne 1?
###             Answer: Because of imputeLastKnown function

# --- Inspect 2020-04-30 Date on datCredit_smp
describe(datCredit_smp[Date=="2020-04-30",Age_Adj])

# - 2020-03-31 has 80 Cases, where Age_Adj=1
datCredit_smp[Date=="2020-03-31" & Age_Adj==1, list(LoanID,Date,Age_Adj,New_Ind,Counter,Max_Counter,Date_Origination)]

# - 2020-04-30 has 0 Cases, where Age_Adj=1
datCredit_smp[Date=="2020-04-30" & Age_Adj==1, list(LoanID,Date,Age_Adj,New_Ind,Counter,Max_Counter,Date_Origination)]

# - 2020-05-31 has 11 Cases, where Age_Adj=1
datCredit_smp[Date=="2020-05-31" & Age_Adj==1, list(LoanID,Date,Age_Adj,New_Ind,Counter,Max_Counter,Date_Origination)]

# --- Inspect 2020-04-30 Date on datCredit_real
# - 2020-04-30 has 33 Cases, where New_Ind=1 and Age_Adj=1
datCredit_real[Date=="2020-04-30" & Age_Adj==1, list(LoanID,Date,Age_Adj,New_Ind,Counter,Max_Counter,Date_Origination)]
### CONCLUSION: It seems that none of the 33 Cases that were present in the full dataset were subsampled into the subsampled set

# - Create and compare the NewLoans_Aggr_Prop variables on each dataset
dat_NewLoans_Aggr_Full <- datCredit_real[, list(NewLoans_Aggr_Prop = sum(Age_Adj==1, na.rm=T)/.N), by=list(Date)]
dat_NewLoans_Aggr_Samp <- datCredit_smp[, list(NewLoans_Aggr_Prop = sum(Age_Adj==1, na.rm=T)/.N), by=list(Date)]

# - Sort to ensure logical time series plot
dat_NewLoans_Aggr_Full<-dat_NewLoans_Aggr_Full[order(dat_NewLoans_Aggr_Full$Date),]
dat_NewLoans_Aggr_Samp<-dat_NewLoans_Aggr_Samp[order(dat_NewLoans_Aggr_Samp$Date),]

# - Plot to observe differences
plot(dat_NewLoans_Aggr_Full[,Date], dat_NewLoans_Aggr_Full[,NewLoans_Aggr_Prop], type="l",ylab="New Loans Aggr Prop",xlab="Date")
lines(dat_NewLoans_Aggr_Samp[,Date], dat_NewLoans_Aggr_Samp[,NewLoans_Aggr_Prop], type="l",col="red")
legend("topright", c("Full","SubSamp"),pch=19, col=c("black","red"))

# --- ANOTHER ISSUE:
(ArrearsMax<-data.table(datCredit_real[,max(Arrears),by=list(Date)]))
colnames(ArrearsMax)<-c("Date","MaxArrears")

ArrearsMax[MaxArrears==0,]








#rm(datCredit_real)
#if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4a"), tempPath)

#datCredit_smp[Date>="2022-01-31" & DefaultStatus1==0 & DefaultStatus1_lead_12_max==1,.N]
#datCredit_smp[Date>="2022-01-31" & DefaultStatus1==0 & DefaultStatus1_lead_12_max==1,LoanID]

#LOOK1<-datCredit_real[LoanID==3000011253678,list(Date,DefaultStatus1,DefaultStatus1_lead_12_max)]
#LOOK2<-datCredit_real[LoanID==3000015769202,list(Date,DefaultStatus1,DefaultStatus1_lead_12_max)]