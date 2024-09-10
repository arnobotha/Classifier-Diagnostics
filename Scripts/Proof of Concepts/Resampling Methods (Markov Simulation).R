# =========================== RESAMPLING SCHEMES (MARKOV SIMULATION STUDY) ===================================
# Implement a 2-state markov chain towards simulating a typical credit dataset over time. Then 3 variants of
# a simple cross-validation resampling scheme is illustrated: basic, 1-way stratified, 2-way stratified
# ------------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha
# -----------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R | Only for plotting purposes, though uses imputeLastKnown() custom function as well
# ============================================================================================================




# ------- 0. Setup

# --- Setup R-packages
require(ggplot2)
require(scales)
require(extrafont)
require(RColorBrewer)
require(Hmisc) #for describe()
require(data.table)
require(dplyr)
require(lubridate)
require(rsample)
require(foreach); require(doParallel) # for multi-threaded computing
options(scipen=999)


# --- Simulation parameters
n <- 240000 # number of amortising loans to simulate using 2-state Markov chain
n_periods <- 60 # contractual period for amortising loan
(diag1 <- n/n_periods == round(n/n_periods)) # should be whole number to facilitate simulation process
if (!diag1) stop("Revise simulation parameters!")

# --- Generic parameters
dpi <- 180 # graphing
chosenFont <- "Cambria" # graphing
confLevel <- 0.95 # confidence interval


# --- Custom functions
# custom function for markov simulation with arguments for the initial state probability vector, transition matrix, and observation period
markov_sim <- function(p_vect, t_matrix, t){ 
  # creates a vector of 0s with length equal to that of observation period for an account - 'placeholders' for the generation of credit states in following steps
  credit_state <- numeric(t)
  # generate initial state
  credit_state[1] <- sample(1:2, 1, prob = p_vector) 
  # generate states thereafter
  for(i in 2:t){
    credit_state[i] <- sample(1:2, 1, prob = t_matrix[credit_state[i-1], ])
  }
  return(credit_state)
}


# --- Specify transition matrix for Markov chain-based simulation engine

# transition matrix - default state is absorbing ---- Xij(t) represents the probability of an account transitioning from state 'i' to 'j' over time period 't'
t_matrix <- matrix(c(0.995, 0.15, 0.005, 0.85), nrow = 2); t_matrix
rownames(t_matrix) <- c("from.performing", "from.default")
colnames(t_matrix) <- c("to.performing", "to.default")

# vector governing probability of initial state
p_vector <- c(1, 0)





# ------ 1. Create designed dataset using 2-state Markov chain
# Simulate the structure of a typical longitudinal credit dataset

# - Simulate a matrix of loan performances
# Yields matrix of n_periods rows (periods) x n columns (loans)
y_raw <- sapply(1:n, function(x) {markov_sim(p_vector, t_matrix, n_periods)})
colnames(y_raw) <- c(1:n) # account numbers
rownames(y_raw) <- paste0("T", c(1:n_periods)) # years under observation

# - Transform: 1 if account is performing; 0 if in default
y <- ifelse(y_raw >= 2, 1, 0) 

# - Loan Production period | Imposing an artificial cohort month date per loan
production_period <- 48; productionLag <- 24
(cohort_start <- rollback(Sys.Date()-months(production_period+productionLag), roll_to_first = T))
(date_v <- seq.Date(from=cohort_start, length.out=n_periods, by="1 month"))

# - Cohort date vector
# Assume loans are disbursed in equal measure over production period
cohort_date <- rep(Sys.Date(), n/n_periods)
for (t in 1:n_periods) {
  cohort_date[((t-1)*n/n_periods + 1):((t)*n/n_periods)] <- as.Date(rep(date_v[t], n/n_periods))
}
describe(cohort_date) # should contain no missings
if (any(is.na(cohort_date))) stop("Missingness found in cohort date vector!")



# --- Cast simulated credit dataset into a longitudinal format
# Multiple observations over time for each subject

# - Iterate across loan space using multi-threaded setup
ptm <- proc.time() #IGNORE: for computation time calculation
cl.port <- makeCluster(6); registerDoParallel(cl.port) # multi-threading setup
sim.data <- foreach(it=1:n, .combine='rbind', .verbose=F, .inorder=T, 
                         .packages=c('data.table')) %dopar%
  {

    # prepare credit dataset into the typical Subject-Period format
    prepDat <- data.table(LoanID=it, Counter=1:n_periods, CohortDate=cohort_date[it], 
                          Date=seq.Date(from=cohort_date[it], length.out=n_periods, by="1 month"), Default=y[,it]) 
  }  
stopCluster(cl.port); proc.time() - ptm



# --- Prepare simulated dataset
# - Set key for intrinsic ordering
setkey(sim.data, LoanID, Counter)

# - Create index using .N macro variable output internally by data.table
sim.data[, Index := 1:.N]

# - Create default flags by taking the max across a 12-month rolling window ahead in time | "Worst-ever" approach
# NOTE: Need to specify a (k+1)-window for the "frollapply()" function
# Uses the custom "imputeLastKnown" function defined in script 0
sim.data[, Default_max12 := imputeLastKnown(frollapply(x=Default, n=13, align="left", FUN=max)) , by=list(LoanID)]

# - Find suitable date-cut-offs
test <- sim.data[,list(N = .N),by=list(Date)][order(Date),]
describe(test$N); plot(test);
boundA <- test[which(N==max(N)),Date] - months(n_periods*.75)
boundB <- test[which(N==max(N)),Date] + months(n_periods*.75)
cat("\nChosen bounds: ", format.Date(boundA), " - ", format.Date(boundB))

# - Enforce sampling period
sim.data.use <- subset(sim.data, Date >= boundA & Date <= boundB)
sim.data.use[, .N] / sim.data[,.N] * 100 # >= 90% of data still used




# ------ 2. RESAMPLING SCHEME 
# Implement a basic/simple cross-validation resampling scheme | Validation-set approach

table(sim.data.use$Default_max12) %>% prop.table() # checking dataset imbalance
prop <- 0.6 # proportion split between training and validation set sizes, i.e., training contains <prop>% of original dataset


# ---- 1) Basic Random Sampling method

# - Apply resampling scheme with given sampling method
train_strat1 <- as.data.table(sim.data.use %>% slice_sample(prop=prop))
test_strat1 <- subset(sim.data.use, !(Index %in% train_strat1$Index)) # ensure non-overlapping occurs

# - Check representativeness | proportions should be similar
table(train_strat1$Default_max12) %>% prop.table()
table(test_strat1$Default_max12) %>% prop.table()

# - calculate 95% confidence interval for proportion (one sample, dichotomous variable)
mean_EventProp <- train_strat1[, sum(Default_max12,na.rm=T)/.N]
stdError_EventProp <- sqrt(mean_EventProp*(1-mean_EventProp)/train_strat1[, .N]) # sqrt(p*(1-p)/n) | large sample (>5 success/failures) under central limit theorem for binomial outcomes
margin_EventProp <- qnorm(1-(1-confLevel)/2) * stdError_EventProp
cat("\nMean event proportion with 95% confidence intervals in training sample: ", sprintf("%.2f", mean_EventProp*100) , "% +-", sprintf("%.2f", margin_EventProp*100), "%")

# - Combine two samples with full sample for graphing purposes
full.sample1 <- rbind(data.table(train_strat1, Sample="a_Training"), data.table(test_strat1, Sample="b_Validation"),
                      data.table(sim.data.use, Sample="c_Full"))

# Aggregate to period-level (reporting date)
plot.sample1 <- full.sample1[Default==0, list(EventRate_Time = sum(Default_max12,na.rm=T)/.N,N=.N), by=list(Sample, Date)]

# - DIAGNOSTIC: Compute average event rates over time | at a glance
plot.sample1[, list(Avg_EventRate = round(mean(EventRate_Time, na.rm=T)*100,digits=3)), by=list(Sample)]
# plot(plot.sample1[,list(Date,N)]) # Should have adequate sample size across chosen sampling period for both training + validation

# - Calculate aggregated MAEs
AEs1 <- plot.sample1[Sample %in% c("a_Training", "b_Validation"), 
                     list(AE = abs(diff(EventRate_Time))), by=list(Date)][order(Date),]
(MAE1 <- mean(AEs1$AE, na.rm=T))
AEs2 <- plot.sample1[Sample %in% c("c_Full", "a_Training"), 
                     list(AE = abs(diff(EventRate_Time))), by=list(Date)][order(Date),]
(MAE2 <- mean(AEs2$AE, na.rm=T))
AEs3 <- plot.sample1[Sample %in% c("c_Full", "b_Validation"), 
                     list(AE = abs(diff(EventRate_Time))), by=list(Date)][order(Date),]
(MAE3 <- mean(AEs3$AE, na.rm=T))

### UNIT TEST: expect equality in a sample of calculated values, i.e., TRUE-value
test <- plot.sample1[Sample %in% c("a_Training", "b_Validation"), list(Sample, Date, EventRate_Time)][order(Date,Sample),]
all.equal(AEs1[1:2, AE], c(abs(test[2,EventRate_Time] - test[1,EventRate_Time]), abs(test[4,EventRate_Time] - test[3,EventRate_Time])))
### RESULTS: If TRUE, then safe and equal, whereupon the unit test succeeds

# - calculate TTC event rate and confidence interval for one sample, dichotomous outcome (population proportion)
mean_EventRate <- plot.sample1[Sample == "a_Training", mean(EventRate_Time, na.rm=T)]
stdError_EventRate <- plot.sample1[Sample == "a_Training", sd(EventRate_Time, na.rm=T)] / plot.sample1[Sample == "a_Training", .N]
margin_EventRate <- qnorm(1-(1-confLevel)/2) * stdError_EventRate
cat("\nMean event rate with 95% confidence intervals in training sample: ", sprintf("%.2f", mean_EventRate*100) , "% +-", sprintf("%.3f", margin_EventRate*100), "%")

# - Graphing parameters
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_Training"=bquote(italic(A)[t]*": Training Set "*italic(D[T])), 
             "b_Validation"=bquote(italic(B)[t]*": Validation Set "*italic(D[V])), 
             "c_Full"=bquote(italic(C)[t]*": Full Set "*italic(D)))
vShape <- c(16,4,NA); linetype.v <- c("dashed", "dotted", "solid")
aggrSeries <- min(plot.sample1$EventRate_Time, na.rm=T)

# - Create time plot (event rate) by sample
(plot.sample_rep1 <- ggplot(plot.sample1, aes(x=Date, y=EventRate_Time)) + theme_minimal() +
    theme(text=element_text(family=chosenFont),legend.position="bottom") + 
    labs(x="Reporting date (ccyymm)", y=bquote("Event rate  "*italic(r(t,bar(D))))) + 
    # Main geoms
    geom_line(aes(colour=Sample, linetype=Sample, linewidth=Sample)) + 
    geom_point(aes(shape=Sample, colour=Sample), size=2) + 
    # Annotations
    annotate("text", x=boundA+months(15), y=aggrSeries*1.025, size=3, family=chosenFont,
             label=paste0("'TTC-mean '*E(italic(A[t]))*': ", sprintf("%.3f", mean_EventRate*100), "% ± ", 
                          sprintf("%.3f", margin_EventRate*100),"%'"), parse=T) +     
    annotate("text", x=boundA+months(14), y=aggrSeries*1.017, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.3f", MAE1*100), "%'"), parse=T) + 
    annotate("text", x=boundA+months(14), y=aggrSeries*1.012, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(C[t])*': ", sprintf("%.3f", MAE2*100), "%'"), parse=T) + 
    annotate("text", x=boundA+months(14), y=aggrSeries*1.007, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(B[t])*' and '*italic(C[t])*': ", sprintf("%.3f", MAE3*100), "%'"), parse=T) + 
    # Scale & facet options
    scale_colour_manual(name=bquote("Sample "*italic(bar(D))), values=vCol, labels=vLabel) + 
    scale_shape_manual(name=bquote("Sample "*italic(bar(D))), values=vShape, labels=vLabel) + 
    scale_linetype_manual(name=bquote("Sample "*italic(bar(D))), values=linetype.v, labels=vLabel) + 
    scale_linewidth_manual(name=bquote("Sample "*italic(bar(D))), values=c(0.75,0.75,0.5), labels=vLabel) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent)
)

# - Save graph
ggsave(plot.sample_rep1, file=paste0(genFigPath, "Fig1-Sample_Rep-rndSampling.png"), width=1200/dpi, height=1100/dpi, dpi=dpi, bg="white")




# ---- 2) 1-way stratified random sampling

# - Apply resampling scheme with given sampling method
train_strat2 <- as.data.table(sim.data.use %>% group_by(Default_max12) %>% slice_sample(prop=prop))
test_strat2 <- subset(sim.data.use, !(Index %in% train_strat2$Index)) # ensure non-overlapping occurs

# - Check representativeness | proportions should be similar
table(train_strat2$Default_max12) %>% prop.table()
table(test_strat2$Default_max12) %>% prop.table()

# - calculate 95% confidence interval for proportion (one sample, dichotomous variable)
mean_EventProp <- train_strat2[, sum(Default_max12,na.rm=T)/.N]
stdError_EventProp <- sqrt(mean_EventProp*(1-mean_EventProp)/train_strat2[, .N]) # sqrt(p*(1-p)/n) | large sample (>5 success/failures) under central limit theorem for binomial outcomes
margin_EventProp <- qnorm(1-(1-confLevel)/2) * stdError_EventProp
cat("\nMean event proportion with 95% confidence intervals in training sample: ", sprintf("%.2f", mean_EventProp*100) , "% +-", sprintf("%.2f", margin_EventProp*100), "%")

# - Combine two samples with full sample for graphing purposes
full.sample2 <- rbind(data.table(train_strat2, Sample="a_Training"), data.table(test_strat2, Sample="b_Validation"),
                      data.table(sim.data.use, Sample="c_Full"))

# - Aggregate to period-level (reporting date)
plot.sample2 <- full.sample2[Default==0, list(EventRate_Time = sum(Default_max12,na.rm=T)/.N, N=.N), by=list(Sample, Date)] 

# - DIAGNOSTIC: Compute average event rates over time 
plot.sample2[, list(Avg_EventRate = round(mean(EventRate_Time, na.rm=T)*100,digits=3)), by=list(Sample)]
# plot(plot.sample2[,list(Date,N)]) 

# - Calculate aggregated MAE
AEs1 <- plot.sample2[Sample %in% c("a_Training", "b_Validation"), 
                     list(AE = abs(diff(EventRate_Time))), by=list(Date)]
(MAE1 <- mean(AEs1$AE, na.rm=T))
AEs2 <- plot.sample2[Sample %in% c("c_Full", "a_Training"), 
                     list(AE = abs(diff(EventRate_Time))), by=list(Date)]
(MAE2 <- mean(AEs2$AE, na.rm=T))
AEs3 <- plot.sample2[Sample %in% c("c_Full", "b_Validation"), 
                     list(AE = abs(diff(EventRate_Time))), by=list(Date)]
(MAE3 <- mean(AEs3$AE, na.rm=T))

# - calculate TTC event rate and confidence interval for one sample, dichotomous outcome (population proportion)
mean_EventRate <- plot.sample2[Sample == "a_Training", mean(EventRate_Time, na.rm=T)]
stdError_EventRate <- plot.sample2[Sample == "a_Training", sd(EventRate_Time, na.rm=T)] / plot.sample2[Sample == "a_Training", .N]
margin_EventRate <- qnorm(1-(1-confLevel)/2) * stdError_EventRate
cat("\nMean event rate with 95% confidence intervals in training sample: ", sprintf("%.2f", mean_EventRate*100) , "% +-", sprintf("%.3f", margin_EventRate*100), "%")

# - Graphing parameters
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_Training"=bquote(italic(A)[t]*": Training Set "*italic(D[T])), 
             "b_Validation"=bquote(italic(B)[t]*": Validation Set "*italic(D[V])), 
             "c_Full"=bquote(italic(C)[t]*": Full Set "*italic(D)))
vShape <- c(16,4,NA); linetype.v <- c("dashed", "dotted", "solid")
aggrSeries <- min(plot.sample2$EventRate_Time, na.rm=T)

# - Create time plot (event rate) by sample
(plot.sample_rep2 <- ggplot(plot.sample2, aes(x=Date, y=EventRate_Time)) + theme_minimal() +
    geom_line(aes(colour=Sample, linetype=Sample, linewidth=Sample)) + 
    geom_point(aes(shape=Sample, colour=Sample), size=2) + 
    labs(x="Reporting date (ccyymm)", y=bquote("Event rate  "*italic(r(t,bar(D))))) + 
    annotate("text", x=boundA+months(22), y=aggrSeries*1.018, size=3, family=chosenFont,
             label=paste0("'TTC-mean '*E(italic(A[t]))*': ", sprintf("%.3f", mean_EventRate*100), "% ± ", 
                          sprintf("%.3f", margin_EventRate*100),"%'"), parse=T) +     
    annotate("text", x=boundA+months(21), y=aggrSeries*1.012, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.3f", MAE1*100), "%'"), parse=T) + 
    annotate("text", x=boundA+months(21), y=aggrSeries*1.008, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(C[t])*': ", sprintf("%.3f", MAE2*100), "%'"), parse=T) + 
    annotate("text", x=boundA+months(21), y=aggrSeries*1.004, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(B[t])*' and '*italic(C[t])*': ", sprintf("%.3f", MAE3*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position="bottom") + 
    scale_colour_manual(name=bquote("Sample "*italic(bar(D))), values=vCol, labels=vLabel) + 
    scale_shape_manual(name=bquote("Sample "*italic(bar(D))), values=vShape, labels=vLabel) + 
    scale_linetype_manual(name=bquote("Sample "*italic(bar(D))), values=linetype.v, labels=vLabel) + 
    scale_linewidth_manual(name=bquote("Sample "*italic(bar(D))), values=c(0.75,0.75,0.5), labels=vLabel) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent)
)

# - Save graph
ggsave(plot.sample_rep2, file=paste0(genFigPath, "Fig2-Sample_Rep-1wayStrat.png"), width=1200/dpi, height=1100/dpi, dpi=dpi, bg="white")





# ---- 3) 2-way stratified random sampling

# DIAGNOSTIC: Get default rate and associated summaries at first period | All data
sim.data.use %>% group_by(Default, Date) %>% summarise(n=n(),defaults=sum(Default_max12[Default==0])) %>% 
  mutate(DefaultRate = defaults/n) %>% filter(Date==boundA)

# - Apply resampling scheme with given sampling method
# Inspiration drawn from https://stackoverflow.com/questions/23479512/stratified-random-sampling-from-data-frame
train_strat3 <- as.data.table(sim.data.use %>% group_by(Default_max12, Date) %>% slice_sample(prop=prop))
test_strat3 <- subset(sim.data.use, !(Index %in% train_strat3$Index)) # ensure non-overlapping occurs

# DIAGNOSTIC: Get default rate and associated summaries at first period | Training data
train_strat3 %>% group_by(Default, Date) %>% summarise(n=n(),defaults=sum(Default_max12[Default==0])) %>% 
  mutate(DefaultRate = defaults/n) %>% filter(Date==boundA)
# DIAGNOSTIC: Get default rate and associated summaries at first period | Validation data
test_strat3 %>% group_by(Default, Date) %>% summarise(n=n(),defaults=sum(Default_max12[Default==0])) %>% 
  mutate(DefaultRate = defaults/n) %>% filter(Date==boundA)

# - Check representativeness | proportions should be similar
table(train_strat3$Default_max12) %>% prop.table()
table(test_strat3$Default_max12) %>% prop.table()

# - calculate 95% confidence interval for proportion (one sample, dichotomous variable)
mean_EventProp <- train_strat3[, sum(Default_max12,na.rm=T)/.N]
stdError_EventProp <- sqrt(mean_EventProp*(1-mean_EventProp)/train_strat3[, .N]) # sqrt(p*(1-p)/n) | large sample (>5 success/failures) under central limit theorem for binomial outcomes
margin_EventProp <- qnorm(1-(1-confLevel)/2) * stdError_EventProp
cat("\nMean event proportion with 95% confidence intervals in training sample: ", sprintf("%.2f", mean_EventProp*100) , "% +-", sprintf("%.2f", margin_EventProp*100), "%")

# - Combine two samples with full sample for graphing purposes
full.sample3 <- rbind(data.table(train_strat3, Sample="a_Training"), data.table(test_strat3, Sample="b_Validation"),
                      data.table(sim.data.use, Sample="c_Full"))

# - Aggregate to period-level (reporting date)
plot.sample3 <- full.sample3[Default==0, list(EventRate_Time = sum(Default_max12,na.rm=T)/.N, N=.N), by=list(Sample, Date)]

# - DIAGNOSTIC: Compute average event rates over time 
plot.sample3[, list(Avg_EventRate = round(mean(EventRate_Time, na.rm=T)*100,digits=3)), by=list(Sample)]
# plot(plot.sample3[,list(Date,N)]) 

# - Calculate aggregated MAE
# - Calculate aggregated MAE
AEs1 <- plot.sample3[Sample %in% c("a_Training", "b_Validation"), 
                     list(AE = abs(diff(EventRate_Time))), by=list(Date)]
(MAE1 <- mean(AEs1$AE, na.rm=T))
AEs2 <- plot.sample3[Sample %in% c("c_Full", "a_Training"), 
                     list(AE = abs(diff(EventRate_Time))), by=list(Date)]
(MAE2 <- mean(AEs2$AE, na.rm=T))
AEs3 <- plot.sample3[Sample %in% c("c_Full", "b_Validation"), 
                     list(AE = abs(diff(EventRate_Time))), by=list(Date)]
(MAE3 <- mean(AEs3$AE, na.rm=T))

# - calculate TTC event rate and confidence interval for one sample, dichotomous outcome (population proportion)
mean_EventRate <- plot.sample3[Sample == "a_Training", mean(EventRate_Time, na.rm=T)]
stdError_EventRate <- plot.sample3[Sample == "a_Training", sd(EventRate_Time, na.rm=T)] / plot.sample3[Sample == "a_Training", .N]
margin_EventRate <- qnorm(1-(1-confLevel)/2) * stdError_EventRate
cat("\nMean event rate with 95% confidence intervals in training sample: ", sprintf("%.2f", mean_EventRate*100) , "% +-", sprintf("%.3f", margin_EventRate*100), "%")

# - Graphing parameters
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_Training"=bquote("Training Set "*italic(D[T])), 
             "b_Validation"=bquote("Validation Set "*italic(D[V])), 
             "c_Full"=bquote("Full Set "*italic(D)))
vShape <- c(16,4,NA); linetype.v <- c("dashed", "dotted", "solid")
aggrSeries <- max(plot.sample3$EventRate_Time, na.rm=T)

# - Create time plot (event rate) by sample
(plot.sample_rep3 <- ggplot(plot.sample3, aes(x=Date, y=EventRate_Time)) + theme_minimal() +
    geom_line(aes(colour=Sample, linetype=Sample, linewidth=Sample)) + 
    geom_point(aes(shape=Sample, colour=Sample), size=2) + 
    labs(x="Reporting date (ccyymm)", y=bquote("Event rate  "*italic(r(t,bar(D))))) + 
    annotate("text", x=boundA+months(15), y=aggrSeries*0.96, size=3, family=chosenFont,
             label=paste0("'TTC-mean '*E(italic(A[t]))*': ", sprintf("%.3f", mean_EventRate*100), "% ± ", 
                          sprintf("%.3f", margin_EventRate*100),"%'"), parse=T) +     
    annotate("text", x=boundA+months(14), y=aggrSeries*0.95, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.3f", MAE1*100), "%'"), parse=T) + 
    annotate("text", x=boundA+months(14), y=aggrSeries*0.947, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(C[t])*': ", sprintf("%.3f", MAE2*100), "%'"), parse=T) + 
    annotate("text", x=boundA+months(14), y=aggrSeries*0.944, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(B[t])*' and '*italic(C[t])*': ", sprintf("%.3f", MAE3*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position="bottom") + 
    scale_colour_manual(name=bquote("Sample "*italic(bar(D))), values=vCol, labels=vLabel) + 
    scale_shape_manual(name=bquote("Sample "*italic(bar(D))), values=vShape, labels=vLabel) + 
    scale_linetype_manual(name=bquote("Sample "*italic(bar(D))), values=linetype.v, labels=vLabel) + 
    scale_linewidth_manual(name=bquote("Sample "*italic(bar(D))), values=c(0.75,0.75,0.5), labels=vLabel) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent)
)

# - Save graph
ggsave(plot.sample_rep3, file=paste0(genFigPath, "Fig3-Sample_Rep-2wayStrat.png"), width=1200/dpi, height=1100/dpi, dpi=dpi, bg="white")







# ---- 4) Bootstrapped resampling | Comparisons
bootstrap.n <- 750

# --- Random Sampling
# - Re-perform sampling to get an average MAE calculation
ptm <- proc.time() #IGNORE: for computation time calculation
cl.port <- makeCluster(6); registerDoParallel(cl.port) # multi-threading setup
MAE1_v <- foreach(it=1:bootstrap.n, .combine='rbind', .verbose=F, .inorder=T, 
                  .packages=c('data.table', 'dplyr')) %dopar%
  {
    train_strat1 <- as.data.table(sim.data.use %>% slice_sample(prop=prop))
    test_strat1 <- subset(sim.data.use, !(Index %in% train_strat1$Index)) # ensure non-overlapping occurs
    
    # combine two samples for graphing purposes
    full.sample1 <- rbind(data.table(train_strat1, Sample="Training"), data.table(test_strat1, Sample="Validation"))
    
    # aggregate to period-level (reporting date)
    plot.sample1 <- full.sample1[Default==0, list(EventRate_Time = sum(Default_max12,na.rm=T)/.N), by=list(Sample, Date)]
    
    # calculate aggregated MAE
    AEs1 <- plot.sample1[, list(AE = abs(diff(EventRate_Time))), by=list(Date)]
    return(mean(AEs1$AE, na.rm=T))
  }  
stopCluster(cl.port); proc.time() - ptm


# --- 1-way Stratified Sampling
# - Re-perform sampling to get an average MAE calculation
ptm <- proc.time() #IGNORE: for computation time calculation
cl.port <- makeCluster(6); registerDoParallel(cl.port) # multi-threading setup
MAE2_v <- foreach(it=1:bootstrap.n, .combine='rbind', .verbose=F, .inorder=T, 
                  .packages=c('data.table', 'dplyr')) %dopar%
  {
    train_strat2 <- as.data.table(sim.data.use %>% group_by(Default_max12) %>% slice_sample(prop=prop))
    test_strat2 <- subset(sim.data.use, !(Index %in% train_strat2$Index)) # ensure non-overlapping occurs
    
    # combine two samples for graphing purposes
    full.sample2 <- rbind(data.table(train_strat2, Sample="Training"), data.table(test_strat2, Sample="Validation"))
    
    # aggregate to period-level (reporting date)
    plot.sample2 <- full.sample2[Default==0, list(EventRate_Time = sum(Default_max12,na.rm=T)/.N), by=list(Sample, Date)]
    
    # calculate aggregated MAE
    AEs2 <- plot.sample2[, list(AE = abs(diff(EventRate_Time))), by=list(Date)]
    return(mean(AEs2$AE, na.rm=T))
  }  
stopCluster(cl.port); proc.time() - ptm


# --- 2-way Stratified Sampling
# - Re-perform sampling to get an average MAE calculation
ptm <- proc.time() #IGNORE: for computation time calculation
cl.port <- makeCluster(6); registerDoParallel(cl.port) # multi-threading setup
MAE3_v <- foreach(it=1:bootstrap.n, .combine='rbind', .verbose=F, .inorder=T, 
                  .packages=c('data.table','dplyr')) %dopar%
  {
    train_strat3 <- as.data.table(sim.data.use %>% group_by(Default_max12, Date) %>% slice_sample(prop=prop))
    test_strat3 <- subset(sim.data.use, !(Index %in% train_strat3$Index)) # ensure non-overlapping occurs
    
    # combine two samples for graphing purposes
    full.sample3 <- rbind(data.table(train_strat3, Sample="Training"), data.table(test_strat3, Sample="Validation"))
    
    # aggregate to period-level (reporting date)
    plot.sample3 <- full.sample3[Default==0, list(EventRate_Time = sum(Default_max12,na.rm=T)/.N), by=list(Sample, Date)]
    
    # calculate aggregated MAE
    AEs3 <- plot.sample3[, list(AE = abs(diff(EventRate_Time))), by=list(Date)]
    return(mean(AEs3$AE, na.rm=T))
  }  
stopCluster(cl.port); proc.time() - ptm


# --- Final Comparisons
mean(MAE1_v, na.rm=T); hist(MAE1_v, breaks="FD")
mean(MAE2_v, na.rm=T); hist(MAE2_v, breaks="FD"); (mean(MAE2_v, na.rm=T)/mean(MAE1_v, na.rm=T) - 1)*100
mean(MAE3_v, na.rm=T); hist(MAE3_v, breaks="FD"); (mean(MAE3_v, na.rm=T)/mean(MAE1_v, na.rm=T) - 1)*100
### NoTE: central limit theorem in action since the sampling distribution of MAE_V will tend towards Normal
# MAE1: 0.001491; (120k accounts) | 0.001062 (240k accounts)
# MAE2: 0.001487; (120k accounts) | 0.001058 (240k accounts) [0.3% improvement]
# MAE3: 0.000831; (120k accounts) | 0.000597 (240k accounts) [44% improvement]

