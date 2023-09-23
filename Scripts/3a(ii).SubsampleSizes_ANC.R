# =============================== RESAMPLING SCHEMES FOR SURVIVAL MODELS ===============================
# Determining the effect of a wide range of subsample sizes within a resampling scheme
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Default survival modelling
# SCRIPT AUTHOR(S): Dr Arno Botha

# DESCRIPTION:
# This ancillary & exploratory script iterates across a given vector of subsample sizes towards
# resampling data into a basic cross-validation setup (training:validation), using 2-way stratified 
# sampling. Each chosen size is executed multiple times over various seed values to account for randomness
# in a broader Monte Carlo setup. Thereafter, various error measures are calculated within each iteration,
# whereupon these error values are appropriately aggregated and graphed into a single cohesive graph.
# ------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup
#   - 2f.Data_Fusion1

# -- Inputs:
#   - datCredit_real | Prepared from script 2d.
#
# -- Outputs:
#   - Difference graph across subsample sizes, where 'difference' is the value of an error measure 
#       between prior probabilities across resampled sets (full:training)
# ------------------------------------------------------------------------------------------------------




# ------ 1. Preliminaries

# - Load in Dataset
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4a"), tempPath)

# - Subsampling & resampling parameters
smp_frac <- 0.7 # sampling fraction for resampling scheme
stratifiers <- c("DefaultStatus1_lead_12_max", "Date") # Must at least include target variable used in graphing event rate
targetVar <- "DefaultStatus1_lead_12_max"
currStatusVar <- "DefaultStatus1"
timeVar <- "Date"

# - Subset given dataset accordingly; an efficiency enhancement
datCredit <- subset(datCredit_real, select=unique(c(stratifiers,targetVar,currStatusVar,timeVar)))
rm(datCredit_real); gc()

# - Calculate prior probability of default event over all time on population. 
# NOTE: Precalculating this is merely a coding optimisation
prior_pop <- datCredit[, list(Target=get(targetVar))][Target==1,.N] / datCredit[,.N] 

# - Calculate 12-month conditional default rate on population.
# NOTE: Precalculating this is merely a coding optimisation
def_StartDte <- min(datCredit[,get(timeVar)], na.rm=T)
def_EndDte <- max(datCredit[,get(timeVar)], na.rm=T)
maxDate <- def_EndDte - years(1)
eventRate_pop <- datCredit[, list(Target=get(targetVar), 
                                       Status=get(currStatusVar), Time=get(timeVar))][Status==0, list(EventRate = sum(Target, na.rm=T)/.N),
                    by=list(Time)][Time >= def_StartDte & Time <= maxDate, ][order(Time), EventRate]
plot(eventRate_pop, type="b")

# - General
cpu.threads <- 6
confLevel <- 0.95

# - Iteration parameters
smp_size_v <- c(100000,150000,200000,250000,375000,500000,625000,750000,875000,1000000,1250000,1500000,1750000,2000000,
                2500000,3000000,3500000,4000000,4500000,5000000,6000000,7000000,8000000,9000000,10000000,12500000,15000000,20000000)
seed_v <- c(1:100)



# ------ 2. Subsampled resampling scheme: basic cross-validation with random sampling

# --- Defines function for applying a subsampled resampling scheme given parameters on given data
# This function serves as an "outer job" to be called within a multithreaded environment
# - Inputs: smp_size: Subsample size; smp_frac: sampling fraction for resmpling scheme;
# stratifiers: vector of stratification field names for n-way stratified sampling inner technique;
# targetVar: outcome field name within cross-sectional modelling (also first element of [stratifiers]);
# currStatusVar: current status field name within cross-sectional modelling for event rate calculations
# timeVar: field name of date for event rate calculations; seed: specific seed value
# prior_pop: pre-calculated prior probability within population for error measurement
# eventRate_pop: pre-calculated event rates over time within population for error measurement
# datGiven: given dataset from which to subsample and resample
subSmp_strat <- function(smp_size, smp_frac, stratifiers=NA, targetVar=NA, currStatusVar=NA, timeVar=NA, seed=123, 
                         prior_pop=NA, eventRate_pop=NA, datGiven) {
  
  # - Preliminaries: Error Checks
  if (any(is.na(stratifiers)) & is.na(targetVar)) { stop("Stratifiers and target variables are unspecified! Must at least include the target variable")}
  if (any(is.na(stratifiers)) & !is.na(targetVar)) { stratifiers <- targetVar}
  if (any(!is.na(stratifiers)) & is.na(targetVar)) { targetVar <- stratifiers[1] }
  
  # - Preliminaries: assignments
  smp_perc <- smp_size/datGiven[, .N] # Implied sampling fraction for downsampling step
  
  # - Downsample data into a set with a fixed size (using stratified sampling) before implementing resampling scheme
  set.seed(seed)
  datCredit_smp <- datGiven %>% group_by(vars(stratifiers)) %>% slice_sample(prop=smp_perc) %>% as.data.table()
  datCredit_smp[, Ind := 1:.N] # prepare for resampling scheme
  
  # - Implement resampling scheme using given main sampling fraction
  set.seed(seed)
  datCredit_train <- datCredit_smp %>% group_by(vars(stratifiers)) %>% slice_sample(prop=smp_frac) %>% as.data.table()
  datCredit_valid <- subset(datCredit_smp, !(Ind %in% datCredit_train$Ind)) %>% as.data.table()
  
  
  # --- Calculate error measure 1: Difference in prior probabilities between population and training (as subsampled + resampled)
  # Calculate prior probabilities within each relevant dataset, e.g., proportion of defaults across all time
  
  # - Population
  if (is.na(prior_pop)) {
    prior_pop <- datGiven[, list(Target=get(targetVar))][Target==1,.N] / datGiven[,.N] # population-level 
  }
  
  # - Subsampled + resampled training set
  prior_train <- datCredit_train[, list(Target=get(targetVar))][Target==1,.N] / datCredit_train[,.N] # training set level
  
  # - Compare population with training set using chosen error measure
  err_priorProb_AE <- abs(prior_pop - prior_train) # absolute error
  err_priorProb_SqrdErr <- (prior_pop - prior_train)^2 # squared error
  
  # - create output table (preliminary)
  datTemp <- data.table("SubSampleSize"=smp_size, "SampleFrac"=smp_frac, "Stratifiers"=paste(stratifiers, collapse="; "),
                        "Seed"=seed, "Err_PriorProb_AE" = err_priorProb_AE, "Err_PriorProb_SqrdErr" = err_priorProb_SqrdErr)
  
  
  # --- Calculate error measure 2: MAE between 2 time series of the event rate between population/training and training/validation (as subsampled + resampled)
  # NOTE: event rate is a 12-month conditional event rate, e.g., k-month default rate at t+k given that event has not happened at t
  # This is an optional error measure
  if (!is.na(currStatusVar) & !is.na(timeVar)) {

    # - Population
    if (any(is.na(eventRate_pop))) {
      def_StartDte <- min(datGiven[,get(timeVar)], na.rm=T)
      def_EndDte <- max(datGiven[,get(timeVar)], na.rm=T)
      maxDate <- def_EndDte - years(1)
      eventRate_pop <- datGiven[, list(Target=get(targetVar), 
                                             Status=get(currStatusVar), Time=get(timeVar))][Status==0, list(EventRate = sum(Target, na.rm=T)/.N),
               by=list(Time)][Time >= def_StartDte & Time <= maxDate,][order(Time), EventRate]
    }
    
    # - Subsampled + resampled training set
    def_StartDte <- min(datCredit_train[,get(timeVar)], na.rm=T)
    def_EndDte <- max(datCredit_train[,get(timeVar)], na.rm=T)
    maxDate <- def_EndDte - years(1)
    eventRate_train <- datCredit_train[, list(Target=get(targetVar), 
                                           Status=get(currStatusVar), Time=get(timeVar))][Status==0, list(EventRate = sum(Target, na.rm=T)/.N),
              by=list(Time)][Time >= def_StartDte & Time <= maxDate,][order(Time), EventRate]
    
    
    # - Subsampled + resampled validation set
    eventRate_valid <- datCredit_valid[, list(Target=get(targetVar), 
                                              Status=get(currStatusVar), Time=get(timeVar))][Status==0, list(EventRate = sum(Target, na.rm=T)/.N),
               by=list(Time)][Time >= def_StartDte & Time <= maxDate,][order(Time), EventRate]
    
    
    # - Compare event rates across different sets using chosen error measure
    err_eventRate_MAE_train <- mean(abs(eventRate_pop - eventRate_train), na.rm=t) # mean absolute error
    err_eventRate_MAE_valid <- mean(abs(eventRate_pop - eventRate_valid), na.rm=t) # mean absolute error
    err_eventRate_MAE_trainvalid <- mean(abs(eventRate_train - eventRate_valid), na.rm=t) # mean absolute error
    
    # - Append error value to output table
    datTemp <- data.table(datTemp, "Err_EventRate_PopTrain_MAE" = err_eventRate_MAE_train, 
                          "Err_EventRate_PopValid_MAE" = err_eventRate_MAE_valid,
                          "Err_EventRate_TrainValid_MAE" = err_eventRate_MAE_trainvalid)
  }

  # Return value of chosen error measure
  return(datTemp)
} # end of function


# - Testing function call
ptm <- proc.time() #IGNORE: for computation time calculation
subSmp_strat(smp_size=100000, smp_frac=smp_frac, seed=1, 
             stratifiers=stratifiers, targetVar=targetVar, currStatusVar=currStatusVar, timeVar=timeVar, 
             prior_pop=prior_pop, eventRate_pop=eventRate_pop, datGiven=datCredit)
proc.time() - ptm  #IGNORE: for computation time calculation



# --- Main Loop (outer function call)

cl.port <- makeCluster(cpu.threads)
registerDoParallel(cl.port)

cat(paste0("1 (", Sys.time(),"). Iterating across subsample sizes ..."),
    file="subsampleLoop.txt", append=F)

ptm <- proc.time() #IGNORE: for computation time calculation

# - Multithreaded looping procedure using the foreach-package
datResults <- foreach(it=1:(length(seed_v)*length(smp_size_v)), .combine='rbind', .verbose=F, .inorder=T, 
                         .packages=c('dplyr','data.table', 'lubridate', "scales"), .export=unique(c('subSmp_strat'))) %dopar%
  {
    # - Testing 
    #it <- 101
    
    # - Set indices
    iSeed <- (it-1) %% length(seed_v) + 1 # modulo operation
    iSize <- (it-1) %/% length(seed_v) + 1 # integer-valued division
    
    # - Iterate 
    temp <-subSmp_strat(smp_size=smp_size_v[iSize], smp_frac=smp_frac, seed=seed_v[iSeed],
                        stratifiers=stratifiers, targetVar=targetVar, currStatusVar=currStatusVar, timeVar=timeVar,
                        prior_pop=prior_pop, eventRate_pop=eventRate_pop, datGiven=datCredit)
    
    # - Reporting
    if (iSeed == length(seed_v)) {
      cat(paste0("\n2 (", Sys.time(),"). Subsample size: ", comma(smp_size_v[iSize]), " tested ",length(seed_v), " times."),
          file="subsampleLoop.txt", append=T) 
    }
    
    return(temp)
  }  

t <- proc.time() - ptm  #IGNORE: for computation time calculation
cat(paste0("\n3 (", Sys.time(),"). ForEach-loop done. Elapsed time: ", round(t[3]/60), " minutes."),
    file="subsampleLoop.txt", append=T)

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genObjPath, "subSampleSizes"), datResults); gc()
stopCluster(cl.port)




# --- Graphing

# - Load in Dataset
if (!exists('datResults')) unpack.ffdf(paste0(genObjPath,"subSampleSizes"), tempPath)


# - Aggregate to subsample size level
datGraph <- datResults[, list(PriorProb_MAE = mean(Err_PriorProb_AE , na.rm=T), PriorProb_MAE_SD = sd(Err_PriorProb_AE , na.rm=T),
                  PriorProb_RMSE = sqrt(sum(Err_PriorProb_SqrdErr, na.rm=T)/.N), PriorProb_RMSE_SE = sd(Err_PriorProb_SqrdErr, na.rm=T),
                  EventRate_PopTrain_MAE_Mean = mean(Err_EventRate_PopTrain_MAE, na.rm=T), EventRate_PopTrain_MAE_SD = sd(Err_EventRate_PopTrain_MAE, na.rm=T),
                  EventRate_TrainValid_MAE_Mean = mean(Err_EventRate_TrainValid_MAE, na.rm=T), EventRate_TrainValid_MAE_SD = sd(Err_EventRate_TrainValid_MAE, na.rm=T),N=.N),
           by=list(SubSampleSize)]

# - Create 95% confidence interval for point estimate (mean) : Population-training set comparison
datGraph[, EventRate_MAE_PopTrain_Mean_ErrMargin := (qnorm(1-(1-confLevel)/2)*EventRate_PopTrain_MAE_SD/sqrt(N))]
datGraph[, EventRate_MAE_PopTrain_Mean_lower := EventRate_PopTrain_MAE_Mean - EventRate_MAE_PopTrain_Mean_ErrMargin]
datGraph[, EventRate_MAE_PopTrain_Mean_upper := EventRate_PopTrain_MAE_Mean + EventRate_MAE_PopTrain_Mean_ErrMargin]

# - Create 95% confidence interval for point estimate (mean) : Training-Validation set comparison
datGraph[, EventRate_MAE_TrainValid_Mean_ErrMargin := (qnorm(1-(1-confLevel)/2)*EventRate_TrainValid_MAE_SD/sqrt(N))]
datGraph[, EventRate_MAE_TrainValid_Mean_lower := EventRate_TrainValid_MAE_Mean - EventRate_MAE_TrainValid_Mean_ErrMargin]
datGraph[, EventRate_MAE_TrainValid_Mean_upper := EventRate_TrainValid_MAE_Mean + EventRate_MAE_TrainValid_Mean_ErrMargin]

# - Create summary table for annotations within graph
datAnnotate <- datGraph[SubSampleSize %in% c(100000,150000,250000,375000,500000,750000,1000000,1250000, 1500000,2000000,
                                             3000000,4000000,5000000,10000000), 
                        list(`"Size "*italic(s)`=comma(SubSampleSize), 
                             `italic(E)(epsilon(italic(s)))*" for "*italic(D):italic(D[T])`=paste0(sprintf("%.3f", EventRate_PopTrain_MAE_Mean*100),"%"),
                             `95% CI`=paste0("± ",sprintf("%.4f",EventRate_MAE_PopTrain_Mean_ErrMargin*100),"%"),
                              `italic(E)(epsilon(italic(s)))*" for "*italic(D[T]):italic(D[V])`=paste0(sprintf("%.3f", EventRate_TrainValid_MAE_Mean*100),"%"),
                             `95% CI`=paste0("± ",sprintf("%.4f",EventRate_MAE_TrainValid_Mean_ErrMargin*100),"%"))]

# SCRATCH
plot(x=datGraph$SubSampleSize, y=datGraph$PriorProb_MAE, type="b")
plot(x=datGraph$SubSampleSize, y=datGraph$PriorProb_RMSE, type="b")
plot(x=datGraph$SubSampleSize, y=datGraph$EventRate_PopTrain_MAE_Mean, type="b")
plot(x=datGraph$SubSampleSize, y=datGraph$EventRate_TrainValid_MAE_Mean, type="b")
plot(x=datGraph$SubSampleSize, y=datGraph$EventRate_MAE_PopTrain_Mean_ErrMargin, type="b")

# - Pivot for graphing purposes: 2 different set comparisons using single error measure
datGraph2 <- pivot_longer(datGraph[,list(SubSampleSize, a_EventRate_PopTrain=EventRate_PopTrain_MAE_Mean, b_EventRate_TrainValid=EventRate_TrainValid_MAE_Mean)],
                          cols=a_EventRate_PopTrain:b_EventRate_TrainValid, names_to = "Set", values_to = "Value") %>% as.data.table()

datGraph2_lower <- pivot_longer(datGraph[,list(SubSampleSize, a_EventRate_PopTrain=EventRate_MAE_PopTrain_Mean_lower, b_EventRate_TrainValid=EventRate_MAE_TrainValid_Mean_lower)],
                          cols=a_EventRate_PopTrain:b_EventRate_TrainValid, names_to = "Set", values_to = "Value_Lower") %>% as.data.table()
datGraph2_upper <- pivot_longer(datGraph[,list(SubSampleSize, a_EventRate_PopTrain=EventRate_MAE_PopTrain_Mean_upper, b_EventRate_TrainValid=EventRate_MAE_TrainValid_Mean_upper)],
                                cols=a_EventRate_PopTrain:b_EventRate_TrainValid, names_to = "Set", values_to = "Value_Upper") %>% as.data.table()
datGraph2_margins <- merge(datGraph2_lower, datGraph2_upper, by=c("SubSampleSize", "Set"))
datGraph3 <- merge(datGraph2, datGraph2_margins, by=c("SubSampleSize", "Set"))

# Aesthetic engineering
datGraph3[, Facet_label := factor("'Comparison of 12-month default rate series across sets: '*(italic(D):italic(D[T]))*' ; '*(italic(D[T]):italic(D[V]))")]

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 180
col.v <- brewer.pal(10, "Paired")[c(10,8)]
fill.v <- brewer.pal(10, "Paired")[c(9,7)]
linetype.v <- c("solid", "dotted")
label.v <- list(expression(italic(D)*" vs "*italic(D[T])),
                expression(italic(D[T])*" vs "*italic(D[V])) )
label.v2 <- list(expression(italic(D)*" vs "*italic(D[T])),
                 expression(italic(D[T])*" vs "*italic(D[V])) )

# - Create main graph
(g1 <- ggplot(datGraph3, aes(x=SubSampleSize, y=Value, group=Set)) + theme_minimal() + 
  labs(x=bquote("Subsample size "*italic(s)*" = |"*italic(D[S])*"|"), y=bquote("Error measure value "*epsilon(italic(s))*" (%)")) + 
  theme(text=element_text(family=chosenFont),legend.position = "bottom",
        axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
  # main line graph with overlaid points
  geom_ribbon(aes(x=SubSampleSize, ymin=Value_Lower, ymax=Value_Upper, fill=Set), alpha=0.5) + 
  geom_line(aes(colour=Set, linetype=Set), linewidth=0.5) + 
  geom_point(aes(x=SubSampleSize, y=Value, colour=Set, shape=Set), size=1.3) + 
  #geom_errorbar(aes(x=SubSampleSize, ymin=Value_Lower, ymax=Value_Upper),width=1, position=position_dodge(0.1), linetype="solid", colour="gray") +
  annotate(geom="table", x=2500000, y=0.013, family=chosenFont, size=3,
             label=datAnnotate, parse=T) +
  # facets & scale options
  facet_grid(Facet_label ~ ., labeller=label_parsed) + 
  scale_colour_manual(name="Mean MAE", values=col.v, label=label.v) + 
  scale_shape_discrete(name="Mean MAE", label=label.v) + 
  scale_linetype_manual(name="Mean MAE", values=linetype.v, label=label.v) + 
  scale_fill_manual(name="95% CI for mean", values=fill.v, label=label.v2) + 
  scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
  scale_x_continuous(breaks=pretty_breaks(), label=label_comma(scale=0.000001, suffix="m"))
)

# - Save graph
ggsave(g1, file=paste0(genFigPath, "DefaultRates_SubSampleRates_PopTrain_Experiment.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")
