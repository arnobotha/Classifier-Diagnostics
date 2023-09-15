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

# - Calculate prior probability of default event over all time on population. 
# NOTE: Precalculating this is merely a coding optimisation
prior_pop <- datCredit_real[, list(Target=get(targetVar))][Target==1,.N] / datCredit_real[,.N] 

# - Calculate 12-month conditional default rate on population.
# NOTE: Precalculating this is merely a coding optimisation
def_StartDte <- min(datCredit_real[,get(timeVar)], na.rm=T)
def_EndDte <- max(datCredit_real[,get(timeVar)], na.rm=T)
maxDate <- def_EndDte - years(1)
eventRate_pop <- datCredit_real[, list(Target=get(targetVar), 
                                       Status=get(currStatusVar), Time=get(timeVar))][Status==0, list(EventRate = sum(Target, na.rm=T)/.N),
                    by=list(Time)][Time >= def_StartDte & Time <= maxDate,EventRate]

# - General
cpu.threads <- 6
confLevel <- 0.95

# - Iteration parameters
smp_size_v <- c(100000,150000,200000,250000,375000,500000,750000,1000000,1500000,2000000,3000000,4000000,5000000,7500000,10000000)
seed_v <- c(1:20)



# ------ 2. Subsampled resampling scheme: basic cross-validation with random sampling

# --- Defines function for applying a subsampled resampling scheme given parameters on given data
# This function serves as an "outer job" to be called within a multithreaded environment
subSmp_strat <- function(smp_size, smp_frac, stratifiers=NA, targetVar=NA, currStatusVar=NA, timeVar=NA, datGiven, seed=123, 
                         prior_pop=NA, eventRate_pop=NA) {
  
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
  datCredit_train <- datCredit_smp %>% group_by(vars(stratifiers)) %>% slice_sample(prop=smp_frac) %>% mutate(Sample="Train") %>% as.data.table()
  datCredit_valid <- subset(datCredit_smp, !(Ind %in% datCredit_train$Ind)) %>% mutate(Sample="Validation") %>% as.data.table()
  
  
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
  
  
  # --- Calculate error measure 2: MAE between 2 time series of the event rate between population and training (as subsampled + resampled)
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
               by=list(Time)][Time >= def_StartDte & Time <= maxDate,EventRate]
    }
    
    # - Subsampled + resampled training set
    def_StartDte <- min(datCredit_train[,get(timeVar)], na.rm=T)
    def_EndDte <- max(datCredit_train[,get(timeVar)], na.rm=T)
    maxDate <- def_EndDte - years(1)
    eventRate_train <- datCredit_train[, list(Target=get(targetVar), 
                                           Status=get(currStatusVar), Time=get(timeVar))][Status==0, list(EventRate = sum(Target, na.rm=T)/.N),
              by=list(Time)][Time >= def_StartDte & Time <= maxDate,EventRate]
    
    # - Compare population with training set using chosen error measure
    err_eventRate_MAE <- mean(abs(eventRate_pop - eventRate_train), na.rm=t) # mean absolute error
    
    # - Append error value to output table
    datTemp <- data.table(datTemp, "Err_EventRate_MAE" = err_eventRate_MAE)
  }

  # Return value of chosen error measure
  return(datTemp)
} # end of function


# - Testing call
subSmp_strat(smp_size=100000, smp_frac=smp_frac, datGiven=datCredit_real, seed=123, 
             stratifiers=stratifiers, targetVar=targetVar, currStatusVar=currStatusVar, timeVar=timeVar, 
             prior_pop=prior_pop, eventRate_pop=eventRate_pop)



# --- Main Loop (outer function call)

cl.port <- makeCluster(cpu.threads)
registerDoParallel(cl.port)

cat(paste0("1 (", Sys.time(),"). Iterating across subsample sizes ..."),
    file="subsampleLoop.txt", append=F)

ptm <- proc.time() #IGNORE: for computation time calculation

# - Multithreaded looping procedure using the foreach-package
datResults <- foreach(it=1:(length(smp_size_v)*length(seed_v)), .combine='rbind', .verbose=F, .inorder=T, 
                         .packages=c('dplyr','data.table', 'lubridate'), .export=unique(c('subSmp_strat'))) %dopar%
  {
    
    # - Testing 
    #it <- 9
    
    # - Set indices
    iSeed <- (it-1) %% length(seed_v) + 1
    iSize <- (it-1) %/% length(seed_v) + 1
    
    # - Iterate 
    temp <-subSmp_strat(smp_size=smp_size_v[iSize], smp_frac=smp_frac, datGiven=datCredit_real, seed=seed_v[iSeed],
                        stratifiers=stratifiers, targetVar=targetVar, currStatusVar=currStatusVar, timeVar=timeVar,
                        prior_pop=prior_pop)
    
    # - Reporting
    if (iSeed == length(seed_v)) {
      cat(paste0("\n2 (", Sys.time(),"). Subsample size: ", comma(smp_size_v[iSize]), " tested ",length(seed_v), " times."),
          file="subsampleLoop.txt", append=T) 
    }
    
    return(temp)
  }  

t <- proc.time() - ptm 
cat(paste0("\n3 (", Sys.time(),"). ForEach-loop done. Elapsed time: ", round(t[3]/60), " minutes."),
    file="subsampleLoop.txt", append=T)

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genObjPath, "subSampleSizes"), datResults); gc()
stopCluster(cl.port)


# --- Graphing
datResults[, list(MAE = mean(ErrVal_AE, na.rm=T), MAE_SD = sd(ErrVal_AE, na.rm=T),
                  RMSE = sqrt(sum(Err_PriorProb_SqrdErr, na.rm=T)/.N), RMSE_SE = sd(Err_PriorProb_SqrdErr, na.rm=T)), by=list(SubSampleSize)]



