# ============================== CUSTOM FUNCTIONS ==============================
# Defining custom functions used across various projects
# ------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha

# DESCRIPTION:
# This script defines various functions that are used elsewhere in this project
# or, indeed, used across other projects. Functions are grouped thematically.
# ==============================================================================



# -------- Ternary functions
# from https://stackoverflow.com/questions/8790143/does-the-ternary-operator-exist-in-r
`%?%` <- function(x, y) list(x = x, y = y)
`%:%` <- function(xy, z) if(xy$x) xy$y else z



# -------- Utility functions
# - Mode function (R doesn't have a built-int one)
getmode <- function(v) {
  uniqv <- unique(v);
  # discard any missingness
  uniqv <- uniqv[complete.cases(uniqv)]
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# - Memory function using 'gdata' package
getMemUsage <- function(limit=1000){
  require(gdata); require(scales)
  # - Get list of significant object sizes occupied in memory, order ascendingly
  totUsage <- ll()
  memSize <- subset(totUsage, KB >= limit)
  memSize$MB <- memSize$KB/1000
  gc(verbose=F)
  cat("Total memory used: ", comma(sum(totUsage$KB)/1000), "MB\n")
  cat("Big objects size: ", comma(sum(memSize$MB)), "MB\n\n")
  return(  memSize[order(memSize$KB), c(1,3)])
}



# -------- Cleaning functions

# Custom function that curates a main vector [x] to equal the previous/most-recent non-
# missing element in a given vector
imputeLastKnown <- function (x) {
  # -- Testing purposes
  # x <- Lookup$ZeroBal_Remain_Ind; x_lead <- Lookup$ZeroBal_Remain_Ind_lead
  # x <- c(0,0,0,1,1,1,0,1)
  # x <- c(0,0,0,1,1,1,0,NA)
  # x <- c(0,0,0,1,1,1,1,NA)
  # x <- c(0,0,0,1,NA,1,0,NA)
  # x <- c(0,NA)
  
  firstOne <- which(is.na(x))[1]
  if (!is.na(firstOne) & firstOne > 1) {
    x[firstOne] <- x[firstOne-1]
    # call function recursively to fix earlier missing-cases
    return( imputeLastKnown(x))
  } else { # no missing value found, return original vector
    return(x)
  }
}


# Custom function that curates a main vector [x] where x[1] is missing.
# This is achieve by finding the first non-missing element and back-filling that value
imputeFirstKnown <- function(x) {
  # -- Testing purposes
  # x <- c(NA, NA, 2,3,4)
  firstOne <- which(!is.na(x))[1]
  if (!is.na(firstOne) & firstOne > 1) {
    x[1:(firstOne-1)] <- x[firstOne]
    return(x)
  } else { # no non-missing value found, return original vector
    return(x)
  }
}




# -------------------------- INTERLEAVING FUNCTION ------------------------------
# - Coalescing function to facilitate data fusion between two given vectors
# Input: two scalar values (x & y) that may have selective missingness in either side (left: x; right: y)
# Output: Returns the non-missing side. If both are non-missing, then returns the (given) preference.
interleave <- function(x,y, na.value = as.integer(NA), pref='X') {
  # ensure require(dplyr)
  case_when(!is.na(x) & is.na(y) ~ x,
            is.na(x) & !is.na(y) ~ y,
            is.na(x) & is.na(y) ~ na.value,
            x == y ~ x,
            x != y & pref=='X' ~ x,
            x != y & pref=='Y' ~ y,
  )
}




# ------------------------- INTERPOLATION FUNCTION -----------------------------
# - Missing value Treatment: Interpolate the values between two known non-missing points
# Assumes all missingness are 'encased' between two known points.
# Input: [given]: a time series possibly with some missing values for which we like to interpolate;
#     [shouldRollBackward]: If the first element is missing, should we try to fix this by 'back-interpolating'
#       from the first non-missing point found?;
#     [SilenceWarnings]: Self-explanatory;
#     [shouldRollForward]: When there is only a single non-missing element, should we simply copy that value forward?
# Output: Linearly interpolated vector
interPol <- function(given, shouldRollForward=T, shouldRollBackward=T, SilenceWarnings=T) {
  
  # -- Testing conditions
  #given <- macro_data_hist$Inflation # for testing
  #given <- as.vector(subset(macro_data, Scenario=="Historic")[order(Date_T), RealGDP_Growth_yoy])
  #unique(macro_data$Scenario)
  #given <- as.vector(subset(macro_data, Scenario=="Baseline")[order(Date_T), rbqn_rb5339q])
  # (given <- as.vector(subset(macro_data, Scenario=="SevereStress")[order(Date_T), Consumption_Level_1q]))
  
  
  # first, check if there are any non-missing element
  if (all(is.na(given))) {
    # yes, there is, so just return the same input values and throw a warning (if allowed)
    if (SilenceWarnings==F) {
      warning("All data is missing, returning NA throughout..") 
    }
    return(given)
  }
  
  # second, check if there is any missing value; if so, then exit the function
  if (all(!is.na(given))) {
    return(given)
  }
  
  # third, check if first value is missing, which can hamper our interpolation procedure
  if (is.na(given[1])) {
    # yup, so should we try to fix this by 'back-interpolating' based on the first set of 2 non-missing values in the series?
    if (shouldRollBackward == T) {
      
      start.point <- 1 # starting point for filling in interpolated vaues at the end of this procedure
      
      # find first non-missing value in the series, which will be our 'ending value' for interpolating backwards
      end.point <- which(!is.na(given))[1]-1 # position before first non-missing element
      end.val <- given[end.point+1] # first non-missing element
      
      # we need to find second non-missing value and perform an 'interim' interpolation so that we have a one-period value
      # by which to change [end.val] backwards to [start.point] at the 'same speed' (as an assumption)
      start.point2 <- which(!is.na(given))[1]+1 # position after first non-missing element
      start.val2 <- given[start.point2-1] # first non-missing element
      end.point2 <- which(!is.na(given))[2]-1 # position before second non-missing element
      end.val2 <- given[end.point2+1] # second non-missing element
      
      # interpolate across this range, including the two values as outer bounds (therefore add 2 to the interpolation length)
      # note that (end.point - start.point + 1) denotes the length of this missingness-episode
      inter.vals <- seq(from=start.val2, to=end.val2, length.out = end.point2 - start.point2 + 1 + 2)
      
      # - might as well linearly interpolate here (saving a computing cycle of the while loop later on) ..
      # delete the first and last observation (they are the outer values outside of the missingness range)
      # and assign these interpolated values to the given vector
      given[start.point2:end.point2] <- inter.vals[2:(end.point2 - start.point2 + 2)]
      
      # check if we have non-zero elements at both sides
      if (start.val2 == 0 & end.val2 == 0) {
        # yes, so by-pass this treatment and just fill with 0s
        given[start.point:end.point] <- rep(0, end.point-start.point + 1)
      } else {
        
        # get interpolation 'speed'
        speed <- diff(given[start.point2:(start.point2+1)]) / given[start.point2]
        # given[start.point2]*(1+speed) # test
        
        # 'discount' the value backwards from the first non-missing value, using the previously calculated speed as the 'discount rate'
        for (i in end.point:start.point ) {
          given[i] <- given[i+1]*(1+speed)^(-1)
        } 
      }
      
    } else {
      # no we cannot. So throw error and exit
      stop("Error: Base assumption violated - First observation is missing, cannot interpolate. Exiting ..") 
    }
  }
  
  # repeat until no more missingness in given vector
  while ( any(is.na(given)) ) {
    
    # -- testing conditions
    #given <- c(2,NA,NA,5,NA,NA,8) # works
    #given <- c(2,NA,NA,5,6, NA,NA, 9) # works
    #given <- c(2,NA,NA,5,6, NA, NA, NA)
    
    # find the indices of all missing observations
    miss.ind <- which(is.na(given))
    
    # find "episodes" of missingness in these indices, since there may be more than 1 episode in the general case,
    # for which we need to repeat this procedure.
    # 1. Do this by first isolating the cases where the lagged differences are greater than 1
    # 2. Add 1 to these found positions to move to the "initial starting points" of the next episode in succession
    # 3. Pre-fix this vector with '1' to re-include the first 'episode' that was deselected previously
    # 4. Given this vector of indices (of indices), return starting positions again
    episode.starting.times <- miss.ind[c(1, which(diff(miss.ind) > 1) + 1)]
    
    # - check if we have data points outside of the first episode from which to interpolate
    # get staring point of first episode of missingness
    start.point <- episode.starting.times[1]
    # get ending point of first episode (got to test first if we have multiple episodes and diverge logic from there)
    if (length(episode.starting.times) > 1) {
      # we have multiple episodes. Therefore, scan the series from missingness's start up to the first non-missing element, then minus 1
      # add this to the starting point, minus 1 to exclude the first missing value (otherwise we are double-counting it when adding this range)
      end.point <- start.point + (Position(function(x) {!is.na(x)}, x=given[start.point:(episode.starting.times[2]-1)] ) - 1) - 1
    } else {
      # we don't have multiple episodes. Therefore, take last known missingness index
      end.point <- miss.ind[length(miss.ind)]
    }
    
    # given the starting and ending points for the actual interpolation, test for non-missing data outside of this range from 
    # which we need to interpolate
    if (!is.na(given[start.point-1]) & !is.na(given[end.point+1])) {# returns true if we can interpolate (no missingness outside of range)
      start.val <- given[start.point-1]
      end.val <- given[end.point+1]
      # interpolate across this range, including the two values as outer bounds (therefore add 2 to the interpolation length)
      # note that (end.point - start.point + 1) denotes the length of this missingness episode
      inter.vals <- seq(from=start.val, to=end.val, length.out = (end.point - start.point + 1) + 2)
      # delete the first and last observation (they are the outer values outside of the missingness range)
      # and assign these interpolated values to the given vector
      given[start.point:end.point] <- inter.vals[2:(end.point - start.point + 2)]
      
    } else {
      # assumption violated or episode's length = 1. Check if we can simply replace NAs with last known value in either case?
      if (shouldRollForward == T){
        if (SilenceWarnings==F) {
          warning("Base assumption violated - no available data outside of missingness range from which to interpolate. Rolling values forward instead ..")
        }
        # by definition, we must have a non-missing first element (should start.point >= 2)
        start.val <- given[start.point-1]
        given[start.point:end.point] <- rep(start.val, (end.point - start.point + 1)) # just repeat for the length of the missingness episode
        
      } else {
        # no we cannot. So throw error and exit
        stop("Error: Base assumption violated - no available data outside of missingness range from which to interpolate. Exiting ..") 
      }
    }
    
  }
  
  return(given)
}




# --------------------------- SCALING FUNCTIONS --------------------------------
# - two scaling functions to standardize given vectors unto a uniform scale
# Input: [given]: a real-valued vector
# Output: standardized vector

# 1) Range-based scaler | vectors will have equal ranges (min-max)
scaler <- function(given){
  output <- (given - min(given,na.rm=T)) / (max(given, na.rm=T) - min(given, na.rm=T))
  return(output)
}
# 2) Z-score/normalized scaler | vectors should roughly be N(0,1) distributed
scaler.norm <- function(given){
  # (given <- as.vector(subset(macro_data_hist1, Scenario=="Baseline")$DebtToIncome_Rate)) # for testing
  output <- (given - mean(given,na.rm=T)) / (sqrt(var(given,na.rm=T)))
  # check for NaN values (which can result if there is 0 variance)
  if (all(is.na(output))) {
    # just assign the central value, in this case, 0
    output <- rep(0, length(output))
  }
  return(output)
}




# ------------------------- SICR-DEFINITION FUNCTION ---------------------------
# Function that defines a SICR-event for a given loan's history
# Input: [delinq]: g1-measured delinqeuncy vector (number of payments in arrears) at every time t
#     [d]: threshold for g1-mesaure beyond which a SICR-event is said to have occured at t
#     [s]: "stickiness" of the delinquency test, or the number of consecutive periods for which
#         g1(t) >= d must hold before a SICR-event is said to occur
SICR_flag <- function(delinq, d, s) {
  
  # Prepare vectors into a mini data.table for easier wrangling
  dat <- data.table(delinq)
  
  # Main delinquency test at every time period
  dat[, Test0 := ifelse(delinq >= d, 1, 0)]
  
  # Second condition: assessing whether this delinquency threshold was met for (s-1) lagged periods
  varList <- c('Test0')
  if (s > 1) {
    for (l in 1:(s-1)) {
      dat[, paste0('Test',l) := shift(Test0, n=l, type="lag")]
      
      # add newly created variable to a list
      varList <- c(varList, paste0('Test',l))
    }
  }
  
  # Sum the number of lagged flags per row, used for final logic test
  dat[, Test_Sum := rowSums(.SD, na.rm = T), .SDcols=varList]
  
  # Finally, test whether g_0(t) >= d for at least s number of periods
  # This is achieved by equating summed lagged flags and evaluating against s >=1
  dat[, Sticky := ifelse(Test_Sum == s, 1, 0)]  
  
  # return SICR-flag vector
  return(dat$Sticky)
}




# ------------------------- YEO-JOHNSON TRANSFORMATION ---------------------------
# A function for applying Yeo-Johnson transformation to a given vector. The optimal transformation is selected based on either a normal log-likelihood
# function of the transformed vector. The optimal transformation is thus chosen based on the best approximation to normality.
# Input: [x]: a real-valued vector
# Output:vector transformed with an optimal power transformation
transform_yj <- function(x, bound_lower=-2, bound_upper=2, lambda_inc=0.5, verbose=FALSE, plotopt=FALSE, plotqq=FALSE, norm_test=FALSE){
  # --- Unit test
  # x <- 1560*rbeta(10000, shape1=1, shape2=20); hist(x)
  # x <- rgamma(10000, shape=2, rate=5); hist(x) 
  # bound_lower<--5; bound_upper<-5; lambda_inc<-0.5; verbose<-FALSE; plotopt<-TRUE; plotqq<-TRUE; norm_test=TRUE
  
  # - Preliminaries
  require(MASS) # Ensure the requried pacakage in loaded of the Box-Cox function
  lambda_search <- seq(from=bound_lower, to=bound_upper, by=lambda_inc) # Check if lambda_search exists and if not, assign a value: This parameter specifies the search space to obatin the optimal power transformation in th boxcox function
  
  # - Ensuring the plotting area is ready for possible plots 
  par(mfcol=c(1,1))
  
  # - Selecting the optimal lambda1 parameter based on the choice of the loss function
  lambda <- boxcox((x-min(x)+0.000001)~1,lambda=seq(from=bound_lower,to=bound_upper,by=lambda_inc), plotit=FALSE)
  lambda_opt <- lambda$x[which.max(lambda$y)]
  lambda_yj <- lambda_opt
  
  # - Applying the Yeo-Johnson transformation with the optimal lambda parameter
  con1 <- as.integer(x>=0)*(lambda_yj!=0)
  con2 <- as.integer(x>=0)*(lambda_yj==0) 
  con3 <- as.integer(x<0)*(lambda_yj!=2)
  con4 <- as.integer(x<0)*(lambda_yj==2)
  
  y <- ((con1*x+1)^lambda_yj-1)/(ifelse(lambda_yj!=0,lambda_yj,1)) + log(con2*x+1) - ((-con3*x+1)^(2-lambda_yj)-1)/(2-ifelse(lambda_yj!=2,lambda_yj,1)) - log(-con4*x+1)
  
  
  # - Reporting the optimal lambda parameter, as well as the corresponding log-likelihood
  cat('\nNOTE:\tThe optimal power-transformation is lambda1 = ', lambda_yj)
  cat('\n \tThe optimal transformation has a log-likelihood =', round(max(lambda$y[which(lambda$x==lambda_yj)])), '\n')
  
  # - Plotting two qq-plots to show the normality of the given vector (x) before and after the optimal transformation 
  if(plotqq==TRUE){
    par(mfcol= c(1,2)); qqnorm(x); qqline(x, distribution = qnorm); qqnorm(y); qqline(y, distribution = qnorm);
    par(mfcol=c(1,1)) # Resetting plotting graph dimensions
  }
  
  # - Conducting a KS test for normality
  if(norm_test){
    ks_test_x <- ks.test(x, "pnorm")$p.value
    ks_test_y <- ks.test(y, "pnorm")$p.value
    
    cat('\nNOTE:\tThe KS-test for normality on the un-transformed data yields a p-value of ', ks_test_x)
    cat('\n \tThe KS-test for normality on the transformed data yields a p-value of ', ks_test_y)
  }
  
  # - Return the transformed vector
  cat('\n \n')
  return(y)
  #rm(x,y,bound_lower,bound_upper, verbose, plotopt, plotqq, lambda1, lambda2, lambda_search, norm_test)
}

# Some more testing conditions
# transform_yj(x=1560*rbeta(10000, shape1=1, shape2=20), bound_lower=4, plotopt=TRUE)
# transform_yj(x=1560*rbeta(10000, shape1=1, shape2=20),bound_lower=-2,bound_upper=2,lambda_inc=0.1, plotopt=TRUE, plotqq=TRUE, norm_test=TRUE)




# ------------------------- VARIABLE IMPORTANCE FOR LOGIT MODELS ---------------------------
# A function for assessing the variable importance of a logit model. The absolute size of
# the coefficients are use to rank the variables from most to least importance.
# A larger/ smaller absolute value of a coefficient indicates a more/ less important
# variable. This technique assumes that the variables have the exact same scale.
# Input:  [logit_model]: A logistic regression model
#         [datTrain]:    Dataset on which the model is trained
#         [method]:      The method use for computing the variable importance; either "ac" for the absolute value of the coefficients; "sc" for standardised coefficients; "FIRM" for the FIRM technique that uses partial dependence (a form of explainable AI) (https://arxiv.org/pdf/1904.03959.pdf)
#         [same_scales]: Are the coefficients the same scales (TRUE), if not (FALSE) then the coefficients will be standardised
#         [sig_level]:   The threshold under which the variables are considered significant
#         [plot]:        Should a bar chart be produced which shows the variable importance
#         [pd_plot]:     Should a partial dependence plot be created for each variable
# Output: A data table containing the variable importance information
varImport_logit <- function(logit_model, method="sc", standardise=F, sig_level=0.05, plot=F, pd_plot=F){ ### Use the SC method as standard (get a tutorial on this for "backup")
  ### Standardise output of function
  
  # - Unit testing conditions:
  # datTrain <- data.table(ISLR::Default); datTrain[, `:=`(default=as.factor(default), student=as.factor(student))]
  # logit_model <- glm(default ~ student + balance + income, data=datTrain, family="binomial")
  # method <- "pd"; same_scales=F; sig_level<-0.05; plot<-T; pd_plot<-T; standardise<-T
  
  # - Get the data the model was trained on
  datTrain1 <- subset(logit_model$data, select = names(logit_model$data)[names(logit_model$data) %in% names(model.frame(logit_model))])
  # Getting the names of the original training dataset
  datTrain1_names <- names(datTrain1)

  # - Filtering for significant variables
  if (!is.na(sig_level)){ # Filter for significant variables
    # Get the index of the names that are significant
    coefficients_ind <- summary(logit_model)$coefficients[,4][-1]; coefficients_ind <- which(coefficients_ind<=sig_level)
    # Displaying an message if there are no significant variables
    if (is.null(coefficients_ind)){
      stop("ERROR: Variable importance not conducted since there are no significant variables.") # TEST (use warning() to display a warning...but function execution continues on)
    }
  } else { # Do not filter for significant variables
    # Assign all index values
    coefficients_ind <- summary(logit_model)$coefficients[,4][-1]
    
  }
  # Get the corresponding names of the significant variables in the training data
  coefficients_sig_model <- names(summary(logit_model)$coefficients[,4][-1][coefficients_ind])
  # Get the names of the significant variables in the model (this may differ from the training data if there are factors/categorical variables present)
  coefficients_sig_data <- names(datTrain1)[-which(names(datTrain1) %in% names(model.frame(logit_model))[1])][coefficients_ind] # The chaining ensures that the target variable is excluded from the variables' names
  
  # - Initiating the dataset to be returned (results dataset)
  results <- list(data = data.table(Variable = coefficients_sig_data,
                                    Value = 0,
                                    Rank = 0))
  
  ### MM: This first technique is under construction. I took a stab at it and I hope that my interpretation of the technique was correct?
  ###     A quick comparison to the "ac" method reveals that the ranking of the variables differ between techniques, but those that are considered "more" important by the one technique are also considers as such bu the other
  # - Computing the variable importance according to the selected technqiue
  if (method=="sc") { # - Variable importance as determined by the standardised coefficients (Rank variables according to the absolute value of the variables' odd ratios divided by the associated standard error of the estimated coefficient)
    # Assigning the method to the results
    results$Method <- "Standardised Coefficient"
    # Computing the importance measure and populating the results dataset
    results$data <- data.table(Variable=coefficients_sig_data, Coefficient=summary(logit_model)$coefficients[coefficients_ind+1,1], # Get the estimated coefficients and their associated standard errors
                               SE=summary(logit_model)$coefficients[coefficients_ind+1,2])
    results$data[,Value:=abs(Coefficient/SE)] # Compute the importance measure
    results$data[,`:=`(Coefficient=NULL,SE=NULL)]
  } else if (method=="ac") { # - Variable importance as determined by the absolute values of the variables' coefficients (Rank variables according to the absolute values of the variables' estimated coefficients)
    # Assigning the method to the results
    results$Method <- "Absolute Coefficient"
    # Assigning new names to the training dataset according to the names of the coefficients in the model (this is since factor variables have different names in the model, e.g., "student" vs "studentYES")
    names(datTrain1)[-which(names(datTrain1) %in% names(model.frame(logit_model))[1])] <- names(logit_model$coefficients)[-1]
    # Scaling the variables
    if (standardise==T){
      datTrain2 <- copy(datTrain1)
      for (i in 1:length(coefficients_sig_model)){
        # Checking if the variable is numeric so that the underlying training data can be scaled
        if (class(datTrain2[, get(coefficients_sig_model[i])]) %in% "numeric"){ ### Can have a "binary" as well as "factor" type variables - check how these types of variables are handled - check in an additional script to confirm; factorise numeric variables (in model call)P and fit model - check funcitonality
          datTrain2[, (coefficients_sig_model[i]) := (get(coefficients_sig_model[i])-mean(get(coefficients_sig_model[i]),na.rm=T))/sd(get(coefficients_sig_model[i]), na.rm=T)]
        } # if
      } # else
      # Re-training the model on the scaled data
      names(datTrain2) <- datTrain1_names
      logit_model2 <- glm(logit_model$formula, data=datTrain2, family="binomial")
    } else {
      logit_model2 <- logit_model
    } # if else
    # Populating the results dataset
    results$data[,Std_Coefficient := logit_model2$coefficients[which(names(logit_model2$coefficients) %in% coefficients_sig_model)]]
    results$data[,Value:=abs(Std_Coefficient)]
    results$data <- results$data %>% arrange(desc(abs(Std_Coefficient))) %>% mutate(Rank=row_number())
    results$data[,Std_Coefficient:=NULL]
    # Plotting the odds ratios (if specified)
    if (plot==T){ 
      # Computing the odds ratios and the corresponding confidence interval
      datPlot_odds_ratios <- data.table(Variable=coefficients_sig_data, round(exp(cbind(OR = coef(logit_model2), confint.default(logit_model2))), 3)[coefficients_ind+1,])
      colnames(datPlot_odds_ratios) <- c("Variable","OR","CI_Lower", "CI_Upper")
       
      (results$plots$Odds_Ratios <- ggplot(datPlot_odds_ratios, aes(x=OR, y=Variable)) + theme_minimal() + labs(x="Odds ratio (log scale)") + geom_vline(aes(xintercept=1), linewidth=0.25, linetype="dashed") +
                                     geom_errorbarh(aes(xmax=CI_Upper,xmin=CI_Lower), size=0.5, height=0.2) + geom_point(size=3.5, colour="blue") + theme(panel.grid.minor=element_blank()) +
                                     coord_trans(x="log10") + scale_x_continuous(breaks=c(0,0.1,0.25,0.5,1,2,4,max(datPlot_odds_ratios$CI_Upper))))
    } # if
   } else if (method=="FIRM") { # - Variable importance as determined by feature importance rank measure (FIRM) (explainable AI technique)
    # Assigning the method to the results
    results$Method <- "FIRM"
    # Create the results table
    results$data <- vip::vi_firm(logit_model, feature_names=coefficients_sig_data, method="firm", train=datTrain1)
    # Plotting the partial dependence
    if (pd_plot==T){
      for (i in seq_along(coefficients_sig_model)){
        datPlot_pd <- data.table(attr(results$data, which = "effects")[[i]])
        names(datPlot_pd) <- c("x","y")
        (results$plots[[paste0("pd_",coefficients_sig_model[i])]] <- ggplot(datPlot_pd) + geom_point(aes(x=x,y=y)) + theme_minimal() +
           labs(x=coefficients_sig_data[i],y="yhat") + theme(plot.title = element_text(hjust=0.5)))
      } # for
    } # if
    # Adding a column to indicate the rank order of the variables' importance and getting the data in the desired format
    results$data <- data.table(results$data) %>% rename(Value = Importance) %>% arrange(desc(Value)) %>% mutate(Rank=row_number())
  } else {stop(paste0('"', method,'" is not supported. Please use either "sc", "ac", or "pd" as method.'))}# if else (method)
  # - Creating a general plot of the variable importance (if desired)
  if (plot==T){
    (results$plots[["Ranking"]] <- ggplot(results$data, aes(x=reorder(Variable, Value))) + geom_col(aes(y=Value), col='blue', fill='blue') +
       coord_flip() + theme_minimal() + theme(plot.title = element_text(hjust=0.5)) +
       xlab("Variable name") + ylab(ifelse(method=="sc", "Absolute value of standardised coefficient",
                                           ifelse(method=="ac","Absolute value of fitted coefficient",
                                                  "FIRM Value"))))
  } # if
  # - Return results
  return(results)
  # rm(logit_model, logit_model2, datTrain1, datTrain2, method, same_scales, plot, coefficients_sig_model, coefficients_sig_data)
}
# - Unit test
# install.packages("ISLR"); require(ISLR)
# datTrain <- data.table(ISLR::Default); datTrain[, `:=`(default=as.factor(default), student=as.factor(student))]
# logit_model <- glm(default ~ student + balance + income, data=datTrain, family="binomial")
# a<-varImport_logit(logit_model = logit_model, method="pd", standardise = F, sig_level = 0.05, plot=T, pd_plot = T)
# b<-varImport_logit(logit_model = logit_model, method="ac", standardise = T, sig_level = 0.05, plot=T)
