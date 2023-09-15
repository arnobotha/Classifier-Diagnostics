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







