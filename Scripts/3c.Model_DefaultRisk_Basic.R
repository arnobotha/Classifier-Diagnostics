# ============================= MODEL DEFAULT RISK =============================
# Develop several logistic regression models ranging from models with few features
# to models with more features to predict default risk
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha, Esmerelda Oberholzer

# DESCRIPTION:
# This script uses the previously prepared credit dataset fused with macroeconomic
# variables to create multiple logistic regression models for default
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 0a.CustomFunctions.R
#
# -- Inputs:
#   - datCredit_real | Prepared credit data from script 2f
#
# -- Outputs:
#   - 
# =======================================================================================


# AB (2023-09-12): Need to rethink some of the structure of this script, given that we'd like to 
# investigate optimal size of the subsampled resampling scheme first, as discussed in our last catch-up. 
# I've begun with this in script 3a-series,
# leaning heavily on the work between Marcel and I in the survival modelling context (also needing a resampling scheme)



# ------- 1. Import data and setup parameter definition

ptm <- proc.time() # for runtime calculations (ignore)

# - Confirm prepared data after exclusions is loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"datCredit_real"), tempPath)

# -- Parameters used in the default definition
# k: - outcome period
# s: - number of consecutive payments (stickiness)
# d: - delinquency threshold

# - Define the parameters
p.k <- 12
p.s <- 1
p.d <- 3





# ------ 2. Define the target event and conduct some data prep

# - Create the default definition based on the parameters
datCredit_real[, default_def := SICR_flag(g0_Delinq, d=p.d, s=p.s), by=list(LoanID)]
describe(datCredit_real$default_def) #ensure there are no missing values and only two distinct values (binary) - success

# - Calculate the cumulative sum of the default flag
datCredit_real[, cum_default_ind := sum_run(default_def, na_rm = TRUE, k = p.k), by=list(LoanID)]
datCredit_real[, cum_default_lead := shift(cum_default_ind, type='lead', n=p.k), by=list(LoanID)]
datCredit_real[, default_target := ifelse(cum_default_lead > 0, 1, 0), by=list(LoanID)]

# - Exclude the observations that are already in default (too late)
datCredit_real <- subset(datCredit_real, g0_Delinq != 3)

# check whether k-periods have NA for each account
datCredit_real[, check_periods := ifelse(is.na(default_target), 1, 0), ]
# check the number of observations impacted
(exclusions_missing_periods <- datCredit_real[(check_periods == 1), .N] / datCredit_real[, .N] * 100)
# Number of impacted observations: 14.36%

# - Discard observations where target has NA, implying insufficient history
datCredit_real <- subset(datCredit_real, !is.na(default_target))
describe(datCredit_real$default_target)
# checked for missing target events as well as two unique binary events - success

# clean-up
datCredit_real <- subset(datCredit_real, select = -c(cum_default_ind, cum_default_lead, default_def, check_periods))

# - Check the event rate of each class
# RECORD-LEVEL
table(datCredit_real$default_target) %>% prop.table()

# Default events:      3.45% 
# Non Default events:  96.55%

# ACCOUNT-LEVEL
datCredit_real[, HasDef := max(default_target, na.rm=T), by=list(LoanID)]
(def_events_account_level <- datCredit_real[Counter == 1 & HasDef == 1, .N] / datCredit_real[Counter == 1, .N] * 100)
(non_def_events_account_level <- datCredit_real[Counter == 1 & HasDef == 0, .N] / datCredit_real[Counter == 1, .N] * 100)

# Default events:      17.62%
# Non Default events:  82.38%

# - Convert the target variable to a categorical variable for modelling
datCredit_real[, default_target := factor(default_target)]





# ------- 3. Train and test datasets

# Split the data into training and validation
# The split is based on 70% training data and 30% validation data
# Need to ensure we have a balanced dataset over time to limit sampling bias
# Since this is an extremely large dataset, memory is a problem
# Therefore, subsample the train and test datasets to have 3 million observations in total

# - Firstly, resample 3000000 observations of the data - two-way stratified dataset by default event and date
set.seed(1) # ensure that we get the same split each time
smp_size <- 3000000 # we want 3 million observations from the population
smp_percentage <- smp_size/nrow(datCredit_real)
datCredit_real_resample <- stratified(datCredit_real, c("default_target", "Date"), smp_percentage)
# - check representativeness | proportions should be similar
table(datCredit_real_resample$default_target) %>% prop.table() #success
rm(datCredit_real); gc()

# - Resample the smaller dataset into 70% train and 30% test
datCredit_real_resample[, ind := 1:.N]
set.seed(1) # ensure that we get the same split each time
datCredit_real_train_s <- stratified(datCredit_real_resample, c("default_target", "Date"), 0.7)
vec_def_train <- pull(datCredit_real_train_s, "ind") # identify the observations in the training dataset
datCredit_real_valid_s <- datCredit_real_resample[!(datCredit_real_resample$ind %in% vec_def_train),]
# - Clean-up
rm(vec_def_train); gc()
datCredit_real_resample[, ind := NULL]
datCredit_real_train_s[, ind := NULL]
datCredit_real_valid_s[, ind := NULL]

# - Check the event rate of the training and validation data sets to ensure the default events are balanced
table(datCredit_real_train_s$default_target) %>% prop.table()
table(datCredit_real_valid_s$default_target) %>% prop.table()
# success - the event rates are the same

# Calculate the default incidence rates for the training and validation datasets and construct a graph
# This is done to check whether we have sampling bias
# In other words, check the default incidence rates for the train and test datasets across time

# - Calculate the total number of default events per month
# training data
def_count_train <- datCredit_real_train_s[default_target == 1, .N, by=.(year(Date), month(Date))]
names(def_count_train)[names(def_count_train)=="N"] <- "def_obs_train"

all_obs_train <- datCredit_real_train_s[, .N, by=.(year(Date), month(Date))]
names(all_obs_train)[names(all_obs_train)=="N"] <- "all_obs_train"

# merge to calculate the proportions
def_rates_train <- merge(all_obs_train, def_count_train, by=c("year", "month"), all.x=T)
def_rates_train[, def_rates_train := def_obs_train/all_obs_train]

# validation data
def_count_valid <- datCredit_real_valid_s[default_target == 1, .N, by=.(year(Date), month(Date))]
names(def_count_valid)[names(def_count_valid)=="N"] <- "def_obs_valid"

all_obs_valid <- datCredit_real_valid_s[, .N, by=.(year(Date), month(Date))]
names(all_obs_valid)[names(all_obs_valid)=="N"] <- "all_obs_valid"

# merge to calculate the proportions
def_rates_valid <- merge(all_obs_valid, def_count_valid, by=c("year", "month"), all.x=T)
def_rates_valid[, def_prop_valid := def_obs_valid/all_obs_valid]

# - Merge all the SICR-rate data sets into one to construct a graph to check whether the SICR-incidence rates align
def_rates_all <- merge(def_rates_train, def_rates_valid, by=c("year", "month"), all.x=T)

# define a date variable to use in the plot
def_rates_all[, Date := as.Date(paste(year, month,"01",sep="-"))]

# clean-up
rm(all_obs_train, all_obs_valid, def_count_train, def_count_valid, def_rates_train, def_rates_valid); gc()

# - Now plot the proportions
# note - change the font and y-axis to percentage
plot.data_def_rates <- as.data.table(gather(def_rates_all[, list(Date, a=def_rates_train, b=def_prop_valid)], 
                                            key="Prop", value = "Proportion", -Date))

col.v <- brewer.pal(3, "Set2")
label.vec <- c("Stratified training data set", "Stratified validation data set")
shape.v <- c(15,16)
chosenFont <- "Cambria"

ggplot(plot.data_def_rates, aes(x=Date, y=Proportion, colour=Prop)) + 
  theme_minimal() + 
  geom_line(aes(x=Date, y=Proportion, colour=Prop), size=0.5) + 
  geom_point(aes(x=Date, y=Proportion, colour=Prop, shape=Prop), size=2) + 
  theme(legend.position = "bottom", text=element_text(family=chosenFont)) + 
  labs(y="Default incidence rates over time", x= "Time") + 
  scale_colour_manual(name="Data sets", values=col.v, labels=label.vec) + 
  scale_shape_manual(name="Data sets", values=shape.v, labels=label.vec) + 
  scale_y_continuous(breaks=pretty_breaks(), labels = percent) + 
  scale_x_date(date_breaks = "2 year", date_labels = "%b %Y") +
  ggtitle("Line graphs of default incidence representativeness across different data sets") +
  theme(plot.title = element_text(hjust = 0.5))










# AB Series of candidate models, starting with simplest logit-models containing ONLY the most basic of information, e.g., delinquency-themed input spaces.
# Focus: default risk (PD-model) + most basic input space: delinquency-themed
# Approach: simple to complex: 1) model-by-input (expanding input space); 2) model-by-segment (expanding segmentation scheme); 3) modelling technique f
# Aspects to consider include: missing value treatments, interactions, 
# transformations (Tukey Power, Box-Cox, Yeo-Johnson); ask Marcel, also in sec. 5 in Std-PrinciplesForDataPrep