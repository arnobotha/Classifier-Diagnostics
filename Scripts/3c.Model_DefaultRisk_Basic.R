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


### AB (2023-09-12): Need to rethink some of the structure of this script, given that we'd like to 
# investigate optimal size of the subsampled resampling scheme first, as discussed in our last catch-up. 
# I suspect much of this will need to be rewritten, or at least restructured.
# For this script, let's assume that a subsampled set was already created, whereupon necessary data fusion
# already happened, after which a resampling scheme is pre-created.
# Therefore, I imagine this script's starting point ought to be purely to load in the "_train" and "_valid" sets
# As such, I've already prepared a very rough structure for this script; see below



# GOAL: Series of candidate models, starting with simplest logit-models containing ONLY the most basic of information, e.g., delinquency-themed input spaces.
# Focus: default risk (PD-model) + most basic input space: delinquency-themed
# Approach: simple to complex: 1) model-by-input (expanding input space); 2) model-by-segment (expanding segmentation scheme); 3) modelling technique f
# Aspects to consider include: missing value treatments, interactions, 
# transformations (Tukey Power, Box-Cox, Yeo-Johnson); ask Marcel, also in sec. 5 in Std-PrinciplesForDataPrep




# ------ 1. Preliminaries

ptm <- proc.time() # for runtime calculations (ignore)

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)





# ------ 2. Modelling & Feature Selection by theme
# Modelling Tutorials: https://stats.oarc.ucla.edu/r/dae/logit-regression/
# https://stackoverflow.com/questions/63748817/odds-ratio-and-95-ci-for-interaction-in-logistic-model-in-r
# https://stackoverflow.com/questions/41384075/r-calculate-and-interpret-odds-ratio-in-logistic-regression

# --- 4.1 Account-level information
# Analyse the significance of the account-level information

# - "Raw" variables
inputs_acc1 <- DefaultStatus1_lead_12_max ~ Age_Adj + Term + Receipt_Inf + Balance + InterestRate_Margin + 
  AgeToTerm + BalanceToPrincipal

logitMod_acc1 <- glm(inputs_acc1, data=datCredit_train, family="binomial")
summary(logitMod_acc1)
### RESULTS: AgeToTerm insignificant, coef of InterestRate_Margin has relatively large standard error

# - Odds Ratio (exp(beta)) with 95% confidence intervals
round(  exp(cbind(OR = coef(logitMod_acc1), confint.default(logitMod_acc1))), 3)

# - Effect size analysis | Maybe do this only for final model
# mean odds of event (unconditioned on any input): 0.038 | prob = odds / ( 1+ odds)
# odds of event given unit increase in InterestRate_Margin, exp(2.26322360946448 x 1) = 9.614 | Biggest effect size


### AB: Build graphing function (I'll help with this later, have an interesting idea to illustrate effect sizes of all inputs on
# a single graph, similar to Shapley-values' graph)
# https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
# https://www.geeksforgeeks.org/how-to-plot-odds-ratio-of-prediction-of-logistic-model-in-r/
# https://argoshare.is.ed.ac.uk/healthyr_book/odds-ratio-plot-1.html
# https://stackoverflow.com/questions/47085514/simple-way-to-visualise-odds-ratios-in-r
probs <- seq(0,1,by=0.01)
odds <- probs / (1-probs)
plot(x=probs, y=odds, type="b")
# Put odds ratios as annotations on above graph (ggplot2-variant of course)


