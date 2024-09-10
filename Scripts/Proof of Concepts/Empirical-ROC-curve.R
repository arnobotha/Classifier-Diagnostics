# ================================= EMPIRICAL ROC-CURVE CONSTRUCTION =========================================
# Demonstrates the empirical estimation of an ROC-curve using dummy data, whereafter the AUC-summary statistic
# is calculated using the Trapezoidal Rule. 95% confidence intervals are derived by estimating the standard 
# error of the AUC-statistic, using both the Hanley-method and the DeLong-method. These custom methods
# are successfully unit tested aggainst those within the pROC-package.
# Sources: Fawcett2006 (DOI: https://doi.org/10.1016/j.patrec.2005.10.010)
#         DeLong1988 (DOI: https://doi.org/10.2307/2531595)
#         Hanley1982 (DOI: https://doi.org/10.1148/radiology.143.1.7063747)
#         Hanley1997 (DOI: 10.1016/S1076-6332(97)80161-4)
#         Sun2014 (DOI: https://doi.org/10.1109/LSP.2014.2337313)
# ------------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha
# -----------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R | Only for plotting purposes
# ============================================================================================================



# ------- 0. Setup

require(ggplot2)
library(extrafont)
require(scales)
require(ggthemes)
require(extrafont)
require(RColorBrewer)
require(tidyr)
require(data.table)
require(ggpp) # Extensions to ggplot2, particularly geom_table




# ------- 1. Designed dataset
# Embeds the same example from Fawcett2006


# - Assume a discriminative/probabilistic classifier has scored a test dataset in predicting class membership or event P(Y=C_1|X)
# Unique on score
datExample <- data.table(Score=c(0.9,0.8,0.7,0.6,0.55,0.54,0.53,0.52,0.51,0.505,
                                 0.4,0.39,0.38,0.37,0.36,0.35,0.34,0.33,0.3,0.1),
                         Class_Actual=c("P","P","N","P","P","P","N","N","P","N",
                                        "P","N","P","N","N","N","P","N","P","N"))


# ------- 2. Construct ROC-graph
# Implements algorithm from Fawcett2006 (Algo 1 + 2)

# - Sort and initialize variables
datExample <- datExample[order(-Score),]
FP <- 0; TP <- 0 # False & True positive counts
v_prev <- -Inf
datROC <- data.table() # empty object, to be populated iteratively
n_pos <- datExample[Class_Actual == "P", .N] # Positive count
n_neg <- datExample[Class_Actual == "N", .N] # Negative count
if (!(n_pos > 0 & n_neg > 0)) stop("Counts of positives/negatives are zero. Exiting ..") 

# - Create 2-sided annotation table for graphing purposes
i_set <- 1:10
datAnnotate1 <- data.table(i=i_set,Class=datExample[i_set, ifelse(Class_Actual=="P", "italic(C)[1]", "italic(C)[0]")], 
                           `Score~italic(v)`=datExample[i_set,percent(Score,accuracy=0.1)])
i_set2 <- 11:20
datAnnotate2 <- data.table(i=i_set2,Class=datExample[i_set2,ifelse(Class_Actual=="P", "italic(C)[1]", "italic(C)[0]")], 
                           `Score~italic(v)`=datExample[i_set2,percent(Score,accuracy=0.1)])
datAnnotate <- cbind(datAnnotate1,datAnnotate2)

# - Create list of ROC-points for plotting; Algorithm 1 from Fawcett2006, though amended for graphing purposes
for (i in 1:NROW(datExample)) {
  # Add ROC-point only if scores are not tied
  if (datExample[i,Score] != v_prev) {
    # Lookup correct score for appending to ROC-entry
    if (i==1) v <- Inf else v <- datExample[i-1,Score] 
    ROC_entry <- data.table(i=i, FPR=FP/n_neg, TPR=TP/n_pos, Score=v)
    if (NROW(datROC) > 0 ) datROC <- rbind(datROC, ROC_entry) else datROC <- ROC_entry
    v_prev <- datExample[i,Score]
  }
  # Increment counts of TP and FP, assuming current score is used as cut-off
  if (datExample[i,Class_Actual] == "P") TP <- TP + 1 else FP <- FP + 1
}

# Append ROC-point (1,1) to stack
datROC <- rbind(datROC, data.table(i=i+1, FPR=FP/n_neg, TPR=TP/n_pos, Score=datExample[i,Score] ))

# - Graph options
chosenFont <- "Cambria"
dpi <- 200
vCol <- brewer.pal(6, "Set1")[c(3,1)]

# - Plot empirical ROC-graph
(g2 <- ggplot(datROC,aes(x = FPR, y = TPR)) + theme_minimal() +
    theme(text=element_text(family=chosenFont),
          legend.position="bottom") + 
    labs(x = bquote(False~positive~rate~~italic(F)^'+'~'='~1-italic(S)^'-'),
         y = bquote(True~positive~rate~~italic(T)^'+'~'='~italic(S)^{'+'})) + 
    geom_polygon(aes(x=FPR, y=TPR), alpha=0.1, fill=vCol[1], show.legend=F) +
    geom_line(aes(x = FPR, y = TPR), colour=vCol[1], linewidth = 1, alpha = 0.7) +
    geom_text(aes(x=FPR,y=TPR,label=percent(Score,accuracy=0.1)), family=chosenFont) + 
    geom_abline(slope = 1, intercept = 0) +
    # annotate data tables
    annotate(geom="table", x=1, y=0.01, family=chosenFont, size=2.9,
             label=datAnnotate, parse=T) +
    scale_y_continuous(breaks=pretty_breaks(), labels = percent) +
    scale_x_continuous(breaks=pretty_breaks(), labels = percent) 
)

# - Save graph
ggsave(g2, file=paste0(genFigPath, "ROC_Curve2.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")




# ------- 3. Calculate AUC using Trapezoidal rule
# Implements Algorithm 2 from Fawcett2006

# - Sort and initialize variables
datExample <- datExample[order(-Score),]
FP <- 0; TP <- 0 # False & True positive counts
FP_prev <- 0; TP_prev <- 0 # previous values of FP and TP
v_prev <- -Inf # previous value of probability score v
A <- 0 # area under the ROC-curve
n_pos <- datExample[Class_Actual == "P", .N] # Positive count
n_neg <- datExample[Class_Actual == "N", .N] # Negative count

# function for calculating the area of a trapezoid
trapArea <- function(x1, x2, y1,y2) {
  base <- abs(x1-x2)
  height_avg <- (y1+y2)/2
  return (base*height_avg)
}

# - Create list of ROC-points for plotting; Algorithm 2 from Fawcett2006
for (i in 1:NROW(datExample)) {
  # Add ROC-point only if scores are not tied
  if (datExample[i,Score] != v_prev) {
    A <- A + trapArea(FP, FP_prev, TP, TP_prev)
    v_prev <- datExample[i,Score]
    FP_prev <- FP
    TP_prev <- TP
  }
  # Increment counts of TP and FP, assuming current score is used as cut-off
  if (datExample[i,Class_Actual] == "P") TP <- TP + 1 else FP <- FP + 1
}

A <- A + trapArea(n_neg, FP_prev, n_neg, TP_prev)
A <- A/(n_pos*n_neg) # scale from PxN unto the unit square

cat("AUC: ", A)


# ------- 4. Compare to output given by pROC-package
library(pROC)

# - Create ROC-object
pROC1 <- roc(response=datExample$Class_Actual, predictor=datExample$Score, levels=c("N","P"),
             ci=T,ci.method="delong", conf.level=0.95)
SE_A_DeLong <- sqrt(var(pROC1,method="delong"))
cat(paste0("AUC: ",percent(pROC1$auc, accuracy=0.1), " ± ", percent((pROC1$ci[3]-pROC1$ci[1])/2, accuracy=0.01),
           " error margin (DeLong-method) using 95% confidence intervals\nStandard error: ", SE_A_DeLong))
### [SANITY CHECK]
percent(((pROC1$auc + SE_A_DeLong*1.96) - (pROC1$auc - SE_A_DeLong*1.96))/2, accuracy=0.01)
### Seems like same error margin, SAFE

# - Plot ROC-graph
plot(pROC1, auc.polygon=T, max.auc.polygon=T, grid=T, xlab="False Positve Rate (%)", ylab="True Positve Rate (%)",
     identity=T, legacy.axes=T, percent=T)

# - Compare estimates
datROC2 <- data.table(FPR=1-pROC1$specificities, TPR=pROC1$sensitivities, Scores=pROC1$thresholds)
all.equal(datROC[,list(TPR,FPR)], datROC2[order(TPR,FPR), list(TPR,FPR)]) # IF TRUE, then SAFE
as.numeric(pROC1$auc) == A # IF TRUE, then SAFE

# - Investigate logic of LeLong1988's method
getAnywhere("ci.auc.roc") # run this to inspect the inner logic of the pROC-package regarding CIs
ci.auc(pROC1, method="delong")
getAnywhere("ci_auc_delong") # previous "ci.auc.roc()" function calls this for DeLong1988's method
getAnywhere("var.roc") # different function that also computes the variance of AUC
getAnywhere("delongPlacements") # another inner function used in "ci_auc_delong()" and "var.roc" given an ROC-object
getAnywhere("delongPlacementsCpp") # yet another function, this time based in C++.
### RESULTS: DeLong-method is widely accepted in literature, even though its calculation doesn't seem straightforward



# --- Calculate standard error of AUC-estimate using simple (but biased) method from Hanley1982
Q1 <- A / (2-A)
Q2 <- (2*A^2) / (1+A)
SE_A_Hanley <- sqrt(1/(n_pos*n_neg)*( (A*(1-A) + (n_pos-1)*(Q1-A^2) + (n_neg-1)*(Q2-A^2)  ) )) # standard error
upperBound <- A + 1.96*SE_A_Hanley; lowerBound <- A - 1.96*SE_A_Hanley
cat(paste0("AUC: ",percent(A,accuracy=0.1), " ± ", percent((upperBound - lowerBound)/2, accuracy=0.01),
           " error margin (Hanley-method) using 95% confidence intervals"))



# --- Calculate standard error of AUC-estimate using DeLong1988 based on U-statistics theory
# Wilcoxon-Mann-Whitney Kernel \psi
WMW.kernel <- function(x, y) {
  if (y < x) return(1)
  if (y == x) return(.5)
  if (y > x) return(0)
}

# - Calculate Wilcoxon-Mann-Whitney U-statistics (matrix)
# First extract probability scores from positives (X) and negatives (Y) subpopulations
X <- datExample[Class_Actual == "P", Score]; Y <- datExample[Class_Actual == "N", Score]
# For each negative Y_j for j=1,...,n_neg, calculate kernel (score comparison) across 
# all positives X_i for i=1,...,n_pos
U <- sapply(1:n_neg, function(j) sapply(1:n_pos, function(i, j) WMW.kernel(X[i], Y[j]), j=j))

# Slower version (but more readable):
# For each positive X_i for i=1,...,n_pos, calculate kernel across all negatives Y_j for j=1,...,n_neg
U2 <- matrix(NA, nrow=max(n_pos,n_neg), ncol=max(n_pos,n_neg))
for (i in 1:n_pos) { for(j in 1:n_neg) { U2[i,j] <- WMW.kernel(X[i], Y[j]) }}
all.equal(U,U2) # should be TRUE, then SAFE

# Can obtain AUC again from this U-statistic matrix
all.equal(A, sum(U)/(n_pos*n_neg) )  # AUC


# - Calculate the X and Y "structural components" from DeLong1988
V_X <- sapply(1:n_pos, function(i) {sum(U[i,])})/n_neg # V_10 in DeLong1988
V_Y <- sapply(1:n_neg, function(j) {sum(U[,j])})/n_pos # V_01 in DeLong1988
# Note: the mean of either quantity equals the AUC again

# Calculate "S" or the variance of AUC
var_AUC <- var(V_Y)/n_pos + var(V_X)/n_neg
# Note: var(V_Y) is the same as sum((V_Y - A)^2) / (n_pos - 1), and vice versa for var(V_X)
SE_A_DeLong <- sqrt(var_AUC)


# Output
cat(paste0("AUC: ", percent(A, accuracy=0.1), " ± ", percent(1.96 * SE_A_DeLong, accuracy=0.01), 
           " error margin (DeLong-method) using 95% confidence intervals\nStandard error: ", SE_A_DeLong))



##### Historical method from pROC for calculating variance of AUC
# Last implemented in v1.5.4 of the pROC-package circa 2012, before the authors moved
# parts thereof (i.e., "delongPlacementsCpp()") to C++

delong.placements <- function(roc_obj) {
  V <- list()
  X <- roc_obj$cases # probability scores of positives, the 10 component
  Y <- roc_obj$controls # probability scores of negatives, the 01 component
  n_neg <- length(Y)
  n_pos <- length(X)
  MW <- sapply(1:n_neg, function(j) sapply(1:n_pos, function(i, j) MW.kernel(X[i], Y[j]), j=j))
  V$theta <- sum(MW)/(n_pos*n_neg) # AUC
  # Delong-specific computations
  V$X <- sapply(1:n_pos, function(i) {sum(MW[i,])})/n_neg
  V$Y <- sapply(1:n_neg, function(j) {sum(MW[,j])})/n_pos
  return(V)
}

V <- delong.placements(pROC1)
var_AUC <- var(V$Y)/n_pos + var(V$X)/n_neg

# Calculate standard error of AUC
SE_A_DeLong2 <- sqrt(var_AUC)

# Output
cat(paste0("AUC: ", percent(A, accuracy=0.1), " ± ", percent(1.96 * SE_A_DeLong2, accuracy=0.01), 
           " error margin (DeLong-method) using 95% confidence intervals\nStandard error: ", SE_A_DeLong2))