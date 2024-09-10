# =================================== STUDY OF INFORMATION MEASURES ==========================================
# Manual calculation, illustration, and comparison of various information measures, which includes:
#     Shannon Entropy, cross-entropy, Kullback-Leibler's D-divergence, and Jeffrey's J-divergence
# ------------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha
# -----------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R | Only for plotting purposes
# ============================================================================================================



# ------- 0. Setup

library(ggplot2)
library(scales)
library(RColorBrewer)
library(extrafont)
library(Hmisc)
library(tidyr)
library(dplyr)
library(data.table)
library(Metrics) # for validating Cross-Entropy ("logloss")
library(entropy) # for validating Shannon entropy
library(LaplacesDemon) # for validating Kullback-Leibler and Jeffrey divergences




# ------ 1. Custom functions

# - Function to convert NaN-values or infinite values within a vector to the given value 
Treat_NaN <- function(vec, replaceVal=0) {
  vec[is.nan(vec) | is.infinite(vec)] <- replaceVal
  return (vec)
}

# - Messaging function
reportBack <- function(H_qq, H_qp, D_qp, J_qp) { 
  cat("Shannon entropy H(q): \t\t\t\t\t", H_qq, "\nCross-entropy of p relative to q, H_q(p): \t\t", H_qp, 
      "\nKullback-Leibler D-divergence of p to q, D_q(p): \t", D_qp, "\nJeffreys J-divergence between q and p, J(q,p): \t\t", J_qp)
}


# Checking specific relations and boundaries of divergences, supported by literature
divChecks <- function(H_qq, H_qp, D_qp, D_pq, J_qp) {
  cat("\n=== Performing various sanity checks on given divergence measures ===\n")
  
  # - Demonstrate innate relationship between Kullback-Leibler divergence, and cross-entropy and entropy: KL = H_q(p) - H(q)
  check1 <- round(H_qp - H_qq,3) == round(D_qp,3)
  if (check1) {cat("\nSAFE: D_q(p) = H_q(p) - H(q) holds, i.e.,", round(D_qp,4), "=",round(H_qp,4),"-",round(H_qq,4))} else 
  {cat("\nERROR: D_q(p) = H_q(p) - H(q) does not hold, i.e.,", round(D_qp,4), "=",round(H_qp,4),"-",round(H_qq,4))}
  
  # - Demonstrate innate relationship between Jeffrey's J-divergence and Kullback-Leibler divergence: J(q,p) = D_q(p) + D_p(q)
  check2 <- round(D_qp + D_pq,3) == round(J_qp,3)
  if (check2) {cat("\nSAFE: J(q,p) = D_q(p) + D_p(q) holds, i.e.,", round(J_qp,4), "=",round(D_qp,4),"+",round(D_pq,4))} else 
  {cat("\nERROR: J(q,p) = D_q(p) + D_p(q) does not hold, i.e.,", round(J_qp,4), "=",round(D_qp,4),"+",round(D_pq,4))}
  
  cat("\n\n=== End of sanity checks ===")
}



# ------ 2. Shannon Entropy over probability spectrum

# --- Illustrating Shannon entropy of a Bernoulli random variable (0 & 1), with probability spectrum thereof as p

# - Design probability spectrum
p <- seq(0,1,0.01)
H <- -1*(p*log2(p) + (1-p)*log2(1-p)) # Shannon Entropy H
dat <- data.frame(p=p, H=H)

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 220

# - Create plot
(g <- ggplot(dat, aes(x=p, y=H)) + theme_minimal() + 
    labs(x=bquote("Probability of Bernoulli random variable "*italic(q)), 
         y=bquote("Shannon Entropy "*italic(H) )) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    geom_point(aes(colour=H)) + 
    scale_colour_distiller(type="seq", palette="YlOrRd", direction=1, name=bquote("Surprisal level "*italic(H))) + 
    scale_x_continuous(labels=percent)
)

# - Save graph
ggsave(g, file=paste0(genFigPath, "ShannonEntropy.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")



# --- Illustrating Shannon entropy for a known probability of a Bernoulli random variable
q1 <- 0.7
(H <- -1*(q1*log2(q1) + (1-q1)*log2(1-q1))) # Shannon Entropy H(q)


# --- Illustrating Shannon entropy for a sample of generated probabilities for a Bernoulli random variable
q <- as.factor(rbinom(n=100000, size=1, prob=0.7)) # size starts at 0, therefore minus 1
q_outcomes <- unique(q)
describe(q); qplot(q) + theme_minimal()

q1 <- sum(q==q_outcomes[1]) / length(q)
(H <- -1*(q1 *log2(q1) + (1-q1)*log2(1-q1))) # Shannon Entropy H(q)



# --- Illustrating Shannon entropy for a sample of generated probabilities for a Binomial random variable
outcomes <- 3
q <- as.factor(rbinom(n=100000, size=outcomes-1, prob=0.7)) # size starts at 0, therefore minus 1
q_outcomes <- unique(q)
describe(q); qplot(q) + theme_minimal()

for (y in 1:(outcomes)) {
  q_y <- sum(q==q_outcomes[y]) / length(q)
  if (y==1) { H <- q_y*log2(q_y) } else { H <- H + q_y*log2(q_y) }
}
(H <- -H)


# --- Illustrating Shannon entropy for multiple rolls of a fair 6-sided dice
outcomes <- 6
q <- as.factor(sample(x=1:outcomes, size=100000, prob=rep(1/outcomes, times=outcomes), replace=T))
q_outcomes <- unique(q)
describe(q); qplot(q) + theme_minimal()

for (y in 1:(outcomes)) {
  q_y <- sum(q==q_outcomes[y]) / length(q)
  if (y==1) { H <- q_y*log2(q_y) } else { H <- H + q_y*log2(q_y) }
}
(H <- -H)




# ------ 3. Illustrating various information measures

# -- Initialize simulation parameters
n <- 100000
MainEvent <- 1



# -- Simulate distributions

# - Simulate baseline distribution for q in generating rare events, e.g., loan defaults
q <- rbinom(n=n, size=1, prob=0.1) # size starts at 0, therefore minus 1
describe(q); ggplot(data.frame(x=q),aes(x)) + geom_bar() + theme_minimal()

# - Simulate posterior class probability distribution for p
# We deliberately introduce some error by misaligning the rbinom()-specification, compared to q's specification
p <- rbinom(n=100000, size=1, prob=0.2) # size starts at 0, therefore minus 1
describe(p); ggplot(data.frame(x=p),aes(x)) + geom_bar() + theme_minimal()

# - Simulate probability scores for estimating the posterior class probability distribution
# NOTE: This p-distribution can be the probability scores yielded by some logistic model given X, in approximating q
p_scores <- 0.00001 + c(round(rbeta(n, shape1 = 5, shape2 = 20),5))
describe(p_scores); hist(p_scores, breaks="FD")



# --- Calculate  various information metrics of p relative to q
# - Prevalence (Actual) in estimating the Prior class probability
q1 <- sum(q==MainEvent) / length(q)

# - Prevalence (Expected) in estimating the Posterior class probability given X
p1 <- mean(p, na.rm=T)


# - Shannon entropy of q
H_qq <- -1*(q1 *log2(q1) + (1-q1)*log2(1-q1))
# Alternative (gives same result)
q_probs <- prop.table(table(q)); -sum(q_probs * log2(q_probs)); all.equal( -sum(q_probs * log2(q_probs)), H_qq)
# Validate against R-package
entropy(q_probs, unit="log2"); all.equal(entropy(q_probs, unit="log2"), H_qq)


# - Binary Cross-Entropy (BCE) of p relative to q | Discrete probability distributions
H_qp_disc <- -1*(q1 *log2(p1) + (1-q1)*log2(1-p1))
logLoss(c(q1,1-q1), c(mean(p), mean(1-p))) # slightly different but only due to them using log instead of log2


# - Binary Cross-Entropy (BCE) of p relative to q | Continuous probability distributions
H_qp <- -mean( ifelse(q==MainEvent,1,0) * log2(p_scores) +
                   (1-ifelse(q==MainEvent,1,0)) * log2(1-p_scores), na.rm=T )
# Validate against R-package
logLoss(q, p_scores) # slightly different but only due to them using log instead of log2



# - Kullback-Leibler (KL) divergence of p relative to q | Discrete probability distributions
chosenBase <- 2
D_qp <- q1*log(q1/p1, base=chosenBase) + (1-q1)*log((1-q1)/(1-p1), base=chosenBase)
D_pq <- p1*log(p1/q1, base=chosenBase) + (1-p1)*log((1-p1)/(1-q1), base=chosenBase) # Kullback-Leibler (KL) divergence of q relative to p (used as check later)
# NOTE: the following definition will equal BCE H_qp and is invalid since KL is defined only for discrete probability distributions
#mean( Treat_NaN( ifelse(q==MainEvent,1,0) * 
 #                          log2(ifelse(q==MainEvent,1,0) / p) ) +
  #              Treat_NaN( (1-ifelse(q==MainEvent,1,0)) * 
   #                          log2( (1-ifelse(q==MainEvent,1,0)) / (1-p)) 
    #            ), na.rm=T )
# Validate against R-package
KLD(c(q1,1-q1), c(mean(p), mean(1-p)), base=chosenBase)$sum.KLD.px.py


# - Jeffreys' J-divergence (information value) of p relative to q | Discrete probability distributions
J_qp <- (q1-p1)*log(q1/p1, base=chosenBase) + ((1-q1)-(1-p1))*log2((1-q1)/(1-p1))
# NOTE: the following definition seems plausible but is invalid since KL is defined only for discrete probability distributions
#mean( Treat_NaN ( (ifelse(q==MainEvent,1,0) - p) * 
 #                           log2(ifelse(q==MainEvent,1,0) / p) ) +
  #              Treat_NaN ( ((1-ifelse(q==MainEvent,1,0)) - (1-p)) * 
   #                           log2( (1-ifelse(q==MainEvent,1,0)) / (1-p))
    #            ), na.rm=T )


# - Sanity checks
reportBack(H_qq, H_qp, D_qp, J_qp) # basic information measures
divChecks(H_qq, H_qp, D_qp, D_pq, J_qp) # While correct, the (continuous) BCE-estimator breaks certain relationships
reportBack(H_qq, H_qp=H_qp_disc, D_qp, J_qp) # basic information measures
divChecks(H_qq, H_qp=H_qp_disc, D_qp, D_pq, J_qp)




# ------ 4. Comparing information measures across 2 candidate classifiers

# --- Simulate more distributions towards creating comparative graph
# simulate another 'posterior': a fair coin
p2 <- rbinom(n=100000, size=1, prob=0.5) # size starts at 0, therefore minus 1
p2_outcomes <- unique(p2)
describe(p2); ggplot(data.frame(x=p2),aes(x)) + geom_bar() + theme_minimal()

# - Calculate additional information metrics of new p2 relative to q | Discrete probability distributions
p1_2 <- sum(p2==p2_outcomes[1]) / length(p2)
H_qp2 <- -1*(q1 *log2(p1_2) + (1-q1)*log2(1-p1_2)) # Binary Cross-Entropy (BCE) of p relative to q
D_qp2 <- q1 *log2(q1/p1_2) + (1-q1)*log2((1-q1)/(1-p1_2)) # Kullback-Leibler (KL) divergence of p relative to q
D_pq2 <- p1_2 *log2(p1_2/q1) + (1-p1_2)*log2((1-p1_2)/(1-q1)) # Kullback-Leibler (KL) divergence of q relative to p (used as check later)
J_qp2 <- (q1-p1_2)*log2(q1/p1_2) + ((1-q1)-(1-p1_2))*log2((1-q1)/(1-p1_2)) # Jeffreys' J-divergence (information value) of p relative to q
reportBack(H_qq, H_qp, D_qp, J_qp) # basic information measures
divChecks(H_qq, H_qp2, D_qp2, D_pq2, J_qp2)

# - Combine into a plotting-specific dataset
datPlot <- data.table(id= 1:length(q), a_Y=as.factor(q), b_x1=as.factor(p), c_x3=as.factor(p2)) %>% 
  pivot_longer(cols=a_Y:c_x3, names_to="Dist", values_to="Value") %>% as.data.table()

# - Aesthetic engineering | Proportion annotations
props <- datPlot[order(Dist), list(Avg=mean( as.numeric(as.character(Value)), na.rm=T)), by=list(Dist)] # first summarise
props[, Value := as.factor(1)]
props <- rbind(props, data.table(Dist=unique(props$Dist), Avg=1-props$Avg, Value=as.factor(0)))
props[, Avg_Label := paste0(sprintf("%.1f", Avg*100),"%")]

# - Aesthetic engineering | General
vLabels <- c(bquote("Baseline "*italic(q)*" (prior)"), bquote("a: Model "*italic(p)[1]*" (posterior)"), 
              bquote("b: Fair coin "*italic(p[F])))
vLabels2 <- c(bquote(italic(C)[0]), bquote(italic(C)[1]))
chosenFont <- "Cambria"

# - Create graph to compare distributions and information metrics
(g <- ggplot(datPlot, aes(x=Value, group=Dist)) + theme_minimal() + 
  theme(legend.position = "bottom", text=element_text(family=chosenFont)) + 
  labs(x=bquote("Value of Bernoulli random variable "*italic(Y)), y="Outcome proportion (%)") + 
    # main bar graph & text annotations
  geom_bar(aes(fill=Dist, y=after_stat(prop)), position = position_dodge()) + 
  geom_text(aes(group=Dist,x=Value, y=Avg, label=Avg_Label), data=props,
            position = position_dodge(width = 0.9), vjust = -0.6, size=4, family=chosenFont) + 
    # annotations | Model 1
  annotate(geom="text", x=1.63, y=0.92, family=chosenFont, size=3, parse=T,
           label=paste0("'Shannon entropy '*italic(H(q))=='", sprintf("%#.3f", H_qq),"'")) + 
  annotate(geom="text", x=1.71, y=0.85, family=chosenFont, size=3, parse=T,
             label=paste0("'Model 1: Cross-entropy '*italic(H[q](p[1]))=='", sprintf("%#.3f", H_qp),"'")) +  
  annotate(geom="text", x=1.865, y=0.81, family=chosenFont, size=3, parse=T,
           label=paste0("'Model 1: Kullback-Leibler Divergence '*italic(D[q](p[1]))=='", sprintf("%#.3f", D_qp),"'")) +   
  annotate(geom="text", x=1.765, y=0.77, family=chosenFont, size=3, parse=T,
            label=paste0("'Model 1: Jeffrey Divergence '*italic(J(q,p[1]))=='", sprintf("%#.3f", J_qp),"'")) +   
    # annotationts | Model 2
  annotate(geom="text", x=1.72, y=0.69, family=chosenFont, size=3, parse=T,
           label=paste0("'Model 2: Cross-entropy '*italic(H[q](p[F]))=='", sprintf("%#.3f", H_qp2), "'")) +  
  annotate(geom="text", x=1.875, y=0.65, family=chosenFont, size=3, parse=T,
           label=paste0("'Model 2: Kullback-Leibler Divergence '*italic(D[q](p[F]))=='", sprintf("%#.3f", D_qp2),"'")) +      
  annotate(geom="text", x=1.77, y=0.61, family=chosenFont, size=3, parse=T,
             label=paste0("'Model 2: Jeffrey Divergence '*italic(J(q,p[F]))=='", sprintf("%#.3f", J_qp2),"'")) +       
    # facets & scale options
  scale_fill_brewer(palette="Dark2", name="Distribution", labels=vLabels) + 
  scale_y_continuous(labels=percent) + scale_x_discrete(labels=vLabels2)
)

# - Save graph
dpi <- 220 # manually tweaked until all subscripts are shown
ggsave(g, file=paste0(genFigPath, "InformationMeasures_Basic.png"), width=1200/dpi, height=1200/dpi, dpi=dpi, bg="white")










# --- 5. Continuous probability distributions
### AB: Study https://www.tsc.uc3m.es/~fernando/bare_conf3.pdf and implement their estimator and experiment

# - Simulate baseline distribution for q from given probability distribution
q <- dnorm(runif(100000), 0, 1)
describe(q); ggplot(data.frame(x=q),aes(x)) + geom_histogram() + theme_minimal()

# - Simulate posterior class probability distribution for p 
p <- dnorm(runif(100000), 0, 2)
describe(p); ggplot(data.frame(x=p),aes(x)) + geom_histogram() + theme_minimal()

