# =================================== BIAS-VARIANCE TRADE-OFF (KNN) ==========================================
# Fitting 3 KNN-classifiers on designed data towards illustrating the bias-variance trade-off phenomenon
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
library(Hmisc) #for describe()
library(data.table)
library(MASS) # for sampling from multivariate normal distribution
library(caret) # for KNN algorithm
library(class) # for another KNN algorithm
library(mixtools) #for drawing ellipse
options(scipen=999)




# ------- 1. Tutorial: Sampling from bivariate normal distribution:
# Generate two Normally-distributed random variables with a given correlation structure.
# source material: https://blog.revolutionanalytics.com/2016/08/simulating-form-the-bivariate-normal-distribution-in-r-1.html

N <- 200 # sample size
rho <- -0.6 # correlation factor
mu <- c(1,1) # mean vector
sigma <- c(2, 8) #standard deviation vector
cov.mat <- matrix(c(sigma[1]^2, sigma[1]*sigma[2]*rho, sigma[2]*sigma[1]*rho, sigma[2]^2),NROW(sigma)) # covariance matrix)

# Function to draw ellipse for bivariate normal data
ellipse_bvn <- function(bvn, alpha){
  Xbar <- apply(bvn,2,mean)
  S <- cov(bvn)
  ellipse(Xbar, S, alpha = alpha, col="red")
}

# 1) the MASS-way (eigen vectors)
bivar.norm.1 <- mvrnorm(n=N, mu=mu, Sigma=cov.mat)
colnames(bivar.norm.1) <- c("bvn1_X1","bvn1_X2")


# 2) the Cholesky decomposition-way (taking that of the covariance matrix, which is a positive definite matrix)
M <- t(chol(cov.mat))
# M %*% t(M) should give originally input matrix back again
Z <- matrix(rnorm(2*N),2,N) # 2 rows, N/2 columns
bivar.norm.2 <- t(M %*% Z) + matrix(rep(mu,N), byrow=TRUE,ncol=2)
colnames(bivar.norm.2) <- c("bvn2_X1","bvn2_X")



# ------- 2. Simulating binary-valued data (Cats & Dogs) in 2D-space
# Follow the 2-class simulation approach from Hastie2009 (pp. 16--17)
# We want to output a "mixture of low-variance Gaussian clusters for each class" at the end.
# with "individual menas themselves distribution as Gaussian".

# -- Step 1
# - GREEN's means "m_1"
N <- 10; # number of means
mu <- c(1,0);
cov.mat <- matrix(c(1,0,0,1),NROW(mu)) # identity matrix, so no correlation structure.
bivar.norm.1 <- mvrnorm(n=N, mu=mu, Sigma=cov.mat)
colnames(bivar.norm.1) <- c("X1", "X2")

# - ORANGE's means "m_2"
N <- 5; # number of means
mu <- c(0,1);
cov.mat <- matrix(c(1,0,0,1),NROW(mu)) # identity matrix, so no correlation structure.
bivar.norm.2 <- mvrnorm(n=N, mu=mu, Sigma=cov.mat)
colnames(bivar.norm.2) <- c("X1", "X2")

# -- Step 2
NN <- 300;
# choose GREEN means randomly NN times, following a simple uniform distribution.
chosen_means.1 <- bivar.norm.1[sample(x=1:N,size=NN, replace=T),]
# choose ORANGE means randomly NN times, following a simple uniform distribution.
chosen_means.2 <- bivar.norm.2[sample(x=1:N,size=NN, replace=T),]

# -- Step 3
# - GREEN
data.1 <- t(apply(chosen_means.1, 1, function(x) mvrnorm(n=1, mu=x, Sigma=cov.mat/5)))
# - ORANGE
data.2 <- t(apply(chosen_means.2, 1, function(x) mvrnorm(n=1, mu=x, Sigma=cov.mat/5)))

# -- Step 4
# Combine data
sim.data <- rbind(data.table(data.1, Class="DOG"),
                   data.table(data.2, Class="CAT"))

vCol <- c("#fc8d62", "#66c2a5")
ggplot(sim.data, aes(x=X1, y=X2, shape=Class, colour=Class)) + theme_minimal() +
  geom_point(size=3) + scale_color_manual(values=vCol) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(color="black", fill="white"),
        legend.position = "bottom")
  

# -- Data partitioning for test/validation
set.seed(123)
sim.data<- sim.data[sample(nrow(sim.data)),]
dat.train <- sim.data[1:as.integer(0.8*nrow(sim.data)),]
dat.valid <- sim.data[as.integer(0.8*nrow(sim.data) + 1):nrow(sim.data) ,]





# ------- 3. KNN model | caret (autoselect k)
# Source material: https://dataaspirant.com/2017/01/09/knn-implementation-r-using-caret-package/
# -- Training
trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(3333)
knn_fit <- train(Class ~., data = dat.train, method = "knn",
                 trControl=trctrl,
                 #preProcess = c("center", "scale"),
                 tuneLength = 350)
knn_fit

# -- Testing
test_pred <- predict(knn_fit, newdata = dat.valid)
confusionMatrix(test_pred, as.factor(dat.valid$Class) )




# ------- 4. KNN model | class (knn with specific value)
# Source material: https://stats.stackexchange.com/questions/21572/how-to-plot-decision-boundary-of-a-k-nearest-neighbor-classifier-from-elements-o

# custom training/plotting function for creating a 2-dimensional decision plot.
# Assumes input variables are named X1 and X2, and target variable is named "Class"
decisionPlot <- function(data, given.k) {
  #data<-sim.data
  # get the range of x1 and x2
  rx1 <- range(data$X1)
  rx2 <- range(data$X2)
  
  # get lattice points in input space. This is to help with background ("lattice") of eventual plot
  px1 <- seq(from = rx1[1], to = rx1[2], by = 0.1 )
  px2 <- seq(from = rx2[1], to = rx2[2], by = 0.1 )
  xnew <- expand.grid(x1 = px1, x2 = px2)
  
  # Train a KNN
  knn.fit <-  knn(train=data[,list(X1,X2)], test=xnew, cl=data$Class, k=given.k, prob=T)
  
  # prepare class probabilities into matrix form, according to lattice
  prob <- attr(knn.fit, "prob")
  prob <- ifelse(knn.fit==levels(knn.fit)[1], prob, 1-prob)
  prob.1 <- matrix(prob, nrow = length(px1), ncol = length(px2))
  
  # plot decision plot with boundaries
  par(mar = rep(2,4))
  contour(px1, px2, prob.1, levels=0.5, labels="", xlab="", ylab="", main=, axes=FALSE, lwd=2)
  points(data[,list(X1,X2)], col=ifelse(data$Class==levels(knn.fit)[1], "#fc8d62", "#66c2a5"),
         pch=ifelse(data$Class==levels(knn.fit)[1], 16,17))
  points(xnew, pch=".", cex=1.2, col=ifelse(prob.1>0.5, "#fc8d62", "#66c2a5"))
  legend(min(data$X1), max(data$X2), legend=c("CAT", "DOG"), col = c("#fc8d62", "#66c2a5"), 
         pch=c(16,17), cex=0.8, box.lty=0, bg="transparent")
  box()
}

png(paste0(genFigPath, "knn-4.png"), width=1200, height=1100, res=200)
decisionPlot(data=sim.data, given.k=4)
dev.off() # close file

png(paste0(genFigPath, "knn-75.png"), width=1200, height=1100, res=200)
decisionPlot(data=sim.data, given.k=75)
dev.off() # close file

png(paste0(genFigPath, "knn-300.png"), width=1200, height=1100, res=200)
decisionPlot(data=sim.data, given.k=300)
dev.off() # close file
