# Sample size estimation based on the liver data set

rm(list = ls())

# load the packages
pacman::p_load(multcomp, nparcomp, rankFD, tidyr)

# load the functions
# setwd()
source("./ShinyApp/source/functions_par.R")
source("./ShinyApp/source/functions_nonpar.R")

# load the data
data(liver)


# parametric approach -----------------------------------------------------
n <- table(liver$dosage)
Xbar <- as.numeric( tapply(liver$weight, liver$dosage, mean) )
varEst <- as.numeric( tapply(liver$weight, liver$dosage, var) )

CI_l <- Xbar - qnorm(0.975)*sqrt(varEst)/sqrt(n) 
CI_u <- Xbar + qnorm(0.975)*sqrt(varEst)/sqrt(n) 

cohens_d <- c((Xbar[2]- Xbar[1])/sqrt(0.5 * (varEst[1]+varEst[2])),
              (Xbar[3]- Xbar[1])/sqrt(0.5 * (varEst[1]+varEst[3])), 
              (Xbar[4]- Xbar[1])/sqrt(0.5 * (varEst[1]+varEst[4])),
              (Xbar[5]- Xbar[1])/sqrt(0.5 * (varEst[1]+varEst[5])))

mu_star <- contrMat(n = rep(10,5),"Dunnett")%*%Xbar

C <- contrMat(n = rep(10,4), "Dunnett")

# allocation rate in the balanced and unbalanced case
t <- rep(1/4, 4) # balanced
# t <- c(0.4,.2,.2,.2) # unbalanced

# calculate the sample size when assuming homogeneous variances 
SingleSimHom_tfix(mu = Xbar[1:4], sigma = sqrt(varEst[5]), C = C, t = t, power = 0.8, seed = 2)
SingleSimHom_tfix(mu = CI_l[1:4], sigma = sqrt(varEst[5]), C = C, t = t, power = 0.8, seed = 2) # lower CI
SingleSimHom_tfix(mu = CI_u[1:4], sigma = sqrt(varEst[5]), C = C, t = t, power = 0.8, seed = 2) # upper CI

# calculate the sample size when assuming heteroscedastic variances 
SingleSimHet_tfix(mu = Xbar[1:4], sigma = sqrt(c(.05,.1,.05,.2)), C = C, t = t, power = 0.8, seed = 2)
SingleSimHet_tfix(mu = CI_l[1:4], sigma = sqrt(c(.05,.1,.05,.2)), C = C, t = t, power = 0.8, seed = 2) # lower CI
SingleSimHet_tfix(mu = CI_u[1:4], sigma = sqrt(c(.05,.1,.05,.2)), C = C, t = t, power = 0.8, seed = 2) # upper CI

# calculate the correlation matrix, homogeneous vars
Sigmahat <- sqrt(varEst[5])*diag(t)
CShat <- C %*% Sigmahat %*% t(C)
cov2cor(CShat)

# calculate the correlation matrix, heteroscedastic vars
Sigmahat <- diag( t * sqrt(c(.05,.1,.05,.2)) )
CShat <- C %*% Sigmahat %*% t(C)
cov2cor(CShat)


# nonparametric approach --------------------------------------------------
# create subsets of the data
liver12 <- subset(liver, dosage %in%c(1,2))
liver13 <- subset(liver, dosage %in%c(1,2))
liver14 <- subset(liver, dosage %in%c(1,2))

# two sample tests for the nonparametric Behrens-Fisher problem, 
# calculate the relative effect and confidence interval
npar12 <- npar.t.test(weight ~ dosage, data = liver12, method = "normal")$Analysis
npar13 <- npar.t.test(weight ~ dosage, data = liver12, method = "normal")$Analysis
npar14 <- npar.t.test(weight ~ dosage, data = liver12, method = "normal")$Analysis

# relative effect 
phat <- c(npar12$Estimator, npar13$Estimator, npar14$Estimator)
phat[3] <- 0.9 # shrink the effect conservatively

# transform the data to the wide format
liver$seq <- with(liver, ave(weight, dosage, FUN = seq_along))
liver_wide <- reshape2::dcast(seq ~ dosage, data = liver, value.var = "weight")
colnames(liver_wide)[2:ncol (liver_wide)] <- c("placebo", paste0("dose", 1:4))
pilot <- liver_wide[, 2:ncol(liver_wide)]
pilot <- liver_wide[, c( "placebo", "dose1", "dose2", "dose3")]

# allocation rates in the balanced and unbalanced case
t <- rep(1/4, 4) # balanced
# t <- c(0.4,.2,.2,.2) # unbalanced

# calculate the sample size
SingleSim_tfix(p = phat, t = t, power_inp = 0.8, approx = "A", seed = 10, C = "Dunnett")
SingleSim_tfix(p = phat, t = t, power_inp = 0.8, approx = "B", seed = 10, C = "Dunnett", datPilot = pilot)
SingleSim_tfix(p = phat, t = t, power_inp = 0.8, approx = "C", seed = 10, C = "Dunnett", datPilot = pilot)
SingleSim_tfix(p = phat, t = t, power_inp = 0.8, approx = "D", seed = 10, C = "Dunnett", datPilot = pilot)
SingleSim_tfix(p = phat, t = t, power_inp = 0.8, approx = "E", seed = 10, C = "Dunnett")

# estimate the variance-covariance matrix based on the pilot data 
Results <- estimatorstar(weight ~ dosage,liver)
a <- 5
nc <- a-1
W <- matrix(0, ncol = a^2, nrow = nc)
for(s in 1:nc){for(ss in 1:(a^2)){if(ss==(s+1)){W[s,ss]<-1}}}
sigma0star <- round(c(W%*%Results$sigma0star),4)
round(W%*%Results$V1star%*%t(W) + W%*%Results$V2star%*%t(W),4)


