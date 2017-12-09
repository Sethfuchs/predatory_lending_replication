########################################################
# K. Steven Brown, Jackie Hwang, and Michael Hankinson #
###### Gov 2001 Final Replication Paper assignment #####
################ Fixed Effects Analysis ################
########################################################


setwd("E:/Replication_paper/April Analyses")

library(Design)
library(Zelig)
install.packages("xtable")
library(xtable)


# Uploading Raw Census Demographic data and creating a subset of majority-minority tracts
tracts.data2 <- read.csv("tracts_demogdata.csv", header=TRUE, sep=",")
tracts.frame2 <- as.data.frame(tracts.data2)
minor.data2 <- subset(tracts.data2, tracts.data2$blhisp_perc>=.5)
minor.frame2 <- as.data.frame(minor.data2)

#######################################
# Step 1
# All Tracts (including robust std err)
# 1.1 Only cluster as control
#######################################

olsreg1 <- ols(subprime_rate ~ cluster + factor(cbsay), data = tracts.frame2, x=TRUE, y=TRUE)
clust.err <- robcov(olsreg1, tracts.data2$cbsay)
cbind(clust.err$coefficients[1:2], sqrt(diag(clust.err$var))[1:2])

lmreg1 <- lm(subprime_rate ~ cluster + factor(cbsa), data = tracts.frame2)
reg1.resid <- resid(lmreg1)
plot(tracts.data2$subprime_rate, reg1.resid,
	xlab="Subprime Rate", ylab="Residuals")
abline(0,0)

# 1.2 All controls
##################

olsreg2 <- ols(subprime_rate ~ cluster + incomeratio_tractmsa + ownerunits_tract + factor(cbsa), data = tracts.frame2, x=TRUE, y=TRUE)
clust.err <- robcov(olsreg2, tracts.data2$cbsay)
cbind(clust.err$coefficients[1:4], sqrt(diag(clust.err$var))[1:4])

lmreg2 <- lm(subprime_rate ~ cluster + incomeratio_tractmsa + ownerunits_tract + factor(cbsa), data = tracts.frame2)
reg2.resid <- resid(lmreg2)
plot(tracts.data2$subprime_rate, reg2.resid,
	xlab="Subprime Rate", ylab="Residuals")
abline(0,0)


###########################
###########################



#####################################################
# Step 2
# Majority-Minority Tracts (including robust std err)
# 2.1 Only cluster as control
######################################################

olsreg3 <- ols(subprime_rate ~ cluster + factor(cbsa), data = minor.frame2, x=TRUE, y=TRUE)
clust.err <- robcov(olsreg3, minor.data2$cbsay)
cbind(clust.err$coefficients[1:2], sqrt(diag(clust.err$var))[1:2])

lmreg3 <- lm(subprime_rate ~ cluster + factor(cbsa), data = minor.frame2)
reg3.resid <- resid(lmreg3)
plot(minor.data2$subprime_rate, reg3.resid,
	xlab="Subprime Rate", ylab="Residuals")
abline(0,0)


# 2.2. All controls
###################

olsreg4 <- ols(subprime_rate ~ cluster + incomeratio_tractmsa + ownerunits_tract + factor(cbsa), data = minor.frame2, x=TRUE, y=TRUE)
clust.err <- robcov(olsreg4, minor.data2$cbsay)
cbind(clust.err$coefficients[1:4], sqrt(diag(clust.err$var))[1:4])

lmreg4 <- lm(subprime_rate ~ cluster + incomeratio_tractmsa + ownerunits_tract + factor(cbsa), data = minor.frame2)
reg4.resid <- resid(lmreg4)
plot(minor.data2$subprime_rate, reg4.resid,
	xlab="Subprime Rate", ylab="Residuals")
abline(0,0)


######################################
######################################



