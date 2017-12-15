# Code by:
# Isaac Schaal
# Seth Fuchs
# Minerva Schools at KGI, CS112

######################################################################
########################### DATA PREP ################################
######################################################################
options("scipen" =100, "digits" = 4) # override R's tendency to use scientific notation
library(Matching)
library(foreign)
library(rbounds)


# create a copy of replication paper's dataset
matchquant3 <- matchquant2 

# two outcome variables, as per replication paper
Y_overlap_index <- matchquant3$overlap_index
Y_forclosure_rate <- matchquant3$lnfcrate

# specify the treatment variable
Tr <- matchquant3$diss_quant

# All covariates we want balance on
# Same as covariates of interest in the replication paper
X_all <- matchquant3[, c("lnpop2008", "lnmedhhinc", "ratio_hs_hu2000", "bhisp")]

# Run gen match on ALL FOUR covariates of interest, utilizing the
# full power of gen match in terms of achieving better overall balance
# variables: lnpop2000, lnmedhhinc, ratio_hs_hu2000, bhisp
# note that original replication only matched on two covars

######################################################################
########################### NO CALIPER ###############################
######################################################################

all_four_vars <- matchquant3[, c("bhisp", "ratio_hs_hu2000", "lnpop2008", "lnmedhhinc")]

genout.four <- GenMatch(Tr=Tr, X=all_four_vars, BalanceMatrix=X_all, estimand="ATT",
                        pop.size=100, max.generations=200, wait.generations=200)

# see outcome of matching + estimate casual effect using these weights
gen.four.match.out <- Match(Y=Y_overlap_index, Tr=Tr, X=all_four_vars, estimand="ATT", 
                            Weight.matrix=genout.four)

summary(gen.four.match.out)

# Lets see if we have achieved balance
gen.mb_four  <- MatchBalance(diss_quant ~ lnpop2008 + lnmedhhinc + ratio_hs_hu2000 + bhisp, 
                             data=matchquant3, match.out=gen.four.match.out, nboots=500)


######################################################################
########################### WITH CALIPER #############################
######################################################################

genout.four.caliper <- GenMatch(Tr=Tr, X=all_four_vars, BalanceMatrix=X_all, estimand="ATT", 
                                caliper = 1, pop.size=100, max.generations=300, 
                                wait.generations=100)

# see outcome of matching + estimate casual effect using these weights
gen.four.caliper.match.out <- Match(Y=Y_overlap_index, Tr=Tr, X=all_four_vars, estimand="ATT", 
                                    caliper = 1, Weight.matrix=genout.four.caliper)
summary(gen.four.caliper.match.out)

# Lets see if we have achieved balance
gen.mb_four.caliper  <- MatchBalance(diss_quant ~ lnpop2008 + lnmedhhinc + ratio_hs_hu2000 + bhisp, 
                             data=matchquant3, match.out=gen.four.caliper.match.out, nboots=500)


######################################################################
######################## Sensitivity Test ############################
######################################################################

psens(gen.four.match.out, Gamma=5, GammaInc=.3)

hlsens(gen.four.match.out, Gamma=5, GammaInc=.1)
