#### Replication Paper
#### MSA-level Analysis 

#### VARIABLE DESCRIPTIONS (seth + isaac
# overlap index: 
# dissimilarity index: 
# lnfcrate: loan forclosure rate percentage of loaners who stop making payments, essentially defaulting

#### Upload the dataset cbsa_data.csv. This file contains the combined data at the CBSA-level for the calculated overlap indices. the dissimilarity indices calculations, the subprime and loan counts from the HMDA data, and Rugh and Massey's shared data. 
setwd("~/workspace/cs112/final project/segregation subprime/provided data")
# FIX: blank column corrected in original data set
cbsadata <- read.csv("cbsa_data_(publicview)-no-head1.csv", header=TRUE, stringsAsFactors=FALSE)
#### Install the following packages for R. 
install.packages("Zelig")

install.packages("MatchIt")
install.packages("cem")
library(Zelig)
library(MatchIt)

# FIX NOTE: make sure to download XQuartz from https://www.xquartz.org/ before loading cem library
library(cem)
library(Matching)
library("xtable") 

#### Remove CBSAs--McAllen, TX, El Paso, TX, Essex, MA, and Miami, FL.

#### add a column calculating the subprime rate
# FIX: fixed for loop
subprime_rate <- vector(length=nrow(cbsadata)) 
for (i in 1:nrow(cbsadata)) {
  # the subprime loan rate is the amount of sub prime loans given, divided by total loans given
  splrate <- cbsadata$subprime_count[i]/cbsadata$loans_count[i]
  subprime_rate[i] <- splrate 
}

# insert the calculated subprime loan rate from above into column 18 in cbsadata
cbsadata <- cbind(cbsadata[1:17],subprime_rate,cbsadata[18:33])

#### convert the overlap index to a number
# seth: what is the overlap index?
cbsadata$overlap_index <- as.numeric(cbsadata$overlap_index) 

#### add a column calculating the total black/hispanic population percentage
dim(cbsadata)
cbsadata[34:35, "bhisp"]<-NA
dim(cbsadata)
cbsadata$bhisp<-cbsadata$pblack2000+cbsadata$phisp2000
head(cbsadata)

#### subset a dataframe of variables of interest
# seth question: are these appropriate variables of interest?
matchdata <- cbind(cbsadata[1], cbsadata[6], cbsadata[18], cbsadata[19], cbsadata[8], cbsadata[23], cbsadata[24:27], cbsadata[29:31], cbsadata[32], cbsadata[34:35])
#matchdata <- cbind(cbsadata$cbsa, cbsadata$overlap_index, cbsadata$subprime_rate, cbsadata$lnfcrate, cbsadata$bh_w_diss, cbsadata$pcollege, cbsadata[24:27], cbsadata[29:31], cbsadata[32], cbsadata[34:35])

#### create a column indicating which quartile each CBSA is in by black/hispanic-white dissimilarity index
# FIX: not used, so we will take this out
summary(cbsadata$bh_w_diss)
diss_quart <- vector(length=nrow(matchdata))
for (i in 1:nrow(matchdata)){
  if(matchdata[i,5] >= 0.6323){
    diss <- 1} else {
      if(matchdata[i,5] >= 0.5410){
        diss <- 0.75} else {
          if(matchdata[i,5] >= 0.4808){
            diss <- .5} else { 
              diss <- 0.25}}}
  diss_quart[i] <- diss
}

matchdata <- cbind(matchdata[1:4],diss_quart,matchdata[5:16])

#### create a column indicating which quintile each CBSA is in by the black/hispanic-white dissimilarity index
quantile(cbsadata$bh_w_diss, seq(0,1,0.2))
diss_quant <- vector(length=nrow(matchdata))
for (i in 1:nrow(matchdata)){   
  if(matchdata[i,6] >= 0.5817562){
    diss <- 1} else {
      if(matchdata[i,6] < 0.4627850){
        diss <-0} else { 
          diss <- 2}}
  diss_quant[i] <- diss
}

matchdata <- cbind(matchdata[1:5],diss_quant,matchdata[6:17])

#### remove the middle quintile
# seth: they only want to play with exteme dissimilarity indexes, correct?
# Thus, looks like they're doing artificial treatment assignment based only on dissimilarity index
matchdata.quant <- subset(matchdata, matchdata$diss_quant!=2)

#### subset the data to even fewer variables of interest
matchquant2 <- as.data.frame(cbind(matchdata.quant[1:2], matchdata.quant[4], matchdata.quant[6], matchdata.quant[9], matchdata.quant[11], matchdata.quant[16:18]))

#### use coarsened exact matching to match, set to wide bins due to small 
#### number of observations and only match on necessary variables to match
# Note: here we are only based on the covariates bhisp and ratio_hs_hu2000, not others
cem.match.quant <- cem(treatment="diss_quant", data = matchquant2, 
                       cutpoints=list(bhisp=4, ratio_hs_hu2000=4), 
                       drop=c("cbsa","overlap_index","lnfcrate",
                              "diss_quant", "lnpop2008","ratio_boom_hist","lnmedhhinc"))
cem.match.quant

#### check balance prior to matching for only the covariates matched
# FIX: commented out line below to fix drop list variable names
#pre.imbalance <- imbalance(group=matchquant2$diss_quant, data=matchquant2, drop=c("matchquant2$cbsa","matchquant2$overlap_index","matchquant2$diss_quant","matchquant2$lnfcrate", "matchquant2$lnpop2008","matchquant2$ratio_boom_hist","matchquant2$lnmedhhinc"))
pre.imbalance <- imbalance(group=matchquant2$diss_quant, data=matchquant2, drop=c("cbsa","overlap_index","diss_quant","lnfcrate", "lnpop2008", "ratio_boom_hist", "lnmedhhinc"))
pre.imbalance

#### check balance prior to matching for additional covariates
pre.imbalance2 <- imbalance(group=matchquant2$diss_quant, data=matchquant2, drop=c("cbsa","overlap_index","diss_quant","lnfcrate", "ratio_boom_hist"))
pre.imbalance2

#### check balance after matching with additional covariates
cem.match.quant.data <- cbind(matchquant2, cem.match.quant$matched)
cem.match.quant.data <- subset(cem.match.quant.data, cem.match.quant$matched==TRUE)
post.imbalance <- imbalance(group=cem.match.quant.data$diss_quant, data=cem.match.quant.data, drop=c("cbsa","overlap_index","diss_quant","lnfcrate","cem.match.quant$matched", "ratio_boom_hist"))
# result: better matching on the variables we matched on
# result: worse matching on other test variables: lnpop2008 and lnmedhhinc
post.imbalance


# This section is their main regression, trying to find the treatment effect
# They group the variables into 4 categories\
# Outcome - dissimilarity index
# Segregation ( the main aspect they are testing) - bh_w

#### Average treatment effect on the treated (ATT) for just segregation plus demographic controls
# bh_w_diss index is segregation , lnpop2008 + lnmedhhinc + west + mw + south is demographic controls
cem.model.quant.diss <- att(obj=cem.match.quant, formula= overlap_index ~ bh_w_diss + lnpop2008 + lnmedhhinc + west + mw + south, data=matchdata.quant, model = "linear")
summary(cem.model.quant.diss)
xtable(summary(cem.model.quant.diss), digits=3)

#### ATT for alternative explanatory variables plus demographic controls 
# ratio_hs_hu2000 + ratio_boom_hist + bhisp are eplanatory, 
cem.model.quant.alt <- att(obj=cem.match.quant, formula= overlap_index ~ ratio_hs_hu2000 + ratio_boom_hist + bhisp + lnpop2008 + lnmedhhinc + west + mw + south, data=matchdata.quant, model = "linear")
summary(cem.model.quant.alt)
xtable(summary(cem.model.quant.alt), digits=3)

#### ATT for segregation plus other explanatory variables plus demographic controls
cem.model.quant.all <- att(obj=cem.match.quant, formula= overlap_index ~ bh_w_diss +ratio_hs_hu2000 + ratio_boom_hist + bhisp + lnpop2008 + lnmedhhinc + west + mw + south, data=matchdata.quant, model = "linear")
summary(cem.model.quant.all)
xtable(summary(cem.model.quant.all), digits=3)

#### Running original Rugh and Massey regression with their outcome variable (lnfcrate) with matched pairs
#### ATT for just segregation plus demographic controls on log forclosure rate
cem.model.quant.diss.rm <- att(obj=cem.match.quant, formula= lnfcrate ~ bh_w_diss + lnpop2008 + lnmedhhinc + west + mw + south, data=matchdata.quant, model = "linear")
summary(cem.model.quant.diss.rm)

#### ATT for alternative explanatory variables plus demographic controls on log foreclosure rate
cem.model.quant.alt <- att(obj=cem.match.quant, formula= lnfcrate ~ ratio_hs_hu2000 + ratio_boom_hist + bhisp + lnpop2008 + lnmedhhinc + west + mw + south, data=matchdata.quant, model = "linear")
summary(cem.model.quant.alt)

#### ATT for segregation plus other explanatory variables plus demographic controls on log foreclosure rate
cem.model.quant.all <- att(obj=cem.match.quant, formula= lnfcrate ~ bh_w_diss + ratio_hs_hu2000 + ratio_boom_hist + bhisp + lnpop2008 + lnmedhhinc + west + mw + south, data=matchdata.quant, model = "linear")
summary(cem.model.quant.all)
  
  