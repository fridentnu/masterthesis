#### LOAD PACKAGES #####
library(scoringRules) # crps
library(DescTools) # brier score
library(tidyverse) # ggplot2, dplyr etc.

# References for the evaluation methods can be found in Chapter 3 in the master thesis

# Load Framingham model
load("MyData/Framingham.RData")

# Run Models.R to access models and the estimated parameters of the prediction distributions
source("R code/Models.R")

###### OBSERVED HYPERTENSION #######
obs.hyp <- sum(df.total$SystolicHyp)/length(df.total$SystolicHyp)
obs.hyp

######################### PROBABILITY OF SYSTOLIC HYPERTENSION ###########################

####### FULL GAUSSIAN MODEL
prob.hyp.full.pred <-pnorm(140, mean=full.pred.mod$fitted.values, sd=full.sd.y, lower.tail = F)
prob.hyp.full.pred

# Mean of probabilities of systolic hypertension
exp.prob.hyp.full.pred <- round(100*mean(prob.hyp.full.pred),3)
exp.prob.hyp.full.pred

######## SMALL GAUSSIAN MODEL
prob.hyp.small.pred <-pnorm(140, mean=small.pred.mod$fitted.values, sd=small.sd.y, lower.tail = F)
prob.hyp.small.pred

# Mean of probabilities of systolic hypertension
exp.prob.hyp.small.pred <- round(100*mean(prob.hyp.small.pred),3)
exp.prob.hyp.small.pred


###### FULL GAMMA MODEL
prob.hyp.full.gamma.pred <-pgamma(140, shape=full.gamma.shape,rate=full.gamma.rate, lower.tail = F)

# Mean of probabilities of systolic hypertension
exp.prob.hyp.full.gamma.pred <- round(100*mean(prob.hyp.full.gamma.pred),3)
exp.prob.hyp.full.gamma.pred


### SMALL GAMMA MODEL
prob.hyp.small.gamma.pred <-pgamma(140, shape=small.gamma.shape,rate=small.gamma.rate, lower.tail = F)

# Mean of probabilities of systolic hypertension
exp.prob.hyp.small.gamma.pred <- round(100*mean(prob.hyp.small.gamma.pred),3)
exp.prob.hyp.small.gamma.pred


#### FRAMINGHAM MODEL
exp.prob.hyp.fram <- round(100*mean(fram.risk.ad.age),3)
exp.prob.hyp.fram


########################### HISTOGRAM OF PROBABILITY OF SYSTOLIC HYPERTENSION ##########################

### SMALL GAUSSIAN MODEL
par(mar=c(6.1, 5.1, 4.1, 2.1))
hist(prob.hyp.small.pred, xlab="Predicted probability of sys.hyp",main="SM Gaussian",ylab="#Participants",ylim=c(0,6000),xlim=c(0,0.8),
     cex.main=2.5, cex.lab=2.7, cex.axis=2.2)
dev.copy(pdf,'~/figures/Models/Eval/HistHypSmallGauss.pdf') 
dev.off()

### SMALL GAMMA MODEL
par(mar=c(6.1, 5.1, 4.1, 2.1))
hist(prob.hyp.small.gamma.pred, xlab="Predicted probability of sys.hyp",main="SM gamma",ylab="#Participants",ylim=c(0,6000),xlim=c(0,0.8),
     cex.main=2.5, cex.lab=2.7, cex.axis=2.2)
dev.copy(pdf,'~/figures/Models/Eval/HistHypSmallGamma.pdf')
dev.off()

### FRAMINGHAM
par(mar=c(6.1, 5.1, 4.1, 2.1))
hist(fram.risk.ad.age, xlab="Predicted probability of hyp",main="Framingham",ylab="#Participants",ylim=c(0,6000),xlim=c(0,0.8),
     cex.main=2.5, cex.lab=2.7, cex.axis=2.2)
dev.copy(pdf,'~/figures/Models/Eval/HistHypFram.pdf') 
dev.off()


########################### HISTOGRAM OF STANDARD DEVIATION OF PREDICTION DISTRIBUTIONS ##########################

### SMALL GAUSSIAN MODEL
par(mar=c(6.1, 5.1, 4.1, 2.1))
hist(small.sd.y, xlab="SD of prediction distributions",main="SM Gaussian",ylab="#Participants",ylim=c(0,8000),
     cex.main=2.5, cex.lab=2.7, cex.axis=2.2)
dev.copy(pdf,'~/figures/Models/Eval/HistSDSmallGauss.pdf') 
dev.off()

### SMALL GAMMA MODEL
small.gamma.sd.y<-sqrt(small.gamma.shape/(small.gamma.rate**2))
par(mar=c(6.1, 5.1, 4.1, 2.1))
hist(small.gamma.sd.y, xlab="SD of prediction distributions",main="SM gamma",ylab="#Participants",ylim=c(0,3500),
     cex.main=2.5, cex.lab=2.7, cex.axis=2.2)
dev.copy(pdf,'~/figures/Models/Eval/HistSDSmallGamma.pdf') # Save the plot
dev.off()


########################## RMSE ########################################

### FULL GAUSSIAN MODEL
full.rmse <- round(sqrt(mean((df.total$SystolicBP3-full.pred.mod$fitted.values)**2)),3)
full.rmse


### SMALL GAUSSIAN MODEL
small.rmse <- round(sqrt(mean((df.total$SystolicBP3-small.pred.mod$fitted.values)**2)),3)
small.rmse


### FULL GAMMA MODEL
full.gamma.rmse <-round(sqrt(mean((df.total$SystolicBP3-full.pred.mod.gamma$fitted.values)**2)),3)
full.gamma.rmse

### SMALL GAMMA MODEL
small.gamma.rmse <- round(sqrt(mean((df.total$SystolicBP3-small.pred.mod.gamma$fitted.values)**2)),3)
small.gamma.rmse


######################### CRPS ##########################################

### FULL GAUSSIAN MODEL
full.crps <- round(mean(crps(y=df.total.sc$SystolicBP3,family="normal", mean=full.pred.mod$fitted.values, sd=full.sd.y)),4)
full.crps


### SMALL GAUSSIAN MODEL
small.crps <- round(mean(crps(y=df.total.sc$SystolicBP3,family="normal", mean=small.pred.mod$fitted.values, sd=small.sd.y)),4)
small.crps


#### FULL GAMMA MODEL
full.gamma.crps <- round(mean(crps_gamma(y=df.total.sc$SystolicBP3,shape=full.gamma.shape, rate=full.gamma.rate)),4)
full.gamma.crps


### SMALL GAMMA MODEL
small.gamma.crps <- round(mean(crps_gamma(y=df.total.sc$SystolicBP3,shape=small.gamma.shape, rate=small.gamma.rate)),4)
small.gamma.crps


######################## BRIER SCORE ###################################

### FULL GAUSSIAN MODEL
full.brier <- round(BrierScore(resp=df.total.sc$SystolicHyp, pred=prob.hyp.full.pred),5)
full.brier 

### SMALL GAUSSIAN MODEL
small.brier<- round(BrierScore(resp=df.total.sc$SystolicHyp, pred=prob.hyp.small.pred),5)
small.brier

### FULL GAMMA MODEL
full.gamma.brier<- round(BrierScore(resp=df.total.sc$SystolicHyp, pred=prob.hyp.full.gamma.pred),5)
full.gamma.brier

### SMALL GAMMA MODEL 
small.gamma.brier<-round(BrierScore(resp=df.total.sc$SystolicHyp, pred=prob.hyp.small.gamma.pred),5)
small.gamma.brier



### FRAMINGHAM MODEL
fram.brier<- round(BrierScore(resp=df.total.sc$SystolicHyp, pred=fram.risk.ad.age),5)
fram.brier


########################################## PIT DIAGRAM #######################################
# probability integral transform diagram

### FULL GAUSSIAN MODEL
par(mar=c(6.1, 5.1, 4.1, 2.1))
prob.obs.full.pred <- pnorm(df.total$SystolicBP3, mean=full.pred.mod$fitted.values, sd=full.sd.y)
hist(prob.obs.full.pred,xlab="CDF-values", main="PIT, Full Gaussian",cex.main=1.8, cex.lab=1.8, cex.axis=1.7)
dev.copy(pdf,'~/figures/Models/Eval/PITFullGauss.pdf') 
dev.off()

### SMALL GAUSSIAN MODEL
par(mar=c(6.1, 5.1, 4.1, 2.1))
prob.obs.small.pred <- pnorm(df.total$SystolicBP3, mean=small.pred.mod$fitted.values, sd=small.sd.y)
hist(prob.obs.small.pred, xlab="CDF-values", main="PIT, Small Gaussian",cex.main=1.8, cex.lab=1.8, cex.axis=1.7)
dev.copy(pdf,'~/figures/Models/Eval/PITSmallGauss.pdf') 
dev.off()

### FULL GAMMA MODEL
par(mar=c(6.1, 5.1, 4.1, 2.1))
prob.obs.full.gamma.pred <- pgamma(df.total$SystolicBP3, shape=full.gamma.shape, rate=full.gamma.rate)
hist(prob.obs.full.gamma.pred, xlab="CDF-values", main="PIT, Full Gamma",cex.main=1.8, cex.lab=1.8, cex.axis=1.7)
dev.copy(pdf,'~/figures/Models/Eval/PITFullGamma.pdf') 
dev.off()

### SMALL GAMMA MODEL
par(mar=c(6.1, 5.1, 4.1, 2.1))
prob.obs.small.gamma.pred <- pgamma(df.total$SystolicBP3, shape=small.gamma.shape, rate=small.gamma.rate)
hist(prob.obs.small.gamma.pred, xlab="CDF-values", main="PIT, Small Gamma",cex.main=1.8, cex.lab=1.8, cex.axis=1.7)
dev.copy(pdf,'~/figures/Models/Eval/PITSmallGamma.pdf')
dev.off()


########################## Sensitivity and Specificity  ############### 

### FULL GAUSSIAN MODEL
# Sensitivity
full.sens<- round(100*sum(full.pred.mod$fitted.values>=140 & df.total$SystolicHyp)/sum(df.total$SystolicHyp),3)

# Specificity
full.spec<- round(100*sum(full.pred.mod$fitted.values<140 & !df.total$SystolicHyp)/sum(!df.total$SystolicHyp),3)


### SMALL GAUSSIAN MODEL
# Sensitivity
small.sens <-round(100*sum(small.pred.mod$fitted.values>=140 & df.total$SystolicHyp)/sum(df.total$SystolicHyp),3)

# Specificity
small.spec <- round(100*sum(small.pred.mod$fitted.values<140 & !df.total$SystolicHyp)/sum(!df.total$SystolicHyp),3)


### FULL GAMMA MODEL
# Sensitivity
full.gamma.sens <- round(100*sum(full.pred.mod.gamma$fitted.values>=140 & df.total$SystolicHyp)/sum(df.total$SystolicHyp),3)

# Specificity
full.gamma.spec <- round(100*sum(full.pred.mod.gamma$fitted.values<140 & !df.total$SystolicHyp)/sum(!df.total$SystolicHyp),3)

### SMALL GAMMA MODEL
# Sensitivity
small.gamma.sens <- round(100*sum(small.pred.mod.gamma$fitted.values>=140 & df.total$SystolicHyp)/sum(df.total$SystolicHyp),3)

# Specificity
small.gamma.spec <- round(100*sum(small.pred.mod.gamma$fitted.values<140 & !df.total$SystolicHyp)/sum(!df.total$SystolicHyp),3)

### FRAMINGHAM
# Sensitivity
# Hypertensive and over 50% risk for hypertension
fram.sens <- round(100*sum(fram.risk.ad.age>0.5&df.total$SystolicHyp)/sum(df.total$SystolicHyp),3)

# Specificity
# Not hypertensive and under or equal to 50% risk for hypertension
fram.spec <- round(100*sum(fram.risk.ad.age<=0.5 & !df.total$SystolicHyp)/sum(!df.total$SystolicHyp),3)



############################# C-stat ########################

# Function that calculates the C-statistic 
cstat.func <- function(df.total, mod.risk){
  c.stat <- 0
  pid.hyp <- df.total$PID[df.total$SystolicHyp==TRUE]
  pid.nonhyp <- df.total$PID[df.total$SystolicHyp==FALSE]
  index.hyp <- match(pid.hyp, df.total$PID)
  index.nonhyp <- match(pid.nonhyp, df.total$PID)
  
  #hypertensive
  for(i in index.hyp){
    #non-hypertensive
    for(j in index.nonhyp){
      if(mod.risk[i]>mod.risk[j]){
        c.stat <- c.stat+1 
      }else if(mod.risk[i]==mod.risk[j]){
        c.stat <- c.stat+0.5
      }
    }
  }
  c.stat <- c.stat/(sum(df.total$SystolicHyp)*sum(!df.total$SystolicHyp))
  return(c.stat)
}

# FULL GAUSSIAN MODEL
cstat.full<-round(cstat.func(df.total,prob.hyp.full.pred),5)
cstat.full

# SMALL GAUSSIAN MODEL
cstat.small <- round(cstat.func(df.total,prob.hyp.small.pred),5)
cstat.small

# FULL GAMMA MODEL 
cstat.full.gamma<- round(cstat.func(df.total,prob.hyp.full.gamma.pred),5)  
cstat.full.gamma

# SMALL GAMMA MODEL
cstat.small.gamma <- round(cstat.func(df.total,prob.hyp.small.gamma.pred),5)
cstat.small.gamma

# FRAMINGHAM model
cstat.fram <- round(cstat.func(df.total,fram.risk.ad.age),5)
cstat.fram 


### CREATE TABLE OF ALL NUMERICAL EVALUATIONS
# Expected % hypertensive, RMSE, Brier, CRPS, Sensitivity, specificity, c-stat, auc, hoslem p

# FULL GAUSSIAN MODEL
full.eval <- c(exp.prob.hyp.full.pred, full.rmse, full.brier, 
               full.crps, full.sens, full.spec, 100*cstat.full)
# SMALL GAUSSIAN MODEL
small.eval <-c(exp.prob.hyp.small.pred, small.rmse, small.brier, small.crps,
               small.sens, small.spec,100*cstat.small)

# FULL GAMMA MODEL
full.gamma.eval <-c(exp.prob.hyp.full.gamma.pred, full.gamma.rmse, full.gamma.brier,
                    full.gamma.crps, full.gamma.sens, full.gamma.spec, 100*cstat.full.gamma)
# SMALL GAMMA MODEL
small.gamma.eval <-c(exp.prob.hyp.small.gamma.pred, small.gamma.rmse, small.gamma.brier, 
                     small.gamma.crps, small.gamma.sens, small.gamma.spec, 100*cstat.small.gamma)
# FRAMINGHAM MODEL
fram.eval <-c(exp.prob.hyp.fram, "NA", fram.brier, "NA", fram.sens,
              fram.spec,100*cstat.fram)

# Create Table
eval.methods <- c("Exp. Hyp", "RMSE", "BrierScore", "CRPS",
                  "Sensitivity", "Specificity", "C-statistic")

df.eval <- data.frame("Eval.Method"=eval.methods, "Full.Gauss"=full.eval, 
                      "Small.Gauss"=small.eval, "Full.Gamma"=full.gamma.eval, 
                      "Small.Gamma"=small.gamma.eval, "Framingham"=fram.eval)
write.csv(df.eval,"Tables/Eval/TotalEval.csv", row.names = FALSE)


