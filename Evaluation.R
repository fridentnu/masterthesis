
library(scoringRules) # crps
library(DescTools) # brier score
library(MASS)
library(pROC)
library(ResourceSelection)

#load("MyData/Models.RData")
load("MyData/Framingham.RData")

source("R code/Models.R")
###### OBSERVATIONS #######
obs.hyp <- sum(df.total$SystolicHyp)/length(df.total$SystolicHyp)
obs.hyp

######################### PROBABILITY HYPERTENSION ###########################




####### FULL MODEL
# probability that each systolic pressure is equal to or above 140 mmHg
prob.hyp.full.pred <-pnorm(140, mean=full.pred.mod$fitted.values, sd=full.sd.y, lower.tail = F)
prob.hyp.full.pred


hist(prob.hyp.full.pred)

# mean of probabilities of systolic hypertension
exp.prob.hyp.full.pred <- round(100*mean(prob.hyp.full.pred),3)
exp.prob.hyp.full.pred
# Expected number of hypertensives are nearly 20% of population

########## SMALL MODEL
# probability that each systolic pressure is equal to or above 140 mmHg
prob.hyp.small.pred <-pnorm(140, mean=small.pred.mod$fitted.values, sd=small.sd.y, lower.tail = F)
prob.hyp.small.pred


hist(prob.hyp.small.pred)

# mean of probabilities of systolic hypertension
exp.prob.hyp.small.pred <- round(100*mean(prob.hyp.small.pred),3)
exp.prob.hyp.small.pred
# Expected number of hypertensives are nearly 20% of population 


### FULL GAMMA

### ALT 1, add dispersion in predict++
# full.gamma.var <- predict(full.pred.mod.gamma, se.fit = T)$se.fit**2 + predict(full.pred.mod.gamma, se.fit = T)$residual.scale**2
# full.gamma.sd <- sqrt(full.gamma.var)
# full.gamma.shape= mean(full.pred.mod.gamma$fitted.values)**2/full.gamma.var
# full.gamma.rate= mean(full.pred.mod.gamma$fitted.values)/full.gamma.var
# prob.hyp.full.gamma.pred <-pgamma(140, shape=full.gamma.shape, rate=full.gamma.rate, lower.tail = F)
# hist(prob.hyp.full.gamma.pred)

### ALT 2
# can I use this function??
#full.gamma.shape <- gamma.shape(full.pred.mod.gamma)
#prob.hyp.full.gamma.pred <-pgamma(140, shape=full.gamma.shape$alpha, lower.tail = F)
#hist(prob.hyp.full.gamma.pred)

### ALT 3

prob.hyp.full.gamma.pred <-pgamma(140, shape=full.gamma.shape,rate=full.gamma.rate, lower.tail = F)

hist(prob.hyp.full.gamma.pred)


### Er dette riktig måte å gjøre det på? 
# eller git dette ikke mening siden jeg nå plotter alle fordelingene over hverandre?
#skal jeg i steden for x bruke de faktiske blodtrykkene til deltagerne?
# ser ganske normalfordelt ut med bare en rate (eks. rate[100])

# scatterplot
x <- seq(50, 250, length=17365)
hx <- dgamma(x,shape=full.gamma.shape,rate=full.gamma.rate[c(1,2)])
plot(x,hx, type="l")


# Boxplot
data.frame("x"=x) %>%
  mutate( bin=cut_width(x, width=5, boundary=0) ) %>%
  ggplot( aes(x=bin, y=hx) ) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=16),
        axis.text.y=element_text(size=16),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24))+
  labs(x="Systolic blood pressure [mmHg]", x="Density")

#### Snodig cut-off


## just checking that I code correctly ------------------------------------------
data.frame("x"=seq(-2,2, length=100)) %>%
  mutate( bin=cut_width(x, width=0.5, boundary=0) ) %>%
  ggplot( aes(x=bin, y=dnorm(x)) ) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=16),
        axis.text.y=element_text(size=16),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24))+
  labs(x="Systolic blood pressure [mmHg]", x="Density")

plot(seq(-2,2, length=100),dnorm(seq(-2,2, length=100)))

#----------------------------------------------------------------------------------


# mean of probabilities of systolic hypertension
exp.prob.hyp.full.gamma.pred <- round(100*mean(prob.hyp.full.gamma.pred),3)
exp.prob.hyp.full.gamma.pred
# Expected number of hypertensives are nearly 20% of population


### SMALL GAMMA
prob.hyp.small.gamma.pred <-pgamma(140, shape=small.gamma.shape,rate=small.gamma.rate, lower.tail = F)

hist(prob.hyp.small.gamma.pred)


# mean of probabilities of systolic hypertension
exp.prob.hyp.small.gamma.pred <- round(100*mean(prob.hyp.small.gamma.pred),3)
exp.prob.hyp.small.gamma.pred
# Expected number of hypertensives are nearly 20% of population


#### Framingham
exp.prob.hyp.fram <- round(100*mean(fram.risk.ad.age),3)
exp.prob.hyp.fram






########################### HISTOGRAM PROBABILITY HYPERTENSION ##########################


### SMALL GAUSS
par(mar=c(6.1, 5.1, 4.1, 2.1))
hist(prob.hyp.small.pred, xlab="Predicted probability of sys.hyp",main="SM Gaussian",ylab="#Participants",ylim=c(0,6000),xlim=c(0,0.8),
     cex.main=2.5, cex.lab=2.7, cex.axis=2.2)
dev.copy(pdf,'~/figures/Models/Eval/HistHypSmallGauss.pdf') # Save the plot
dev.off()

### SMALL GAMMA
par(mar=c(6.1, 5.1, 4.1, 2.1))
hist(prob.hyp.small.gamma.pred, xlab="Predicted probability of sys.hyp",main="SM gamma",ylab="#Participants",ylim=c(0,6000),xlim=c(0,0.8),
     cex.main=2.5, cex.lab=2.7, cex.axis=2.2)
dev.copy(pdf,'~/figures/Models/Eval/HistHypSmallGamma.pdf') # Save the plot
dev.off()

### Framingham
par(mar=c(6.1, 5.1, 4.1, 2.1))
hist(fram.risk.ad.age, xlab="Predicted probability of hyp",main="Framingham",ylab="#Participants",ylim=c(0,6000),xlim=c(0,0.8),
     cex.main=2.5, cex.lab=2.7, cex.axis=2.2)
dev.copy(pdf,'~/figures/Models/Eval/HistHypFram.pdf') # Save the plot
dev.off()




########################### HISTOGRAM STANDARD DEVIATION PREDICTION DIST ##########################

### SMALL GAUSS
par(mar=c(6.1, 5.1, 4.1, 2.1))
hist(small.sd.y, xlab="SD of prediction distributions",main="SM Gaussian",ylab="#Participants",ylim=c(0,8000),
     cex.main=2.5, cex.lab=2.7, cex.axis=2.2)
dev.copy(pdf,'~/figures/Models/Eval/HistSDSmallGauss.pdf') # Save the plot
dev.off()

small.gamma.sd.y<-sqrt(small.gamma.shape/(small.gamma.rate**2))

### SMALL GAMMA
par(mar=c(6.1, 5.1, 4.1, 2.1))
hist(small.gamma.sd.y, xlab="SD of prediction distributions",main="SM gamma",ylab="#Participants",ylim=c(0,3500),
     cex.main=2.5, cex.lab=2.7, cex.axis=2.2)
dev.copy(pdf,'~/figures/Models/Eval/HistSDSmallGamma.pdf') # Save the plot
dev.off()
########################### QQPLOT ##################################


#### CHECK NORMALITY
n <- length(df.total$PID)

# Full model
quant.full.mod <- pnorm(df.total$SystolicBP3, mean=full.pred.mod$fitted.values, sd=full.sd.y)
quant.sort.full.mod <- sort(quant.full.mod)

plot(c(1:n/n), quant.sort.full.mod, main="QQ-plot, full Gaussian model",
     ylab="Observed quantiles",xlab="Theoretical quantiles", pch=20)
lines(c(0:1),c(0,1), col="steelblue",lwd=3)
dev.copy(pdf,'~/figures/Models/Eval/QQFullGauss.pdf') # Save the plot
dev.off()

# Full model
quant.full.mod <- qnorm(c(1:n)/(n+1),mean=full.pred.mod$fitted.values, sd=full.sd.y)

df.quant.full.mod <- data.frame("Quant"=quant.full.mod,"Obs"=df.total$SystolicBP3)

plot(sort(df.total$SystolicBP3), quant.full.mod, main="QQ-plot, full Gaussian model",
     ylab="Observed quantiles",xlab="Theoretical quantiles", pch=20)
lines(c(0:200),c(0:200), col="steelblue",lwd=3)
dev.copy(pdf,'~/figures/Models/Eval/QQFullGaussSources.pdf') # Save the plot
dev.off()

# Small model
quant.small.mod <- pnorm(df.total$SystolicBP3,mean=small.pred.mod$fitted.values, sd=small.sd.y)
quant.sort.small.mod <- sort(quant.small.mod)

plot(c(1:n/n), quant.sort.small.mod, main="QQ-plot, small Gaussian model",
     ylab="Observed quantiles",xlab="Theoretical quantiles", pch=20)
lines(c(0:1),c(0,1), col="steelblue",lwd=3)
dev.copy(pdf,'~/figures/Models/Eval/QQSmallGauss.pdf') # Save the plot
dev.off()

# Full gamma model
quant.full.mod.gamma <- pgamma(df.total$SystolicBP3,  shape=full.gamma.shape,rate=full.gamma.rate)
quant.sort.full.mod.gamma <- sort(quant.full.mod.gamma)

plot(c(1:n/n), quant.sort.full.mod.gamma, main="QQ-plot, full gamma model",
     ylab="Observed quantiles",xlab="Theoretical quantiles", pch=20)
lines(c(0:1),c(0,1), col="steelblue",lwd=3)
dev.copy(pdf,'~/figures/Models/Eval/QQFullGamma.pdf') # Save the plot
dev.off()

# Small gamma model
quant.full.mod.gamma <- pgamma(df.total$SystolicBP3,  shape=full.gamma.shape,rate=full.gamma.rate)
quant.sort.full.mod.gamma <- sort(quant.full.mod.gamma)

plot(c(1:n/n), quant.sort.full.mod.gamma, main="QQ-plot, small gamma model",
     ylab="Observed quantiles",xlab="Theoretical quantiles", pch=20)
lines(c(0:1),c(0,1), col="steelblue",lwd=3)
dev.copy(pdf,'~/figures/Models/Eval/QQSmallGamma.pdf') # Save the plot
dev.off()

########################## RMSE ########################################


### FULL MODEL
full.rmse <- round(sqrt(mean((df.total$SystolicBP3-full.pred.mod$fitted.values)**2)),3)
full.rmse


### SMALL MODEL
small.rmse <- round(sqrt(mean((df.total$SystolicBP3-small.pred.mod$fitted.values)**2)),3)
small.rmse


### FULL GAMMA 
# full.gamma.rmse <- predict(full.pred.mod.gamma, se.fit=T)$residual.scale
# round(full.gamma.rmse,3)
# 
# full.gamma.rmse <-sqrt(mean(residuals(full.pred.mod.gamma)**2))
# full.gamma.rmse

full.gamma.rmse <-round(sqrt(mean((df.total$SystolicBP3-full.pred.mod.gamma$fitted.values)**2)),3)
full.gamma.rmse

### SMALL GAMMA
small.gamma.rmse <- round(sqrt(mean((df.total$SystolicBP3-small.pred.mod.gamma$fitted.values)**2)),3)
small.gamma.rmse


### CONSTANT
constant.rmse <- sqrt(mean((df.total$SystolicBP3-df.total$SystolicBP2)**2))
round(constant.rmse,3)


######################### CRPS ##########################################
# justert absoluttfeil

### FULL MODEL
full.crps <- round(mean(crps(y=df.total.sc$SystolicBP3,family="normal", mean=full.pred.mod$fitted.values, sd=full.sd.y)),4)
full.crps

df.crps <- data.frame("Observed"=df.total.sc$SystolicBP3, "CRPS"=full.crps)
df.crps[order(df.crps$Observed),]
plot(df.crps$Observed, df.crps$CRPS)
# ligner på absolute error av residuals

### SMALL MODEL
small.crps <- round(mean(crps(y=df.total.sc$SystolicBP3,family="normal", mean=small.pred.mod$fitted.values, sd=small.sd.y)),4)
small.crps


#### FULL GAMMA 
full.gamma.crps <- round(mean(crps_gamma(y=df.total.sc$SystolicBP3,shape=full.gamma.shape, rate=full.gamma.rate)),4)
full.gamma.crps


### SMALL GAMMA
small.gamma.crps <- round(mean(crps_gamma(y=df.total.sc$SystolicBP3,shape=small.gamma.shape, rate=small.gamma.rate)),4)
small.gamma.crps

######################## BRIER SCORE ###################################

### FULL MODEL
full.brier <- round(BrierScore(resp=df.total.sc$SystolicHyp, pred=prob.hyp.full.pred),5)
full.brier 
# 0.13254599
# the smaller the better, range between 0 and 1

### SMALL MODEL
small.brier<- round(BrierScore(resp=df.total.sc$SystolicHyp, pred=prob.hyp.small.pred),5)
small.brier
# 0.1325129


### FULL MODEL GAMMA
full.gamma.brier<- round(BrierScore(resp=df.total.sc$SystolicHyp, pred=prob.hyp.full.gamma.pred),5)
full.gamma.brier
# 0.1324363

### SMALL MODEL GAMMA
small.gamma.brier<-round(BrierScore(resp=df.total.sc$SystolicHyp, pred=prob.hyp.small.gamma.pred),5)
small.gamma.brier
# 0.1324363

### SAME PROB FOR ALL 
round(BrierScore(resp=df.total.sc$SystolicHyp, pred=equal.prob.mod),5)
# 0.1572847

############ Framingham 

#### With age adjustment
fram.brier<- round(BrierScore(resp=df.total.sc$SystolicHyp, pred=fram.risk.ad.age),5)
fram.brier
# 0.1347959

### Comment: the framingingham model with age adjustment had slightly better Brier score 


########################################## PIT DIAGRAM #######################################
# probability integral transform
# should be uniform if response comes from the given distribution
# see that for all distributions there are disproportionally many outside right tail
# see that distribution of responses are more skewed than prediction distribution
# more clearly for normal than for gamma (as expected)
# samme trends as observed for the plot of the fitted values


### FULL MODEL

prob.obs.full.pred <- pnorm(df.total$SystolicBP3, mean=full.pred.mod$fitted.values, sd=full.sd.y)
hist(prob.obs.full.pred,xlab="CDF-values", main="PIT, Full Gaussian",cex.main=1.8, cex.lab=1.4, cex.axis=1.7)
dev.copy(pdf,'~/figures/Models/Eval/PITFullGauss.pdf') # Save the plot
dev.off()

### SMALL MODEL
par(mar=c(6.1, 5.1, 4.1, 2.1))
prob.obs.small.pred <- pnorm(df.total$SystolicBP3, mean=small.pred.mod$fitted.values, sd=small.sd.y)
hist(prob.obs.small.pred, xlab="CDF-values", main="PIT, Small Gaussian",cex.main=1.8, cex.lab=1.8, cex.axis=1.7)
dev.copy(pdf,'~/figures/Models/Eval/PITSmallGauss.pdf') # Save the plot
dev.off()

### FULL GAMMA
prob.obs.full.gamma.pred <- pgamma(df.total$SystolicBP3, shape=full.gamma.shape, rate=full.gamma.rate)
hist(prob.obs.full.gamma.pred, xlab="CDF-values", main="PIT, Full Gamma",cex.main=1.8, cex.lab=1.4, cex.axis=1.7)
dev.copy(pdf,'~/figures/Models/Eval/PITFullGamma.pdf') # Save the plot
dev.off()

### SMALL GAMMA
par(mar=c(6.1, 5.1, 4.1, 2.1))
prob.obs.small.gamma.pred <- pgamma(df.total$SystolicBP3, shape=small.gamma.shape, rate=small.gamma.rate)
hist(prob.obs.small.gamma.pred, xlab="CDF-values", main="PIT, Small Gamma",cex.main=1.8, cex.lab=1.8, cex.axis=1.7)
dev.copy(pdf,'~/figures/Models/Eval/PITSmallGamma.pdf') # Save the plot
dev.off()


########################## Sensitivity and Specificity  ############### 

### FULL MODEL
# Sensitivity
# Hypertensive and predicted hypertensive
full.sens<- round(100*sum(full.pred.mod$fitted.values>=140 & df.total$SystolicHyp)/sum(df.total$SystolicHyp),3)

# Specificity
# Not hypertensive and not predicted hypertensive
full.spec<- round(100*sum(full.pred.mod$fitted.values<140 & !df.total$SystolicHyp)/sum(!df.total$SystolicHyp),3)


### SMALL MODEL
# Sensitivity
# Hypertensive and predicted hypertensive
small.sens <-round(100*sum(small.pred.mod$fitted.values>=140 & df.total$SystolicHyp)/sum(df.total$SystolicHyp),3)

# Specificity
# Not hypertensive and not predicted hypertensive
small.spec <- round(100*sum(small.pred.mod$fitted.values<140 & !df.total$SystolicHyp)/sum(!df.total$SystolicHyp),3)


### FULL GAMMA
# Sensitivity
# Hypertensive and predicted hypertensive
full.gamma.sens <- round(100*sum(full.pred.mod.gamma$fitted.values>=140 & df.total$SystolicHyp)/sum(df.total$SystolicHyp),3)

# Specificity
# Not hypertensive and not predicted hypertensive
full.gamma.spec <- round(100*sum(full.pred.mod.gamma$fitted.values<140 & !df.total$SystolicHyp)/sum(!df.total$SystolicHyp),3)

### SMALL GAMMA
# Sensitivity
# Hypertensive and predicted hypertensive
small.gamma.sens <- round(100*sum(small.pred.mod.gamma$fitted.values>=140 & df.total$SystolicHyp)/sum(df.total$SystolicHyp),3)

# Specificity
# Not hypertensive and not predicted hypertensive
small.gamma.spec <- round(100*sum(small.pred.mod.gamma$fitted.values<140 & !df.total$SystolicHyp)/sum(!df.total$SystolicHyp),3)

### FRAMINGHAM
# Sensitivity
# Hypertensive and over 50% risk for hypertension
fram.sens <- round(100*sum(fram.risk.ad.age>0.5&df.total$SystolicHyp)/sum(df.total$SystolicHyp),3)


# Specificity
# not hypertensive and under or equal to 50% risk for hypertension
fram.spec <- round(100*sum(fram.risk.ad.age<=0.5 & !df.total$SystolicHyp)/sum(!df.total$SystolicHyp),3)







############################# C-stat ########################


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

cstat.full<-round(cstat.func(df.total,prob.hyp.full.pred),5)
cstat.full

cstat.small <- round(cstat.func(df.total,prob.hyp.small.pred),5)
cstat.small

cstat.full.gamma<- round(cstat.func(df.total,prob.hyp.full.gamma.pred),5)  # biggest cstat
cstat.full.gamma

cstat.small.gamma <- round(cstat.func(df.total,prob.hyp.small.gamma.pred),5)
cstat.small.gamma

cstat.fram <- round(cstat.func(df.total,fram.risk.ad.age),5)
cstat.fram 



# Expected % hypertensive, RMSE, Brier, CRPS, Sensitivity, specificity, c-stat, auc, hoslem p

full.eval <- c(exp.prob.hyp.full.pred, full.rmse, full.brier, 
               full.crps, full.sens, full.spec, 100*cstat.full)

small.eval <-c(exp.prob.hyp.small.pred, small.rmse, small.brier, small.crps,
               small.sens, small.spec,100*cstat.small)

full.gamma.eval <-c(exp.prob.hyp.full.gamma.pred, full.gamma.rmse, full.gamma.brier,
                    full.gamma.crps, full.gamma.sens, full.gamma.spec, 100*cstat.full.gamma)

small.gamma.eval <-c(exp.prob.hyp.small.gamma.pred, small.gamma.rmse, small.gamma.brier, 
                     small.gamma.crps, small.gamma.sens, small.gamma.spec, 100*cstat.small.gamma)

fram.eval <-c(exp.prob.hyp.fram, "NA", fram.brier, "NA", fram.sens,
              fram.spec,100*cstat.fram)


eval.methods <- c("Exp. Hyp", "RMSE", "BrierScore", "CRPS",
                  "Sensitivity", "Specificity", "C-statistic")

df.eval <- data.frame("Eval.Method"=eval.methods, "Full.Gauss"=full.eval, 
                      "Small.Gauss"=small.eval, "Full.Gamma"=full.gamma.eval, 
                      "Small.Gamma"=small.gamma.eval, "Framingham"=fram.eval)
write.csv(df.eval,"Tables/Eval/TotalEval.csv", row.names = FALSE)

### -----------------------------------------------------------------------------

# specificity is very good, which is as expected since we started with all negatives
# sensitivity is less good,
# but we see that there are still realtively more people with predicted hypertension in group of obs hyp
# than in group with non.obs

# ### Prediction intervals full model
# 
# ## Prøve å plotte prediksjonsintervall og observerte verdier
# df.total.sort <- df.total[order(df.total$SystolicBP3),]
# 
# ## observed systolic hypertension sorted
# ggplot(data=full.pred.mod)+
#   geom_point(mapping=aes(x=c(1:length(df.total.sort$PID)), y=df.total.sort$SystolicBP3))
# 
# 
# # sort the fitted values
# full.fit.val.sort <- full.pred.mod$fitted.values[order(full.pred.mod$fitted.values)]
# 
# ## fitted systolic hypertension sorted
# ggplot(data=full.pred.mod)+
#   geom_point(mapping=aes(x=c(1:length(df.total.sort$PID)), y=full.fit.val.sort))
# 
# 
# ### Full model
# 
# full.predictions <-  predict(full.pred.mod, type="prediction")
# 
# df.pred.obs <- data.frame("Observed"=df.total$SystolicBP3, "Fitted"=full.predictions[,1],
#                           "Lower"=full.predictions[,2], "Upper"=full.predictions[,3])
# 
# 
# df.pred.obs.sort <- df.pred.obs[order(df.pred.obs$Fitted),]
# df.pred.obs.sort$Participant <- c(1:length(df.total.sort$PID))
# 
# # how to change colors and keep legend?
# ggplot(data=df.pred.obs.sort)+
#   geom_point(mapping=aes(x=Participant, y=Observed, col="Observed values"),alpha=0.4)+
#   geom_point(mapping=aes(x=Participant, y=Fitted, col="Fitted values"))+
#   geom_line(mapping = aes(x=Participant, y=Lower,col="95% pred int"), size=1.5)+
#   geom_line(mapping = aes(x=Participant, y=Upper,col="95% pred int"),size=1.5)+
#   labs(x="Participants", y= "Systolic blood pressure [mmHg]", col=element_blank())
# 
# ggplot(data=df.pred.obs.sort)+
#   geom_point(mapping=aes(x=Participant, y=Observed, col="Observed values"),alpha=0.4)+
#   geom_point(mapping=aes(x=Participant, y=Fitted, col="Fitted values"))+
#   geom_line(mapping = aes(x=Participant, y=Fitted-full.sd.y,col="Minus one sd"), size=1.5)+
#   geom_line(mapping = aes(x=Participant, y=Fitted+full.sd.y,col="Plus one sd"),size=1.5)+
#   labs(x="Participants", y= "Systolic blood pressure [mmHg]", col=element_blank())
# 
# # base plot
# plot(df.pred.obs.sort$Participant, df.pred.obs.sort$Observed, type="p",pch=19, col=alpha("black",0.3), 
#      xlab="Participants", ylab="Systolic blood pressure [mmHg]")+
#   lines(df.pred.obs.sort$Participant, df.pred.obs.sort$Fitted, col="red")+
#   lines(df.pred.obs.sort$Participant, df.pred.obs.sort$Lower, col="green")+
#   lines(df.pred.obs.sort$Participant, df.pred.obs.sort$Upper, col="green")+
#   legend("topright", legend=c("Observed values", "Fitted values", "95% lower pred int", "95% upper pred int "),
#          col=c("black","red", "green", "green"),lty=1)
# 
# 
# above.upper <- 100*sum(df.pred.obs.sort$Observed>df.pred.obs.sort$Upper)/length(df.pred.obs.sort$Observed)
# above.upper
# # only 3.83% above upper prediction interval limit
# 
# 
# below.lower <- 100*sum(df.pred.obs.sort$Observed<df.pred.obs.sort$Lower)/length(df.pred.obs.sort$Observed)
# below.lower
# # Only 1.36% below lower prediction interval limit
# 
# 
# #### Small model
# 
# df.pred.obs.small <- data.frame("Observed"=df.total$SystolicBP3, "Fitted"=small.predictions[,1],
#                                 "Lower"=small.predictions[,2], "Upper"=small.predictions[,3])
# 
# 
# df.pred.obs.small.sort <- df.pred.obs.small[order(df.pred.obs.small$Fitted),]
# df.pred.obs.small.sort$Participant <- c(1:length(df.total.sort$PID))
# 
# 
# 
# ggplot(data=df.pred.obs.small.sort)+
#   geom_point(mapping=aes(x=Participant, y=Observed, col="Observed values"),alpha=0.4)+
#   geom_point(mapping=aes(x=Participant, y=Fitted, col="Fitted values"))+
#   geom_line(mapping = aes(x=Participant, y=Lower,col="95% pred int"), size=1.5)+
#   geom_line(mapping = aes(x=Participant, y=Upper,col="95% pred int"),size=1.5)+
#   labs(x="Participants", y= "Systolic blood pressure [mmHg]", col=element_blank())
# 
# above.upper.small <- 100*sum(df.pred.obs.small.sort$Observed>df.pred.obs.small.sort$Upper)/length(df.pred.obs.small.sort$Observed)
# above.upper.small
# # only 3.82% above upper prediction interval limit
# 
# 
# below.lower.small <- 100*sum(df.pred.obs.small.sort$Observed<df.pred.obs.small.sort$Lower)/length(df.pred.obs.small.sort$Observed)
# below.lower.small
# # Only 1.39% below lower prediction interval limit
# 
# 
# ######################################################################################################
# ################################################# NOT IN USE #########################################
# 
# 
# # diff.pred.mod <- lm(SysDiff ~ BirthYear + Sex + BMI2 + SystolicBP2 + DiastolicBP2 + 
# #                       PAI2 + RecPA2 + BPHigPar2 + Smoking2 + Cholesterol2 + HDLCholesterol2 +
# #                       Glucose2 + GFR2 + Creatinine2 + Education2, data=df.total.sc)
# 
# #summary(diff.pred.mod)
# 
# #diff.predictions <- predict(diff.pred.mod, interval="prediction")
# 
# diff.pred.se <- predict(diff.pred.mod, se.fit=T)$se.fit
# 
# mean(diff.pred.se)
# 
# diff.sigma <- sd(diff.pred.mod$residuals)
# 
# 
# # responses versus predictions
# diff <-ggplot(data=diff.pred.mod)+
#   geom_histogram(aes(diff.pred.mod$fitted.values+df.total$SystolicBP2), binwidth = 1)+
#   coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
#   xlab(" Systolic blood pressure HUNT2 + predicted diff")



# 
# ########################### Hosmer-Lemeshow ##################
# 
# 
# full.hoslem <- hoslem.test(as.numeric(df.total$SystolicHyp), as.numeric(full.pred.mod$fitted.values>=140), g=10)
# 
# small.hoslem <-hoslem.test(as.numeric(df.total$SystolicHyp), as.numeric(small.pred.mod$fitted.values>=140), g=10)
# 
# full.gamma.hoslem <-hoslem.test(as.numeric(df.total$SystolicHyp), as.numeric(full.pred.mod.gamma$fitted.values>=140), g=10)
# 
# small.gamma.hoslem <-hoslem.test(as.numeric(df.total$SystolicHyp), as.numeric(small.pred.mod.gamma$fitted.values>=140), g=10)
# 
# fram.hoslem <-hoslem.test(as.numeric(df.total$SystolicHyp), as.numeric(fram.risk.ad.age>0.5), g=10)
# 
# # Model not well specified for any of the models since the p-value is below 0.05
# 
# 
# ############################ ROC ##########################
# 
# plot(roc(as.numeric(df.total$SystolicHyp), as.numeric(full.pred.mod$fitted.values>=140)), main="full")
# 
# plot(roc(as.numeric(df.total$SystolicHyp), as.numeric(small.pred.mod$fitted.values>=140)), main="small")
# 
# plot(roc(as.numeric(df.total$SystolicHyp), as.numeric(full.pred.mod.gamma$fitted.values>=140)), main="full gamma")
# 
# plot(roc(as.numeric(df.total$SystolicHyp), as.numeric(small.pred.mod.gamma$fitted.values>=140)), main="small gamma")
# 
# plot(roc(as.numeric(df.total$SystolicHyp), as.numeric(fram.risk.ad.age>0.5)), main="Framingham")
# 
# #plot(roc(as.numeric(df.total$SystolicHyp), as.numeric(prob.hyp.full.pred>0.5)), main="full")
# 
# # plot ROC til framingham og en av de andre modellene i samme plott. trenger ikke flere
# 
# 
# ############################ AUC #############################
# 
# full.auc <- round(as.numeric(auc(as.numeric(df.total$SystolicHyp), as.numeric(full.pred.mod$fitted.values>=140))),5)
# 
# small.auc <- round(as.numeric(auc(as.numeric(df.total$SystolicHyp), as.numeric(small.pred.mod$fitted.values>=140))),5)
# 
# full.gamma.auc <- round(as.numeric(auc(as.numeric(df.total$SystolicHyp), as.numeric(full.pred.mod.gamma$fitted.values>=140))),5)
# 
# small.gamma.auc <- round(as.numeric(auc(as.numeric(df.total$SystolicHyp), as.numeric(small.pred.mod.gamma$fitted.values>=140))),5)
# 
# fram.auc <- round(as.numeric(auc(as.numeric(df.total$SystolicHyp), as.numeric(fram.risk.ad.age>0.5))),5)
# fram.auc
# 



