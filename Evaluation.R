
library(scoringRules) # crps
library(DescTools) # brier score
library(MASS)
library(pROC)
library(ResourceSelection)

load("MyData/Models.RData")
load("MyData/Framingham.RData")

###### OBSERVATIONS #######
obs.hyp <- sum(df.total$SystolicHyp)/length(df.total$SystolicHyp)
obs.hyp

######################### DISTRIBUTIONS ###########################
### FULL MODEL
# Variance and standard deviation for each participants response

full.design.mat <- model.matrix(full.pred.mod)

# check that should use confidence
full.var.coeff <- var(full.pred.mod$residuals)*solve(t(full.design.mat)%*%full.design.mat)
full.exp.var <- rep(0,length(df.total$PID))

for(i in 1:length(df.total$PID)){
  full.exp.var[i] <- t(full.design.mat[i,])%*%full.var.coeff%*%full.design.mat[i,]
}
mean(predict(full.pred.mod, se.fit=T)$se.fit**2 - full.exp.var)

# Alternative: predict(full.pred.mod, se.fit=T)$se.fit**2 + predict(full.pred.mod, se.fit=T)$residual.scale**2
full.var.y <- full.exp.var + mean((df.total$SystolicBP3-full.pred.mod$fitted.values)**2)
full.sd.y<- sqrt(full.var.y)

# probability that each systolic pressure is equal to or above 140 mmHg
prob.hyp.full.pred <-pnorm(140, mean=full.pred.mod$fitted.values, sd=full.sd.y, lower.tail = F)
prob.hyp.full.pred


hist(prob.hyp.full.pred)

# mean of probabilities of systolic hypertension
exp.prob.hyp.full.pred <- mean(prob.hyp.full.pred)
round(100*exp.prob.hyp.full.pred,3)
# Expected number of hypertensives are nearly 20% of population


### SMALL MODEL
# Variance and standard deviation for each participants response
small.design.mat <- model.matrix(small.pred.mod)

# check that should use confidence
small.var.coeff <- var(small.pred.mod$residuals)*solve(t(small.design.mat)%*%small.design.mat)
small.exp.var <- rep(0,length(df.total$PID))

for(i in 1:length(df.total$PID)){
  small.exp.var[i] <- t(small.design.mat[i,])%*%small.var.coeff%*%small.design.mat[i,]
}
mean(predict(small.pred.mod, se.fit=T)$se.fit**2 - small.exp.var)

# Alternative: predict(small.pred.mod, se.fit=T)$se.fit**2 + predict(small.pred.mod, se.fit=T)$residual.scale**2
small.var.y <- small.exp.var + mean((df.total$SystolicBP3-small.pred.mod$fitted.values)**2)
small.sd.y<- sqrt(small.var.y)

# probability that each systolic pressure is equal to or above 140 mmHg
prob.hyp.small.pred <-pnorm(140, mean=small.pred.mod$fitted.values, sd=small.sd.y, lower.tail = F)
prob.hyp.small.pred


hist(prob.hyp.small.pred)

# mean of probabilities of systolic hypertension
exp.prob.hyp.small.pred <- mean(prob.hyp.small.pred)
round(100*exp.prob.hyp.small.pred,3)
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

full.gamma.shape <- 1/summary(full.pred.mod.gamma)$dispersion
full.gamma.shape

full.gamma.rate <- full.gamma.shape/full.pred.mod.gamma$fitted.values

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
exp.prob.hyp.full.gamma.pred <- mean(prob.hyp.full.gamma.pred)
round(100*exp.prob.hyp.full.gamma.pred,3)
# Expected number of hypertensives are nearly 20% of population


### SMALL GAMMA

small.gamma.shape <- 1/summary(small.pred.mod.gamma)$dispersion
small.gamma.shape

small.gamma.rate <- small.gamma.shape/small.pred.mod.gamma$fitted.values

prob.hyp.small.gamma.pred <-pgamma(140, shape=small.gamma.shape,rate=small.gamma.rate, lower.tail = F)

hist(prob.hyp.small.gamma.pred)


# mean of probabilities of systolic hypertension
exp.prob.hyp.small.gamma.pred <- mean(prob.hyp.small.gamma.pred)
round(100*exp.prob.hyp.small.gamma.pred,3)
# Expected number of hypertensives are nearly 20% of population


#### Framingham

round(100*mean(fram.risk.ad.age),3)



########################## RMSE ########################################


### FULL MODEL
full.rmse <- sqrt(mean((df.total$SystolicBP3-full.pred.mod$fitted.values)**2))
round(full.rmse,3)


### SMALL MODEL
small.rmse <- sqrt(mean((df.total$SystolicBP3-small.pred.mod$fitted.values)**2))
round(small.rmse,3)


### FULL GAMMA 
# full.gamma.rmse <- predict(full.pred.mod.gamma, se.fit=T)$residual.scale
# round(full.gamma.rmse,3)
# 
# full.gamma.rmse <-sqrt(mean(residuals(full.pred.mod.gamma)**2))
# full.gamma.rmse

full.gamma.rmse <-sqrt(mean((df.total$SystolicBP3-full.pred.mod.gamma$fitted.values)**2))
round(full.gamma.rmse,3)

### SMALL GAMMA
small.gamma.rmse <- sqrt(mean((df.total$SystolicBP3-small.pred.mod.gamma$fitted.values)**2))
round(small.gamma.rmse,3)

### MUCH SMALLER RMSE FOR GAMMA WHY??

### CONSTANT
constant.rmse <- sqrt(mean((df.total$SystolicBP3-df.total$SystolicBP2)**2))
round(constant.rmse,3)


######################### CRPS ##########################################
# justert absoluttfeil

### FULL MODEL
full.crps <- crps(y=df.total.sc$SystolicBP3,family="normal", mean=full.pred.mod$fitted.values, sd=full.sd.y)
round(mean(full.crps),3)

df.crps <- data.frame("Observed"=df.total.sc$SystolicBP3, "CRPS"=full.crps)
df.crps[order(df.crps$Observed),]
plot(df.crps$Observed, df.crps$CRPS)
# ligner på absolute error av residuals

### SMALL MODEL
small.crps <- crps(y=df.total.sc$SystolicBP3,family="normal", mean=small.pred.mod$fitted.values, sd=small.sd.y)
round(mean(small.crps),3)


#### FULL GAMMA 
full.gamma.crps <- crps_gamma(y=df.total.sc$SystolicBP3,shape=full.gamma.shape, rate=full.gamma.rate) 
round(mean(full.gamma.crps),3)


### SMALL GAMMA
small.gamma.crps <- crps_gamma(y=df.total.sc$SystolicBP3,shape=small.gamma.shape, rate=small.gamma.rate)
round(mean(small.gamma.crps),3)

######################## BRIER SCORE ###################################

### FULL MODEL
round(BrierScore(resp=df.total.sc$SystolicHyp, pred=prob.hyp.full.pred),5)
# 0.13254599
# the smaller the better, range between 0 and 1

### SMALL MODEL
round(BrierScore(resp=df.total.sc$SystolicHyp, pred=prob.hyp.small.pred),5)
# 0.1325129


### FULL MODEL GAMMA
round(BrierScore(resp=df.total.sc$SystolicHyp, pred=prob.hyp.full.gamma.pred),5)
# 0.1324363

### SMALL MODEL GAMMA
round(BrierScore(resp=df.total.sc$SystolicHyp, pred=prob.hyp.small.gamma.pred),5)
# 0.1324363

### SAME PROB FOR ALL 
round(BrierScore(resp=df.total.sc$SystolicHyp, pred=equal.prob.mod),5)
# 0.1572847

############ Framingham 
##### Without age adjustment
round(BrierScore(resp=df.total.sc$SystolicHyp, pred=fram.risk),5)
# 0.1382371

#### With age adjustment
round(BrierScore(resp=df.total.sc$SystolicHyp, pred=fram.risk.ad.age),5)
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
hist(prob.obs.full.pred)


### SMALL MODEL

prob.obs.small.pred <- pnorm(df.total$SystolicBP3, mean=small.pred.mod$fitted.values, sd=small.sd.y)
hist(prob.obs.small.pred)


### FULL GAMMA
prob.obs.full.gamma.pred <- pgamma(df.total$SystolicBP3, shape=full.gamma.shape, rate=full.gamma.rate)
hist(prob.obs.full.gamma.pred)


### SMALL GAMMA
prob.obs.small.gamma.pred <- pgamma(df.total$SystolicBP3, shape=small.gamma.shape, rate=small.gamma.rate)
hist(prob.obs.small.gamma.pred)



########################## Sensitivity and Specificity  ############### 

### FULL MODEL
# Sensitivity
# Hypertensive and predicted hypertensive
round(100*sum(full.pred.mod$fitted.values>=140 & df.total$SystolicHyp)/sum(df.total$SystolicHyp),3)

# Specificity
# Not hypertensive and not predicted hypertensive
round(100*sum(full.pred.mod$fitted.values<140 & !df.total$SystolicHyp)/sum(!df.total$SystolicHyp),3)


### SMALL MODEL
# Sensitivity
# Hypertensive and predicted hypertensive
round(100*sum(small.pred.mod$fitted.values>=140 & df.total$SystolicHyp)/sum(df.total$SystolicHyp),3)

# Specificity
# Not hypertensive and not predicted hypertensive
round(100*sum(small.pred.mod$fitted.values<140 & !df.total$SystolicHyp)/sum(!df.total$SystolicHyp),3)


### FULL GAMMA
# Sensitivity
# Hypertensive and predicted hypertensive
round(100*sum(full.pred.mod.gamma$fitted.values>=140 & df.total$SystolicHyp)/sum(df.total$SystolicHyp),3)

# Specificity
# Not hypertensive and not predicted hypertensive
round(100*sum(full.pred.mod.gamma$fitted.values<140 & !df.total$SystolicHyp)/sum(!df.total$SystolicHyp),3)

### SMALL GAMMA
# Sensitivity
# Hypertensive and predicted hypertensive
round(100*sum(small.pred.mod.gamma$fitted.values>=140 & df.total$SystolicHyp)/sum(df.total$SystolicHyp),3)

# Specificity
# Not hypertensive and not predicted hypertensive
round(100*sum(small.pred.mod.gamma$fitted.values<140 & !df.total$SystolicHyp)/sum(!df.total$SystolicHyp),3)

### FRAMINGHAM
# Sensitivity
# Hypertensive and over 50% risk for hypertension
round(100*sum(fram.risk.ad.age>0.5&df.total$SystolicHyp)/sum(df.total$SystolicHyp),3)

# Specificity
# not hypertensive and under or equal to 50% risk for hypertension
round(100*sum(fram.risk.ad.age<=0.5 & !df.total$SystolicHyp)/sum(!df.total$SystolicHyp),3)




############################# C-stat ########################

cstat.full<-Cstat(full.pred.mod)

cstat.small <- Cstat(small.pred.mod)

cstat.full.gamma<- Cstat(full.pred.mod.gamma)  # biggest cstat

cstat.small.gamma <- Cstat(small.pred.mod.gamma)

max(cstat.full,cstat.small,cstat.full.gamma,cstat.small.gamma)




############################ ROC ##########################
plot(roc(as.numeric(df.total$SystolicHyp), as.numeric(full.pred.mod$fitted.values>=140)), main="full")

plot(roc(as.numeric(df.total$SystolicHyp), as.numeric(small.pred.mod$fitted.values>=140)), main="small")

plot(roc(as.numeric(df.total$SystolicHyp), as.numeric(full.pred.mod.gamma$fitted.values>=140)), main="full gamma")

plot(roc(as.numeric(df.total$SystolicHyp), as.numeric(small.pred.mod.gamma$fitted.values>=140)), main="small gamma")

plot(roc(as.numeric(df.total$SystolicHyp), as.numeric(fram.risk.ad.age>0.5)), main="Framingham")

#plot(roc(as.numeric(df.total$SystolicHyp), as.numeric(prob.hyp.full.pred>0.5)), main="full")



############################ AUC #############################

auc(as.numeric(df.total$SystolicHyp), as.numeric(full.pred.mod$fitted.values>=140))

auc(as.numeric(df.total$SystolicHyp), as.numeric(small.pred.mod$fitted.values>=140))

auc(as.numeric(df.total$SystolicHyp), as.numeric(full.pred.mod.gamma$fitted.values>=140))

auc(as.numeric(df.total$SystolicHyp), as.numeric(small.pred.mod.gamma$fitted.values>=140))

auc(as.numeric(df.total$SystolicHyp), as.numeric(fram.risk.ad.age>0.5))


########################### Hosmer-Lemeshow ##################


hoslem.test(as.numeric(df.total$SystolicHyp), as.numeric(full.pred.mod$fitted.values>=140), g=20)

hoslem.test(as.numeric(df.total$SystolicHyp), as.numeric(small.pred.mod$fitted.values>=140), g=20)

hoslem.test(as.numeric(df.total$SystolicHyp), as.numeric(full.pred.mod.gamma$fitted.values>=140), g=20)

hoslem.test(as.numeric(df.total$SystolicHyp), as.numeric(small.pred.mod.gamma$fitted.values>=140), g=20)

hoslem.test(as.numeric(df.total$SystolicHyp), as.numeric(fram.risk.ad.age>0.5), g=20)

# Model not well specified for any of the models since the p-value is below 0.05




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









