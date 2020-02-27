
library(scoringRules) # crps
library(DescTools) # brier score
library(MASS)



load("MyData/Models.RData")
load("MyData/Framingham.RData")



######################### DISTRIBUTIONS ###########################


### FULL MODEL
# Variance and standard deviation for each participants response
full.var.y <- predict(full.pred.mod, se.fit=T)$se.fit**2 + predict(full.pred.mod, se.fit=T)$residual.scale**2
full.sd.y<- sqrt(full.var.y)

# probability that each systolic pressure is equal to or above 140 mmHg
prob.hyp.full.pred <-pnorm(140, mean=full.pred.mod$fitted.values, sd=full.sd.y, lower.tail = F)
prob.hyp.full.pred


hist(prob.hyp.full.pred)

# mean of probabilities of systolic hypertension
exp.prob.hyp.full.pred <- mean(prob.hyp.full.pred)
exp.prob.hyp.full.pred
# Expected number of hypertensives are nearly 20% of population


### SMALL MODEL
# Variance and standard deviation for each participants response
small.var.y <- predict(small.pred.mod, se.fit=T)$se.fit**2 + predict(small.pred.mod, se.fit=T)$residual.scale**2
small.sd.y<- sqrt(small.var.y)

# probability that each systolic pressure is equal to or above 140 mmHg
prob.hyp.small.pred <-pnorm(140, mean=small.pred.mod$fitted.values, sd=small.sd.y, lower.tail = F)
prob.hyp.small.pred


hist(prob.hyp.small.pred)

# mean of probabilities of systolic hypertension
exp.prob.hyp.small.pred <- mean(prob.hyp.small.pred)
exp.prob.hyp.small.pred
# Expected number of hypertensives are nearly 20% of population 


### FULL GAMMA

full.gamma.var <- predict(full.pred.mod.gamma, se.fit = T)$se.fit**2 + predict(full.pred.mod.gamma, se.fit = T)$residual.scale**2
full.gamma.sd <- sqrt(full.gamma.var)


#### GJELDER DETTE FREMDELES NÅR DET ER DISPERSJONSPARAMETER??
full.gamma.shape= mean(full.pred.mod.gamma$fitted.values)**2/full.gamma.var
full.gamma.rate= mean(full.pred.mod.gamma$fitted.values)/full.gamma.var

prob.hyp.full.gamma.pred <-pgamma(140, shape=full.gamma.shape, rate=full.gamma.rate, lower.tail = F)

hist(prob.hyp.full.gamma.pred)


# can I use this function??
full.gamma.shape <- gamma.shape(full.pred.mod.gamma)

#prob.hyp.full.gamma.pred <-pgamma(140, shape=full.gamma.shape$alpha, scale=full.gamma.scale, lower.tail = F)



### SMALL GAMMA


########################## RMSE ########################################


### FULL MODEL
full.rmse <- predict(full.pred.mod, se.fit=T)$residual.scale
full.rmse

### SMALL MODEL
small.rmse <- predict(small.pred.mod, se.fit=T)$residual.scale
small.rmse


### FULL GAMMA 
full.gamma.rmse <- predict(full.pred.mod.gamma, se.fit=T)$residual.scale
full.gamma.rmse

### SMALL GAMMA
small.gamma.rmse <- predict(small.pred.mod.gamma, se.fit=T)$residual.scale
small.gamma.rmse

### MUCH SMALLER RMSE FOR GAMMA WHY??

### CONSTANT
constant.rmse <- sqrt(mean((df.total$SystolicBP3-df.total$SystolicBP2)**2))
constant.rmse


######################### CRPS ##########################################
# justert absoluttfeil

### FULL MODEL
full.crps <- crps(y=df.total.sc$SystolicBP3,family="normal", mean=full.pred.mod$fitted.values, sd=full.sd.y)
mean(full.crps)

df.crps <- data.frame("Observed"=df.total.sc$SystolicBP3, "CRPS"=full.crps)
df.crps[order(df.crps$Observed),]
plot(df.crps$Observed, df.crps$CRPS)
# ligner på absolute error av residuals

### SMALL MODEL
small.crps <- crps(y=df.total.sc$SystolicBP3,family="normal", mean=small.pred.mod$fitted.values, sd=small.sd.y)
mean(small.crps)


#### FULL GAMMA 
full.gamma.crps <- crps_gamma(y=df.total.sc$SystolicBP3,shape=2, rate=1) ##### NEED TO FIND PARAMETERS
mean(full.gamma.crps)


### SMALL GAMMA
small.gamma.crps <- crps_gamma(y=df.total.sc$SystolicBP3,shape=2, rate=1) ##### NEED TO FIND PARAMETERS
mean(small.gamma.crps)

######################## BRIER SCORE ###################################

### FULL MODEL
BrierScore(resp=df.total.sc$SystolicHyp, pred=prob.hyp.full.pred)
# 0.13254599
# the smaller the better, range between 0 and 1

### SMALL MODEL
BrierScore(resp=df.total.sc$SystolicHyp, pred=prob.hyp.small.pred)
# 0.1325129


### FULL MODEL GAMMA
#BrierScore(resp=df.total.sc$SystolicHyp, pred=prob.hyp.full.gamma.pred)


### SMALL MODEL GAMMA
#BrierScore(resp=df.total.sc$SystolicHyp, pred=prob.hyp.small.gamma.pred)


### SAME PROB FOR ALL 
BrierScore(resp=df.total.sc$SystolicHyp, pred=equal.prob.mod)
# 0.1572847

############ Framingham 
##### Without age adjustment
BrierScore(resp=df.total.sc$SystolicHyp, pred=fram.risk)
# 0.1382371

#### With age adjustment
BrierScore(resp=df.total.sc$SystolicHyp, pred=fram.risk.ad.age)
# 0.1347959

### Comment: the framingingham model with age adjustment had slightly better Brier score 


########################################## PIT DIAGRAM #######################################
# probability integral transform


### FULL MODEL

prob.obs.full.pred <- pnorm(df.total$SystolicBP3, mean=full.pred.mod$fitted.values, sd=full.sd.y)
hist(prob.obs.full.pred)


### SMALL MODEL

prob.obs.small.pred <- pnorm(df.total$SystolicBP3, mean=small.pred.mod$fitted.values, sd=small.sd.y)
hist(prob.obs.small.pred)


### FULL GAMMA

# need to find shape and scale parameters

### SMALL GAMMA


# need to find shape and scale parameters


######### Predictions ########


# #### Fitted values
# pred.hyper <-full.pred.mod$fitted.values>=140
# true.hyper <- df.total$SystolicHyp
# 
# 
# 100*sum(pred.hyper)/length(pred.hyper)
# # Only 5.4% predicted hypertensive
# 
# 100*sum(true.hyper)/length(true.hyper)
# # versus 19.6 % observed hypertenive
# 
# 
# # hypertensive in prediction and in observation
# 100*sum(pred.hyper & true.hyper)/length(true.hyper)
# 
# # hypertensive in pred but not in observation
# 100*sum(pred.hyper & !true.hyper)/length(true.hyper)
# # 2.4 %
# 
# 100*sum(!pred.hyper & true.hyper)/length(true.hyper)
# # 16.6% of hypertension not predicted
# 
# 
# ###### Expected hypertension from prediction distribution
# 
# # variance and standard deviation for each participants response
# full.var.y <- predict(full.pred.mod, se.fit=T)$se.fit**2 + predict(full.pred.mod, se.fit=T)$residual.scale**2
# full.sd.y<- sqrt(full.var.y)
# 
# # probability that each systolic pressure is equal to or above 140 mmHg, given full model
# prob.hyp.full.pred <-pnorm(140, mean=full.pred.mod$fitted.values, sd=full.sd.y, lower.tail = F)
# prob.hyp.full.pred
# 
# 
# hist(prob.hyp.full.pred)
# 
# # mean of probabilities of systolic hypertension
# exp.prob.hyp.full.pred <- mean(prob.hyp.full.pred)
# exp.prob.hyp.full.pred
# # Expected number of hypertensives are nearly 20% of population, stemmer dette? 
# 
# ###### Probability of hypertension with different cut-offs
# 
# # Observed systolic hypertension and predicted probability of hypertension over 30%
# hyp.pred.obs <- df.total$SystolicHyp  & prob.hyp.full.pred > 0.30
# 
# sum(hyp.pred.obs)/length(df.total$SystolicHyp)  
# # only 11% of the participants were both observed hypertensive and had more than 30% pred prob of hyp  
# 
# sum(hyp.pred.obs)/sum(df.total$SystolicHyp)  
# # 56% of the observed hypertensive had more than 30% pred prob of hyp
# 
# hyp.pred.not.obs <- (!df.total$SystolicHyp)  & prob.hyp.full.pred > 0.30
# 
# sum(hyp.pred.not.obs)/length(df.total$SystolicHyp)
# # 15% of the participants are non hypertensive and have more than 30% pred prob of hypertension
# 
# sum(hyp.pred.not.obs)/sum(!df.total$SystolicHyp) 
# # 19% of the nonhypertensive have more than 30 chance of hypertension
# 
# 
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









