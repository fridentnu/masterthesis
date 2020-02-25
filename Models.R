
library(scales)
library(scoringRules) # crps
library(DescTools) # brier score


source("R code/EDA.R")
############################## STANDARDIZATION ###############################################
### Standardize the continuous variables to make it easier to compare effects
## subtract the mean and divide by the standard deviance

introduce(df.total)

df.total.sc <- df.total
df.total.sc[,c(2,4,5,6,11,12,13,15)] <- scale(df.total.sc[,c(2,4,5,6,11,12,13,15)])

plot_histogram(df.total.sc)

describe(df.total.sc)

################################# MODELS ###########################################

full.pred.mod <- lm(SystolicBP3 ~ BirthYear + Sex + BMI2 + SystolicBP2 + DiastolicBP2 + 
                      PAI2 + RecPA2 + BPHigPar2 + Smoking2 + Cholesterol2 + HDLCholesterol2 +
                      Glucose2 + GFR2 + Creatinine2 + Education2, data=df.total.sc)
full.pred.mod.log <- lm(log(SystolicBP3) ~ BirthYear + Sex + BMI2 + SystolicBP2 + DiastolicBP2 + 
                      PAI2 + RecPA2 + BPHigPar2 + Smoking2 + Cholesterol2 + HDLCholesterol2 +
                      Glucose2 + GFR2 + Creatinine2 + Education2, data=df.total.sc)

full.pred.mod.gamma <- glm(SystolicBP3 ~ BirthYear + Sex + BMI2 + SystolicBP2 + DiastolicBP2 + 
                      PAI2 + RecPA2 + BPHigPar2 + Smoking2 + Cholesterol2 + HDLCholesterol2 +
                      Glucose2 + GFR2 + Creatinine2 + Education2, data=df.total.sc, family=Gamma(link = "identity"))


small.pred.mod <- lm(SystolicBP3 ~ BirthYear + BMI2 + SystolicBP2 + DiastolicBP2 + 
                      PAI2+ BPHigPar2 + HDLCholesterol2 + Education2, data=df.total.sc)


#### Summary of models
summary(full.pred.mod)
summary(full.pred.mod.gamma)
summary(full.pred.mod.log)
summary(small.pred.mod)
# same residual standard error, but diff has smaller multiple r squared
# ie. same residuals, but smaller degree of variance explained by the model in diff
# small model slightly higher adjusted r

######### Predictions ########

full.predictions <- predict(full.pred.mod, interval="prediction")
head(full.predictions)

small.predictions <- predict(small.pred.mod, interval="prediction")
head(small.predictions)

## Standard error of fitted values (sd/sqrt(n))
full.pred.se <- predict(full.pred.mod, se.fit=T)$se.fit
small.pred.se <- predict(small.pred.mod, se.fit=T)$se.fit


## Mean of standard error of fitted values
mean(full.pred.se)
mean(small.pred.se)
# mean of se is larger for full model which is as expected since 
# standard error increase with number of exp. variables


## Sigma, standard deviation of reisuduals
full.sigma <- sd(full.pred.mod$residuals)
full.sigma

small.sigma <- sd(small.pred.mod$residuals)
small.sigma
# residual standard deviation is slightly larger for small model
# as expected


################################# EVALUATION ######################################3

#### Predictions


small <-ggplot(data=small.pred.mod)+
  geom_histogram(aes(small.pred.mod$fitted.values), binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Fitted values, small model")


fit <-ggplot(data=full.pred.mod)+
  geom_histogram(aes(full.pred.mod$fitted.values),binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Fitted values, full model")

fit.gamma <-ggplot(data=full.pred.mod.gamma)+
  geom_histogram(aes(full.pred.mod.gamma$fitted.values),binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Fitted values, full model gamma")

fit.log <-ggplot(data=full.pred.mod.log)+
  geom_histogram(aes(exp(full.pred.mod.log$fitted.values)),binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Fitted values, full model log")
res <-ggplot(data=df.total.sc)+
  geom_histogram(aes(df.total.sc$SystolicBP3), binwidth = 1)+
  coord_cartesian(xlim=c(0,200),ylim=c(0,800))+
  xlab("Observed systolic blood pressure")

grid.arrange(res,fit,fit.gamma, fit.log,nrow=4)


## Similar mean, but much smaller variance
summary(df.total.sc$SystolicBP3)
summary(full.pred.mod$fitted.values)
summary(small.pred.mod$fitted.values)
summary(df.total$SystolicBP2)
# higher median than sysbp2 and sysbp3


mean(df.total.sc$SystolicBP3)
mean(full.pred.mod$fitted.values)
mean(small.pred.mod$fitted.values)
mean(df.total$SystolicBP2)
# exactly equal mean for sysbp3 and full and small
# higher than sysbp2

##################################### Coefficients ###############################
df.coef <- data.frame(full.pred.mod$coefficients)

df.coef
# intercept, systolic, then birthyear, then diastolic, then parental hyp, then bmi

################################### Residual plots ################################
###### Full model #####
ggplot(data=full.pred.mod)+
  geom_point(mapping=aes(x=df.total$SystolicBP3, y=full.pred.mod$residuals))+
  labs(x="Observed systolic blood pressure", y= "Residuals")

df.residual.full <- data.frame("Observed"=df.total.sc$SystolicBP3, "Residuals"=full.pred.mod$residuals)
plot.residual.full <- df.residual.full %>%
  mutate( bin=cut_width(x=Observed, width=10, boundary=0) ) %>%
  ggplot( aes(x=bin, y=Residuals )) +
  geom_boxplot()+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("Observed systolic blood pressure") + ylab("Residuals")+
  ggtitle("Full")


# Not homoscedastic variance in residuals


#### Small model 
df.residual.small <- data.frame("Observed"=df.total.sc$SystolicBP3, "Residuals"=small.pred.mod$residuals)
plot.residual.small <- df.residual.small %>%
                    mutate( bin=cut_width(x=Observed, width=10, boundary=0) ) %>%
                    ggplot( aes(x=bin, y=Residuals )) +
                    geom_boxplot()+
                    theme(axis.title.x = element_text(size=14),
                          axis.title.y = element_text(size=14),
                          axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
                          axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
                    xlab("Observed systolic blood pressure") + ylab("Residuals")+
                    ggtitle("Small")
# Not homoscedastic variance in residuals
grid.arrange(plot.residual.full, plot.residual.small, nrow=1)
# not homoscedastic variance, residuals increasing the further you get from the mean

#### QQ-plot of residual 
qqnorm(full.pred.mod$residuals, main="Normal QQ-plot of residuals", ylab="Residuals from full model")
qqline(full.pred.mod$residuals, col = "steelblue", lwd = 2)


qqnorm(small.pred.mod$residuals, main="Normal QQ-plot of residuals", ylab="Residuals from small model")
qqline(small.pred.mod$residuals, col = "steelblue", lwd = 2)

# for both models, predictions have heavier tails than normal dist. especially heavy right tail

###########  Residuals versus explanatory variables, full model 

df.residual.exp.var <- data.frame("SystolicBP2"=df.total.sc$SystolicBP2,
                                  "DiastolicBP2"=df.total.sc$DiastolicBP2,
                                  "Birthyear"=df.total.sc$BirthYear,
                                  "BMI"=df.total.sc$BMI2,
                                  "PAI"=df.total.sc$PAI2,
                                  "BPHigPar"=df.total.sc$BPHigPar2,
                                  "HDLCholesterol"=df.total.sc$HDLCholesterol2,
                                  "Residuals"=full.pred.mod$residuals)

plot.residual.sysBP2 <- df.residual.exp.var %>%
  mutate( bin=cut_width(x=SystolicBP2, width=0.5, boundary=0) ) %>%
  ggplot( aes(x=bin, y=Residuals )) +
  geom_boxplot()+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("Systolic blood pressure BP2") + ylab("Residuals")+
  ggtitle("Full")
grid.arrange(plot.residual.sysBP2, nrow=1)
# almost homoscedastic with zero mean, but more outliers for bigger systolic bp2

plot.residual.diaBP2 <- df.residual.exp.var %>%
  mutate( bin=cut_width(x=DiastolicBP2, width=0.5, boundary=0) ) %>%
  ggplot( aes(x=bin, y=Residuals )) +
  geom_boxplot()+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("Diastolic blood pressure BP2") + ylab("Residuals")+
  ggtitle("Full")
grid.arrange(plot.residual.diaBP2, nrow=1)
# almost homoscedastic with zero mean, 
# higher mean for low values
# but more outliers for bigger diastolic bp2

plot.residual.BMI <- df.residual.exp.var %>%
  mutate( bin=cut_width(x=BMI, width=0.5, boundary=0) ) %>%
  ggplot( aes(x=bin, y=Residuals )) +
  geom_boxplot()+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("BMI") + ylab("Residuals")+
  ggtitle("Full")
grid.arrange(plot.residual.BMI, nrow=1)
# almost homoscedastic with zero mean, 
# strange effects towrds upper end (where there are few people)

plot.residual.birthyear<- df.residual.exp.var %>%
  mutate( bin=cut_width(x=Birthyear, width=0.5, boundary=0) ) %>%
  ggplot( aes(x=bin, y=Residuals )) +
  geom_boxplot()+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("Birthyear") + ylab("Residuals")+
  ggtitle("Full")
grid.arrange(plot.residual.birthyear, nrow=1)
# almost homoscedastic with zero mean, 
# strange effects towrds upper end (where there are few people)

plot.residual.hdl<- df.residual.exp.var %>%
  mutate( bin=cut_width(x=HDLCholesterol, width=0.5, boundary=0) ) %>%
  ggplot( aes(x=bin, y=Residuals )) +
  geom_boxplot()+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("HDL Cholesterol") + ylab("Residuals")+
  ggtitle("Full")
grid.arrange(plot.residual.hdl, nrow=1)
# almost homoscedastic with zero mean, 
# strange effects towrds upper end (where there are few people)



ggplot(df.residual.exp.var)+
  geom_boxplot(mapping = aes(x=PAI, y=Residuals))

ggplot(df.residual.exp.var)+
  geom_boxplot(mapping = aes(x=BPHigPar, y=Residuals))


################################  HYPERTENSION PREDICTION ##############################################

#### Fitted values
pred.hyper <-full.pred.mod.log$fitted.values>=140
true.hyper <- df.total$SystolicHyp


100*sum(pred.hyper)/length(pred.hyper)
# Only 5.4% predicted hypertensive

100*sum(true.hyper)/length(true.hyper)
# versus 19.6 % observed hypertenive


# hypertensive in prediction and in observation
100*sum(pred.hyper & true.hyper)/length(true.hyper)

# hypertensive in pred but not in observation
100*sum(pred.hyper & !true.hyper)/length(true.hyper)
# 2.4 %

100*sum(!pred.hyper & true.hyper)/length(true.hyper)
# 16.6% of hypertension not predicted


###### Expected hypertension from prediction distribution

# variance and standard deviation for each participants response
full.var.y <- predict(full.pred.mod, se.fit=T)$se.fit**2 + predict(full.pred.mod, se.fit=T)$residual.scale**2
full.sd.y<- sqrt(full.var.y)

# probability that each systolic pressure is equal to or above 140 mmHg, given full model
prob.hyp.full.pred <-pnorm(140, mean=full.pred.mod$fitted.values, sd=full.sd.y, lower.tail = F)
prob.hyp.full.pred

# mean of probabilities of systolic hypertension
exp.prob.hyp.full.pred <- mean(prob.hyp.full.pred)
exp.prob.hyp.full.pred
# Expected number of hypertensives are nearly 20% of population, stemmer dette? 

###### Probability of hypertension with different cut-offs

# Observed systolic hypertension and predicted probability of hypertension over 30%
hyp.pred.obs <- df.total$SystolicHyp  & prob.hyp.full.pred > 0.30
  
sum(hyp.pred.obs)/length(df.total$SystolicHyp)  
# only 11% of the participants were both observed hypertensive and had more than 30% pred prob of hyp  
  
sum(hyp.pred.obs)/sum(df.total$SystolicHyp)  
# 56% of the observed hypertensive had more than 30% pred prob of hyp

hyp.pred.not.obs <- (!df.total$SystolicHyp)  & prob.hyp.full.pred > 0.30

sum(hyp.pred.not.obs)/length(df.total$SystolicHyp)
# 15% of the participants are non hypertensive and have more than 30% pred prob of hypertension

sum(hyp.pred.not.obs)/sum(!df.total$SystolicHyp) 
# 19% of the nonhypertensive have more than 30 chance of hypertension


### Prediction intervals full model

## Prøve å plotte prediksjonsintervall og observerte verdier
df.total.sort <- df.total[order(df.total$SystolicBP3),]

## observed systolic hypertension sorted
ggplot(data=full.pred.mod)+
  geom_point(mapping=aes(x=c(1:length(df.total.sort$PID)), y=df.total.sort$SystolicBP3))


# sort the fitted values
full.fit.val.sort <- full.pred.mod$fitted.values[order(full.pred.mod$fitted.values)]

## fitted systolic hypertension sorted
ggplot(data=full.pred.mod)+
  geom_point(mapping=aes(x=c(1:length(df.total.sort$PID)), y=full.fit.val.sort))


### Full model

df.pred.obs <- data.frame("Observed"=df.total$SystolicBP3, "Fitted"=full.predictions[,1],
                          "Lower"=full.predictions[,2], "Upper"=full.predictions[,3])


df.pred.obs.sort <- df.pred.obs[order(df.pred.obs$Fitted),]
df.pred.obs.sort$Participant <- c(1:length(df.total.sort$PID))

# how to change colors and keep legend?
ggplot(data=df.pred.obs.sort)+
  geom_point(mapping=aes(x=Participant, y=Observed, col="Observed values"),alpha=0.4)+
  geom_point(mapping=aes(x=Participant, y=Fitted, col="Fitted values"))+
  geom_line(mapping = aes(x=Participant, y=Lower,col="95% pred int"), size=1.5)+
  geom_line(mapping = aes(x=Participant, y=Upper,col="95% pred int"),size=1.5)+
  labs(x="Participants", y= "Systolic blood pressure [mmHg]", col=element_blank())

ggplot(data=df.pred.obs.sort)+
  geom_point(mapping=aes(x=Participant, y=Observed, col="Observed values"),alpha=0.4)+
  geom_point(mapping=aes(x=Participant, y=Fitted, col="Fitted values"))+
  geom_line(mapping = aes(x=Participant, y=Fitted-full.sd.y,col="Minus one sd"), size=1.5)+
  geom_line(mapping = aes(x=Participant, y=Fitted+full.sd.y,col="Plus one sd"),size=1.5)+
  labs(x="Participants", y= "Systolic blood pressure [mmHg]", col=element_blank())

# base plot
plot(df.pred.obs.sort$Participant, df.pred.obs.sort$Observed, type="p",pch=19, col=alpha("black",0.3), 
     xlab="Participants", ylab="Systolic blood pressure [mmHg]")+
  lines(df.pred.obs.sort$Participant, df.pred.obs.sort$Fitted, col="red")+
  lines(df.pred.obs.sort$Participant, df.pred.obs.sort$Lower, col="green")+
  lines(df.pred.obs.sort$Participant, df.pred.obs.sort$Upper, col="green")+
  legend("topright", legend=c("Observed values", "Fitted values", "95% lower pred int", "95% upper pred int "),
         col=c("black","red", "green", "green"),lty=1)


above.upper <- 100*sum(df.pred.obs.sort$Observed>df.pred.obs.sort$Upper)/length(df.pred.obs.sort$Observed)
above.upper
# only 3.83% above upper prediction interval limit


below.lower <- 100*sum(df.pred.obs.sort$Observed<df.pred.obs.sort$Lower)/length(df.pred.obs.sort$Observed)
below.lower
# Only 1.36% below lower prediction interval limit


#### Small model

df.pred.obs.small <- data.frame("Observed"=df.total$SystolicBP3, "Fitted"=small.predictions[,1],
                          "Lower"=small.predictions[,2], "Upper"=small.predictions[,3])


df.pred.obs.small.sort <- df.pred.obs.small[order(df.pred.obs.small$Fitted),]
df.pred.obs.small.sort$Participant <- c(1:length(df.total.sort$PID))



ggplot(data=df.pred.obs.small.sort)+
  geom_point(mapping=aes(x=Participant, y=Observed, col="Observed values"),alpha=0.4)+
  geom_point(mapping=aes(x=Participant, y=Fitted, col="Fitted values"))+
  geom_line(mapping = aes(x=Participant, y=Lower,col="95% pred int"), size=1.5)+
  geom_line(mapping = aes(x=Participant, y=Upper,col="95% pred int"),size=1.5)+
  labs(x="Participants", y= "Systolic blood pressure [mmHg]", col=element_blank())

above.upper.small <- 100*sum(df.pred.obs.small.sort$Observed>df.pred.obs.small.sort$Upper)/length(df.pred.obs.small.sort$Observed)
above.upper.small
# only 3.82% above upper prediction interval limit


below.lower.small <- 100*sum(df.pred.obs.small.sort$Observed<df.pred.obs.small.sort$Lower)/length(df.pred.obs.small.sort$Observed)
below.lower.small
# Only 1.39% below lower prediction interval limit


######################################################################################################
################################################# NOT IN USE #########################################


# diff.pred.mod <- lm(SysDiff ~ BirthYear + Sex + BMI2 + SystolicBP2 + DiastolicBP2 + 
#                       PAI2 + RecPA2 + BPHigPar2 + Smoking2 + Cholesterol2 + HDLCholesterol2 +
#                       Glucose2 + GFR2 + Creatinine2 + Education2, data=df.total.sc)

#summary(diff.pred.mod)

#diff.predictions <- predict(diff.pred.mod, interval="prediction")

diff.pred.se <- predict(diff.pred.mod, se.fit=T)$se.fit

mean(diff.pred.se)

diff.sigma <- sd(diff.pred.mod$residuals)


# responses versus predictions
diff <-ggplot(data=diff.pred.mod)+
  geom_histogram(aes(diff.pred.mod$fitted.values+df.total$SystolicBP2), binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab(" Systolic blood pressure HUNT2 + predicted diff")



################################## CRPS ##########################

full.crps <- crps(y=df.total.sc$SystolicBP3,family="normal", mean=full.pred.mod$fitted.values, sd=full.sd.y)

mean(full.crps)

df.crps <- data.frame("Observed"=df.total.sc$SystolicBP3, "CRPS"=full.crps)
df.crps[order(df.crps$Observed),]
plot(df.crps$Observed, df.crps$CRPS)
# ligner på absolute error av residuals



############################# Brier Score ####################

BrierScore(resp=df.total.sc$SystolicHyp, pred=prob.hyp.full.pred)
# the smaller the better, range between 0 and 1




################################### Own calculations

# RMSE
sqrt(mean(residuals(full.pred.mod)**2))


## Check prediction variance
# Create design matrices
full.design.mat <- model.matrix(full.pred.mod)

# check that should use confidence
full.var.coeff <- var(full.pred.mod$residuals)*solve(t(full.design.mat)%*%full.design.mat)
full.pred.var1 <- t(full.design.mat[1,])%*%full.var.coeff%*%full.design.mat[1,]
