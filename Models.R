




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

diff.pred.mod <- lm(SysDiff ~ BirthYear + Sex + BMI2 + SystolicBP2 + DiastolicBP2 + 
                      PAI2 + RecPA2 + BPHigPar2 + Smoking2 + Cholesterol2 + HDLCholesterol2 +
                      Glucose2 + GFR2 + Creatinine2 + Education2, data=df.total.sc)

small.pred.mod <- lm(SystolicBP3 ~ BirthYear + BMI2 + SystolicBP2 + DiastolicBP2 + 
                      PAI2+ BPHigPar2 + HDLCholesterol2 + Education2, data=df.total.sc)

full.design.mat <- model.matrix(full.pred.mod)

# check that should use confidence
full.var.coeff <- var(full.pred.mod$residuals)*solve(t(full.design.mat)%*%full.design.mat)
full.pred.var1 <- t(full.design.mat[1,])%*%full.var.coeff%*%full.design.mat[1,]

summary(full.pred.mod)
summary(diff.pred.mod)
summary(small.pred.mod)
# same residual standard error, but diff has smaller multiple r squared
# ie. same residuals, but smaller degree of variance explained by the model in diff
# small model slightly higher adjusted r

full.predictions <- predict(full.pred.mod, interval="prediction")
# fordelingen til prediksjonen til alle sammen 
head(full.predictions)

diff.predictions <- predict(diff.pred.mod, interval="prediction")

small.predictions <- predict(small.pred.mod, interval="prediction")


## Standard error of fitted values
full.pred.se <- predict(full.pred.mod, se.fit=T)$se.fit
small.pred.se <- predict(small.pred.mod, se.fit=T)$se.fit
diff.pred.se <- predict(diff.pred.mod, se.fit=T)$se.fit


## Mean of standard deviation of fitted values
mean(full.pred.se)
mean(small.pred.se)
mean(diff.pred.se)

## Sigma, standard deviation of reisuduals
full.sigma <- sd(full.pred.mod$residuals)
diff.sigma <- sd(diff.pred.mod$residuals)
small.sigma <- sd(small.pred.mod$residuals)
# residual standard deviation is equal for all three models







################################# EVALUATION ######################################3

#### Predictions
# responses versus predictions
diff <-ggplot(data=diff.pred.mod)+
  geom_histogram(aes(diff.pred.mod$fitted.values+df.total$SystolicBP2), binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab(" Systolic blood pressure HUNT2 + predicted diff")

small <-ggplot(data=small.pred.mod)+
  geom_histogram(aes(small.pred.mod$fitted.values), binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Predicted systolic BP, small model")


fit <-ggplot(data=full.pred.mod)+
  geom_histogram(aes(full.pred.mod$fitted.values), binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Predicted systolic blood pressure")

res <-ggplot(data=df.total.sc)+
  geom_histogram(aes(df.total.sc$SystolicBP3), binwidth = 1)+
  coord_cartesian(xlim=c(0,200),ylim=c(0,800))+
  xlab("Observed systolic blood pressure")
grid.arrange(res,fit,diff,small,nrow=4)



## Similar mean, but much smaller variance
summary(df.total.sc$SystolicBP3)
summary(full.pred.mod$fitted.values)
summary(df.total$SystolicBP2)


### Coefficients

df.coef <- data.frame(full.pred.mod$coefficients)

df.coef
# gfr stage4 biggest after intercept, systolic, then birthyear, then diastolic, then parental hyp


# lettere å sjå med density eller boxplot
### Residuals
ggplot(data=full.pred.mod)+
  geom_point(mapping=aes(x=df.total$SystolicBP3, y=full.pred.mod$residuals))+
  labs(x="Observed systolic blood pressure", y= "Residuals")


# Absolute
ggplot(data=full.pred.mod)+
  geom_point(mapping=aes(x=df.total$SystolicBP2, y=abs(full.pred.mod$residuals)))+
  labs(x="Observed systolic blood pressure", y= "Absolute value of residuals")



# RMSE
sqrt(mean(residuals(full.pred.mod)**2))


### Accuracy fitted full model

pred.hyper <-full.pred.mod$fitted.values>=140
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


#################### Distribution of prediction full model

# variance and standard deviation for each participants response
full.var.y <- predict(full.pred.mod, se.fit=T)$se.fit**2 + predict(full.pred.mod, se.fit=T)$residual.scale**2
full.sd.y<- sqrt(full.var.y)

# probability that each systolic pressure is equal to or above 140 mmHg, given full model
prob.hyp.full.pred <-pnorm(140, mean=full.pred.mod$fitted.values, sd=full.sd.y, lower.tail = F)
prob.hyp.full.pred

# mean of probabilities of systolic hypertension
exp.prob.hyp.full.pred <- mean(1-pnorm(140, mean=full.pred.mod$fitted.values, sd=full.sd.y))
exp.prob.hyp.full.pred

### Prob hypertension

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
  
ggplot(data=df.pred.obs.sort)+
  geom_point(mapping=aes(x=Participant, y=Observed), alpha=0.4)+
  geom_point(mapping=aes(x=Participant, y=Fitted, col="blue"))+
  geom_line(mapping = aes(x=Participant, y=Lower,col="red"))+
  geom_line(mapping = aes(x=Participant, y=Upper,col="red"))+
  labs(x="Participants", y= "Systolic blood pressure")

df.pred.obs.sort %>%
  mutate( bin=cut_width(x=Participant, width=2000, boundary=0) ) %>%
  ggplot( aes(x=bin, y=Observed) ) +
  geom_boxplot()

above.upper <- 100*sum(df.pred.obs.sort$Observed>df.pred.obs.sort$Upper)/length(df.pred.obs.sort$Observed)
# only 3.83% above upper prediction interval limit


below.lower <- 100*sum(df.pred.obs.sort$Observed<df.pred.obs.sort$Lower)/length(df.pred.obs.sort$Observed)
# Only 1.35% below lower prediction interval limit





#### Small model

df.pred.obs.small <- data.frame("Observed"=df.total$SystolicBP3, "Fitted"=small.predictions[,1],
                          "Lower"=small.predictions[,2], "Upper"=small.predictions[,3])


df.pred.obs.small.sort <- df.pred.obs.small[order(df.pred.obs.small$Fitted),]
df.pred.obs.small.sort$Participant <- c(1:length(df.total.sort$PID))

ggplot(data=df.pred.obs.small.sort)+
  geom_point(mapping=aes(x=Participant, y=Observed), alpha=0.4)+
  geom_point(mapping=aes(x=Participant, y=Fitted, col="blue"))+
  geom_line(mapping = aes(x=Participant, y=Lower,col="red"))+
  geom_line(mapping = aes(x=Participant, y=Upper,col="red"))+
  labs(x="Participants", y= "Systolic blood pressure")

above.upper.small <- 100*sum(df.pred.obs.small.sort$Observed>df.pred.obs.small.sort$Upper)/length(df.pred.obs.small.sort$Observed)
above.upper.small
# only 3.82% above upper prediction interval limit


below.lower.small <- 100*sum(df.pred.obs.small.sort$Observed<df.pred.obs.small.sort$Lower)/length(df.pred.obs.small.sort$Observed)
below.lower.small
# Only 1.39% below lower prediction interval limit


