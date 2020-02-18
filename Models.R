




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
                      PAI2 + RecPA2 + BPHigPar2 + Smoking2 + Cholestrol2 + HDLCholestrol2 +
                      Glucose2 + GFR2 + Creatinine2 + Education2, data=df.total.sc)

diff.pred.mod <- lm(SysDiff ~ BirthYear + Sex + BMI2 + SystolicBP2 + DiastolicBP2 + 
                      PAI2 + RecPA2 + BPHigPar2 + Smoking2 + Cholestrol2 + HDLCholestrol2 +
                      Glucose2 + GFR2 + Creatinine2 + Education2, data=df.total.sc)
small.pred.mod <- lm(SystolicBP3 ~ BirthYear + BMI2 + SystolicBP2 + DiastolicBP2 + 
                      PAI2+ BPHigPar2 + HDLCholestrol2 + Education2, data=df.total.sc)


summary(full.pred.mod)
summary(diff.pred.mod)
summary(small.pred.mod)
# same residual standard error, but diff has smaller multiple r squared
# ie. same residuals, but smaller degree of variance explained by the model in diff
# small model slightly higher adjusted r

full.predictions <- predict(full.pred.mod, se.fit=TRUE)
# fordelingen til prediksjonen til alle sammen 

diff.predictions <- predict(diff.pred.mod, se.fit=TRUE)

small.predictions <- predict(small.pred.mod, se.fit=TRUE)

mean(full.predictions$se.fit)
mean(diff.predictions$se.fit)
# same residuals

mean(small.predictions$se.fit)
#smaller residuals


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
  xlab(" predicted small model")


fit <-ggplot(data=full.pred.mod)+
  geom_histogram(aes(full.pred.mod$fitted.values), binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Predicted systolic blood pressure")

res <-ggplot(data=df.total.sc)+
  geom_histogram(aes(df.total.sc$SystolicBP3), binwidth = 1)+
  coord_cartesian(xlim=c(0,200),ylim=c(0,800))+
  xlab("Observed systolic blood pressure")
grid.arrange(res,fit,small,nrow=3)



## Similar mean, but much smaller variance
summary(df.total.sc$SystolicBP3)
summary(full.pred.mod$fitted.values)
summary(df.total$SystolicBP2)





pred.int.full <- predict(full.pred.mod, interval = "prediction")

# Fikse dette plottet
#ggplot(cbind(df.total.sc,pred.int.full))+
#  geom_point(aes(y=fit,x=df.total.sc$PID))+ 
#  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
#  geom_line(aes(y = upr), color = "red", linetype = "dashed")


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


# Accuracy

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




############################# Alternative model #######################

alt.pred.mod <- lm(SystolicBP3 ~ BirthYear + Sex + BMI2 + SystolicBP2  + 
                      PAI2 + RecPA2 + BPHigPar2 + Cholestrol2 + HDLCholestrol2 +
                      Glucose2 + GFR2 + Creatinine2, data=df.total.sc)


ggplot(data=alt.pred.mod)+
  geom_point(mapping=aes(x=df.total$SystolicBP3, y=abs(alt.pred.mod$residuals)))+
  labs(x="Observed systolic blood pressure", y= "Absolute value of residuals")

# RMSE
sqrt(mean(residuals(alt.pred.mod)**2))

# a little bit worse 
