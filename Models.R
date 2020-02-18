




source("R code/EDA.R")
############################## STANDARDIZATION ###############################################
### Standardize the continuous variables to make it easier to compare effects
## subtract the mean and divide by the standard deviance

introduce(df.total)

df.total.sc <- df.total
df.total.sc[,c(2,4,5,6,11,12,13,15)] <- scale(df.total.sc[,c(2,4,5,6,11,12,13,15)])

plot_histogram(df.total.sc)

describe(df.total.sc)

################################# MODEL ###########################################

full.pred.mod <- lm(SystolicBP3 ~ BirthYear + Sex + BMI2 + SystolicBP2 + DiastolicBP2 + 
                      PAI2 + RecPA2 + BPHigPar2 + Smoking2 + Cholestrol2 + HDLCholestrol2 +
                      Glucose2 + GFR2 + Creatinine2 + Education2, data=df.total.sc)

################################# EVALUATION ######################################3


# responses versus predictions
fit <-ggplot(data=full.pred.mod)+
  geom_histogram(aes(full.pred.mod$fitted.values), binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Predicted systolic blood pressure")

res <-ggplot(data=df.total.sc)+
  geom_histogram(aes(df.total.sc$SystolicBP3), binwidth = 1)+
  coord_cartesian(xlim=c(0,200),ylim=c(0,800))+
  xlab("Observed systolic blood pressure")
grid.arrange(res,fit,nrow=2)

## Similar mean, but much smaller variance
summary(df.total.sc$SystolicBP3)
summary(full.pred.mod$fitted.values)


## Residuals
ggplot(data=full.pred.mod)+
  geom_point(mapping=aes(x=df.total$SystolicBP3, y=full.pred.mod$residuals))+
  labs(x="Observed systolic blood pressure", y= "Residuals")


# Absolute
ggplot(data=full.pred.mod)+
  geom_point(mapping=aes(x=df.total$SystolicBP3, y=abs(full.pred.mod$residuals)))+
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

