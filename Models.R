




source("R code/EDA.R")
############################## STANDARDIZATION ###############################################
### Standardize the continuous variables to make it easier to compare effects
## subtract the mean and divide by the standard deviance

introduce(df.total)

df.total.sc <- df.total
df.total.sc[,c(2,4,5,6,11,12,13,15)] <- scale(df.total.sc[,c(2,4,5,6,11,12,13,15)])

plot_histogram(df.total.sc)

describe(df.total.sc)

full.pred.mod <- lm(SystolicBP3 ~ BirthYear + Sex + BMI2 + SystolicBP2 + DiastolicBP2 + 
                      PAI2 + RecPA2 + BPHigPar2 + Smoking2 + Cholestrol2 + HDLCholestrol2 +
                      Glucose2 + GFR2 + Creatinine2 + Education2, data=df.total.sc)

fit <-ggplot(data=full.pred.mod)+
  geom_histogram(aes(full.pred.mod$fitted.values), binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Predicted systolic blood pressure")

res <-ggplot(data=df.total.sc)+
  geom_histogram(aes(df.total.sc$SystolicBP3), binwidth = 1)+
  coord_cartesian(xlim=c(0,200),ylim=c(0,800))+
  xlab("Observed systolic blood pressure")
grid.arrange(res,fit,nrow=2)

