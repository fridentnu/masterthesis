#library(scales)


load("MyData/EDA.df.total.RData")

############################## STANDARDIZATION ###############################################
### Standardize the continuous variables to make it easier to compare effects
## subtract the mean and divide by the standard deviance

introduce(df.total)

df.total.sc <- df.total
df.total.sc[,c(2,4,5,6,13,14,15,17)] <- scale(df.total.sc[,c(2,4,5,6,13,14,15,17)])

plot_histogram(df.total.sc)

#describe(df.total.sc)

################################# MODELS ###########################################

full.pred.mod <- glm(SystolicBP3 ~ BirthYear + Sex + BMI2 + SystolicBP2 + DiastolicBP2 + 
                      PAI2 + RecPA2 + BPHigPar2 + Smoking2 + Cholesterol2 + HDLCholesterol2 +
                      Glucose2 + GFR2 + Creatinine2 + Education2, family= gaussian(link="identity"), data=df.total.sc)

full.pred.mod.gamma <- glm(SystolicBP3 ~ BirthYear + Sex + BMI2 + SystolicBP2 + DiastolicBP2 + 
                      PAI2 + RecPA2 + BPHigPar2 + Smoking2 + Cholesterol2 + HDLCholesterol2 +
                      Glucose2 + GFR2 + Creatinine2 + Education2, data=df.total.sc, family=Gamma(link = "identity"))


small.pred.mod <- glm(SystolicBP3 ~ BirthYear + BMI2 + SystolicBP2 + DiastolicBP2 + 
                      PAI2+ BPHigPar2 + HDLCholesterol2 + Education2, family=gaussian(link="identity"),data=df.total.sc)

small.pred.mod.gamma <- glm(SystolicBP3 ~ BirthYear + BMI2 + SystolicBP2 + DiastolicBP2 + 
                       PAI2+ BPHigPar2 + HDLCholesterol2 + Education2, data=df.total.sc, family=Gamma(link = "identity"))


## Not real models, but good for comparison

constant.pred.mod <- df.total$SystolicBP2

equal.prob.mod <- rep(sum(df.total$SystolicHyp)/length(df.total$PID), length(df.total$PID))


################################ SUMMARY #######################################
summary(full.pred.mod)
summary(full.pred.mod.gamma)
summary(small.pred.mod)
summary(small.pred.mod.gamma)
mean(constant.pred.mod)
# same residual standard error and adjusted r for full and small lm, 
# dispersion parameter for full gamma very slightly better
# AIC for small gamma sligthly smaller

################################# FITTED VALUES ######################################


small <-ggplot(data=small.pred.mod)+
  geom_histogram(aes(small.pred.mod$fitted.values), binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Fitted values, small model")


full <-ggplot(data=full.pred.mod)+
  geom_histogram(aes(full.pred.mod$fitted.values),binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Fitted values, full model")

small.gamma <-ggplot(data=small.pred.mod.gamma)+
  geom_histogram(aes(small.pred.mod.gamma$fitted.values), binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Fitted values, small model gamma")

full.gamma <-ggplot(data=full.pred.mod.gamma)+
  geom_histogram(aes(full.pred.mod.gamma$fitted.values),binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Fitted values, full model gamma")


res <-ggplot(data=df.total.sc)+
  geom_histogram(aes(df.total.sc$SystolicBP3), binwidth = 1)+
  coord_cartesian(xlim=c(0,200),ylim=c(0,800))+
  xlab("Observed systolic blood pressure HUNT3")

const <- ggplot(data=data.frame(constant.pred.mod))+
  geom_histogram(aes(constant.pred.mod), binwidth = 1)+
  coord_cartesian(xlim=c(0,200),ylim=c(0,800))+
  xlab("Observed systolic blood pressure HUNT2")

grid.arrange(res, const, small, full, small.gamma, full.gamma, nrow=3)



mean(df.total.sc$SystolicBP3)
mean(df.total$SystolicBP2)
mean(full.pred.mod$fitted.values)
mean(small.pred.mod$fitted.values)
mean(full.pred.mod.gamma$fitted.values)
mean(small.pred.mod.gamma$fitted.values)
# exactly equal mean for sysbp3 and full and small
# higher than sysbp2

##################################### Coefficients ###############################
df.coef.full <- summary(full.pred.mod)$coefficients
df.coef.small <- summary(small.pred.mod)$coefficients
df.coef.full.gamma <- summary(full.pred.mod.gamma)$coefficients
df.coef.small.gamma <- summary(small.pred.mod.gamma)$coefficients


# Full linear model
df.coef.full <- data.frame(round(df.coef.full[-1,-3], 3))
df.coef.full$ExplanatoryVariables <- row.names(df.coef.full)
df.coef.full <- df.coef.full[,c(4,1,2,3)]
colnames(df.coef.full)<-c("Exp. Variable","Estimate", "Std. Error", "p-value")
df.coef.full
write.csv(df.coef.full,"Tables/FullCoeff.csv", row.names = FALSE)


# Small linear model
df.coef.small <- data.frame(round(df.coef.small[-1,-3], 3))
df.coef.small$ExplanatoryVariables <- row.names(df.coef.small)
df.coef.small <- df.coef.small[,c(4,1,2,3)]
colnames(df.coef.small)<-c("Exp. Variable","Estimate", "Std. Error", "p-value")
df.coef.small
write.csv(df.coef.small,"Tables/SmallCoeff.csv", row.names = FALSE)

# Full gamma model
df.coef.full.gamma <- data.frame(round(df.coef.full.gamma[-1,-3], 3))
df.coef.full.gamma$ExplanatoryVariables <- row.names(df.coef.full.gamma)
df.coef.full.gamma <- df.coef.full.gamma[,c(4,1,2,3)]
colnames(df.coef.full.gamma)<-c("Exp. Variable","Estimate", "Std. Error", "p-value")
df.coef.full.gamma
write.csv(df.coef.full.gamma,"Tables/FullGammaCoeff.csv", row.names = FALSE)


# Small gamma model
df.coef.small.gamma <- data.frame(round(df.coef.small.gamma[-1,-3], 3))
df.coef.small.gamma$ExplanatoryVariables <- row.names(df.coef.small.gamma)
df.coef.small.gamma <- df.coef.small.gamma[,c(4,1,2,3)]
colnames(df.coef.small.gamma)<-c("Exp. Variable","Estimate", "Std. Error", "p-value")
df.coef.small.gamma
write.csv(df.coef.small.gamma,"Tables/SmallGammaCoeff.csv", row.names = FALSE)


################################### RESIDUALS ################################

###### Full model #######
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

plot.residual.full
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
plot.residual.small
# not homoscedastic variance, residuals increasing the further you get from the mean


###### Full gamma model #######

df.residual.full.gamma <- data.frame("Observed"=df.total.sc$SystolicBP3, "Residuals"=full.pred.mod.gamma$residuals)
plot.residual.full.gamma <- df.residual.full.gamma %>%
  mutate( bin=cut_width(x=Observed, width=10, boundary=0) ) %>%
  ggplot( aes(x=bin, y=Residuals )) +
  geom_boxplot()+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("Observed systolic blood pressure") + ylab("Residuals")+
  ggtitle("Full gamma")

plot.residual.full.gamma
# Not homoscedastic variance in residuals


#### Small gamma model 
df.residual.small.gamma <- data.frame("Observed"=df.total.sc$SystolicBP3, "Residuals"=small.pred.mod.gamma$residuals)
plot.residual.small.gamma <- df.residual.small.gamma %>%
  mutate( bin=cut_width(x=Observed, width=10, boundary=0) ) %>%
  ggplot( aes(x=bin, y=Residuals )) +
  geom_boxplot()+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("Observed systolic blood pressure") + ylab("Residuals")+
  ggtitle("Small gamma")
# Not homoscedastic variance in residuals
plot.residual.small.gamma


### Constant model 

df.residual.constant <- data.frame("Observed"=df.total.sc$SystolicBP3, "Residuals"=df.total$SystolicBP3-df.total$SystolicBP2)
plot.residual.constant <- df.residual.constant %>%
  mutate( bin=cut_width(x=Observed, width=10, boundary=0) ) %>%
  ggplot( aes(x=bin, y=Residuals )) +
  geom_boxplot()+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("Observed systolic blood pressure") + ylab("Residuals")+
  ggtitle("Constant")
# Not homoscedastic variance in residuals
plot.residual.constant

############## QQ plot
qqnorm(full.pred.mod$residuals, main="Normal QQ-plot of residuals", ylab="Residuals from full model")
qqline(full.pred.mod$residuals, col = "steelblue", lwd = 2)


qqnorm(small.pred.mod$residuals, main="Normal QQ-plot of residuals", ylab="Residuals from small model")
qqline(small.pred.mod$residuals, col = "steelblue", lwd = 2)

qqnorm(full.pred.mod.gamma$residuals, main="Normal QQ-plot of residuals", ylab="Residuals from full gamma model")
qqline(full.pred.mod.gamma$residuals, col = "steelblue", lwd = 2)


qqnorm(small.pred.mod.gamma$residuals, main="Normal QQ-plot of residuals", ylab="Residuals from small gamma model")
qqline(small.pred.mod.gamma$residuals, col = "steelblue", lwd = 2)

# for both models, predictions have heavier tails than normal dist. especially heavy right tail

###########  Residuals versus explanatory variables ###########

##### FULL MODEL #########

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



################################### Own calculations

# RMSE
sqrt(mean(residuals(full.pred.mod)**2))


## Check prediction variance
# Create design matrices
full.design.mat <- model.matrix(full.pred.mod)

# check that should use confidence
full.var.coeff <- var(full.pred.mod$residuals)*solve(t(full.design.mat)%*%full.design.mat)
full.pred.var1 <- t(full.design.mat[1,])%*%full.var.coeff%*%full.design.mat[1,]

full.pred.var <- rep(0,length(df.total$PID))

for(i in 1:length(df.total$PID)){
  full.pred.var[i] <- t(full.design.mat[i,])%*%full.var.coeff%*%full.design.mat[i,]
}
predict(full.pred.mod, se.fit=T)$se.fit**2 - full.pred.var

################# SAVE ########################################

save(df.total.sc, full.pred.mod, small.pred.mod, full.pred.mod.gamma, 
     small.pred.mod, equal.prob.mod, constant.pred.mod, file="MyData/Models.RData")
