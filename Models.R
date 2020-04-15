#library(scales)
library("Ecfun")

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



################################ PREDICTION DISTRIBUTION ##################

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

# Alternative. 
#full.var.y <- predict(full.pred.mod, se.fit=T)$se.fit**2 + predict(full.pred.mod, se.fit=T)$residual.scale**2
full.var.y <- full.exp.var + sum((residuals(full.pred.mod)-mean(residuals(full.pred.mod)))**2)/(length(df.total$PID)-1)
full.sd.y<- sqrt(full.var.y)



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
small.var.y <- small.exp.var + sum((residuals(small.pred.mod)-mean(residuals(small.pred.mod)))**2)/(length(df.total$PID)-1)
small.sd.y<- sqrt(small.var.y)

######## FULL GAMMA

full.gamma.shape <- 1/summary(full.pred.mod.gamma)$dispersion
full.gamma.shape

full.gamma.rate <- full.gamma.shape/full.pred.mod.gamma$fitted.values


###### SMALL GAMMA
small.gamma.shape <- 1/summary(small.pred.mod.gamma)$dispersion
small.gamma.shape

small.gamma.rate <- small.gamma.shape/small.pred.mod.gamma$fitted.values

################################# FITTED VALUES ######################################


small <-ggplot(data=small.pred.mod)+
  geom_histogram(aes(small.pred.mod$fitted.values), binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Fitted values, small Gaussian model")+
  ylab(" ")


full <-ggplot(data=full.pred.mod)+
  geom_histogram(aes(full.pred.mod$fitted.values),binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Fitted values, full Gaussian model")+
  ylab(" ")

small.gamma <-ggplot(data=small.pred.mod.gamma)+
  geom_histogram(aes(small.pred.mod.gamma$fitted.values), binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Fitted values, small gamma model")+
  ylab(" ")

full.gamma <-ggplot(data=full.pred.mod.gamma)+
  geom_histogram(aes(full.pred.mod.gamma$fitted.values),binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Fitted values, full gamma model")+
  ylab(" ")


res <-ggplot(data=df.total.sc)+
  geom_histogram(aes(df.total.sc$SystolicBP3), binwidth = 1)+
  coord_cartesian(xlim=c(0,200),ylim=c(0,800))+
  xlab("Observed systolic blood pressure HUNT3")+
  ylab(" ")

const <- ggplot(data=data.frame(constant.pred.mod))+
  geom_histogram(aes(constant.pred.mod), binwidth = 1)+
  coord_cartesian(xlim=c(0,200),ylim=c(0,800))+
  xlab("Observed systolic blood pressure HUNT2")+
  ylab(" ")

grid.arrange(const,res, small, full, small.gamma, full.gamma, nrow=3, left=textGrob("#Participants", gp=gpar(fontsize=20,font=8), rot=90))
dev.copy(pdf,'~/figures/Models/Present/FittedValues.pdf') # Save the plot
dev.off()


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

## Total linear model 
exp.var.full<- rownames(summary(full.pred.mod)$coefficients)
exp.var.small<- rownames(summary(small.pred.mod)$coefficients)
gauss.coeff <- data.frame("Exp.Variable"=exp.var.full,"FM Est"=round(df.coef.full[,1],3), "SM Est"=rep(NA, length(df.coef.full[,1])),
                                    "FM SD"=round(df.coef.full[,2],3),"SM SD"=rep(NA, length(df.coef.full[,1])), 
                                    "FM p-val"=round(df.coef.full[,4],3), "SM p-val"=rep(NA, length(df.coef.full[,1])))

gauss.coeff$SM.Est[c(1,2,4,5,6,7,8,10,14,19,20,21,22)]<- round(df.coef.small[,1],3)
gauss.coeff$SM.SD[c(1,2,4,5,6,7,8,10,14,19,20,21,22)]<- round(df.coef.small[,2],3)
gauss.coeff$SM.p.val[c(1,2,4,5,6,7,8,10,14,19,20,21,22)]<- round(df.coef.small[,4],3)
gauss.coeff
write.csv(gauss.coeff,"Tables/GaussCoeff.csv", row.names = FALSE)



## Total gamma
exp.var.full.gamma<- rownames(summary(full.pred.mod.gamma)$coefficients)
exp.var.small.gamma<- rownames(summary(small.pred.mod.gamma)$coefficients)
gamma.coeff <- data.frame("Exp.Variable"=exp.var.full.gamma,"FM Est"=round(df.coef.full.gamma[,1],3), 
                          "SM Est"=rep(NA, length(df.coef.full.gamma[,1])),
                          "FM SD"=round(df.coef.full.gamma[,2],3),"SM SD"=rep(NA, length(df.coef.full.gamma[,1])), 
                          "FM p-val"=round(df.coef.full.gamma[,4],3), "SM p-val"=rep(NA, length(df.coef.full.gamma[,1])))

gamma.coeff$SM.Est[c(1,2,4,5,6,7,8,10,14,19,20,21,22)]<- round(df.coef.small.gamma[,1],3)
gamma.coeff$SM.SD[c(1,2,4,5,6,7,8,10,14,19,20,21,22)]<- round(df.coef.small.gamma[,2],3)
gamma.coeff$SM.p.val[c(1,2,4,5,6,7,8,10,14,19,20,21,22)]<- round(df.coef.small.gamma[,4],3)
gamma.coeff
write.csv(gamma.coeff,"Tables/GammaCoeff.csv", row.names = FALSE)


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
predict(full.pred.mod, se.fit=T)$se.fit**2 - mean(full.pred.var)

################# SAVE ########################################

save(df.total.sc, full.pred.mod, small.pred.mod, full.pred.mod.gamma, 
     small.pred.mod, equal.prob.mod, constant.pred.mod, file="MyData/Models.RData")




##### NO LONGER IN USE 

# # Full linear model
# df.coef.full <- data.frame(round(df.coef.full[-1,-3], 3))
# df.coef.full$ExplanatoryVariables <- row.names(df.coef.full)
# df.coef.full <- df.coef.full[,c(4,1,2,3)]
# colnames(df.coef.full)<-c("Exp. Variable","Estimate", "Std. Error", "p-value")
# df.coef.full
# write.csv(df.coef.full,"Tables/FullCoeff.csv", row.names = FALSE)
# 
# 
# # Small linear model
# df.coef.small <- data.frame(round(df.coef.small[-1,-3], 3))
# df.coef.small$ExplanatoryVariables <- row.names(df.coef.small)
# df.coef.small <- df.coef.small[,c(4,1,2,3)]
# colnames(df.coef.small)<-c("Exp. Variable","Estimate", "Std. Error", "p-value")
# df.coef.small
# write.csv(df.coef.small,"Tables/SmallCoeff.csv", row.names = FALSE)
# 
# 
# 
# # Full gamma model
# df.coef.full.gamma <- data.frame(round(df.coef.full.gamma[-1,-3], 3))
# df.coef.full.gamma$ExplanatoryVariables <- row.names(df.coef.full.gamma)
# df.coef.full.gamma <- df.coef.full.gamma[,c(4,1,2,3)]
# colnames(df.coef.full.gamma)<-c("Exp. Variable","Estimate", "Std. Error", "p-value")
# df.coef.full.gamma
# write.csv(df.coef.full.gamma,"Tables/FullGammaCoeff.csv", row.names = FALSE)
# 
# 
# # Small gamma model
# df.coef.small.gamma <- data.frame(round(df.coef.small.gamma[-1,-3], 3))
# df.coef.small.gamma$ExplanatoryVariables <- row.names(df.coef.small.gamma)
# df.coef.small.gamma <- df.coef.small.gamma[,c(4,1,2,3)]
# colnames(df.coef.small.gamma)<-c("Exp. Variable","Estimate", "Std. Error", "p-value")
# df.coef.small.gamma
# write.csv(df.coef.small.gamma,"Tables/SmallGammaCoeff.csv", row.names = FALSE)


